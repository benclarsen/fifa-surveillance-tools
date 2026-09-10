# De-identification ----
#
# Replaces a direct identifier with a salted hash and writes a key file that
# allows re-identification.
#
# The result is PSEUDONYMISED, not anonymous. The de-identified files still
# carry team, dates and clinical detail, and the key reverses the process for
# anyone holding it. Keys live in 05_Keys and move to the secure zone when the
# competition is archived.
#
# The salt is generated once per competition and stored beside the keys. The
# same salt must be used for every file in a competition, or pseudonyms will
# not match between files and the files will not join. Without a salt, a hash
# of an enumerable player ID can be reversed by hashing every possible ID -
# which is why one is now used.


## Salt ----

# Read the competition's salt, creating it on first use.
get_competition_salt <- function(path = file.path(get_setting("dir_keys"), "salt.txt")) {
  
  if (file.exists(path)) {
    return(readLines(path, warn = FALSE)[1])
  }
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  salt <- if (requireNamespace("openssl", quietly = TRUE)) {
    paste(as.character(openssl::rand_bytes(32)), collapse = "")
  } else {
    set.seed(NULL)   # reseed from the clock and process id
    paste(sample(c(letters, LETTERS, 0:9), 40, replace = TRUE), collapse = "")
  }
  
  writeLines(salt, path)
  
  message("New de-identification salt written to: ", path,
          "\nKeep it with the keys. Losing it does not break re-identification, ",
          "but changing it makes new files unjoinable to old ones.")
  
  salt
}


## De-identify ----

# Replace `id_var` with a salted hash and write the key to Excel.
# Returns the de-identified data and the key. Writing the data itself is the
# caller's job.
deidentify <- function(data,
                       id_var,
                       key_path,
                       salt        = get_competition_salt(),
                       hash_algo   = "sha256",
                       hash_length = 16,
                       overwrite   = TRUE) {
  
  if (!is.character(id_var) || length(id_var) != 1) {
    stop("`id_var` must be a single column name.")
  }
  
  if (!id_var %in% names(data)) {
    stop("Column '", id_var, "' not found in the dataset.")
  }
  
  if (!is.numeric(hash_length) || length(hash_length) != 1 || hash_length <= 0) {
    stop("`hash_length` must be a single positive number.")
  }
  
  ids <- as.character(data[[id_var]])
  
  n_missing <- sum(is.na(ids))
  
  if (n_missing > 0) {
    warning(n_missing, " rows have no ", id_var,
            ". They are left missing rather than given a pseudonym.",
            call. = FALSE)
  }
  
  unique_ids <- unique(ids[!is.na(ids)])
  
  hashed <- vapply(
    unique_ids,
    function(x) substr(digest::digest(paste0(x, "::", salt), algo = hash_algo), 1, hash_length),
    character(1),
    USE.NAMES = FALSE
  )
  
  if (any(duplicated(hashed))) {
    stop("Hash collision at ", hash_length, " characters. Increase `hash_length`.")
  }
  
  key <- data.frame(
    original_id = unique_ids,
    hashed_id   = hashed,
    stringsAsFactors = FALSE
  )
  
  data[[id_var]] <- key$hashed_id[match(ids, key$original_id)]
  
  write_key_file(key, key_path, id_var, hash_algo, hash_length, overwrite)
  
  list(data = data, key = key, key_path = key_path)
}


## Key file ----

write_key_file <- function(key, path, id_var, hash_algo, hash_length, overwrite) {
  
  if (file.exists(path) && !overwrite) {
    stop("Key file '", path, "' exists and `overwrite = FALSE`.")
  }
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  metadata <- data.frame(
    field = c("generated_at", "competition", "id_var",
              "hash_algo", "hash_length", "n_unique_ids"),
    value = c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              get_setting("competition_code", "unknown"),
              id_var,
              hash_algo,
              as.character(hash_length),
              as.character(nrow(key))),
    stringsAsFactors = FALSE
  )
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "key")
  openxlsx::writeData(wb, "key", key)
  
  openxlsx::addWorksheet(wb, "metadata")
  openxlsx::writeData(wb, "metadata", metadata)
  
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  
  invisible(path)
}



## Standard de-identify and save ----

# De-identify a dataset and write both the de-identified copy and its key to
# the competition's standard locations.
deidentify_and_save <- function(data, name, id_var = "player_id") {
  
  result <- deidentify(
    data,
    id_var   = id_var,
    key_path = file.path(
      get_setting("dir_keys"),
      paste0(get_setting("competition_code"), "_", name, "_key.xlsx")
    )
  )
  
  dir.create(get_setting("dir_deid"), recursive = TRUE, showWarnings = FALSE)
  
  path <- file.path(get_setting("dir_deid"), paste0(name, ".csv"))
  readr::write_csv(result$data, path, na = "")
  
  message("Wrote ", path)
  
  invisible(result$data)
}