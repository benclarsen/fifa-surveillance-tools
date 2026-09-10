


# ----------------------------
# FUNCTION: deidentify
# ----------------------------
# De-identify a dataset by hashing a unique identifier column and
# create a re-identification key saved to Excel with controllable naming.
# Optionally save the de-identified dataset to disk.
#
# Arguments:
#   data            : data.frame/tibble with an ID column (id_var)
#   id_var          : name of the ID column (character scalar)
#
#   # Key file options (Excel)
#   key_path        : OPTIONAL full path to the Excel key file (e.g., "keys/reid_key_U17_WorldCup_2025.xlsx")
#   key_dir         : OPTIONAL directory to store the key (e.g., "keys/")
#   key_filename    : OPTIONAL filename for the key (e.g., "reid_key_U17_WorldCup_2025.xlsx")
#   tournament_name : OPTIONAL string to auto-generate key file name if key_filename not provided
#   overwrite       : logical, allow overwriting existing KEY file (default TRUE)
#
#   # Hashing options
#   hash_algo       : digest algorithm (default "sha256")
#   hash_length     : number of characters to keep from hash (default 16)
#   salt            : OPTIONAL salt string to reduce cross-project linkability (default NULL)
#
#   # Saving the updated (de-identified) dataset
#   save_data_path   : OPTIONAL full path to write the de-identified dataset
#                      (e.g., "data/processed/player_data_deid.csv")
#   save_data_format : OPTIONAL one of "csv", "xlsx", "rds".
#                      If NULL, it will be inferred from save_data_path extension.
#   save_overwrite   : logical, allow overwriting existing DATA file (default TRUE)
#
# Returns:
#   list(
#     data     = deidentified_data (data.frame),
#     key      = reidentification_key (data.frame),
#     key_path = path used to save key (or NA if not saved),
#     data_path = path used to save deidentified dataset (or NA if not saved)
#   )
#



deidentify <- function(
    data,
    id_var,
    # key controls
    key_path = NULL,
    key_dir = NULL,
    key_filename = NULL,
    tournament_name = NULL,
    overwrite = TRUE,
    # hashing
    hash_algo = "sha256",
    hash_length = 16,
    salt = NULL,
    # saving data controls
    save_data_path = NULL,
    save_data_format = NULL,
    save_overwrite = TRUE
) {
  # ---- validation: columns & packages ----
  if (!id_var %in% names(data)) {
    stop(sprintf("Column '%s' not found in the dataset.", id_var))
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required. Install with install.packages('digest').")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install with install.packages('openxlsx').")
  }
  if (!is.numeric(hash_length) || length(hash_length) != 1 || hash_length <= 0) {
    stop("`hash_length` must be a single positive number.")
  }
  if (!is.character(id_var) || length(id_var) != 1) {
    stop("`id_var` must be a single column name (character).")
  }
  
  # ---- normalize ID column to character for stable hashing ----
  id_vals <- data[[id_var]]
  if (is.factor(id_vals)) id_vals <- as.character(id_vals)
  if (!is.character(id_vals)) id_vals <- as.character(id_vals)
  data[[id_var]] <- id_vals
  
  # ---- collect unique IDs ----
  unique_ids <- unique(data[[id_var]])
  
  # ---- hashing helper (with optional salt) ----
  hash_one <- function(x) {
    to_hash <- if (is.null(salt)) x else paste0(x, "::", salt)
    substr(digest::digest(to_hash, algo = hash_algo), 1, hash_length)
  }
  
  hashed_ids <- vapply(unique_ids, hash_one, FUN.VALUE = character(1))
  
  # ---- collision check within this run ----
  if (any(duplicated(hashed_ids))) {
    warning("Detected hash collisions within truncated hashes. Consider increasing `hash_length` or changing `salt`.")
  }
  
  # ---- build key table ----
  key <- data.frame(
    original_id = unique_ids,
    hashed_id   = hashed_ids,
    stringsAsFactors = FALSE
  )
  
  # ---- map hashed IDs into data ----
  data[[id_var]] <- key$hashed_id[match(data[[id_var]], key$original_id)]
  
  # ---- resolve KEY file path (Excel) ----
  if (!is.null(key_path)) {
    final_key_path <- key_path
  } else {
    # Determine directory
    if (is.null(key_dir)) key_dir <- "."
    if (!dir.exists(key_dir)) {
      dir.create(key_dir, recursive = TRUE, showWarnings = FALSE)
    }
    # Determine filename
    if (is.null(key_filename)) {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      base <- if (!is.null(tournament_name)) {
        paste0("reid_key_", gsub("[^A-Za-z0-9_\\-]", "_", tournament_name))
      } else {
        "reid_key"
      }
      key_filename <- paste0(base, "_", ts, ".xlsx")
    }
    final_key_path <- file.path(key_dir, key_filename)
  }
  
  # ---- write KEY Excel ----
  if (!is.null(final_key_path) && nzchar(final_key_path)) {
    if (file.exists(final_key_path) && !overwrite) {
      stop(sprintf("Key file '%s' exists and `overwrite = FALSE`.", final_key_path))
    }
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "key")
    openxlsx::writeData(wb, sheet = "key", x = key)
    
    meta <- data.frame(
      field = c("generated_at", "hash_algo", "hash_length", "salt_present", "id_var", "n_unique_ids"),
      value = c(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                hash_algo,
                hash_length,
                !is.null(salt),
                id_var,
                length(unique_ids)),
      stringsAsFactors = FALSE
    )
    openxlsx::addWorksheet(wb, "metadata")
    openxlsx::writeData(wb, sheet = "metadata", x = meta)
    
    openxlsx::saveWorkbook(wb, file = final_key_path, overwrite = TRUE)
  } else {
    final_key_path <- NA_character_
  }
  
  # ---- save the UPDATED (de-identified) DATA if requested ----
  final_data_path <- NA_character_
  if (!is.null(save_data_path) && nzchar(save_data_path)) {
    # Create parent dir if needed
    data_dir <- dirname(save_data_path)
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Infer format from extension if save_data_format not provided
    if (is.null(save_data_format)) {
      ext <- tolower(tools::file_ext(save_data_path))
      if (ext %in% c("csv", "xlsx", "rds")) {
        save_data_format <- ext
      } else {
        stop("Could not infer format from 'save_data_path'. Provide `save_data_format` as 'csv', 'xlsx', or 'rds'.")
      }
    } else {
      save_data_format <- tolower(save_data_format)
      if (!save_data_format %in% c("csv", "xlsx", "rds")) {
        stop("`save_data_format` must be one of: 'csv', 'xlsx', 'rds'.")
      }
    }
    
    if (file.exists(save_data_path) && !save_overwrite) {
      stop(sprintf("Data file '%s' exists and `save_overwrite = FALSE`.", save_data_path))
    }
    
    if (save_data_format == "csv") {
      utils::write.csv(data, file = save_data_path, row.names = FALSE, na = "")
    } else if (save_data_format == "xlsx") {
      wb2 <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb2, "data")
      openxlsx::writeData(wb2, "data", data)
      openxlsx::saveWorkbook(wb2, file = save_data_path, overwrite = TRUE)
    } else if (save_data_format == "rds") {
      saveRDS(data, file = save_data_path)
    }
    final_data_path <- save_data_path
  }
  
  # ---- return ----
  return(list(
    data = data,                 # the original dataset with the ID column replaced by hashed IDs
    key = key,                   # re-identification key (original_id -> hashed_id)
    key_path = final_key_path,   # where the key was saved (or NA)
    data_path = final_data_path  # where the updated dataset was saved (or NA)
  ))
}



