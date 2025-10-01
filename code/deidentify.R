# This script de-identifies a dataset by replacing a unique identifier (e.g., IFES code)
# with a hashed version limited to 16 characters. It also creates a re-identification key
# saved as an Excel file named after the tournament.

# ----------------------------
# BEFORE YOU START:
# ----------------------------
# 1. Where is the identifiable file stored?
#    → Example: "data/raw/player_data.csv"
#
# 2. Where should the de-identified file be stored?
#    → Example: "data/processed/player_data_deid.csv"
#
# 3. Where should the re-identification key be stored?
#    → Example: "keys/reid_key_U17_WorldCup_2025.xlsx"
#
# 4. What is the name of the column that contains the unique ID?
#    → Example: "IFES_code"

# ----------------------------
# FUNCTION: deidentify
# ----------------------------
deidentify <- function(data, id_var, key_path = NULL, hash_algo = "sha256", hash_length = 16) {
  # Check if the ID column exists
  if (!id_var %in% names(data)) {
    stop(paste("Column", id_var, "not found in the dataset."))
  }
  
  # Load required packages
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required. Please install it with install.packages('digest').")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Please install it with install.packages('openxlsx').")
  }
  
  # Get all unique IDs from the specified column
  unique_ids <- unique(data[[id_var]])
  
  # Create hashed (anonymized) versions of the IDs, truncated to desired length
  hashed_ids <- vapply(unique_ids, function(x) {
    substr(digest::digest(x, algo = hash_algo), 1, hash_length)
  }, FUN.VALUE = character(1))
  
  # Create a re-identification key (original ID → hashed ID)
  key <- data.frame(
    original_id = unique_ids,
    hashed_id = hashed_ids,
    stringsAsFactors = FALSE
  )
  
  # Replace the original IDs in the dataset with the hashed versions
  data[[id_var]] <- key$hashed_id[match(data[[id_var]], key$original_id)]
  
  # Save the re-identification key as an Excel file
  if (!is.null(key_path)) {
    openxlsx::write.xlsx(key, file = key_path, overwrite = TRUE)
  }
  
  # Return both the de-identified data and the key
  return(list(data = data, key = key))
}
