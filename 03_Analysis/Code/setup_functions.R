# INSTALL/LOAD PACKAGES ----
# loads package if already installed, otherwise installs then loads

load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# Load required packages for functions in this script
load_or_install(c("digest", "openxlsx"))






# SET UP PROJECT FOLDERS --------
# Create standard folder structure for a FIFA tournament

setup_project_folders <- function() {
  folders <- c(
    "00_Admin",
    "01_Materials",
    "02_Data/identifiable",
    "02_Data/deid",
    "03_Analysis/Code",
    "03_Analysis/Code/Preprocessing",
    "03_Analysis/Results",
    "04_Publications/Drafts",
    "04_Publications/Final",
    "05_Keys"
  )
  
  for (folder in folders) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create README in analysis folder
  readme_path <- file.path("04_Analysis", "README.md")
  if (!file.exists(readme_path)) {
    writeLines("# Analysis Folder\n\nContains data, code, and results.", readme_path)
  }
  
  message("Folder structure created in: ", getwd())
}

#To run:
#setup_project_folders()







# DEIDENTIFY --------
deidentify <- function(data, id_var, key_path = NULL, hash_algo = "sha256", hash_length = 16) {
  if (!id_var %in% names(data)) {
    stop(paste("Column", id_var, "not found in the dataset."))
  }
  
  unique_ids <- unique(data[[id_var]])
  
  hashed_ids <- vapply(unique_ids, function(x) {
    substr(digest::digest(x, algo = hash_algo), 1, hash_length)
  }, FUN.VALUE = character(1))
  
  key <- data.frame(
    original_id = unique_ids,
    hashed_id = hashed_ids,
    stringsAsFactors = FALSE
  )
  
  data[[id_var]] <- key$hashed_id[match(data[[id_var]], key$original_id)]
  
  if (!is.null(key_path)) {
    openxlsx::write.xlsx(key, file = key_path, overwrite = TRUE)
  }
  
  return(list(data = data, key = key))
}






# OUTPUT PATH  --------
make_output_path <- function(filename) {
  if (!dir.exists(output_dir)) dir.create(output_dir)
  file.path(output_dir, paste0(project_name, "_", filename))
}

# Example: 
# output_dir <- "03_Analysis/Results
# project_name <- FWC2022
# make_output_path("table_1.xlsx") will save a file called FWC2022_table_1.xlsx in the folder called 03_Analysis/Results

