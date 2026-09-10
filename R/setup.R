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
load_or_install(c("tidyverse", "digest", "openxlsx"))






# SET UP PROJECT FOLDERS --------
# Create standard folder structure for a FIFA tournament

setup_project_folders <- function() {
  folders <- c(
    "00_Admin",
    "01_Materials",
    "02_Data/sensitive",
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
  readme_path <- file.path("03_Analysis", "README.md")
  if (!file.exists(readme_path)) {
    writeLines("# Analysis Folder\n\nContains data, code, and results.", readme_path)
  }
  
  message("Folder structure created in: ", getwd())
}
#To run:
setup_project_folders()







# DEIDENTIFY --------
source("~/Library/CloudStorage/OneDrive-FIFA.org/Projects (FIFA)/Competition surveillance projects - current/fifa-surveillance-tools/03_Analysis/Code/deidentify.R")



# OUTPUT PATH  --------
make_output_path <- function(filename) {
  if (!dir.exists(output_dir)) dir.create(output_dir)
  file.path(output_dir, filename)
}

# Example: 
# output_dir <- "03_Analysis/Results
# project_name <- FWC2022
# make_output_path("table_1.xlsx") will save a file called FWC2022_table_1.xlsx in the folder called 03_Analysis/Results

