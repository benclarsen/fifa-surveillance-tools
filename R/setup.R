# Setup and project utilities ----
#
# Shared helpers used by every competition surveillance project.
# This file defines functions only - nothing here runs when it is sourced.


## Packages ----

# Load a package if it is already installed, otherwise install it first.
load_or_install <- function(packages) {
  
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}


## Project folders ----

# Create the standard folder structure for a new competition.
# Run once, by hand, when setting a competition up. Existing folders are left alone.
setup_project_folders <- function(path = ".") {
  
  folders <- c(
    "00_Admin",
    "01_Materials",
    "02_Data/sensitive",
    "02_Data/deid",
    "03_Analysis/Code",
    "03_Analysis/Results",
    "04_Publications/Drafts",
    "04_Publications/Final",
    "05_Keys"
  )
  
  for (folder in folders) {
    dir.create(file.path(path, folder), recursive = TRUE, showWarnings = FALSE)
  }
  
  message("Folder structure created in: ", normalizePath(path))
}


## Output paths ----

# Build a path inside the competition's results folder, creating it if needed.
# `output_dir` is normally set once in the competition's config.R; pass it
# directly to override.
make_output_path <- function(filename, output_dir = NULL) {
  
  if (is.null(output_dir)) {
    
    if (!exists("output_dir", envir = globalenv())) {
      stop("`output_dir` is not set. Define it in config.R, or pass it to make_output_path().")
    }
    
    output_dir <- get("output_dir", envir = globalenv())
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  file.path(output_dir, filename)
}