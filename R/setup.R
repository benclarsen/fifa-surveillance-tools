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


make_output_path <- function(filename, output_dir = get_setting("output_dir")) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  file.path(output_dir, filename)
}


# Write a result table to the competition's results folder.
write_result <- function(data, name) {
  
  path <- make_output_path(paste0(name, ".csv"))
  readr::write_csv(data, path, na = "")
  
  message("Wrote ", path)
  
  invisible(path)
}



## Configuration lookup ----

# Look up a value defined in the competition's config.R, falling back to a
# default. With no default, a missing setting is an error naming the setting.
get_setting <- function(name, default = NULL) {
  
  if (exists(name, envir = globalenv(), inherits = FALSE)) {
    return(get(name, envir = globalenv()))
  }
  
  if (is.null(default)) {
    stop("`", name, "` is not set. Define it in config.R.")
  }
  
  default
}



## Reading competition data ----

# Read a file from the competition's sensitive folder.
read_sensitive <- function(filename, delim = ";") {
  readr::read_delim(
    file.path(get_setting("dir_sensitive"), filename),
    delim  = delim,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
}


# Read a de-identified file.
read_deid <- function(filename, delim = ",") {
  readr::read_delim(
    file.path(get_setting("dir_deid"), filename),
    delim  = delim,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
}


# Source files write dates as dd/mm/yyyy, but readr sometimes recognises a date
# column on its own. Accept either.
parse_date_dmy <- function(x) {
  if (inherits(x, "Date")) return(x)
  lubridate::dmy(x)
}