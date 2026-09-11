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


## Project folders ----

# Create the standard folder structure for a new competition.
# Run once, by hand, when setting a competition up. Existing folders are left
# alone.
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


# Accept TRUE/FALSE written as a logical, as text, or as 0/1. Source files are
# inconsistent about this and a silent NA is worse than a guess.
parse_logical_loose <- function(x) {
  
  text <- tolower(as.character(x))
  
  dplyr::case_when(
    text %in% c("true",  "yes", "1") ~ TRUE,
    text %in% c("false", "no",  "0") ~ FALSE,
    TRUE ~ NA
  )
}


## Excluded teams ----

# Remove teams excluded from the health surveillance analysis.
#
# A team is excluded when it returned no health data at all, so its exposure
# would sit in the denominator with no possible numerator. Health surveillance
# only - the potential injury analysis observes every team from broadcast
# footage and is unaffected.
exclude_teams <- function(data,
                          teams  = get_setting("excluded_teams", character()),
                          column = "team") {
  
  if (length(teams) == 0) return(data)
  
  if (!column %in% names(data)) {
    stop("Cannot exclude teams: '", column, "' is not a column here. ",
         "Filter by player_id instead.")
  }
  
  filter(data, !.data[[column]] %in% teams)
}