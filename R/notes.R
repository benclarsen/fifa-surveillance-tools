# Run notes ----
#
# Collects notes as a script runs and writes them to a text file alongside the
# results. The point is that the exclusions, data-quality flags and settings
# behind a set of results are recorded with them, rather than scrolling past in
# a console and being gone.
#
# Use note() in place of message(), and note_table() in place of print().


.notes <- new.env(parent = emptyenv())


## Collecting ----

# Start a fresh set of notes. Call once at the top of a script.
start_run_notes <- function() {
  .notes$lines <- character()
  invisible(NULL)
}


# Print a line to the console and record it.
note <- function(...) {
  
  text <- paste0(...)
  
  message(text)
  
  if (is.null(.notes$lines)) .notes$lines <- character()
  .notes$lines <- c(.notes$lines, text)
  
  invisible(text)
}


# Print a table to the console and record it.
note_table <- function(data, caption = NULL) {
  
  if (!is.null(caption)) note(caption)
  
  text <- utils::capture.output(print(data, n = Inf))
  
  message(paste(text, collapse = "\n"))
  
  if (is.null(.notes$lines)) .notes$lines <- character()
  .notes$lines <- c(.notes$lines, text, "")
  
  invisible(data)
}


# Record the settings in force, so a set of results can be tied to the
# thresholds and seeds that produced it.
note_settings <- function(settings = c("competition_code", "competition_name",
                                       "start_date", "min_n_severity", "min_n_iqr",
                                       "min_n_diagnosis", "boot_replicates",
                                       "random_seed", "n_imputations",
                                       "match_day_warmup_minutes")) {
  
  note("Settings in force")
  
  for (name in settings) {
    value <- get_setting(name, NA)
    note("  ", name, ": ", paste(format(value), collapse = ", "))
  }
  
  note("")
}


## Writing ----

write_run_notes <- function(name = "run_notes") {
  
  path <- make_output_path(paste0(name, ".txt"))
  
  header <- c(
    paste0("Run notes: ", get_setting("competition_name", "competition")),
    paste0("Written: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("R version: ", R.version.string),
    strrep("-", 72),
    ""
  )
  
  writeLines(c(header, .notes$lines), path)
  
  message("Wrote ", path)
  
  invisible(path)
}