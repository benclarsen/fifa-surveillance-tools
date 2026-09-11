# Run notes ----
#
# Collects notes as a script runs and writes them to text files alongside the
# results. The point is that the exclusions, data-quality flags and settings
# behind a set of results are recorded with them, rather than scrolling past in
# a console and being gone.
#
# Use note() in place of message(), and note_table() in place of print().
#
# Two files are written for each script:
#   notes_<name>.txt        everything, including the tables - the working record
#   notes_<name>_brief.txt  the narrative lines only - what the report appendix
#                           reproduces, and free of case-level detail


.notes <- new.env(parent = emptyenv())


## Collecting ----

# Start a fresh set of notes. Call once at the top of a script.
start_run_notes <- function() {
  .notes$lines <- character()
  .notes$brief <- character()
  invisible(NULL)
}


# Print a line to the console and record it in both the full and brief notes.
note <- function(...) {
  
  text <- paste0(...)
  
  message(text)
  
  if (is.null(.notes$lines)) .notes$lines <- character()
  if (is.null(.notes$brief)) .notes$brief <- character()
  
  .notes$lines <- c(.notes$lines, text)
  .notes$brief <- c(.notes$brief, text)
  
  invisible(text)
}


# Print a table to the console and record it in the full notes only.
#
# The caption goes into both, so the brief notes still say what was checked
# without reproducing the rows. Those rows are case-level - team, date,
# diagnosis code, days lost - and belong in the working record, not in a
# document that circulates.
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
                                       "start_date", "excluded_teams",
                                       "min_n_severity", "min_n_iqr",
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
  
  header <- c(
    paste0("Run notes: ", get_setting("competition_name", "competition")),
    paste0("Written: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("R version: ", R.version.string),
    strrep("-", 72),
    ""
  )
  
  full  <- make_output_path(paste0(name, ".txt"))
  brief <- make_output_path(paste0(name, "_brief.txt"))
  
  writeLines(c(header, .notes$lines), full)
  writeLines(c(header, .notes$brief), brief)
  
  message("Wrote ", full, " and ", brief)
  
  invisible(c(full = full, brief = brief))
}


## Suggested methods text ----

# A draft methods section assembled from the settings actually in force.
#
# This is a starting point, not a finished passage. Anything it cannot know
# from config.R is marked [CHECK] - figures that come from the run notes, and
# decisions that vary by competition.
#
# Returns a character vector of paragraphs.
methods_text <- function() {
  
  excluded    <- get_setting("excluded_teams", character())
  recode      <- get_setting("body_area_recode", tibble::tibble())
  warmup      <- get_setting("match_day_warmup_minutes", 30)
  imputations <- get_setting("n_imputations", 20)
  replicates  <- get_setting("boot_replicates", 10000)
  
  exclusion_sentence <- if (length(excluded) > 0) {
    paste0(" Teams that returned no health data and did not respond to follow-up ",
           "were excluded from the analysis entirely, contributing neither cases ",
           "nor exposure: ", paste(excluded, collapse = ", "), ".")
  } else {
    ""
  }
  
  recode_sentence <- if (nrow(recode) > 0) {
    paste0(
      "OSIICS 16 combines the hip and groin into a single body area, whereas the ",
      "football-specific extension of the IOC consensus statement recommends ",
      "reporting them separately (Walden et al., 2023). The following codes were ",
      "therefore assigned to hip or groin individually, on the basis of all ",
      "clinical information available for each case: ",
      paste0(recode$osiics_16_code, " (", tolower(recode$osiics_16_level_1), ")",
             collapse = ", "),
      ". This reassignment applies to the present analysis only; alignment of the ",
      "OSIICS classification with the consensus statement recommendations remains ",
      "an outstanding methodological issue."
    )
  } else {
    ""
  }
  
  c(
    paste0(
      "**Participants.** All players in the squads at the ",
      get_setting("competition_name", "competition"),
      " were invited to take part. Only players who gave written informed consent ",
      "were included, and both cases and exposure were restricted to them.",
      exclusion_sentence
    ),
    
    paste0(
      "**Surveillance period.** [CHECK] Health problems and exposure were recorded ",
      "for each team from their arrival at the competition until their final match. ",
      "Each team's period was taken from the first date on which they recorded ",
      "training exposure; state the observed range of days before the first match, ",
      "which is given in the exposure run notes."
    ),
    
    paste0(
      "**Case definitions.** [CHECK] Injuries and illnesses were recorded where they ",
      "resulted in time loss from training or match play, and mental health problems ",
      "where they led to medical attention. Exacerbations of problems already ",
      "recorded were excluded, to avoid counting the same problem twice. Cases ",
      "arising outside training and match play have no applicable exposure ",
      "denominator; they were excluded from all rates and are reported separately."
    ),
    
    paste0(
      "**Classification.** Health problems were classified using the FIFA version of ",
      "the Orchard Sports Injury and Illness Classification System, version 16 ",
      "(OSIICS 16), by body area or organ system, tissue type, pathology type and ",
      "specific diagnosis. ",
      recode_sentence
    ),
    
    paste0(
      "**Exposure.** Match exposure was taken from official playing-time records. ",
      "Training exposure was reported by each team for each player on each day of ",
      "the surveillance period. Where a team recorded no training exposure on a ",
      "match day, ", warmup, " minutes were assigned to represent the pre-match ",
      "warm-up, which counts as training exposure. Remaining missing values were ",
      "imputed at team-day level by predictive mean matching on team and day of ",
      "competition, with ", imputations, " imputations; the value used for each ",
      "team-day was the mean across all of them. [CHECK] Report the range of total ",
      "training hours across the imputations, which is given in the exposure run notes."
    ),
    
    paste0(
      "**Statistical analysis.** Injury incidence was expressed as cases per 1000 ",
      "hours of exposure, and illness and mental health incidence as cases per 1000 ",
      "player-days, each with an exact Poisson 95% confidence interval. Burden was ",
      "expressed as days lost per 1000 exposure units, with 95% confidence intervals ",
      "from a percentile bootstrap of ", format(replicates, big.mark = ","),
      " resamples. Time loss was summed within players before resampling, so that ",
      "players rather than cases were resampled - two cases in the same player are ",
      "not independent. Analyses were carried out in ", R.version.string, "."
    ),
    
    paste0("**Confidentiality.** ", suppression_footnote())
  )
}