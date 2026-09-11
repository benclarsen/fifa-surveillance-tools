# Run the example ----

## About ----

# Builds a throwaway competition project from templates/scripts, points it at
# the synthetic data in examples/data, and runs the pipeline end to end.
#
# Two purposes. It shows anyone who clones this repository how a competition
# project is assembled and run. And it is a regression test: if a change to the
# tools breaks a step, this catches it before a real competition does.
#
# Run examples/make_example_data.R first.
#
# Everything is written to examples/example-competition, which is rebuilt from
# scratch on every run. That folder is in .gitignore - it is output, not
# source.
#
# The plain-language report template is not exercised here. It still reads
# overall_results.csv, which the current pipeline does not write, and is
# awaiting rebuild.


## Settings ----

tools_dir <- getwd()

example_dir  <- file.path(tools_dir, "examples", "data")
template_dir <- file.path(tools_dir, "templates", "scripts")
example_root <- file.path(tools_dir, "examples", "example-competition")

run_report <- TRUE   # set FALSE to skip 05_report.R, which needs pandoc

competition_code <- "EXC2024"


## Check the example data is there ----

if (!dir.exists(example_dir) || length(list.files(example_dir)) == 0) {
  stop("No example data found in examples/data. Run examples/make_example_data.R first.")
}


## Build a clean project ----

unlink(example_root, recursive = TRUE)

project_folders <- c(
  "01_Materials",
  "02_Data/sensitive",
  "02_Data/deid",
  "03_Analysis/Code",
  "03_Analysis/Results",
  "04_Publications/Drafts",
  "05_Keys"
)

for (folder in project_folders) {
  dir.create(file.path(example_root, folder), recursive = TRUE, showWarnings = FALSE)
}

file.copy(
  list.files(template_dir, pattern = "^0[1-5]_.*\\.R$", full.names = TRUE),
  file.path(example_root, "03_Analysis", "Code"),
  overwrite = TRUE
)

file.copy(
  file.path(template_dir, "run_all.R"),
  example_root,
  overwrite = TRUE
)

file.copy(
  list.files(example_dir, full.names = TRUE),
  file.path(example_root, "02_Data", "sensitive"),
  overwrite = TRUE
)


## Write the example configuration ----

# Written here rather than copied from the template, because the template's
# tools_dir is relative to a project sitting beside the tools folder and this
# one does not.

config_lines <- c(
  "# Example competition configuration ----",
  "#",
  "# Written by examples/run_example.R. Do not edit - it is overwritten on",
  "# every run.",
  "",
  "",
  "## Competition ----",
  "",
  'competition_code <- "EXC2024"',
  'competition_name <- "Example Cup 2024"',
  "",
  'start_date <- as.Date("2024-06-01")',
  "",
  "# TMO is excluded so the exclusion path is exercised. TMP is kept, and has",
  "# no cases at all, so the participation table has to separate teams invited",
  "# from teams contributing.",
  "",
  'excluded_teams <- c("TMO")',
  "",
  "",
  "## Shared tools ----",
  "",
  sprintf('tools_dir <- "%s"', tools_dir),
  "",
  'source(file.path(tools_dir, "load_tools.R"))',
  "",
  "",
  "## Folders ----",
  "",
  'dir_sensitive <- "02_Data/sensitive"',
  'dir_deid      <- "02_Data/deid"',
  'dir_keys      <- "05_Keys"',
  'output_dir    <- "03_Analysis/Results"',
  "",
  "",
  "## Source files ----",
  "",
  'file_player_details     <- "EXC2024_player_details.csv"',
  'file_caselist           <- "caselist_EXC2024.csv"',
  'file_match_exposure     <- "playing_times_EXC2024.csv"',
  'file_training_exposure  <- "exposure_training.csv"',
  'file_surveillance       <- "surveillance_periods.csv"',
  'file_potential_injuries <- "potential_injuries_EXC2024.csv"',
  "",
  "",
  "## Body area recoding ----",
  "",
  "# GAX, Hip/groin arthritis, is deliberately left out, so the check that",
  "# reports codes still needing assignment stays exercised.",
  "",
  "body_area_recode <- tibble::tribble(",
  "  ~osiics_16_code, ~osiics_16_level_1,",
  '  "GM8",           "Groin",',
  '  "GJX",           "Groin",',
  '  "GM1",           "Hip",',
  '  "GS1",           "Groin"',
  ")",
  "",
  "",
  "## Reporting thresholds ----",
  "",
  "min_n_severity  <- 2",
  "min_n_iqr       <- 5",
  "min_n_diagnosis <- 5",
  "",
  "",
  "## Figures ----",
  "",
  "matrix_top_n <- 7",
  "",
  "# Empty so the example runs on any machine, installed fonts or not.",
  'figure_font  <- ""',
  "",
  "",
  "## Analysis settings ----",
  "",
  "# Fewer bootstrap draws than a real competition, to keep the example quick.",
  "boot_replicates <- 2000",
  "",
  "random_seed     <- 2024",
  "",
  "n_imputations            <- 20",
  "match_day_warmup_minutes <- 30"
)

writeLines(config_lines, file.path(example_root, "config.R"))


## Run the pipeline ----

# Everything below runs inside a function so that on.exit() restores the
# working directory whatever happens. At the top level of a script, on.exit()
# has no frame to attach to and silently does nothing.

run_example_pipeline <- function() {
  
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  setwd(example_root)
  
  run_step <- function(label, path) {
    
    cat("\n--", label, "--\n")
    
    warnings_seen <- character(0)
    started <- Sys.time()
    
    status <- tryCatch(
      withCallingHandlers(
        {
          source(path)
          "OK"
        },
        warning = function(w) {
          warnings_seen <<- c(warnings_seen, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) paste("FAILED -", conditionMessage(e))
    )
    
    elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")))
    
    cat("   ", status, " (", elapsed, "s)\n", sep = "")
    
    if (length(warnings_seen) > 0) {
      cat("    warnings:\n")
      cat(paste0("      - ", unique(warnings_seen)), sep = "\n")
    }
    
    status
  }
  
  steps <- c(
    "Prepare data"       = "03_Analysis/Code/01_prepare_data.R",
    "Exposure"           = "03_Analysis/Code/02_exposure.R",
    "Potential injuries" = "03_Analysis/Code/03_potential_injuries.R",
    "Analysis"           = "run_all.R",
    "Figures"            = "03_Analysis/Code/04_figures.R"
  )
  
  if (run_report) {
    steps <- c(steps, "Report" = "03_Analysis/Code/05_report.R")
  }
  
  cat("\nRunning the example pipeline in", example_root, "\n")
  
  results <- vapply(
    seq_along(steps),
    function(i) run_step(names(steps)[i], steps[[i]]),
    character(1)
  )
  
  names(results) <- names(steps)
  
  
  ## Checks ----
  
  cat("\n\n-- Checks --\n\n")
  
  problems <- character(0)
  
  expected_files <- c(
    "participation_table.csv", "player_characteristics_table.csv",
    "exposure_table.csv", "exposure_summary.csv", "basic_numbers_table.csv",
    "subsequent_table.csv", "injury_table.csv", "health_problems_table.csv",
    "cases_outside_training_and_matches.csv",
    "pattern_injury_all.csv", "pattern_tissue_all.csv",
    "body_area_summary.csv", "tissue_type_summary.csv",
    "severity_by_body_area.csv",
    "matrix_data_body_area.csv", "matrix_data_tissue_type.csv",
    "risk_matrix.png",
    "notes_exposure.txt", "notes_analysis.txt", "notes_figures.txt",
    "notes_potential_injuries.txt",
    "potential_injuries_summary.csv"
  )
  
  missing_files <- expected_files[
    !file.exists(file.path("03_Analysis/Results", expected_files))
  ]
  
  if (length(missing_files) > 0) {
    problems <- c(problems, paste("Result files not written:",
                                  paste(missing_files, collapse = ", ")))
  }
  
  if (!file.exists("02_Data/deid/caselist_analysis.csv")) {
    problems <- c(problems, "caselist_analysis.csv was not written")
  }
  
  # The four codes listed in the example config must have been reassigned. One
  # of them belongs to the case that occurred outside training and matches, so
  # three of the four reach the analysis caselist. GAX is expected to remain in
  # the combined category and is reported, not failed.
  
  if (file.exists("02_Data/deid/caselist_analysis.csv")) {
    
    analysis_cases <- readr::read_csv("02_Data/deid/caselist_analysis.csv",
                                      show_col_types = FALSE)
    
    recode_codes <- c("GM8", "GJX", "GM1", "GS1")
    recoded <- analysis_cases[analysis_cases$osiics_16_code %in% recode_codes, ]
    
    if (nrow(recoded) == 0) {
      problems <- c(problems,
                    "None of the hip and groin codes reached the analysis caselist")
    } else if (!all(recoded$osiics_16_level_1 %in% c("Groin", "Hip"))) {
      problems <- c(problems,
                    "The body area recode did not apply to every listed code")
    }
    
    unassigned <- sum(grepl("hip/groin", analysis_cases$osiics_16_level_1,
                            ignore.case = TRUE))
    
    cat("  Hip and groin codes reassigned:", nrow(recoded),
        "- expected 3 of 4, the fourth occurred outside training and matches\n")
    cat("  Cases left in the combined Hip/groin category:", unassigned,
        "- expected 1, code GAX, deliberately unassigned\n")
  }
  
  report_file <- file.path("04_Publications", "Drafts",
                           paste0(competition_code, "_surveillance_results.docx"))
  
  if (run_report && !file.exists(report_file)) {
    problems <- c(problems, "The Word report was not written")
  }
  
  
  ## Summary ----
  
  cat("\n")
  
  if (length(problems) == 0) {
    cat("  All checks passed.\n")
  } else {
    cat(paste0("  - ", problems), sep = "\n")
  }
  
  cat("\n\n-- Summary --\n\n")
  
  for (i in seq_along(results)) {
    cat(sprintf("  %-20s %s\n", names(results)[i], results[i]))
  }
  
  failed <- sum(!results %in% "OK") + length(problems)
  
  cat("\n")
  
  if (failed == 0) {
    cat("Example pipeline completed with no failures.\n")
  } else {
    cat(failed, "problem(s). See above.\n")
  }
  
  cat("\nResults are in", file.path(example_root, "03_Analysis", "Results"), "\n")
  
  invisible(list(results = results, problems = problems))
}

example_status <- run_example_pipeline()