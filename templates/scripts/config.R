# Competition configuration ----

## About ----

# Every setting specific to this competition lives in this file. No other file
# in the project should contain a hard-coded path, date, filename or threshold.
#
# Source this first, in a clean session. run_all.R does that for you.
#
# To start a new competition, copy this whole project folder, rename it, and
# work down this file from the top. Settings marked SET are competition
# specific and must be changed. Settings marked CHECK have sensible defaults
# but should be reviewed.


## Competition ----

competition_code <- ""                      # SET  short code, e.g. "FAC2025"
competition_name <- ""                      # SET  full name as it appears in the report

start_date <- as.Date("")                   # SET  first day of the competition


## Excluded teams ----

# Teams removed from the health surveillance analysis, by team code.
#
# Use this only where a team's exposure would sit in the denominator with no
# possible numerator - for example where injuries were observed in their
# matches but none were ever reported, and follow-up confirmed nothing.
# Record the reason here, in this file, so the report can state it.
#
# Exclusions do not affect the potential injury analysis, which observes every
# team from broadcast footage.

excluded_teams <- character(0)               # CHECK  e.g. c("JOR", "KUW")


## Shared tools ----

# The shared library, assumed to sit beside this project folder. An absolute
# path works equally well. It is normalised here so it survives the working
# directory change that rmarkdown::render() makes in 05_report.R.

tools_dir <- normalizePath("../fifa-surveillance-tools", mustWork = TRUE)

source(file.path(tools_dir, "load_tools.R"))


## Folders ----

dir_sensitive <- "02_Data/sensitive"
dir_deid      <- "02_Data/deid"
dir_keys      <- "05_Keys"
output_dir    <- "03_Analysis/Results"


## Source files ----

# Names of the files supplied for this competition, in 02_Data/sensitive.
# These vary between competitions; nothing downstream should name a file.

file_player_details     <- ""                # SET
file_caselist           <- ""                # SET
file_match_exposure     <- ""                # SET
file_training_exposure  <- ""                # SET
file_surveillance       <- ""                # SET
file_potential_injuries <- ""                # SET


## Body area recoding ----

# OSIICS 16 assigns some diagnoses to a combined "Hip/groin" body area. The
# football-specific extension of the IOC consensus statement (Waldén et al.,
# 2023) recommends reporting hip and groin separately.
#
# Any codes listed here are reassigned for this competition only. Assign them
# by hand, case by case, on all the information available. This is a
# competition-level decision, not a change to the OSIICS reference table -
# aligning OSIICS itself with the consensus statement recommendations remains
# outstanding.
#
# Leave the table empty to apply no recoding.

body_area_recode <- tibble::tribble(
  ~osiics_16_code, ~osiics_16_level_1
)


## Reporting thresholds ----

# Disclosure control for published tables and figures.
#
# Counts and count-derived rates may be shown at any number of cases - they
# carry no information beyond the count, which is published anyway. Figures
# derived from individual players' time loss must not be, because exposure is
# published alongside them and the underlying days are recoverable.

min_n_severity  <- 2       # CHECK  median time loss and burden: at least this many cases
min_n_iqr       <- 5       # CHECK  interquartile range: at least this many cases
min_n_diagnosis <- 5       # CHECK  an individual diagnosis: at least this many cases


## Figures ----

matrix_top_n <- 7             # CHECK  categories plotted per panel, heaviest burden first
figure_font  <- "Open Sans"   # CHECK  set to "" to use the device default if not installed

# Burden isobars are chosen from the range in the data. To fix them instead,
# pass isobars = c(...) to plot_risk_matrix() in 04_figures.R.


## Analysis settings ----

boot_replicates <- 10000      # CHECK  bootstrap draws for burden confidence intervals
random_seed     <- 1          # SET    any fixed integer, so results are reproducible

n_imputations            <- 20   # CHECK  imputations drawn for missing training exposure
match_day_warmup_minutes <- 30   # CHECK  training exposure assigned to a match day with none recorded

# n_matches is derived from the match schedule in 03_potential_injuries.R.
# Uncomment and set it here to override if a competition's schedule does not
# list two teams per match.
# n_matches <- 32
