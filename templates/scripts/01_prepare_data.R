# Prepare and de-identify source data ----
#
# Reads the source files from 02_Data/sensitive, de-identifies them, and writes
# de-identified copies to 02_Data/deid with keys in 05_Keys.
#
# Run this and 02_exposure.R once, from a clean session. They are the only
# scripts that touch identifiable data. Everything downstream reads 02_Data/deid.
#
# The objects left in memory here are still IDENTIFIED. Restart R before
# running run_all.R so that no identifiable data is in the session.
#
# When the competition is archived: move 05_Keys to the secure zone and delete
# 02_Data/sensitive.

source("config.R")


## Player details ----
#
# Age is computed here, from the identified data, and the date of birth is then
# dropped. A date of birth alongside a team is close to identifying on its own,
# and nothing downstream needs more than age. The exact date remains in the
# sensitive source file and is recoverable through the key.

player_details <- read_sensitive(file_player_details) %>%
  mutate(
    date_birth = parse_date_dmy(date_birth),
    age        = calculate_age(date_birth, start_date)
  ) %>%
  select(-date_birth)

if (any(is.na(player_details$age))) {
  warning(sum(is.na(player_details$age)),
          " players have no age. Check how date_birth is written in the source file.",
          call. = FALSE)
}

deidentify_and_save(player_details, "player_details")


## Caselist ----

caselist <- read_sensitive(file_caselist)

deidentify_and_save(caselist, "caselist")


## Match exposure ----

exposure_match <- read_sensitive(file_match_exposure)

deidentify_and_save(exposure_match, "exposure_match")


## Training exposure ----
#
# Handled in 02_exposure.R, which reshapes it, restricts it to each team's
# surveillance period, fills match-day warm-ups and imputes missing team-days
# before de-identifying the result.


## Potential injuries ----
#
# The analysts' file records events seen in broadcast footage. It contains
# player names and linking codes, none of which the analysis uses, so only the
# retained columns are carried forward. The result holds no personal data and
# needs no pseudonyms.
#
# The original stays in 02_Data/sensitive and goes when the competition is
# archived.

potential_injuries_raw <- read_sensitive(file_potential_injuries)

potential_injuries <- potential_injuries_raw %>%
  select_potential_injury_columns() %>%
  mutate(assessment_required_on_pitch = parse_logical_loose(assessment_required_on_pitch))

readr::write_csv(
  potential_injuries,
  file.path(dir_deid, "potential_injuries.csv"),
  na = ""
)

message("Wrote ", file.path(dir_deid, "potential_injuries.csv"), " - kept ",
        ncol(potential_injuries), " of ", ncol(potential_injuries_raw),
        " columns, dropping player names and codes.")
