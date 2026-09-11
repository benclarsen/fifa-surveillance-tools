# Run the full analysis ----
#
# Reads the de-identified data and writes every result file to 03_Analysis/Results.
# Run in a clean session, after 01_prepare_data.R and 02_exposure.R.
#
# Nothing here touches identifiable data, and nothing here is competition-
# specific - every setting comes from config.R.
#
# This script also writes the finished analysis caselist and the exposure row,
# which 04_figures.R reads. The caselist is built once, here, so the figures
# cannot be drawn from a different set of cases than the tables.

source("config.R")

start_run_notes()
note_settings()


## Load data ----

player_details_all <- read_deid("player_details.csv")
exposure_training  <- read_deid("exposure_training_daily.csv")
exposure_match     <- read_deid("exposure_match.csv")

osiics <- load_osiics()

caselist <- read_deid("caselist.csv") %>%
  mutate(
    date_incident = parse_date_dmy(date_incident),
    problem_type  = factor(problem_type, levels = problem_type_order)
  )


## Excluded teams ----
#
# Removed from the numerator and the denominator together. Exposure files with
# no team column are filtered by the players belonging to those teams.
#
# This affects the health surveillance only. The potential injury analysis
# observes every team from broadcast footage and keeps all of them.

excluded_players <- player_details_all %>%
  filter(team %in% excluded_teams) %>%
  distinct(player_id)

player_details <- exclude_teams(player_details_all)
caselist       <- exclude_teams(caselist)

exposure_training <- exclude_teams(exposure_training)
exposure_match    <- anti_join(exposure_match, excluded_players, by = "player_id")

note("Teams excluded from the health surveillance: ",
     paste(excluded_teams, collapse = ", "),
     " (", nrow(excluded_players), " players)")

note("Cases read from the de-identified caselist: ", nrow(caselist))


## Exclude exacerbations ----
#
# An exacerbation is a worsening of a problem already counted, so including it
# would double count. `%in%` rather than `!=` so that a case with no
# subsequent_cat is kept rather than silently dropped - `NA != "exacerbation"`
# is NA, and filter drops NA.

n_exacerbations <- sum(tolower(caselist$subsequent_cat) %in% "exacerbation")

caselist <- filter(caselist, !tolower(subsequent_cat) %in% "exacerbation")

note("Exacerbations excluded: ", n_exacerbations, ". Cases remaining: ", nrow(caselist))


## Attach the taxonomy ----
#
# Joined on the OSIICS code alone. Both files also carry problem_type, so an
# unspecified join would silently key on both and produce missing taxonomy
# wherever the two disagreed.

caselist <- caselist %>%
  left_join(
    select(
      osiics,
      osiics_16_code,
      problem_type_osiics = problem_type,
      osiics_16_level_1, osiics_16_level_2, osiics_16_level_3, osiics_16_level_4
    ),
    by = "osiics_16_code"
  )


## Body area recoding ----
#
# OSIICS 16 combines hip and groin into one body area. The football consensus
# statement (Walden et al., 2023) recommends reporting them separately, so the
# codes listed in config.R are reassigned by hand for this competition.

caselist <- apply_body_area_recode(caselist, body_area_recode)

note_table(
  body_area_recode,
  paste0("\nBody areas reassigned for this competition, following Walden et al. ",
         "(2023), which recommends separating hip from groin:")
)

n_recoded <- attr(caselist, "n_recoded")
note("Cases affected: ", if (is.null(n_recoded)) 0 else n_recoded)

combined_remaining <- sum(caselist$osiics_16_level_1 %in% "Hip/groin", na.rm = TRUE)

if (combined_remaining > 0) {
  note_table(
    caselist %>%
      filter(osiics_16_level_1 %in% "Hip/groin") %>%
      count(osiics_16_code, osiics_16_level_4),
    paste0(combined_remaining, " cases remain in the combined Hip/groin category ",
           "and need assigning:")
  )
} else {
  note("No cases remain in the combined Hip/groin category.")
}


## Checks ----

note("")

unmatched_codes <- caselist %>%
  filter(is.na(problem_type_osiics)) %>%
  count(osiics_16_code, problem_type)

if (nrow(unmatched_codes) > 0) {
  note_table(unmatched_codes,
             "OSIICS codes in the caselist that are not in the reference table:")
  warning(sum(unmatched_codes$n), " cases have no taxonomy and will be missing ",
          "from every pattern table.", call. = FALSE)
} else {
  note("All OSIICS codes matched the reference table.")
}

disagreements <- caselist %>%
  filter(!is.na(problem_type_osiics),
         as.character(problem_type) != as.character(problem_type_osiics)) %>%
  count(osiics_16_code, problem_type, problem_type_osiics)

if (nrow(disagreements) > 0) {
  note_table(disagreements,
             "Cases where the recorded problem type disagrees with the OSIICS code:")
}

if (any(is.na(caselist$problem_type))) {
  warning(sum(is.na(caselist$problem_type)),
          " cases have a problem_type outside Injury, Illness and Mental health problem.",
          call. = FALSE)
}

# Injuries with no onset recorded appear in the total but in neither the gradual
# nor the sudden row. By convention a missing when_occurred means gradual onset,
# so a case missing both fields is ambiguous and worth checking at source.
missing_onset <- filter(caselist, problem_type == "Injury", is.na(onset))

if (nrow(missing_onset) > 0) {
  note_table(
    select(missing_onset, team, date_incident, when_occurred, osiics_16_code, timeloss),
    paste0(nrow(missing_onset), " injuries have no onset recorded. Counted in the ",
           "total but in neither the gradual nor the sudden row:")
  )
}

caselist <- select(caselist, -problem_type_osiics)


## Cases outside training and matches ----
#
# `when_occurred == "Other"` covers cases that happened neither in training nor
# in a match - travel, leisure, and so on. There is no exposure denominator for
# them, so they are excluded from all rates and summary tables and reported
# separately in the text of the report.
#
# A missing when_occurred means gradual onset and is retained.

other_setting <- filter(caselist, when_occurred %in% "Other")

if (nrow(other_setting) > 0) {
  
  note_table(
    other_setting %>% count(problem_type, osiics_16_level_1, osiics_16_level_3, timeloss),
    paste0(nrow(other_setting), " cases occurred outside training and matches. ",
           "Excluded from all rates; report these in the text:")
  )
  
  write_result(
    other_setting %>%
      count(problem_type, osiics_16_level_1, osiics_16_level_3, timeloss),
    "cases_outside_training_and_matches"
  )
}

caselist <- filter(caselist, !when_occurred %in% "Other")

note("Cases included in rates and summary tables: ", nrow(caselist))


## Restrict match exposure to consenting players ----
#
# The caselist holds cases from consenting players only, so the denominator must
# too. Training exposure was already restricted in 02_exposure.R.
#
# The unrestricted file is used by the potential injury analysis, which observes
# every player on the pitch from broadcast footage regardless of consent, and so
# uses a different denominator from the same file.

consenting <- player_details %>%
  filter(consent %in% "yes") %>%
  distinct(player_id)

exposure_match_all <- exposure_match
exposure_match     <- semi_join(exposure_match, consenting, by = "player_id")

note("Match exposure restricted to consenting players: ",
     round(sum(exposure_match$total_playing_time, na.rm = TRUE) / 60), " h of ",
     round(sum(exposure_match_all$total_playing_time, na.rm = TRUE) / 60),
     " h played by contributing teams.")


## Exposure ----

data_exposure <- summarise_exposure(exposure_training, exposure_match)

note_table(data_exposure, "\nExposure:")


## Save the analysis inputs ----
#
# The finished caselist and the exposure row, so that 04_figures.R works from
# exactly what the tables were built from rather than rebuilding it.

readr::write_csv(caselist, file.path(dir_deid, "caselist_analysis.csv"), na = "")
note("Wrote ", file.path(dir_deid, "caselist_analysis.csv"))

write_result(data_exposure, "exposure_summary")


## Descriptive tables ----
#
# Participation takes the full player list, including excluded teams, so the
# table can report both how many teams were invited and how many contributed.

write_result(generate_participation_table(player_details_all), "participation_table")

write_result(generate_player_characteristics_table(player_details),
             "player_characteristics_table")

write_result(generate_exposure_table(data_exposure), "exposure_table")

basic_numbers <- generate_basic_numbers_table(caselist)
write_result(basic_numbers, "basic_numbers_table")
note_table(basic_numbers, "\nBasic numbers:")

write_result(generate_subsequent_table(caselist), "subsequent_table")


## Summary rates ----

injury_table <- format_summary_table(
  generate_injury_summary_table(caselist, data_exposure),
  exposure_label = "Exposure (h)"
)

write_result(injury_table, "injury_table")
note_table(injury_table, "\nInjury rates per 1000 hours:")

health_problems_table <- format_summary_table(
  generate_health_problems_table(caselist, data_exposure),
  exposure_label = "Exposure (player days)"
)

write_result(health_problems_table, "health_problems_table")
note_table(health_problems_table, "\nAll health problems per 1000 player-days:")


## Pattern tables ----
#
# Body area -> pathology type -> diagnosis, and tissue type -> pathology type.
# Injuries are expressed per 1000 hours, illnesses and mental health problems
# per 1000 player-days.
#
# These bootstrap a confidence interval for every row, so this section takes a
# minute or two.

injuries <- filter(caselist, problem_type == "Injury")

body_specs <- list(
  list(name = "pattern_injury_all",
       data = injuries,
       exposure = data_exposure$total),
  
  list(name = "pattern_injury_match",
       data = filter(injuries, when_occurred %in% "Match"),
       exposure = data_exposure$match),
  
  list(name = "pattern_injury_training",
       data = filter(injuries, when_occurred %in% "Training"),
       exposure = data_exposure$training),
  
  list(name = "pattern_illness",
       data = filter(caselist, problem_type == "Illness"),
       exposure = data_exposure$player_days),
  
  list(name = "pattern_mental_health",
       data = filter(caselist, problem_type == "Mental health problem"),
       exposure = data_exposure$player_days)
)

for (spec in body_specs) {
  
  if (nrow(spec$data) == 0) {
    note("No cases for ", spec$name, " - skipped.")
    next
  }
  
  result <- generate_body_pattern_table(spec$data, spec$exposure)
  write_result(result, spec$name)
  write_pattern_table_excel(result, make_output_path(paste0(spec$name, ".xlsx")))
}


# Tissue type applies to injuries only - illnesses have no tissue level.
tissue_specs <- list(
  list(name = "pattern_tissue_all",
       data = injuries,
       exposure = data_exposure$total),
  
  list(name = "pattern_tissue_match",
       data = filter(injuries, when_occurred %in% "Match"),
       exposure = data_exposure$match),
  
  list(name = "pattern_tissue_training",
       data = filter(injuries, when_occurred %in% "Training"),
       exposure = data_exposure$training)
)

for (spec in tissue_specs) {
  
  if (nrow(spec$data) == 0) {
    note("No cases for ", spec$name, " - skipped.")
    next
  }
  
  result <- generate_tissue_pattern_table(spec$data, spec$exposure)
  write_result(result, spec$name)
  write_pattern_table_excel(result, make_output_path(paste0(spec$name, ".xlsx")))
}


## Severity and category summaries ----

write_result(generate_severity_distribution(caselist), "severity_by_body_area")

write_result(generate_body_area_summary(caselist), "body_area_summary")

write_result(generate_tissue_type_summary(caselist), "tissue_type_summary")


## Suppression rule ----
#
# Recorded so the report footnote and the code cannot diverge.

note("\nSuppression applied to published tables:")
note(suppression_footnote())


## Potential injuries ----
#
# Analysed separately, by 03_Analysis/Code/03_potential_injuries.R. That script
# reads the match analysts' file, which covers all teams including those
# excluded here, so it runs with the other preprocessing steps rather than from
# here.


## Done ----

write_run_notes("notes_analysis")

message("\nAnalysis complete. Results in ", output_dir)
