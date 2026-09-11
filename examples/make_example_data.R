# Example data generator ----

## About ----

# Creates a complete synthetic competition for the FIFA surveillance pipeline.
# No real people, teams or competitions are represented. Every value is drawn
# from the seed set below, so the output is fully reproducible.
#
# The six files written here carry the same names, delimiter, encoding and
# column contract as real competition source files, so the pipeline runs on
# them unmodified.
#
# Diagnoses are sampled from the OSIICS reference that ships with the tools,
# and area_system, tissue and pathology are derived from that same lookup, so
# the labels are consistent with the codes by construction.
#
# The dataset deliberately contains the edge cases that have caused defects in
# real analyses. These are listed at the foot of this script and printed when
# it finishes.


## Settings ----

tools_dir <- getwd()

source(file.path(tools_dir, "load_tools.R"))

library(tidyverse)

random_seed      <- 2024
n_teams          <- 16
squad_size       <- 23
competition_name <- "Example Cup 2024"
competition_code <- "EXC2024"
first_match_date <- as.Date("2024-06-01")
lead_days        <- 5
n_injuries       <- 45
n_illnesses      <- 6
n_potential      <- 480

example_dir <- file.path(tools_dir, "examples", "data")

set.seed(random_seed)

dir.create(example_dir, recursive = TRUE, showWarnings = FALSE)


## OSIICS reference ----

osiics <- load_osiics()

osiics_injury  <- osiics %>% filter(problem_type == "Injury")
osiics_illness <- osiics %>% filter(problem_type == "Illness")

if (nrow(osiics_injury) == 0 || nrow(osiics_illness) == 0) {
  stop("OSIICS problem_type values are not 'Injury' and 'Illness' - check load_osiics()")
}



# Competition structure ----

## Teams ----

team_words <- c("ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO", "FOXTROT",
                "GOLF", "HOTEL", "INDIA", "JULIETT", "KILO", "LIMA",
                "MIKE", "NOVEMBER", "OSCAR", "PAPA")

teams <- tibble(
  team      = paste0("TM", LETTERS[1:n_teams]),
  team_name = paste("TEAM", team_words[1:n_teams]),
  group     = rep(1:4, each = n_teams / 4)
)


## Fixtures ----

group_dates <- first_match_date + c(0, 3, 6)
qf_date     <- first_match_date + 10
sf_date     <- first_match_date + 13
final_date  <- first_match_date + 16

group_fixtures <- map_dfr(
  split(teams, teams$group),
  function(g) {
    tibble(
      date = rep(group_dates, each = 2),
      home = g$team[c(1, 3, 1, 2, 1, 2)],
      away = g$team[c(2, 4, 3, 4, 4, 3)]
    )
  }
)

first_of_group  <- map_chr(split(teams, teams$group), ~ .x$team[1])
second_of_group <- map_chr(split(teams, teams$group), ~ .x$team[2])

qf_fixtures <- tibble(
  date = qf_date,
  home = c(first_of_group[1], first_of_group[3], first_of_group[2], first_of_group[4]),
  away = c(second_of_group[2], second_of_group[4], second_of_group[1], second_of_group[3])
)

sf_fixtures <- tibble(
  date = sf_date,
  home = c(first_of_group[1], first_of_group[2]),
  away = c(first_of_group[3], first_of_group[4])
)

final_fixtures <- tibble(
  date = final_date,
  home = c(first_of_group[1], first_of_group[3]),
  away = c(first_of_group[2], first_of_group[4])
)

fixtures <- bind_rows(group_fixtures, qf_fixtures, sf_fixtures, final_fixtures) %>%
  arrange(date, home) %>%
  mutate(
    match_number = row_number(),
    match_id     = sprintf("%s-M%02d", competition_code, match_number),
    fixture      = paste(home, "v", away)
  )


## Surveillance periods ----

team_match_dates <- fixtures %>%
  select(match_number, date, home, away) %>%
  pivot_longer(c(home, away), names_to = "side", values_to = "team") %>%
  arrange(team, date)

surveillance_periods <- team_match_dates %>%
  group_by(team) %>%
  mutate(md = paste0("MD", row_number())) %>%
  ungroup() %>%
  mutate(date = format(date, "%d/%m/%Y")) %>%
  select(team, md, date) %>%
  pivot_wider(names_from = md, values_from = date) %>%
  select(team, any_of(paste0("MD", 1:6))) %>%
  arrange(team)

team_windows <- team_match_dates %>%
  group_by(team) %>%
  summarise(
    first_match = min(date),
    last_match  = max(date),
    .groups = "drop"
  ) %>%
  mutate(start_date = first_match - lead_days)



# Squads ----

## Player list ----

positions <- c(rep("GK", 3), rep("DF", 8), rep("MF", 7), rep("FW", 5))

stopifnot(length(positions) == squad_size)

players <- teams %>%
  select(team) %>%
  mutate(player = list(seq_len(squad_size))) %>%
  unnest(player) %>%
  mutate(
    player_id = sprintf("%s%03d", team, player),
    position  = positions[player]
  ) %>%
  select(team, player_id, position)

# A player on the roster who never appears in any exposure file
players <- bind_rows(
  players,
  tibble(team = "TMA", player_id = "TMA024", position = "DF")
)

squad_players <- players %>% filter(player_id != "TMA024")


## Player details ----

n_players <- nrow(players)

age_years  <- round(runif(n_players, 18, 38))
birth_date <- first_match_date - round(age_years * 365.25) -
  sample(0:364, n_players, replace = TRUE)

player_details <- players %>%
  mutate(
    date_birth      = format(birth_date, "%d/%m/%Y"),
    height          = round(rnorm(n_players, 181, 7)),
    weight          = round(rnorm(n_players, 76, 7)),
    consent         = "yes",
    consent_2       = NA_character_,
    consent_3       = NA_character_,
    consent_notes   = NA_character_,
    consent_notes_2 = NA_character_
  ) %>%
  select(team, player_id, position, date_birth, height, weight,
         consent, consent_2, consent_3, consent_notes, consent_notes_2)

# Blank consent for a handful of players
player_details$consent[sample(seq_len(n_players), 5)] <- NA_character_



# Exposure ----

## Training exposure ----

team_day_values <- team_windows %>%
  select(team, start_date, last_match) %>%
  mutate(date = map2(start_date, last_match, seq, by = "day")) %>%
  unnest(date) %>%
  left_join(
    team_match_dates %>% select(team, date) %>% mutate(is_match_day = TRUE),
    by = c("team", "date")
  ) %>%
  mutate(
    is_match_day = coalesce(is_match_day, FALSE),
    team_minutes = if_else(
      is_match_day,
      sample(c(0, 20, 30), n(), replace = TRUE, prob = c(0.50, 0.20, 0.30)),
      sample(c(0, 45, 60, 75, 90), n(), replace = TRUE,
             prob = c(0.15, 0.15, 0.30, 0.25, 0.15))
    )
  )

# Whole team-days with no record at all
team_day_values$team_minutes[
  sample(seq_len(nrow(team_day_values)), round(0.08 * nrow(team_day_values)))
] <- NA_real_

training_long <- squad_players %>%
  inner_join(
    team_day_values %>% select(team, date, team_minutes),
    by = "team",
    relationship = "many-to-many"
  ) %>%
  mutate(
    minutes = team_minutes,
    minutes = if_else(runif(n()) < 0.05, 0, minutes),
    minutes = if_else(runif(n()) < 0.04, NA_real_, minutes)
  )

all_dates <- seq(min(team_windows$start_date), max(team_windows$last_match), by = "day")
date_cols <- format(all_dates, "%d/%m/%Y")

training_wide <- training_long %>%
  mutate(date = format(date, "%d/%m/%Y")) %>%
  select(team, player_id, date, minutes) %>%
  pivot_wider(names_from = date, values_from = minutes) %>%
  select(team, player_id, any_of(date_cols)) %>%
  arrange(team, player_id)


## Match exposure ----

match_duration <- fixtures %>%
  transmute(
    match_id,
    match_number,
    fixture,
    duration = if_else(match_number > 24 & runif(n()) < 0.25, 120, 90) +
      round(runif(n(), 5, 12))
  )

team_match_list <- match_duration %>%
  left_join(fixtures %>% select(match_id, home, away), by = "match_id") %>%
  pivot_longer(c(home, away), names_to = "side", values_to = "team") %>%
  select(match_id, match_number, fixture, duration, team)

build_team_match <- function(match_id, match_number, fixture, duration, team) {
  
  squad <- squad_players$player_id[squad_players$team == team]
  
  chosen   <- sample(squad, 16)
  starters <- chosen[1:11]
  subs     <- chosen[12:16]
  
  sub_times <- sort(round(runif(5, 45, duration - 5)))
  
  starter_minutes <- rep(duration, 11)
  starter_minutes[sample(11, 5)] <- sub_times
  
  tibble(
    competition_name   = competition_name,
    match_id           = match_id,
    match_number       = match_number,
    fixture            = fixture,
    team               = team,
    player_id          = c(starters, subs),
    total_playing_time = c(starter_minutes, duration - sub_times)
  )
}

match_exposure <- pmap(team_match_list, build_team_match) %>%
  bind_rows() %>%
  mutate(
    effective_playing_time = round(total_playing_time * runif(n(), 0.50, 0.56), 4),
    in_possession_time     = round(effective_playing_time * runif(n(), 0.45, 0.55), 4),
    out_of_possession_time = round(effective_playing_time - in_possession_time, 4),
    total_playing_time     = round(total_playing_time, 4)
  ) %>%
  select(competition_name, match_id, match_number, fixture, team, player_id,
         total_playing_time, effective_playing_time, in_possession_time,
         out_of_possession_time)



# Health problems ----

## Helpers ----

sample_case_team <- function(n) {
  sample(setdiff(teams$team, "TMP"), n, replace = TRUE)
}

sample_case_player <- function(team_codes) {
  vapply(
    team_codes,
    function(tm) sample(squad_players$player_id[squad_players$team == tm], 1),
    character(1),
    USE.NAMES = FALSE
  )
}

sample_case_date <- function(team_codes) {
  w <- team_windows[match(team_codes, team_windows$team), ]
  w$start_date + floor(runif(length(team_codes)) *
                         as.numeric(w$last_match - w$start_date + 1))
}


## Injuries ----

forced_codes <- c("GM8", "GJX", "GM1", "GS1")

injury_codes <- c(
  forced_codes,
  sample(osiics_injury$osiics_16_code, n_injuries - length(forced_codes), replace = TRUE)
)

injury_teams <- sample_case_team(n_injuries)

injury_onset <- sample(c("Sudden-onset", "Gradual-onset"), n_injuries,
                       replace = TRUE, prob = c(0.85, 0.15))

injury_when <- if_else(
  injury_onset == "Gradual-onset",
  NA_character_,
  sample(c("Match", "Training"), n_injuries, replace = TRUE, prob = c(0.6, 0.4))
)

injury_when[1] <- "Other"

injury_timeloss <- round(c(
  214, 275,
  rexp(n_injuries - 2, rate = 1 / 12) + 1
))

injury_subsequent <- sample(c("index", "recurrent injury", "unknown"),
                            n_injuries, replace = TRUE, prob = c(0.80, 0.15, 0.05))

injuries <- tibble(
  team           = injury_teams,
  player_id      = sample_case_player(injury_teams),
  date_incident  = sample_case_date(injury_teams),
  problem_type   = "Injury",
  subsequent_cat = injury_subsequent,
  recurrence_type = if_else(
    injury_subsequent == "recurrent injury",
    sample(c("early", "late", "delayed"), n_injuries, replace = TRUE),
    NA_character_
  ),
  onset          = injury_onset,
  when_occurred  = injury_when,
  osiics_16_code = injury_codes,
  match_min      = if_else(injury_when == "Match" & !is.na(injury_when),
                           sample(1:95, n_injuries, replace = TRUE),
                           NA_integer_),
  contact        = if_else(
    injury_onset == "Sudden-onset",
    sample(c("No",
             "Yes, direct contact (to injured body part)",
             "Yes, indirect contact (to other body part)"),
           n_injuries, replace = TRUE, prob = c(0.45, 0.40, 0.15)),
    NA_character_
  ),
  player_action  = if_else(
    injury_onset == "Sudden-onset",
    sample(c("Change of direction", "Collision", "Controlling the ball", "Falling",
             "Heading", "Hit by ball", "Landing", "Other:", "Running (any speed)",
             "Tackle", "Unknown"),
           n_injuries, replace = TRUE),
    NA_character_
  ),
  sanction       = if_else(
    injury_when == "Match" & !is.na(injury_when),
    sample(c("No foul", "Opponent committing foul", "Injured player committing foul"),
           n_injuries, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    NA_character_
  ),
  card           = if_else(
    injury_when == "Match" & !is.na(injury_when),
    sample(c("No card", "Yellow card", "Red card"),
           n_injuries, replace = TRUE, prob = c(0.85, 0.13, 0.02)),
    NA_character_
  ),
  timeloss       = injury_timeloss
) %>%
  mutate(
    contact_type = if_else(
      !is.na(contact) & contact != "No",
      sample(c("Ball", "Opponent", "Pitch object", "Team mate"),
             n(), replace = TRUE, prob = c(0.15, 0.70, 0.05, 0.10)),
      NA_character_
    ),
    player_action_freetext = if_else(player_action == "Other:",
                                     "unspecified action", NA_character_)
  )


## Illnesses ----

illness_teams <- sample_case_team(n_illnesses)

illnesses <- tibble(
  team            = illness_teams,
  player_id       = sample_case_player(illness_teams),
  date_incident   = sample_case_date(illness_teams),
  problem_type    = "Illness",
  subsequent_cat  = "index",
  recurrence_type = NA_character_,
  onset           = "Gradual-onset",
  when_occurred   = NA_character_,
  osiics_16_code  = sample(osiics_illness$osiics_16_code, n_illnesses, replace = TRUE),
  match_min       = NA_integer_,
  contact         = NA_character_,
  contact_type    = NA_character_,
  player_action   = NA_character_,
  player_action_freetext = NA_character_,
  sanction        = NA_character_,
  card            = NA_character_,
  timeloss        = sample(1:7, n_illnesses, replace = TRUE)
)


## Assemble case list ----

caselist <- bind_rows(injuries, illnesses) %>%
  left_join(
    osiics %>% select(osiics_16_code, osiics_16_level_1,
                      osiics_16_level_2, osiics_16_level_3),
    by = "osiics_16_code"
  ) %>%
  mutate(
    area_system = osiics_16_level_1,
    tissue      = osiics_16_level_2,
    pathology   = osiics_16_level_3
  )

# One case whose reported body area contradicts its OSIICS code,
# so check_labels() has something to find
mismatch_row <- which(caselist$area_system != "Ankle")[1]
caselist$area_system[mismatch_row] <- "Ankle"

caselist <- caselist %>%
  arrange(date_incident) %>%
  mutate(
    event_id           = sprintf("E%04d", row_number()),
    case_id            = sprintf("C%04d", row_number()),
    sex                = "male",
    mechanism          = NA_character_,
    date_incident      = format(date_incident, "%d/%m/%Y"),
    timeloss_expected  = as.character(timeloss + sample(-2:5, n(), replace = TRUE)),
    timeloss_confirmed = if_else(runif(n()) < 0.75, as.character(timeloss), NA_character_),
    timeloss_cat       = "timeloss"
  ) %>%
  select(event_id, case_id, team, sex, player_id, date_incident, problem_type,
         subsequent_cat, recurrence_type, area_system, onset, mechanism,
         tissue, pathology, when_occurred, match_min, contact, contact_type,
         player_action, player_action_freetext, sanction, card,
         timeloss_expected, timeloss_confirmed, timeloss, timeloss_cat,
         osiics_16_code)



# Potential injuries ----

## Retained columns ----

pi_source <- fixtures %>%
  select(match_number, home, away) %>%
  pivot_longer(c(home, away), names_to = "side", values_to = "team") %>%
  left_join(teams %>% select(team, team_name), by = "team") %>%
  slice_sample(n = n_potential, replace = TRUE)

pi_referee_action <- sample(c("no_action", "play_on", "play_stopped"),
                            n_potential, replace = TRUE, prob = c(0.35, 0.35, 0.30))

pi_on_pitch <- runif(n_potential) < 0.12

potential_injuries <- tibble(
  match_number = pi_source$match_number,
  team_name    = toupper(pi_source$team_name),
  body_part    = sample(c("ankle", "elbow", "foot", "hands", "head",
                          "hip_pelvis_groin", "knee", "lower_arm", "lower_leg",
                          "neck", "shoulder", "thigh", "trunk",
                          "unidentifiable", "upper_arm"),
                        n_potential, replace = TRUE),
  contact_type = sample(c("direct_contact", "indirect_contact",
                          "non_contact", "unidentifiable"),
                        n_potential, replace = TRUE,
                        prob = c(0.60, 0.20, 0.15, 0.05)),
  contact_type_detail = sample(c("ball_contact", "opposing_player", "other",
                                 "pitch_object", "teammate"),
                               n_potential, replace = TRUE,
                               prob = c(0.15, 0.65, 0.05, 0.05, 0.10)),
  closest_player_action = sample(c("active_engagement", "aerial_control",
                                   "aerial_duel", "attempt_at_goal",
                                   "ball_progression", "block", "clearance",
                                   "defensive_line_support", "duel",
                                   "goal_prevention", "interception", "offer",
                                   "pass", "pressing", "pushing_on"),
                                 n_potential, replace = TRUE),
  referee_action = pi_referee_action,
  referee_action_play_stopped = if_else(
    pi_referee_action == "play_stopped",
    sample(c("foul_against", "foul_for", "injury_stoppage", "other",
             "teammate_foul_against", "teammate_foul_for"),
           n_potential, replace = TRUE),
    NA_character_
  ),
  injured_player_sanction = if_else(runif(n_potential) < 0.03,
                                    "yellow_card", NA_character_),
  other_player_sanction = if_else(
    runif(n_potential) < 0.08,
    sample(c("2nd_yellow_card", "red_card", "yellow_card"),
           n_potential, replace = TRUE, prob = c(0.05, 0.10, 0.85)),
    NA_character_
  ),
  injury_outcome_action = if_else(runif(n_potential) < 0.05, "sub", NA_character_),
  medic_required = sample(c("medic_required", "medic_not_required"),
                          n_potential, replace = TRUE, prob = c(0.25, 0.75)),
  assessment_required_on_pitch = pi_on_pitch,
  assessment_on_pitch_time = if_else(pi_on_pitch,
                                     round(runif(n_potential, 14, 280), 6),
                                     NA_real_)
)


## Pad to the full 53-column export ----

potential_injury_export_columns <- c(
  "competition_id", "team_id", "player_id", "half_time", "match_run_time_in_ms",
  "match_run_time", "match_time_in_ms", "match_time", "ball_state",
  "player_injury_signs", "contact_type", "contact_type_detail", "body_profile",
  "injury_body_location", "body_part", "injured_player_velocity", "jump_detail",
  "run_detail", "run_detail_extra", "other_player_body_location",
  "other_player_body_part", "other_player_velocity", "other_player_jump_detail",
  "other_player_run_detail", "other_player_run_detail_extra", "game_reaction",
  "referee_action", "referee_action_play_stopped", "medic_required",
  "assessment_required_on_pitch", "assessment_required_pitch_side",
  "injury_outcome", "injury_outcome_action", "injured_player_sanction",
  "other_player_sanction", "time_from_injury_start_to_injury_outcome",
  "time_from_injury_start_to_player_injury_signs",
  "time_from_referee_action_to_medic_required",
  "time_from_injury_start_to_medic_required",
  "time_from_medic_required_to_assessment_starting_on_pitch",
  "assessment_on_pitch_time", "assessment_pitch_side_time",
  "total_assessment_time", "closest_player_action",
  "closest_player_action_pressure", "player_tactical_starting_position",
  "match_number", "match_name", "team_name", "player_name",
  "player_shirt_number", "competition_name", "match_id"
)

for (col in setdiff(potential_injury_export_columns, names(potential_injuries))) {
  potential_injuries[[col]] <- NA
}

potential_injuries <- potential_injuries %>%
  select(all_of(potential_injury_export_columns))



# Write files ----

write_example <- function(x, filename) {
  readr::write_delim(
    x,
    file.path(example_dir, filename),
    delim = ";",
    na    = ""
  )
  cat("Wrote", filename, "-", nrow(x), "rows,", ncol(x), "columns\n")
}

cat("\nWriting example data to", example_dir, "\n\n")

write_example(caselist,           paste0("caselist_", competition_code, ".csv"))
write_example(match_exposure,     paste0("playing_times_", competition_code, ".csv"))
write_example(player_details,     paste0(competition_code, "_player_details.csv"))
write_example(potential_injuries, paste0("potential_injuries_", competition_code, ".csv"))
write_example(surveillance_periods, "surveillance_periods.csv")
write_example(training_wide,      "exposure_training.csv")



# Edge cases included ----

cat("\nEdge cases deliberately present in this dataset:\n\n")

cat("  - TMP has no recorded cases (participation table must separate\n",
    "    teams invited from teams contributing)\n")
cat("  - No mental health problems (empty-group guard in calculate_burden)\n")
cat("  - One case with when_occurred = 'Other' (must be excluded from rates)\n")
cat("  - Gradual-onset cases with when_occurred missing\n")
cat("  - Five players with blank consent\n")
cat("  - TMA024 is on the roster with no exposure in any file\n")
cat("  - Hip and groin codes GM8, GJX, GM1, GS1 for the body area recode\n")
cat("  - One case whose area_system contradicts its OSIICS code\n")
cat("  - Two long-term injuries (214 and 275 days) for the burden bootstrap\n")
cat("  - Whole team-days missing from training exposure, for imputation\n")
cat("  - 39 of 53 potential-injury columns unused, for report_dropped()\n\n")

cat("Done.\n")