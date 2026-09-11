# Potential injuries ----
#
# Analyses the match analysts' record of potential injury situations: events
# where a player stayed down for more than five seconds or requested medical
# attention during a match.
#
# Reads the reduced copy written by 01_prepare_data.R, which holds only the
# columns the analysis uses - no player names, no linking codes.
#
# The denominator is every player's match exposure, consenting or not. The
# analysts observe the whole pitch from broadcast footage, and consent to the
# health surveillance has no bearing on what they can see. This is the one
# place the match exposure file is used unrestricted.
#
# Run after 01_prepare_data.R and 02_exposure.R.

source("config.R")

start_run_notes()
note_settings(c("competition_code", "competition_name"))


## Load ----

data_potential <- read_deid("potential_injuries.csv")

exposure_match <- read_deid("exposure_match.csv")
match_days     <- read_deid("match_days.csv")


## Denominators ----
#
# Matches and teams come from the schedule, not from the potential injury file.
# Two teams appear per match, so the number of matches is half the number of
# team-match rows. Set n_matches in config.R to override if a competition's
# schedule does not follow that shape.

n_matches <- get_setting("n_matches", nrow(match_days) / 2)

if (n_matches != round(n_matches)) {
  stop("The schedule has ", nrow(match_days), " team-match rows, which is odd. ",
       "Set n_matches in config.R.")
}

n_teams <- dplyr::n_distinct(match_days$team)

n_players <- exposure_match %>%
  filter(total_playing_time > 0) %>%
  distinct(player_id) %>%
  nrow()

match_hours <- sum(exposure_match$total_playing_time, na.rm = TRUE) / 60

note("Matches played: ", n_matches)
note("Teams: ", n_teams)
note("Players with match exposure: ", n_players)
note("Match exposure, all players: ", round(match_hours), " hours")
note("Potential injuries recorded: ", nrow(data_potential))

matches_with_events <- dplyr::n_distinct(data_potential$match_number)

note("Matches with at least one potential injury: ", matches_with_events,
     " of ", n_matches,
     ". Per-match averages use all ", n_matches, " matches.")


## Summary ----

summary_table <- summarise_potential_injuries(
  data_potential,
  n_matches   = n_matches,
  n_teams     = n_teams,
  n_players   = n_players,
  match_hours = match_hours
)

write_result(summary_table, "potential_injuries_summary")
note_table(summary_table, "\nPotential injuries:")


## Referee actions ----

referee_actions <- summarise_referee_actions(data_potential, n_matches)

write_result(referee_actions, "potential_injuries_referee_actions")
note_table(referee_actions, "\nReferee actions:")


## Distributions ----

distributions <- c(
  body_part               = "potential_injuries_body_part",
  contact_type            = "potential_injuries_contact_type",
  contact_type_detail     = "potential_injuries_contact_detail",
  closest_player_action   = "potential_injuries_player_action",
  injured_player_sanction = "potential_injuries_sanction_injured",
  other_player_sanction   = "potential_injuries_sanction_other"
)

for (column in names(distributions)) {
  
  if (!column %in% names(data_potential)) {
    note("Column '", column, "' is not in the file - distribution skipped.")
    next
  }
  
  write_result(
    potential_injury_distribution(data_potential, column),
    distributions[[column]]
  )
}


# What the referee did, among events where play was stopped.
if (all(c("referee_action", "referee_action_play_stopped") %in% names(data_potential))) {
  
  write_result(
    data_potential %>%
      filter(referee_action %in% "play_stopped") %>%
      potential_injury_distribution("referee_action_play_stopped"),
    "potential_injuries_referee_action_detail"
  )
}


## Per match and per team ----

write_result(
  data_potential %>% count(match_number, name = "potential_injuries"),
  "potential_injuries_per_match"
)

if ("team_name" %in% names(data_potential)) {
  
  write_result(
    data_potential %>%
      group_by(team_name) %>%
      summarise(
        potential_injuries = n(),
        substitutions      = sum(injury_outcome_action %in% "sub"),
        medic_required     = sum(medic_required %in% "medic_required"),
        .groups = "drop"
      ) %>%
      arrange(desc(potential_injuries)),
    "potential_injuries_by_team"
  )
}


## Done ----

write_run_notes("notes_potential_injuries")

message("\nPotential injury analysis complete.")
