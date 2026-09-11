# Training exposure ----

## About ----

# Reshapes the training exposure file, restricts it to each team's surveillance
# period and to consenting players, fills match-day warm-ups, imputes missing
# team-days, and de-identifies the result.
#
# This is the step that varies most between competitions - the shape of the
# source file and the pattern of missing data differ every time. Expect to edit
# it. Nothing downstream should need editing.
#
# Three decisions to make for every competition. Record each one in the methods
# note, because each changes the denominator.
#
#   1. Match-day training. Some competitions record nothing on match days, some
#      record a warm-up, some record a full session. Where nothing is recorded,
#      match days are filled with match_day_warmup_minutes from config.R. Where
#      training IS recorded on match days, remove that fill and let the
#      imputation handle it, with match day as a predictor - otherwise warm-ups
#      and full sessions are drawn from the same donor pool and match-day
#      exposure is overestimated.
#
#   2. Start of the surveillance period. The intended start is a fixed number
#      of days before each team's first match. Teams often begin recording
#      later, and age-group and invitational competitions later still. Decide
#      whether each team's period starts at the intended date or at their first
#      recorded exposure, and state which. The code below takes the first
#      recorded exposure.
#
#   3. Excluded teams. Teams listed in excluded_teams in config.R are dropped
#      from the health surveillance denominator here. Use it only where a team
#      contributes exposure but could not contribute cases.
#
# The match schedule is NOT filtered by excluded team. It is saved for the
# potential injury analysis, which observes every team from broadcast footage.
#
# Run after 01_prepare_data.R. A record of the decisions and figures behind the
# exposure denominator is written to 03_Analysis/Results/notes_exposure.txt.


source("config.R")

start_run_notes()

note_settings(c("competition_code", "competition_name", "start_date",
                "excluded_teams", "match_day_warmup_minutes",
                "n_imputations", "random_seed"))


## Match schedule ----

match_days <- read_sensitive(file_surveillance) %>%
  tidyr::pivot_longer(-team, names_to = "match_day", values_to = "date") %>%
  filter(!is.na(date)) %>%
  mutate(date = parse_date_dmy(date)) %>%
  distinct(team, match_day, date)

if (any(is.na(match_days$date))) {
  stop("Some match dates did not parse. Check the date format in ", file_surveillance)
}

# One row per team per match, so this is team match days, not matches. The
# number of matches is derived from the match schedule in
# 03_potential_injuries.R.

note("Team match days in the schedule: ", nrow(match_days),
     " across ", dplyr::n_distinct(match_days$team), " teams")


## Training exposure in long format ----

training_long <- read_sensitive(file_training_exposure) %>%
  tidyr::pivot_longer(
    cols      = -c(team, player_id),
    names_to  = "date",
    values_to = "training_minutes"
  ) %>%
  mutate(date = parse_date_dmy(date))

if (any(is.na(training_long$date))) {
  stop("Some training exposure dates did not parse. Check the column headers in ",
       file_training_exposure)
}

training_long <- exclude_teams(training_long)

note("Teams excluded from the health surveillance: ",
     paste(excluded_teams, collapse = ", "))


## Surveillance periods ----

# Each team's period runs from the first date they recorded any training
# exposure to their last match. Teams arrive at different times, so this comes
# from the data rather than a fixed offset from the first match.

periods <- training_long %>%
  filter(!is.na(training_minutes)) %>%
  group_by(team) %>%
  summarise(start = min(date), .groups = "drop") %>%
  left_join(
    match_days %>%
      group_by(team) %>%
      summarise(first_match = min(date), end = max(date), .groups = "drop"),
    by = "team"
  )

no_matches <- periods$team[is.na(periods$end)]

if (length(no_matches) > 0) {
  stop("These teams have training exposure but no matches in ", file_surveillance,
       ": ", paste(no_matches, collapse = ", "))
}

no_exposure <- setdiff(unique(match_days$team), c(periods$team, excluded_teams))

if (length(no_exposure) > 0) {
  note("Teams that played matches but recorded no training exposure: ",
       paste(no_exposure, collapse = ", "))
  warning("Those teams are absent from the exposure denominator.", call. = FALSE)
} else {
  note("Every contributing team with matches also recorded training exposure.")
}

note_table(
  periods %>%
    mutate(days_before_first_match = as.numeric(first_match - start)),
  "\nSurveillance periods, taken from the first recorded training exposure:"
)


## Restrict to the period and to consenting players ----

consenting <- read_sensitive(file_player_details) %>%
  exclude_teams() %>%
  filter(consent %in% "yes") %>%
  distinct(player_id)

note("Consenting players in contributing teams: ", nrow(consenting))

training_daily <- training_long %>%
  left_join(select(periods, team, start, end), by = "team") %>%
  filter(date >= start, date <= end) %>%
  semi_join(consenting, by = "player_id") %>%
  select(team, player_id, date, training_minutes)

note("Player-days within the surveillance periods: ", nrow(training_daily))


## Match-day warm-up ----

# The pre-match warm-up counts as training exposure. Only days with nothing
# recorded are filled - anything a team did record is kept as it stands.

training_daily <- training_daily %>%
  left_join(select(match_days, team, date, match_day), by = c("team", "date"))

recorded_on_match_days <- sum(
  !is.na(training_daily$training_minutes) & !is.na(training_daily$match_day)
)

note("Match-day training values recorded by teams and kept unchanged: ",
     recorded_on_match_days)

training_daily <- training_daily %>%
  mutate(
    training_minutes = ifelse(
      is.na(training_minutes) & !is.na(match_day),
      match_day_warmup_minutes,
      training_minutes
    )
  )

note("Remaining match days filled with a ", match_day_warmup_minutes,
     " minute warm-up.")


## Missing training exposure ----

missing_summary <- training_daily %>%
  filter(is.na(match_day)) %>%
  summarise(missing = sum(is.na(training_minutes)), total = n())

note("Training days with no exposure recorded: ",
     missing_summary$missing, " of ", missing_summary$total,
     " (", round(100 * missing_summary$missing / missing_summary$total), "%)")


## Impute missing team-days ----

# Imputation is at team-day level: on a given day a team trains together, so
# the team is the right unit. Match days are excluded from the model - a
# 30-minute warm-up is not a training session and would drag the distribution
# down.
#
# The team-day value is the median of the players who recorded something. The
# previous version used the mode, which returns an arbitrary first value when
# players differ; where players agree the two are identical.

team_day <- training_daily %>%
  filter(is.na(match_day)) %>%
  group_by(team, date) %>%
  summarise(
    team_training_minutes = median(training_minutes, na.rm = TRUE),
    n_observed            = sum(!is.na(training_minutes)),
    .groups = "drop"
  ) %>%
  mutate(day_of_competition = as.numeric(date - min(date)))

note("Team-days requiring imputation: ",
     sum(is.na(team_day$team_training_minutes)), " of ", nrow(team_day))

imputation_input <- team_day %>%
  transmute(
    team = factor(team),
    day_of_competition,
    team_training_minutes
  )

predictors <- mice::make.predictorMatrix(imputation_input)
predictors[, ] <- 0
predictors["team_training_minutes", c("team", "day_of_competition")] <- 1

methods <- mice::make.method(imputation_input)
methods[] <- ""
methods["team_training_minutes"] <- "pmm"

imputed <- mice::mice(
  imputation_input,
  m               = n_imputations,
  method          = methods,
  predictorMatrix = predictors,
  seed            = random_seed,
  printFlag       = FALSE
)

note("Imputed by predictive mean matching on team and day of competition, m = ",
     n_imputations, ", seed ", random_seed, ".")

# All imputations are used. Taking one and discarding the rest makes the result
# depend on an arbitrary draw.

completed <- lapply(
  seq_len(n_imputations),
  function(i) mice::complete(imputed, action = i)$team_training_minutes
)

team_day$team_training_imputed <- rowMeans(do.call(cbind, completed))

note("Each imputed team-day is the mean across all ", n_imputations, " draws.")


## Sensitivity to the imputation ----

# Total training hours under each individual draw, so the range can be reported
# alongside the figure actually used.

total_training_hours <- function(candidate_values) {
  
  candidates <- team_day
  candidates$candidate <- candidate_values
  
  training_daily %>%
    left_join(select(candidates, team, date, candidate), by = c("team", "date")) %>%
    mutate(minutes = dplyr::coalesce(training_minutes, candidate)) %>%
    summarise(hours = sum(minutes, na.rm = TRUE) / 60) %>%
    pull(hours)
}

imputation_sensitivity <- vapply(completed, total_training_hours, numeric(1))

note("Total training hours across the ", n_imputations, " draws: ",
     round(min(imputation_sensitivity)), " to ",
     round(max(imputation_sensitivity)),
     ". Value used: ",
     round(total_training_hours(team_day$team_training_imputed)), ".")


## Fill the missing days ----

training_daily <- training_daily %>%
  left_join(select(team_day, team, date, team_training_imputed),
            by = c("team", "date")) %>%
  mutate(training_minutes = dplyr::coalesce(training_minutes, team_training_imputed)) %>%
  select(team, player_id, date, training_minutes, match_day)

still_missing <- sum(is.na(training_daily$training_minutes))

if (still_missing > 0) {
  note(still_missing, " player-days still have no training exposure after ",
       "imputation and will count as zero hours in the denominator.")
  warning(still_missing, " player-days remain without training exposure.",
          call. = FALSE)
} else {
  note("No player-days remain without training exposure.")
}


## Cases outside the surveillance period ----

# Cases from five days before a team's first match onward are retained. A case
# occurring before that team began recording exposure is therefore a gap
# between numerator and denominator. Reported here, never dropped.

cases <- read_deid("caselist.csv") %>%
  exclude_teams() %>%
  mutate(date_incident = parse_date_dmy(date_incident)) %>%
  left_join(select(periods, team, start, first_match, end), by = "team")

before_exposure <- filter(cases, date_incident < start)
after_period    <- filter(cases, date_incident > end)

if (nrow(before_exposure) > 0) {
  note_table(
    select(before_exposure, team, date_incident, start, first_match,
           problem_type, when_occurred),
    paste0("\n", nrow(before_exposure), " cases occurred before their team began ",
           "recording training exposure. Counted in the numerator with no ",
           "matching exposure in the denominator:")
  )
} else {
  note("\nNo cases occurred before their team began recording exposure.")
}

if (nrow(after_period) > 0) {
  note_table(
    select(after_period, team, date_incident, end, problem_type, when_occurred),
    paste0(nrow(after_period), " cases occurred after their team's last match:")
  )
} else {
  note("No cases occurred after their team's last match.")
}


## Save ----

# The match schedule keeps every team - the potential injury analysis needs all
# of them.

readr::write_csv(periods, file.path(dir_deid, "surveillance_periods.csv"), na = "")
readr::write_csv(match_days, file.path(dir_deid, "match_days.csv"), na = "")

deidentify_and_save(training_daily, "exposure_training_daily")

write_run_notes("notes_exposure")