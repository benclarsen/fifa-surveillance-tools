# Potential injuries ----
#
# A separate data stream from the health surveillance. Match analysts review
# broadcast footage and record every event in which a player stayed down for
# more than five seconds or requested medical attention.
#
# Two consequences for the denominators, both easy to get wrong:
#
#   - every player on the pitch is observed, whether or not they consented to
#     the health surveillance, so the exposure denominator is the whole squad's
#     match minutes rather than the consenting subset
#
#   - the number of matches and teams must come from the competition schedule.
#     Taking either from the potential injury file counts only matches and teams
#     in which something happened, which inflates every per-match average


## Columns retained ----
#
# The only columns the analysis uses. Everything else in the analysts' file -
# player names, linking codes, free text - is dropped in preprocessing, so no
# personal data reaches the working dataset or any output.
#
# Those names and codes do in principle allow potential injuries to be linked to
# the surveillance caselist. That link has never been made, and under the HSS it
# cannot be: potential injury data is collected on a legitimate interest basis
# without consent, so it cannot be joined to consented health data.

potential_injury_columns <- c(
  "match_number",
  "team_name",
  "body_part",
  "contact_type",
  "contact_type_detail",
  "closest_player_action",
  "referee_action",
  "referee_action_play_stopped",
  "injured_player_sanction",
  "other_player_sanction",
  "injury_outcome_action",
  "medic_required",
  "assessment_required_on_pitch",
  "assessment_on_pitch_time"
)


# Keep only the retained columns, and say plainly what the file was missing.
select_potential_injury_columns <- function(data, columns = potential_injury_columns) {
  
  absent <- setdiff(columns, names(data))
  
  if (length(absent) > 0) {
    warning("The potential injury file does not have these columns the analysis ",
            "expects: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  
  select(data, all_of(intersect(columns, names(data))))
}


## Summary ----

# Headline figures. `n_matches` and `n_teams` come from the competition
# schedule, `n_players` is the number with any match exposure, `match_hours`
# the total player-hours played.
summarise_potential_injuries <- function(data, n_matches, n_teams, n_players, match_hours) {
  
  if (!is.numeric(n_matches) || length(n_matches) != 1 || n_matches <= 0) {
    stop("`n_matches` must be a single positive number.")
  }
  
  check_exposure(match_hours)
  
  n_events         <- nrow(data)
  events_per_match <- events_by_match(data, n_matches)
  rate             <- poisson_rate_ci(n_events, match_hours)
  
  stoppages     <- count_matching(data, "referee_action", "play_stopped")
  assessments   <- count_matching(data, "assessment_required_on_pitch", TRUE)
  substitutions <- count_matching(data, "injury_outcome_action", "sub")
  
  tibble::tibble(
    metric = c(
      "Number of matches",
      "Number of teams",
      "Number of players with match exposure",
      "Total match exposure (hours)",
      "Total potential injuries",
      "Potential injuries per match: mean (range)",
      "Potential injuries per match: median (IQR)",
      "Incidence rate per 1000 player-hours [95% CI]",
      "Play stoppages - total (average per match)",
      "On-pitch medical attention - total (average per match)",
      "Substitutions following a potential injury - total (average per match)",
      "On-pitch assessment time, seconds: median (range)"
    ),
    value = c(
      format(n_matches),
      format(n_teams),
      format(n_players),
      fmt(match_hours, 1),
      format(n_events),
      paste0(fmt(mean(events_per_match), 1), " (",
             min(events_per_match), " to ", max(events_per_match), ")"),
      paste0(fmt(median(events_per_match), 0), " (",
             fmt(quantile(events_per_match, 0.25), 0), " to ",
             fmt(quantile(events_per_match, 0.75), 0), ")"),
      paste0(fmt(n_events / match_hours * 1000, 2), " [",
             fmt(rate[["ci_lower"]], 2), " to ", fmt(rate[["ci_upper"]], 2), "]"),
      with_average(stoppages, n_matches),
      with_average(assessments, n_matches),
      with_average(substitutions, n_matches),
      on_pitch_assessment_time(data)
    )
  )
}


# Events per match, padded with zeros for matches in which nothing was recorded.
events_by_match <- function(data, n_matches) {
  
  counts <- data %>%
    count(match_number, name = "events") %>%
    pull(events)
  
  if (length(counts) > n_matches) {
    stop("The potential injury file covers ", length(counts),
         " matches but the schedule has ", n_matches, ".")
  }
  
  c(counts, rep(0L, n_matches - length(counts)))
}


# Count rows where a column takes one of the given values. Returns NA when the
# column is absent, so a missing field shows as blank rather than as zero.
count_matching <- function(data, column, values) {
  
  if (!column %in% names(data)) return(NA_integer_)
  
  sum(data[[column]] %in% values)
}


with_average <- function(n, n_matches) {
  
  if (is.na(n)) return("")
  
  paste0(n, " (", fmt(n / n_matches, 1), ")")
}


# Median and range of on-pitch assessment time, in seconds, among events where a
# medic was required. Returns "" when nothing was recorded, rather than Inf.
on_pitch_assessment_time <- function(data,
                                     time_column  = "assessment_on_pitch_time",
                                     medic_column = "medic_required",
                                     medic_value  = "medic_required") {
  
  if (!time_column %in% names(data)) return("")
  
  times <- data[[time_column]]
  
  if (medic_column %in% names(data)) {
    times <- times[data[[medic_column]] %in% medic_value]
  }
  
  times <- suppressWarnings(as.numeric(times))
  times <- times[!is.na(times)]
  
  if (length(times) == 0) return("")
  
  paste0(fmt(median(times), 0), " (",
         fmt(min(times), 0), " to ", fmt(max(times), 0), ")")
}


## Distributions ----

# Counts and percentages for one categorical column, most frequent first.
# Missing and blank values are labelled "Not recorded" rather than dropped or
# left as an unlabelled row.
potential_injury_distribution <- function(data, column, top_n = NULL) {
  
  if (!column %in% names(data)) {
    stop("Column '", column, "' is not in the potential injury data.")
  }
  
  values <- as.character(data[[column]])
  values[is.na(values) | values == ""] <- "Not recorded"
  
  result <- tibble::tibble(category = values) %>%
    count(category, name = "n") %>%
    mutate(percent = round(100 * n / sum(n), 1)) %>%
    arrange(desc(n))
  
  if (!is.null(top_n)) result <- head(result, top_n)
  
  names(result)[1] <- column
  
  result
}


## Referee actions ----

summarise_referee_actions <- function(data, n_matches) {
  
  stoppages <- count_matching(data, "referee_action", "play_stopped")
  
  free_kicks <- if ("referee_action_play_stopped" %in% names(data)) {
    sum(stringr::str_detect(data$referee_action_play_stopped, "foul_for"), na.rm = TRUE)
  } else {
    NA_integer_
  }
  
  yellows <- count_sanctions(data, c("yellow_card", "2nd_yellow_card"))
  reds    <- count_sanctions(data, "red_card")
  
  free_kick_text <- if (is.na(free_kicks) || is.na(stoppages) || stoppages == 0) {
    format(free_kicks)
  } else {
    paste0(free_kicks, " (", round(100 * free_kicks / stoppages), "% of stoppages)")
  }
  
  tibble::tibble(
    metric = c(
      "Match stoppages due to potential injuries (average per match)",
      "Free kick or penalty awarded to the injured player's team",
      "Yellow cards shown (average per match)",
      "Red cards shown"
    ),
    value = c(
      with_average(stoppages, n_matches),
      free_kick_text,
      with_average(yellows, n_matches),
      format(reds)
    )
  )
}


# Count EVENTS in which either the injured player or another player received one
# of the given sanctions. An event where both were booked counts once, which
# matches how these figures were reported previously - it is a count of
# incidents, not of cards.
count_sanctions <- function(data, values) {
  
  columns <- intersect(
    c("injured_player_sanction", "other_player_sanction"),
    names(data)
  )
  
  if (length(columns) == 0) return(NA_integer_)
  
  sanctioned <- Reduce(`|`, lapply(columns, function(column) data[[column]] %in% values))
  
  sum(sanctioned)
}