# Summary tables ----
#
# Functions that turn analysis output into tables for the report. They return
# data frames and write nothing. Saving is the calling script's job.
#
# Note on the caselist: every case in it already meets the reporting
# threshold - time loss for injuries and illnesses, medical attention for
# mental health problems. There is no need to filter on timeloss_cat.


## Formatting helpers ----

# Round to a fixed number of decimals, returning "" for missing values so
# blanks appear in the report rather than "NA".
fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}


# "12.34 [10.11 to 14.57]", or "" when any part is missing.
fmt_ci <- function(estimate, lower, upper, digits = 2) {
  ifelse(
    is.na(estimate) | is.na(lower) | is.na(upper),
    "",
    paste0(fmt(estimate, digits), " [", fmt(lower, digits), " to ", fmt(upper, digits), "]")
  )
}


# "5 (3, 9)", or "5" when the quartiles are suppressed.
fmt_median_iqr <- function(median, q1, q3) {
  ifelse(
    is.na(median),
    "",
    ifelse(
      is.na(q1) | is.na(q3),
      fmt(median, 0),
      paste0(fmt(median, 0), " (", fmt(q1, 0), ", ", fmt(q3, 0), ")")
    )
  )
}


# "12 (75%)", or just the count when the total is unusable.
fmt_count_percent <- function(n, total) {
  if (is.na(total) || total == 0) return(as.character(n))
  paste0(n, " (", round(100 * n / total), "%)")
}


# Plural labels used in report tables.
problem_type_plural <- c(
  "Injury"                = "Injuries",
  "Illness"               = "Illnesses",
  "Mental health problem" = "Mental health problems"
)


## Participation ----

generate_participation_table <- function(player_details) {
  
  totals <- player_details %>%
    summarise(
      teams_total        = n_distinct(team),
      teams_consenting   = n_distinct(team[consent %in% "yes"]),
      players_total      = n(),
      players_consenting = n_distinct(player_id[consent %in% "yes"])
    )
  
  tibble::tibble(
    Metric = c(
      "Total number of teams",
      "Participating teams",
      "Total number of players",
      "Consenting players"
    ),
    Value = c(
      as.character(totals$teams_total),
      fmt_count_percent(totals$teams_consenting, totals$teams_total),
      as.character(totals$players_total),
      fmt_count_percent(totals$players_consenting, totals$players_total)
    )
  )
}


## Player characteristics ----

generate_player_characteristics_table <- function(player_details,
                                                  start_date = get_setting("start_date")) {
  
  if (!inherits(player_details$date_birth, "Date")) {
    stop("`date_birth` must be a Date. Parse it where the file is read, not here.")
  }
  
  players <- filter(player_details, consent %in% "yes")
  
  bind_rows(
    describe_variable(calculate_age(players$date_birth, start_date), "Age"),
    describe_variable(players$height, "Height (cm)"),
    describe_variable(players$weight, "Body mass (kg)")
  )
}


# Median, interquartile range and range for one variable.
describe_variable <- function(x, label) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(tibble::tibble(Variable = label, Median = NA_character_,
                          IQR = NA_character_, Range = NA_character_))
  }
  
  tibble::tibble(
    Variable = label,
    Median   = as.character(round(median(x))),
    IQR      = paste0(round(quantile(x, 0.25)), "\u2013", round(quantile(x, 0.75))),
    Range    = paste0(round(min(x)), "\u2013", round(max(x)))
  )
}


## Exposure ----

# Total exposure for the competition. Returns one row, used as the denominator
# throughout the analysis.
summarise_exposure <- function(exposure_training, exposure_match) {
  
  missing_training <- sum(is.na(exposure_training$training_minutes))
  
  if (missing_training > 0) {
    warning(missing_training, " player-days still have no training exposure. ",
            "They are counted as zero hours. Check the imputation step.",
            call. = FALSE)
  }
  
  match_hours    <- sum(exposure_match$total_playing_time, na.rm = TRUE) / 60
  training_hours <- sum(exposure_training$training_minutes, na.rm = TRUE) / 60
  
  tibble::tibble(
    match       = match_hours,
    training    = training_hours,
    total       = match_hours + training_hours,
    player_days = nrow(exposure_training)
  )
}


generate_exposure_table <- function(data_exposure) {
  
  tibble::tibble(
    Metric = c(
      "Match exposure (h)",
      "Training exposure (h)",
      "Total exposure (h)",
      "Player days"
    ),
    Value = round(c(
      data_exposure$match,
      data_exposure$training,
      data_exposure$total,
      data_exposure$player_days
    ))
  )
}


## Basic numbers ----

generate_basic_numbers_table <- function(caselist) {
  
  caselist %>%
    mutate(problem_type = recode(as.character(problem_type), !!!problem_type_plural)) %>%
    group_by(problem_type) %>%
    summarise(
      Events             = n_distinct(event_id),
      Cases              = n_distinct(case_id),
      `Affected players` = n_distinct(player_id),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      problem_type = unname(problem_type_plural),
      fill = list(Events = 0L, Cases = 0L, `Affected players` = 0L)
    ) %>%
    filter(problem_type %in% unname(problem_type_plural)) %>%
    arrange(factor(problem_type, levels = unname(problem_type_plural)))
}


## Subsequent cases ----

generate_subsequent_table <- function(caselist) {
  
  columns <- c("Index", "Exacerbation", "Unknown",
               "Recurrence (early)", "Recurrence (late)", "Recurrence (delayed)")
  
  classified <- caselist %>%
    mutate(
      problem_type = recode(as.character(problem_type), !!!problem_type_plural),
      
      category = case_when(
        tolower(subsequent_cat) == "index"        ~ "index",
        tolower(subsequent_cat) == "exacerbation" ~ "exacerbation",
        tolower(subsequent_cat) %in% c("re-injury", "reinjury", "recurrent injury",
                                       "recurrent illness", "recurrence") ~ "recurrence",
        TRUE ~ "unknown"
      ),
      
      timing = ifelse(
        tolower(recurrence_type) %in% c("early", "late", "delayed"),
        tolower(recurrence_type),
        NA_character_
      ),
      
      column = case_when(
        category == "index"        ~ "Index",
        category == "exacerbation" ~ "Exacerbation",
        category == "recurrence" & timing == "early"   ~ "Recurrence (early)",
        category == "recurrence" & timing == "late"    ~ "Recurrence (late)",
        category == "recurrence" & timing == "delayed" ~ "Recurrence (delayed)",
        TRUE ~ "Unknown"
      )
    )
  
  classified %>%
    count(problem_type, column) %>%
    tidyr::pivot_wider(names_from = column, values_from = n, values_fill = 0) %>%
    tidyr::complete(problem_type = unname(problem_type_plural)) %>%
    add_missing_columns(columns) %>%
    mutate(across(all_of(columns), ~ tidyr::replace_na(.x, 0L))) %>%
    arrange(factor(problem_type, levels = unname(problem_type_plural))) %>%
    select(problem_type, all_of(columns))
}


# Add any expected columns the data did not produce, filled with zero.
add_missing_columns <- function(data, columns) {
  
  for (column in setdiff(columns, names(data))) {
    data[[column]] <- 0L
  }
  
  data
}