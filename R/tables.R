# Report tables ----
#
# Functions that turn analysis output into tables for the report. They return
# data frames. Only write_pattern_table_excel() touches the disk, and only
# where it is told to.
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
generate_player_characteristics_table <- function(player_details) {
  
  if (!"age" %in% names(player_details)) {
    stop("`player_details` has no `age` column. Age is computed in ",
         "01_prepare_data.R, before de-identification, and date_birth is dropped there.")
  }
  
  players <- filter(player_details, consent %in% "yes")
  
  bind_rows(
    describe_variable(players$age,    "Age"),
    describe_variable(players$height, "Height (cm)"),
    describe_variable(players$weight, "Body mass (kg)")
  )
}



# Median, interquartile range and range for one variable.
describe_variable <- function(x, label) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(tibble::tibble(
      Variable = label,
      Median   = NA_character_,
      IQR      = NA_character_,
      Range    = NA_character_
    ))
  }
  
  tibble::tibble(
    Variable = label,
    Median   = as.character(round(median(x))),
    IQR      = paste0(round(quantile(x, 0.25)), "\u2013", round(quantile(x, 0.75))),
    Range    = paste0(round(min(x)), "\u2013", round(max(x)))
  )
}


## Exposure ----

# Total exposure for the competition. One row, used as the denominator
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
  
  expected_columns <- c(
    "Index", "Exacerbation", "Unknown",
    "Recurrence (early)", "Recurrence (late)", "Recurrence (delayed)"
  )
  
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
    add_missing_columns(expected_columns) %>%
    mutate(across(all_of(expected_columns), ~ tidyr::replace_na(.x, 0L))) %>%
    arrange(factor(problem_type, levels = unname(problem_type_plural))) %>%
    select(problem_type, all_of(expected_columns))
}


# Add any expected columns the data did not produce, filled with zero.
add_missing_columns <- function(data, columns) {
  
  for (column in setdiff(columns, names(data))) {
    data[[column]] <- 0L
  }
  
  data
}


## Combining incidence and burden ----

# Run incidence and burden over the same subset and put them side by side.
#
# `specs` is a list of lists, each with `label`, `data` and `exposure`.
# Both analyses return exactly one row per subset, so the results are bound
# by position - there is nothing to join, and so nothing to mismatch.
combine_incidence_burden <- function(specs) {
  
  rows <- lapply(specs, function(spec) {
    
    incidence <- calculate_incidence_severity(spec$data, spec$exposure)
    burden    <- calculate_burden(spec$data, spec$exposure)
    
    bind_cols(
      tibble::tibble(outcome = spec$label),
      incidence,
      select(burden, burden_rate, lower_bound, upper_bound)
    )
  })
  
  bind_rows(rows)
}


# Format a combined result table for the report.
format_summary_table <- function(results,
                                 exposure_label = "Exposure (h)",
                                 min_n_severity = get_setting("min_n_severity", 2)) {
  
  results <- suppress_severity(results, min_n_severity)
  
  out <- results %>%
    transmute(
      Outcome                   = outcome,
      exposure                  = round(exposure),
      `Cases (n)`               = n_cases,
      `Incidence rate [95% CI]` = fmt_ci(incidence_rate, ci_lower, ci_upper),
      `Time loss (days)`        = fmt(total_timeloss, 0),
      `Burden rate [95% CI]`    = fmt_ci(burden_rate, lower_bound, upper_bound)
    )
  
  names(out)[2] <- exposure_label
  
  out
}


## Injuries, per 1000 hours ----

generate_injury_summary_table <- function(caselist, data_exposure) {
  
  injuries <- filter(caselist, problem_type %in% "Injury")
  
  specs <- list(
    list(label = "All injuries",
         data  = injuries,
         exposure = data_exposure$total),
    
    list(label = "Gradual onset",
         data  = filter(injuries, onset %in% "Gradual-onset"),
         exposure = data_exposure$total),
    
    list(label = "Sudden onset",
         data  = filter(injuries, onset %in% "Sudden-onset"),
         exposure = data_exposure$total),
    
    list(label = "Match injuries",
         data  = filter(injuries, when_occurred %in% "Match"),
         exposure = data_exposure$match),
    
    list(label = "Training injuries",
         data  = filter(injuries, when_occurred %in% "Training"),
         exposure = data_exposure$training)
  )
  
  combine_incidence_burden(specs)
}


## All health problems, per 1000 player-days ----

generate_health_problems_table <- function(caselist, data_exposure) {
  
  specs <- lapply(names(problem_type_plural), function(type) {
    list(
      label    = unname(problem_type_plural[type]),
      data     = filter(caselist, problem_type %in% type),
      exposure = data_exposure$player_days
    )
  })
  
  combine_incidence_burden(specs)
}


## Severity distribution ----

severity_levels <- c(
  "0 days", "1-3 days", "4-7 days", "8-28 days",
  "29-90 days", "91-180 days", ">180 days"
)


classify_severity <- function(timeloss) {
  cut(
    timeloss,
    breaks = c(-Inf, 0, 3, 7, 28, 90, 180, Inf),
    labels = severity_levels,
    right  = TRUE
  )
}


# Injuries by body area and time-loss band, with a matching total row.
generate_severity_distribution <- function(caselist) {
  
  injuries <- caselist %>%
    filter(problem_type %in% "Injury") %>%
    mutate(severity = classify_severity(timeloss))
  
  report_dropped(injuries, "severity",          "time loss could not be classified")
  report_dropped(injuries, "osiics_16_level_1", "body area is missing")
  
  injuries <- filter(injuries, !is.na(severity), !is.na(osiics_16_level_1))
  
  by_area <- injuries %>%
    count(osiics_16_level_1, severity) %>%
    tidyr::pivot_wider(names_from = severity, values_from = n, values_fill = 0) %>%
    add_missing_columns(severity_levels) %>%
    arrange(osiics_16_level_1) %>%
    mutate(osiics_16_level_1 = as.character(osiics_16_level_1))
  
  totals <- injuries %>%
    count(severity) %>%
    tidyr::pivot_wider(names_from = severity, values_from = n, values_fill = 0) %>%
    add_missing_columns(severity_levels) %>%
    mutate(osiics_16_level_1 = "Total")
  
  bind_rows(by_area, totals) %>%
    select(osiics_16_level_1, all_of(severity_levels)) %>%
    mutate(Total = rowSums(across(all_of(severity_levels))))
}


## Body area and tissue type summaries ----

# Cases and total time loss by category, heaviest first. Used for the figures
# the designer builds from.

generate_body_area_summary <- function(caselist) {
  summarise_by_category(caselist, "osiics_16_level_1")
}


generate_tissue_type_summary <- function(caselist) {
  summarise_by_category(caselist, "osiics_16_level_2")
}


summarise_by_category <- function(caselist, column) {
  
  caselist %>%
    filter(problem_type %in% "Injury") %>%
    group_by(across(all_of(column))) %>%
    summarise(
      cases         = n(),
      timeloss_days = sum(timeloss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(timeloss_days))
}


## Pattern tables ----
#
# Incidence, severity and burden broken down through the taxonomy.
#
# Two views are used in the reports:
#   body area -> pathology type -> diagnosis   (levels 1, 3, 4)
#   tissue type -> pathology type              (levels 2, 3)
#
# Both come from generate_pattern_table(). Suppression thresholds come from
# config.R, so the code and the table footnotes cannot drift apart.


pattern_levels_body <- list(
  list(level = 1, vars = "osiics_16_level_1"),
  list(level = 3, vars = c("osiics_16_level_1", "osiics_16_level_3")),
  list(level = 4, vars = c("osiics_16_level_1", "osiics_16_level_3", "osiics_16_level_4"))
)


pattern_levels_tissue <- list(
  list(level = 2, vars = "osiics_16_level_2"),
  list(level = 3, vars = c("osiics_16_level_2", "osiics_16_level_3"))
)


generate_pattern_table <- function(caselist,
                                   exposure,
                                   level_specs,
                                   min_n_deepest  = get_setting("min_n_diagnosis", 5),
                                   min_n_severity = get_setting("min_n_severity", 2),
                                   min_n_iqr      = get_setting("min_n_iqr", 5)) {
  
  check_exposure(exposure)
  
  # One block of results per level, joined on that level's grouping variables.
  blocks <- lapply(level_specs, function(spec) {
    
    incidence <- calculate_incidence_severity(caselist, exposure, spec$vars)
    burden    <- calculate_burden(caselist, exposure, spec$vars)
    
    left_join(incidence, burden, by = spec$vars) %>%
      mutate(level = spec$level)
  })
  
  combined <- bind_rows(blocks)
  
  deepest_level <- max(vapply(level_specs, function(s) s$level, numeric(1)))
  all_vars      <- level_specs[[length(level_specs)]]$vars
  
  # Rows at the deepest level are shown only when there are enough cases.
  combined <- filter(combined, level != deepest_level | n_cases >= min_n_deepest)
  
  combined <- add_pattern_label(combined, level_specs)
  combined <- sort_pattern_table(combined, all_vars)
  combined <- suppress_severity(combined, min_n_severity)
  
  combined %>%
    mutate(
      q1_timeloss = ifelse(n_cases >= min_n_iqr, q1_timeloss, NA_real_),
      q3_timeloss = ifelse(n_cases >= min_n_iqr, q3_timeloss, NA_real_)
    ) %>%
    transmute(
      level,
      label,
      n_cases,
      incidence_rate  = fmt(incidence_rate, 2),
      incidence_ci    = fmt_ci_only(ci_lower, ci_upper),
      median_timeloss = fmt(median_timeloss, 0),
      median_iqr      = fmt_iqr_only(q1_timeloss, q3_timeloss),
      burden_rate     = fmt(burden_rate, 2),
      burden_ci       = fmt_ci_only(lower_bound, upper_bound)
    )
}


generate_body_pattern_table <- function(caselist, exposure, ...) {
  generate_pattern_table(caselist, exposure, pattern_levels_body, ...)
}


generate_tissue_pattern_table <- function(caselist, exposure, ...) {
  generate_pattern_table(caselist, exposure, pattern_levels_tissue, ...)
}


# The label for each row is the deepest category that row is grouped by.
add_pattern_label <- function(combined, level_specs) {
  
  combined$label <- NA_character_
  
  for (spec in level_specs) {
    deepest <- spec$vars[length(spec$vars)]
    rows    <- combined$level == spec$level
    combined$label[rows] <- as.character(combined[[deepest]][rows])
  }
  
  combined
}


# Hierarchical ordering: parent category, then child category, then the header
# row before its children, then remaining categories alphabetically.
# Categories order by their factor levels, which osiics.R defines.
sort_pattern_table <- function(combined, all_vars) {
  
  var_1 <- all_vars[1]
  var_2 <- if (length(all_vars) >= 2) all_vars[2] else NULL
  var_3 <- if (length(all_vars) >= 3) all_vars[3] else NULL
  
  combined$sort_1 <- as.integer(combined[[var_1]])
  
  combined$sort_2 <- if (is.null(var_2)) {
    0L
  } else {
    dplyr::coalesce(as.integer(combined[[var_2]]), 0L)
  }
  
  combined$sort_3 <- combined$level
  
  combined$sort_4 <- if (is.null(var_3)) {
    ""
  } else {
    dplyr::coalesce(as.character(combined[[var_3]]), "")
  }
  
  arrange(combined, sort_1, sort_2, sort_3, sort_4)
}


## Interval formatting for pattern tables ----
#
# These keep the estimate and its interval in separate columns, because the
# report merges the header cells above each pair.

fmt_ci_only <- function(lower, upper, digits = 2) {
  ifelse(
    is.na(lower) | is.na(upper),
    "",
    paste0(" [", fmt(lower, digits), ", ", fmt(upper, digits), "]")
  )
}


fmt_iqr_only <- function(q1, q3) {
  ifelse(
    is.na(q1) | is.na(q3),
    "",
    paste0(" (", fmt(q1, 0), ", ", fmt(q3, 0), ")")
  )
}


## Excel output ----

# Write a pattern table to Excel, indenting and italicising by level.
# The path is an argument - the function writes where it is told and nowhere else.
write_pattern_table_excel <- function(pattern_table, path, sheet = "table") {
  
  styles <- list(
    openxlsx::createStyle(textDecoration = "bold", border = "top"),
    openxlsx::createStyle(textDecoration = "italic", indent = 10),
    openxlsx::createStyle(textDecoration = "italic", indent = 15)
  )
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, pattern_table)
  
  levels_present <- sort(unique(pattern_table$level))
  
  for (i in seq_along(levels_present)) {
    
    rows <- which(pattern_table$level == levels_present[i]) + 1   # +1 for the header row
    if (length(rows) == 0) next
    
    columns <- if (i == 1) seq_len(ncol(pattern_table)) else 2
    
    openxlsx::addStyle(
      wb, sheet,
      style      = styles[[min(i, length(styles))]],
      rows       = rows,
      cols       = columns,
      gridExpand = TRUE,
      stack      = TRUE
    )
  }
  
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(pattern_table)), widths = "auto")
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  
  invisible(path)
}


## Disclosure control ----
#
# Counts and incidence rates are shown at any number of cases: they add nothing
# to the case count, which is published. Median time loss, total time loss and
# burden all encode how long individual players were unavailable, and exposure
# is published in the same report, so those figures are recoverable at the
# level of one player. They are withheld below the threshold.

suppress_severity <- function(results, min_n_severity = get_setting("min_n_severity", 2)) {
  
  below <- results$n_cases < min_n_severity
  
  for (column in c("median_timeloss", "total_timeloss",
                   "burden_rate", "lower_bound", "upper_bound")) {
    
    if (column %in% names(results)) {
      results[[column]][below] <- NA_real_
    }
  }
  
  results
}


# Footnote text generated from the thresholds actually in force, so the report
# cannot describe a rule the code does not apply.
suppression_footnote <- function(min_n_severity  = get_setting("min_n_severity", 2),
                                 min_n_iqr       = get_setting("min_n_iqr", 5),
                                 min_n_diagnosis = get_setting("min_n_diagnosis", 5)) {
  
  paste0(
    "Case counts and incidence rates are shown for all categories. ",
    "To protect player confidentiality, figures derived from individual players' ",
    "time loss are withheld for small categories: median time loss and burden are ",
    "shown only where there are at least ", min_n_severity, " cases; the interquartile ",
    "range only where there are at least ", min_n_iqr, " cases; and an individual ",
    "diagnosis only where at least ", min_n_diagnosis,
    " cases of that diagnosis were recorded."
  )
}


## Checks ----

# Warn when rows are about to be excluded, so a silent drop becomes a visible one.
report_dropped <- function(data, column, reason) {
  
  n <- sum(is.na(data[[column]]))
  
  if (n > 0) {
    warning(n, " cases excluded because ", reason, ".", call. = FALSE)
  }
  
  invisible(n)
}