# Analysis functions ----
#
# The statistical core: age, incidence, severity, burden and rate ratios.
# These functions compute and return. They do not print, and they do not
# write files - formatting and output live in tables.R.
#
# Conventions:
#   caselist   one row per case, with columns player_id, timeloss
#   exposure   a single number, in the units the rate is expressed per
#              (hours for injuries, player-days for illnesses)
#
# Rates are per 1000 exposure units throughout.


## Age ----

# Age in completed years on a given date. Vectorised; returns NA for a
# missing birth date.
calculate_age <- function(birthdate, on_date) {
  
  birthdate <- as.Date(birthdate)
  on_date   <- as.Date(on_date)
  
  years <- as.integer(format(on_date, "%Y")) - as.integer(format(birthdate, "%Y"))
  
  # A birthday later in the year than `on_date` means one fewer completed year.
  had_birthday <- as.integer(format(on_date,   "%m%d")) >=
    as.integer(format(birthdate, "%m%d"))
  
  years - !had_birthday
}


## Incidence and severity ----

# Case counts, time-loss summaries, and incidence rates with exact Poisson
# confidence intervals.
#
# `exposure` is a single number. Where `grouping_vars` is supplied, every group
# is expressed against that same total exposure - the denominator is the whole
# cohort, not the cases in the group.
calculate_incidence_severity <- function(caselist, exposure, grouping_vars = character()) {
  
  check_exposure(exposure)
  
  if (length(grouping_vars) > 0) {
    caselist <- group_by(caselist, across(all_of(grouping_vars)))
  }
  
  result <- caselist %>%
    summarise(
      n_cases         = n(),
      total_timeloss  = sum(timeloss, na.rm = TRUE),
      median_timeloss = median(timeloss, na.rm = TRUE),
      q1_timeloss     = quantile(timeloss, 0.25, na.rm = TRUE),
      q3_timeloss     = quantile(timeloss, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      exposure       = exposure,
      incidence_rate = n_cases / exposure * 1000
    )
  
  ci <- t(vapply(result$n_cases, poisson_rate_ci, numeric(2), exposure = exposure))
  
  result$ci_lower <- ci[, 1]
  result$ci_upper <- ci[, 2]
  
  result
}


# Exact Poisson confidence interval for one case count, per 1000 units.
poisson_rate_ci <- function(n_cases, exposure) {
  
  test <- stats::poisson.test(n_cases, T = exposure, conf.level = 0.95)
  
  c(ci_lower = test$conf.int[1] * 1000,
    ci_upper = test$conf.int[2] * 1000)
}


## Burden ----

# Time-loss burden - days lost per 1000 exposure units - with a bootstrap
# percentile confidence interval.
#
# Time loss is summed per player before resampling, so the bootstrap resamples
# players rather than cases. Two cases in the same player are not independent.
calculate_burden <- function(caselist,
                             exposure,
                             grouping_vars = character(),
                             replicates = get_setting("boot_replicates", 10000),
                             seed       = get_setting("random_seed", 1)) {
  
  check_exposure(exposure)
  
  grouping_vars <- intersect(grouping_vars, names(caselist))
  
  per_player <- caselist %>%
    group_by(across(all_of(c(grouping_vars, "player_id")))) %>%
    summarise(timeloss = sum(timeloss, na.rm = TRUE), .groups = "drop")
  
  groups <- if (length(grouping_vars) > 0) {
    distinct(select(per_player, all_of(grouping_vars)))
  } else {
    tibble::tibble(.overall = TRUE)
  }
  
  set.seed(seed)
  
  results <- lapply(seq_len(nrow(groups)), function(i) {
    
    rows <- if (length(grouping_vars) > 0) {
      inner_join(per_player, groups[i, ], by = grouping_vars)
    } else {
      per_player
    }
    
    burden_ci(rows$timeloss, exposure, replicates)
  })
  
  results <- bind_rows(results)
  
  if (length(grouping_vars) > 0) {
    bind_cols(groups, results)
  } else {
    results
  }
}


# Burden and its bootstrap interval for one set of players.
# The interval is left as NA where it would be meaningless: fewer than two
# players, no time loss, or no variation between players.
burden_ci <- function(timeloss, exposure, replicates) {
  
  total <- sum(timeloss, na.rm = TRUE)
  
  out <- tibble::tibble(
    burden_rate = total / exposure * 1000,
    lower_bound = NA_real_,
    upper_bound = NA_real_
  )
  
  if (length(timeloss) < 2 || total == 0 || stats::var(timeloss) == 0) {
    return(out)
  }
  
  draws <- boot::boot(
    data      = timeloss,
    statistic = function(x, i) sum(x[i], na.rm = TRUE),
    R         = replicates
  )
  
  bounds <- stats::quantile(draws$t, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  
  out$lower_bound <- bounds[1] / exposure * 1000
  out$upper_bound <- bounds[2] / exposure * 1000
  
  out
}


## Labelled wrappers ----

# Run one analysis and label it, so several can be stacked into one table.

run_analysis_incidence <- function(caselist, exposure, outcome, definition = "Time loss") {
  
  calculate_incidence_severity(caselist, exposure) %>%
    mutate(outcome = outcome, definition = definition) %>%
    relocate(outcome, definition)
}


run_analysis_burden <- function(caselist, exposure, outcome, definition = "Time loss") {
  
  calculate_burden(caselist, exposure) %>%
    mutate(outcome = outcome, definition = definition) %>%
    relocate(outcome, definition)
}


## Rate ratios ----

# Exact incidence rate ratio between two groups, with a Poisson confidence
# interval. Use for comparing competitions, or men against women.
#
# `units` is a label only - "1000 h", "1000 player-days" - carried through to
# the output so the rates cannot be misread later.
incidence_rate_ratio <- function(cases_a, exposure_a,
                                 cases_b, exposure_b,
                                 labels = c("A", "B"),
                                 units  = "1000 h",
                                 conf_level = 0.95) {
  
  if (any(c(cases_a, cases_b) < 0)) {
    stop("Case counts must be zero or greater.")
  }
  
  check_exposure(exposure_a)
  check_exposure(exposure_b)
  
  if (length(labels) != 2) {
    stop("`labels` must be length 2, for example c('Women', 'Men').")
  }
  
  # poisson.test with two counts compares rate 1 against rate 2, so passing
  # (b, a) gives the ratio of b to a.
  test <- stats::poisson.test(
    x = c(cases_b, cases_a),
    T = c(exposure_b, exposure_a),
    r = 1,
    conf.level = conf_level
  )
  
  ci_a <- stats::poisson.test(cases_a, T = exposure_a, conf.level = conf_level)
  ci_b <- stats::poisson.test(cases_b, T = exposure_b, conf.level = conf_level)
  
  tibble::tibble(
    group      = labels,
    cases      = c(cases_a, cases_b),
    exposure   = c(exposure_a, exposure_b),
    rate       = c(cases_a / exposure_a, cases_b / exposure_b) * 1000,
    rate_lower = c(ci_a$conf.int[1], ci_b$conf.int[1]) * 1000,
    rate_upper = c(ci_a$conf.int[2], ci_b$conf.int[2]) * 1000,
    units      = paste("per", units),
    irr        = c(NA_real_, test$estimate),
    irr_lower  = c(NA_real_, test$conf.int[1]),
    irr_upper  = c(NA_real_, test$conf.int[2]),
    p_value    = c(NA_real_, test$p.value)
  )
}


## Checks ----

check_exposure <- function(exposure) {
  
  if (length(exposure) != 1 || !is.finite(exposure) || exposure <= 0) {
    stop("`exposure` must be a single positive number. Received: ",
         paste(exposure, collapse = ", "))
  }
  
  invisible(TRUE)
}