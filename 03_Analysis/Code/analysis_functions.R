
# Load required packages
load_or_install(c("tidyverse", "boot", "tibble", "openxlsx"))


                

# Function to calculate age in years
calculate_age <- function(birthdate, specific_day) {
  if (is.na(birthdate)) {
    return(NA)  # Return NA if birthdate is missing
  } else {
    age <- as.numeric(format(specific_day, "%Y")) - as.numeric(format(birthdate, "%Y"))
    if (as.Date(format(specific_day, "%Y-%m-%d")) < as.Date(format(birthdate, "%Y-%m-%d"))) {
      age <- age - 1
    }
    return(age)
  }
}
# Example: calculate players' age in years on first day of tournament

# start_date <-  as.Date("2024-07-19") # First day of competition
# birthdate <- as.Date("1978-07-21")
# calculate_age(birthdate, start_date)





# Incidence and Severity Calculation Function ------
# Calculates incidence rate per 1000 hours and Poisson confidence intervals



# Notes:
# This code calculates incidence rates and confidence intervals.
# - If grouping_vars is empty (grouping_vars <- c()), it calculates overall incidence.
# - If grouping_vars contains variable names (e.g., c("body_area", "tissue")), 
#   it calculates incidence rates for each combination of these variables.
# - To change the grouping, modify the grouping_vars vector at the beginning of the script.
# - The code automatically adapts to calculate either grouped or overall incidence based on grouping_vars.



# Assumptions of this method:
# 1. Poisson distribution: Injuries follow a Poisson distribution (rare, independent events).
# 2. Constant rate: The injury rate remains constant over the exposure period.
# 3. Independence: Each injury event is independent of other injury events.
# 4. Large sample size: Sufficiently large sample for Poisson distribution approximations.
# 5. Accurate exposure time: Total exposure time (hours) is accurately measured and reported.
# 6. Complete injury reporting: All relevant injuries are correctly identified and reported.
# 7. Homogeneity: The population at risk is homogeneous in terms of injury risk.
# 8. No repeated injuries: Method doesn't account for multiple injuries to the same individual.
# 9. Linear relationship: Assumed linear relationship between exposure time and injury risk.
# 10. Rare events: Injuries are relatively rare compared to the total exposure time.

# Note: Violations of these assumptions could lead to biased or inaccurate estimates of injury rates.





calculate_incidence_severity <- function(caselist, exposure, grouping_vars = c()) {
  options(scipen = 999)  # Avoid scientific notation
  
  # Internal function to calculate Poisson confidence intervals
  calculate_poisson_ci <- function(injuries, exposure) {
    result <- poisson.test(injuries, T = exposure, conf.level = 0.95)
    data.frame(
      ci_lower = result$conf.int[1] * 1000,
      ci_upper = result$conf.int[2] * 1000
    )
  }
  
  # Apply grouping if specified
  if (length(grouping_vars) > 0) {
    caselist <- caselist %>% group_by(across(all_of(grouping_vars)))
  }
  
  caselist %>%
    summarise(
      n_cases = n(),
      total_timeloss = sum(timeloss, na.rm = TRUE),
      median_timeloss = median(timeloss, na.rm = TRUE),
      q1_timeloss = quantile(timeloss, 0.25, na.rm = TRUE),
      q3_timeloss = quantile(timeloss, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_exposure_hours = exposure,
      incidence_rate = (n_cases / total_exposure_hours) * 1000
    ) %>%
    rowwise() %>%
    mutate(calculate_poisson_ci(n_cases, total_exposure_hours)) %>%
    ungroup() %>%
    select(-total_exposure_hours)
}


# Helper function to run and label each analysis
run_analysis <- function(caselist_input, exposure_input, outcome_label, definition_label) {
  calculate_incidence_severity(caselist_input, exposure_input) %>%
    mutate(outcome = outcome_label, definition = definition_label) %>%
    select(outcome, definition, everything()) %>%
    print()
}




# Burden analysis -------



# Note on Confidence Interval Calculation:
# This code attempts to calculate a confidence interval for any group with at least
# two cases (n > 1) that show variability in the bootstrapped samples.
# - If there's only one case or if all bootstrapped values are identical (no variability),
#   the confidence interval is set to NA.
# - For groups with n > 1 and variability in the data, a CI is calculated.
# - The code does not enforce a minimum number of cases for CI calculation beyond n > 1.
# - While CIs can be calculated for small n, they may be very wide and less informative.
# - Interpret CIs from small samples with caution.


# Function: calculate_burden

calculate_burden <- function(caselist, exposure, grouping_vars = c()) {
  grouping_vars <- grouping_vars[grouping_vars != ""]
  valid_grouping_vars <- intersect(grouping_vars, names(caselist))
  
  df <- caselist %>%
    group_by(across(all_of(c(valid_grouping_vars, "player_id")))) %>%
    summarise(
      n_cases = n(),
      timeloss = sum(timeloss),
      .groups = "drop"
    ) %>%
    mutate(burden = timeloss / exposure * 1000)
  
  group_combinations <- if (length(valid_grouping_vars) > 0) {
    df %>% select(all_of(valid_grouping_vars)) %>% distinct()
  } else {
    tibble(.dummy = 1)
  }
  
  bootSum <- function(data, indices) sum(data[indices], na.rm = TRUE)
  results_list <- list()
  
  for (i in 1:nrow(group_combinations)) {
    current_group <- if (length(valid_grouping_vars) > 0) group_combinations[i, ] else tibble()
    subset_data <- if (length(valid_grouping_vars) > 0) {
      df %>% inner_join(current_group, by = valid_grouping_vars)
    } else {
      df
    }
    
    if (nrow(subset_data) == 0) {
      cat("No data for", if (length(valid_grouping_vars) > 0) paste(current_group, collapse = ", ") else "overall analysis", "\n")
      next
    }
    
    burden_boot <- boot(subset_data$burden, bootSum, R = 100000)
    
    if (all(burden_boot$t == burden_boot$t[1])) {
      cat("No variability in bootstrap distribution for", if (length(valid_grouping_vars) > 0) paste(current_group, collapse = ", ") else "overall analysis", "\n")
      ci_lower <- ci_upper <- NA
    } else {
      ci <- boot.ci(burden_boot, type = "perc", conf = 0.95)
      ci_lower <- ci$perc[4]
      ci_upper <- ci$perc[5]
    }
    
    results_row <- if (length(valid_grouping_vars) > 0) {
      current_group %>%
        mutate(
          burden_rate = sum(subset_data$burden, na.rm = TRUE),
          lower_bound = ci_lower,
          upper_bound = ci_upper
        )
    } else {
      tibble(
        burden_rate = sum(subset_data$burden, na.rm = TRUE),
        lower_bound = ci_lower,
        upper_bound = ci_upper
      )
    }
    
    results_list[[i]] <- results_row
  }
  
  bind_rows(results_list)
}

# Helper: Run and label burden analysis
run_analysis <- function(caselist_input, exposure_input, outcome, definition) {
  calculate_burden(caselist_input, exposure_input) %>%
    mutate(outcome = outcome, definition = definition) %>%
    select(outcome, definition, everything())
}


# Generate tables --------------

generate_table_1_3_4 <- function(caselist_input, exposure_input, output_path) {
  # Incidence and severity
  levels <- list(
    list(level = 1, grouping_vars = c("osiics_15_level_1"), fill = list(osiics_15_level_2 = "All", osiics_15_level_3 = "All", osiics_15_level_4 = "All")),
    list(level = 2, grouping_vars = c("osiics_15_level_1", "osiics_15_level_2"), fill = list(osiics_15_level_3 = "All", osiics_15_level_4 = "All")),
    list(level = 3, grouping_vars = c("osiics_15_level_1", "osiics_15_level_2", "osiics_15_level_3"), fill = list(osiics_15_level_4 = "All")),
    list(level = 4, grouping_vars = c("osiics_15_level_1", "osiics_15_level_2", "osiics_15_level_3", "osiics_15_level_4"), fill = list())
  )
  
  incidence_tables <- lapply(levels, function(lvl) {
    calculate_incidence_severity(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      mutate(level = lvl$level) %>%
      mutate(!!!lvl$fill) %>%
      select(level, osiics_15_level_1, osiics_15_level_2, osiics_15_level_3, osiics_15_level_4,
             n_cases, incidence_rate, ci_lower, ci_upper, total_timeloss, median_timeloss, q1_timeloss, q3_timeloss)
  })
  
  table_1 <- bind_rows(incidence_tables)
  
  # Burden
  burden_tables <- lapply(levels, function(lvl) {
    calculate_burden(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      mutate(level = lvl$level) %>%
      mutate(!!!lvl$fill) %>%
      select(level, osiics_15_level_1, osiics_15_level_2, osiics_15_level_3, osiics_15_level_4,
             burden_rate, lower_bound, upper_bound)
  })
  
  table_burden <- bind_rows(burden_tables)
  
  # Join tables
  table_1 <- left_join(table_1, table_burden)
  
  # Apply factor levels
  table_1$osiics_15_level_1 <- factor(table_1$osiics_15_level_1, levels = level_1_order)
  table_1$osiics_15_level_2 <- factor(table_1$osiics_15_level_2, levels = level_2_order)
  table_1$osiics_15_level_3 <- factor(table_1$osiics_15_level_3, levels = level_3_order)
  table_1$osiics_15_level_4 <- factor(table_1$osiics_15_level_4, levels = level_4_order)
  
  table_1 <- table_1 %>%
    arrange(osiics_15_level_1, osiics_15_level_2, osiics_15_level_3) %>%
    mutate(
      label = case_when(
        level == 1 ~ as.character(osiics_15_level_1),
        level == 2 ~ as.character(osiics_15_level_2),
        level == 3 ~ as.character(osiics_15_level_3),
        level == 4 ~ as.character(osiics_15_level_4)
      ),
      sort_order = row_number(),
      
      incidence_rate = sprintf("%.2f", incidence_rate),
      burden_rate = sprintf("%.2f", burden_rate),
      median_timeloss = sprintf("%.2f", median_timeloss),
      
      incidence_ci = paste0(" [", sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]"),
      median_iqr = paste0(" (", q1_timeloss, ", ", q3_timeloss, ")"),
      burden_ci = ifelse(
        is.na(lower_bound) | is.na(upper_bound),
        "",
        paste0(" [", sprintf("%.2f", lower_bound), ", ", sprintf("%.2f", upper_bound), "]")
      )
    ) %>%
    select(level, label, n_cases, incidence_rate, incidence_ci, median_timeloss, median_iqr, burden_rate, burden_ci)
  
  
  # Save as Excel file
  style_level_1 <- openxlsx::createStyle(textDecoration = "bold", border = "top")
  style_level_2 <- openxlsx::createStyle(indent = 5, halign = "left")
  style_level_3 <- openxlsx::createStyle(textDecoration = "italic", indent = 10)
  style_level_4 <- openxlsx::createStyle(textDecoration = "italic", indent = 15)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "table_1")
  openxlsx::writeData(wb, "table_1", table_1)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2:9, rows = 2:1000, rule = "$A2=1", style = style_level_1)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2, rows = 2:1000, rule = "$A2=2", style = style_level_2)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2, rows = 2:1000, rule = "$A2=3", style = style_level_3)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2, rows = 2:1000, rule = "$A2=4", style = style_level_4)
  openxlsx::setColWidths(wb, "table_1", cols = 1:9, widths = "auto")
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
}



generate_table_2_3 <- function(caselist_input, exposure_input, output_path) {
  # Define levels for Table 2
  levels <- list(
    list(level = 2, grouping_vars = c("osiics_15_level_2"), fill = list(osiics_15_level_3 = "All")),
    list(level = 3, grouping_vars = c("osiics_15_level_2", "osiics_15_level_3"), fill = list())
  )
  
  # Incidence and severity
  incidence_tables <- lapply(levels, function(lvl) {
    calculate_incidence_severity(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      mutate(level = lvl$level) %>%
      mutate(!!!lvl$fill) %>%
      select(level, osiics_15_level_2, osiics_15_level_3,
             n_cases, incidence_rate, ci_lower, ci_upper,
             total_timeloss, median_timeloss, q1_timeloss, q3_timeloss)
  })
  
  table_2 <- bind_rows(incidence_tables)
  
  # Burden
  burden_tables <- lapply(levels, function(lvl) {
    calculate_burden(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      mutate(level = lvl$level) %>%
      mutate(!!!lvl$fill) %>%
      select(level, osiics_15_level_2, osiics_15_level_3,
             burden_rate, lower_bound, upper_bound)
  })
  
  table_burden <- bind_rows(burden_tables)
  
  # Join tables
  table_2 <- left_join(table_2, table_burden)
  
  # Apply factor levels
  table_2$osiics_15_level_2 <- factor(table_2$osiics_15_level_2, levels = level_2_order)
  table_2$osiics_15_level_3 <- factor(table_2$osiics_15_level_3, levels = level_3_order)
  
  table_2 <- table_2 %>%
    arrange(osiics_15_level_2, osiics_15_level_3) %>%
    mutate(
      label = case_when(
        level == 2 ~ as.character(osiics_15_level_2),
        level == 3 ~ as.character(osiics_15_level_3)
      ),
      sort_order = row_number(),
      incidence_rate = sprintf("%.2f", incidence_rate),
      burden_rate = sprintf("%.2f", burden_rate),
      median_timeloss = sprintf("%.2f", median_timeloss),
      
      incidence_ci = paste0(" [", sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]"),
      median_iqr = paste0(" (", q1_timeloss, ", ", q3_timeloss, ")"),
      burden_ci = ifelse(
        is.na(lower_bound) | is.na(upper_bound),
        "",
        paste0(" [", sprintf("%.2f", lower_bound), ", ", sprintf("%.2f", upper_bound), "]")
      )) %>%
    select(level, label, n_cases, incidence_rate, incidence_ci,
           median_timeloss, median_iqr, burden_rate, burden_ci)
  
  # Save as Excel file
  style_level_2 <- openxlsx::createStyle(textDecoration = "bold", border = "top")
  style_level_3 <- openxlsx::createStyle(textDecoration = "italic", indent = 5)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "table_2")
  openxlsx::writeData(wb, "table_2", table_2)
  openxlsx::conditionalFormatting(wb, "table_2", cols = 2:9, rows = 2:1000, rule = "$A2=2", style = style_level_2)
  openxlsx::conditionalFormatting(wb, "table_2", cols = 2, rows = 2:1000, rule = "$A2=3", style = style_level_3)
  openxlsx::setColWidths(wb, "table_2", cols = 1:9, widths = "auto")
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
}

# can we have one function where we define which levels to include?
# can we censor level 4 based on minimum cell size (defined in function call)?
