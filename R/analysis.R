source("~/Library/CloudStorage/OneDrive-FIFA.org/Projects (FIFA)/Competition surveillance projects - current/fifa-surveillance-tools/03_Analysis/Code/setup_functions.R")

# Load required packages
load_or_install(c("tidyverse", "boot", "tibble", "openxlsx"))


load_osiics_16 <- function() {
  
  osiics_16 <- read_delim("~/Library/CloudStorage/OneDrive-FIFA.org/Projects (FIFA)/Competition surveillance projects - current/fifa-surveillance-tools/02_Data/osiics_16_fifa_version.csv",
                          delim = ";",
                          locale = readr::locale(encoding = "UTF-8"))
  
# Level 1: Body Part and organ system
level_1_order <- c(
  "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
  "Chest", "Thoracic spine", "Lumbosacral",
  "Abdomen", "Hip/groin", "Hip", "Groin", "Thigh", "Knee",
  "Lower leg", "Ankle", "Foot", "Region unspecified", "Cardiovascular",
  "Dermatological", "Dental", "Endocrinological", "Gastrointestinal",
  "Genitourinary", "Hematological", "Musculoskeletal", "Neurological",
  "Opthalmological", "Otological", "Psychiatric/psychological", "Respiratory",
  "Thermoregulatory", "Multiple systems", "Unknown or not specified"
)


unique(osiics_16$osiics_16_level_2)




# # Level 2: Tissue Type
level_2_order <- c(
  "All", "Muscle/tendon", "Nervous", "Bone",
  "Cartilage/synovium/bursa", "Ligament/joint capsule", "Superficial tissues/skin",
  "Vessels", "Stump", "Internal organs", "Non-specific"
)


# # Level 3: Pathology type and aetiology
level_3_order <- c(
  "All", "Muscle injury", "Muscle contusion", 
  "Muscle compartment syndrome", "Tendinopathy", "Tendon rupture",
  "Brain/spinal cord injury", "Peripheral nerve injury",
  "Fracture", "Bone stress injury", "Bone contusion",
  "Avascular necrosis", "Physis injury", "Cartilage injury",
  "Arthritis", "Synovitis/capsulitis", "Bursitis", "Joint sprain",
  "Chronic instability", "Contusion (superficial)", 
  "Laceration", "Abrasion", "Vascular trauma", "Stump injury", "Organ trauma",
  "Injury without tissue type specified","Allergic",
  "Environmental - exercise-related", "Environmental - non-exercise",
  "Immunological/inflammatory", "Infection", "Neoplasm", "Metabolic/nutritional",
  "Thrombotic/haemorrhagic", "Degenerative or chronic condition",
  "Developmental anomaly", "Drug-related/poisoning", "Multiple",
  "Unknown/not specified", "Unknown"
)
unique(osiics_16$problem_type)

# Apply Factor Levels
osiics_16 <- osiics_16 %>%
  mutate(
    problem_type = factor(problem_type, levels = c("Injury", "Illness", "Mental health problem")),
    osiics_16_level_1 = factor(osiics_16_level_1, levels = level_1_order),
    osiics_16_level_2 = factor(osiics_16_level_2, levels = level_2_order),
    osiics_16_level_3 = factor(osiics_16_level_3, levels = level_3_order)
  ) %>%
  arrange(problem_type, osiics_16_level_1, osiics_16_level_2, osiics_16_level_3)

setdiff(osiics_16$osiics_16_level_1, level_1_order)
setdiff(osiics_16$osiics_16_level_2, level_2_order)
setdiff(osiics_16$osiics_16_level_3, level_3_order)


osiics_16
}


# Level 1: Body Part and organ system
level_1_order <- c(
  "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
  "Chest", "Thoracic spine", "Lumbosacral",
  "Abdomen", "Hip/groin", "Hip", "Groin", "Thigh", "Knee",
  "Lower leg", "Ankle", "Foot", "Region unspecified", "Cardiovascular",
  "Dermatological", "Dental", "Endocrinological", "Gastrointestinal",
  "Genitourinary", "Hematological", "Musculoskeletal", "Neurological",
  "Opthalmological", "Otological", "Psychiatric/psychological", "Respiratory",
  "Thermoregulatory", "Multiple systems", "Unknown or not specified"
)



# # Level 2: Tissue Type
level_2_order <- c(
  "All", "Muscle/tendon", "Nervous", "Bone",
  "Cartilage/synovium/bursa", "Ligament/joint capsule", "Superficial tissues/skin",
  "Vessels", "Stump", "Internal organs", "Non-specific"
)


# # Level 3: Pathology type and aetiology
level_3_order <- c(
  "All", "Muscle injury", "Muscle contusion", 
  "Muscle compartment syndrome", "Tendinopathy", "Tendon rupture",
  "Brain/spinal cord injury", "Peripheral nerve injury",
  "Fracture", "Bone stress injury", "Bone contusion",
  "Avascular necrosis", "Physis injury", "Cartilage injury",
  "Arthritis", "Synovitis/capsulitis", "Bursitis", "Joint sprain",
  "Chronic instability", "Contusion (superficial)", 
  "Laceration", "Abrasion", "Vascular trauma", "Stump injury", "Organ trauma",
  "Injury without tissue type specified","Allergic",
  "Environmental - exercise-related", "Environmental - non-exercise",
  "Immunological/inflammatory", "Infection", "Neoplasm", "Metabolic/nutritional",
  "Thrombotic/haemorrhagic", "Degenerative or chronic condition",
  "Developmental anomaly", "Drug-related/poisoning", "Multiple",
  "Unknown/not specified", "Unknown"
)



level_4_order <- read_delim("~/Library/CloudStorage/OneDrive-FIFA.org/Projects (FIFA)/Competition surveillance projects - current/fifa-surveillance-tools/02_Data/osiics_16_fifa_version.csv", delim = ";")%>%
  pull(osiics_16_level_4)









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
      #exposure = exposure,
      incidence_rate = (n_cases / exposure) * 1000
    ) %>%
    rowwise() %>%
    mutate(calculate_poisson_ci(n_cases, exposure)) %>%
    ungroup() 
}


# Helper function to run and label each analysis
run_analysis_incidence <- function(caselist_input, exposure_input, outcome_label, definition_label) {
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
run_analysis_burden <- function(caselist_input, exposure_input, outcome, definition) {
  calculate_burden(caselist_input, exposure_input) %>%
    mutate(outcome = outcome, definition = definition) %>%
    select(outcome, definition, everything())
}

#Burden 2 -----

# Burden analysis -------

# Robust burden calculator with safe bootstrap CI
calculate_burden <- function(caselist, exposure, grouping_vars = c()) {
  # Sanitize grouping variables
  grouping_vars <- grouping_vars[grouping_vars != ""]
  valid_grouping_vars <- intersect(grouping_vars, names(caselist))
  
  # Summarise per player (and optional grouping vars)
  df <- caselist %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(valid_grouping_vars, "player_id")))) %>%
    dplyr::summarise(
      n_cases  = dplyr::n(),
      timeloss = sum(timeloss, na.rm = TRUE),
      .groups  = "drop"
    )
  
  # Build group combinations (so groups are represented even if df later filters to none)
  group_combinations <- if (length(valid_grouping_vars) > 0) {
    caselist %>% dplyr::select(dplyr::all_of(valid_grouping_vars)) %>% dplyr::distinct()
  } else {
    tibble::tibble(.dummy = 1)
  }
  if (nrow(group_combinations) == 0) {
    group_combinations <- tibble::tibble(.dummy = 1)
  }
  
  # Bootstrap helper (sum of timeloss); CI is computed on timeloss then scaled to burden
  bootSum <- function(x, indices) sum(x[indices], na.rm = TRUE)
  
  results_list <- vector("list", nrow(group_combinations))
  
  for (i in seq_len(nrow(group_combinations))) {
    current_group <- if (length(valid_grouping_vars) > 0) group_combinations[i, ] else tibble::tibble()
    subset_data <- if (length(valid_grouping_vars) > 0) {
      df %>% dplyr::inner_join(current_group, by = valid_grouping_vars)
    } else {
      df
    }
    
    total_timeloss <- sum(subset_data$timeloss, na.rm = TRUE)
    
    # Default outputs
    burden_rate <- 0
    ci_lower    <- NA_real_
    ci_upper    <- NA_real_
    
    # Compute burden and CI only if exposure > 0 and timeloss > 0
    if (is.finite(exposure) && exposure > 0 && total_timeloss > 0) {
      burden_rate <- total_timeloss / exposure * 1000
      
      # Only bootstrap when we have at least 2 rows and variance in timeloss
      if (nrow(subset_data) >= 2 && stats::var(subset_data$timeloss) > 0) {
        # Boot on timeloss; convert CI to burden scale afterwards
        b <- boot::boot(subset_data$timeloss, bootSum, R = 2000)
        
        # Prefer percentile CI; if it fails, fallback to empirical quantiles
        ci_obj <- tryCatch(
          boot::boot.ci(b, type = "perc", conf = 0.95),
          error   = function(e) NULL,
          warning = function(w) NULL
        )
        
        if (!is.null(ci_obj) && !is.null(ci_obj$perc) && length(ci_obj$perc) >= 5) {
          ci_lower <- ci_obj$perc[4] / exposure * 1000
          ci_upper <- ci_obj$perc[5] / exposure * 1000
        } else if (length(b$t) > 10) {
          # Empirical 95% CI as a robust fallback
          qs <- stats::quantile(b$t, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
          ci_lower <- qs[1] / exposure * 1000
          ci_upper <- qs[2] / exposure * 1000
        }
      }
    } else {
      # exposure <= 0 or timeloss == 0: burden = 0, CI = NA (kept blank downstream)
      burden_rate <- 0
      ci_lower    <- NA_real_
      ci_upper    <- NA_real_
    }
    
    # Build results row
    results_row <- if (length(valid_grouping_vars) > 0) {
      current_group %>%
        dplyr::mutate(
          burden_rate = burden_rate,
          lower_bound = ci_lower,
          upper_bound = ci_upper
        )
    } else {
      tibble::tibble(
        burden_rate = burden_rate,
        lower_bound = ci_lower,
        upper_bound = ci_upper
      )
    }
    
    results_list[[i]] <- results_row
  }
  
  dplyr::bind_rows(results_list)
}

# Helper: Run and label burden analysis
run_analysis_burden <- function(caselist_input, exposure_input, outcome, definition) {
  calculate_burden(caselist_input, exposure_input) %>%
    dplyr::mutate(outcome = outcome, definition = definition) %>%
    dplyr::select(outcome, definition, dplyr::everything())
}










# Generate tables --------------


generate_participation_table <- function(player_details) {
  library(dplyr)
  library(tibble)
  library(openxlsx)
  library(readr)
  
  participation_table <- player_details %>%
    summarise(
      `Total number of teams`   = n_distinct(team),
      `Participating teams`     = n_distinct(team[consent == "yes"]),
      `Total number of players` = n(),
      `Consenting players`      = n_distinct(player_id[consent == "yes"])
    ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Metric") %>%
    rename(Value = V1)
  
  # Compute denominators
  total_teams   <- as.numeric(participation_table$Value[participation_table$Metric == "Total number of teams"])
  total_players <- as.numeric(participation_table$Value[participation_table$Metric == "Total number of players"])
  
  # Inline formatting
  consented_teams <- as.numeric(participation_table$Value[participation_table$Metric == "Participating teams"])
  pct_teams <- if (!is.na(total_teams) && total_teams > 0) ceiling(100 * consented_teams / total_teams) else NA_integer_
  participation_table$Value[participation_table$Metric == "Participating teams"] <-
    if (!is.na(pct_teams)) paste0(consented_teams, " (", pct_teams, "%)") else as.character(consented_teams)
  
  consenting_players <- as.numeric(participation_table$Value[participation_table$Metric == "Consenting players"])
  pct_players <- if (!is.na(total_players) && total_players > 0) round(100 * consenting_players / total_players) else NA_integer_
  participation_table$Value[participation_table$Metric == "Consenting players"] <-
    if (!is.na(pct_players)) paste0(consenting_players, " (", pct_players, "%)") else as.character(consenting_players)
  
  # Ensure totals are plain counts
  participation_table$Value[participation_table$Metric == "Total number of teams"]   <- as.character(as.integer(total_teams))
  participation_table$Value[participation_table$Metric == "Total number of players"] <- as.character(as.integer(total_players))
  
 
  

  print(participation_table)
}


generate_subsequent_table <- function(caselist) {
library(dplyr)
library(tidyr)

# Canonical levels
problem_type_levels    <- c("Injuries", "Illnesses", "Mental health problems")
recurrence_type_levels <- c("early", "late", "delayed")
expected_cols <- c("Index", "Exacerbation",
                   "Recurrence (early)", "Recurrence (late)", "Recurrence (delayed)")

subsequent_table <- caselist %>%
  # --- Normalize labels to the specified options ---
  mutate(
    # problem_type: injury | illness | mental health problem  --> plural canonical
    problem_type = case_when(
      tolower(problem_type) == "injury"                ~ "Injuries",
      tolower(problem_type) == "illness"               ~ "Illnesses",
      tolower(problem_type) == "mental health problem" ~ "Mental health problems",
      TRUE                                             ~ problem_type
    ),
    # subsequent_cat: index | re-injury | recurrent illness | exacerbation
    # Combine re-injury + recurrent illness into "recurrence"
    subsequent_cat = case_when(
      tolower(subsequent_cat) == "index"                         ~ "index",
      tolower(subsequent_cat) == "exacerbation"                  ~ "exacerbation",
      tolower(subsequent_cat) %in% c("re-injury","reinjury","re-injury",
                                     "recurrent illness","recurrence", "recurrent injury") ~ "recurrence",
      TRUE ~ NA_character_
    ),
    # recurrence_type: early | late | delayed
    recurrence_type = case_when(
      tolower(recurrence_type) %in% recurrence_type_levels ~ tolower(recurrence_type),
      TRUE ~ NA_character_
    )
  ) %>%
  # --- Build the wide column labels ---
  mutate(
    type_col = case_when(
      subsequent_cat == "index"        ~ "Index",
      subsequent_cat == "exacerbation" ~ "Exacerbation",
      subsequent_cat == "recurrence" & recurrence_type == "early"   ~ "Recurrence (early)",
      subsequent_cat == "recurrence" & recurrence_type == "late"    ~ "Recurrence (late)",
      subsequent_cat == "recurrence" & recurrence_type == "delayed" ~ "Recurrence (delayed)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(type_col),
         !is.na(problem_type),
         problem_type %in% problem_type_levels) %>%
  # --- Count cases (use n_distinct(case_id) if preferred) ---
  group_by(problem_type, type_col) %>%
  summarise(Cases = n(), .groups = "drop") %>%
  # --- Pivot to wide; fill observed cells with 0 ---
  tidyr::pivot_wider(
    names_from  = type_col,
    values_from = Cases,
    values_fill = 0
  ) %>%
  # --- Ensure all three problem types appear ---
  tidyr::complete(
    problem_type = problem_type_levels
  ) %>%
  # --- Add any missing expected columns with 0 (if category absent entirely) ---
  {
    df <- .
    missing_cols <- setdiff(expected_cols, names(df))
    if (length(missing_cols) > 0) {
      for (cn in missing_cols) df[[cn]] <- 0L
    }
    df
  } %>%
  # --- Fill any remaining NAs in the expected numeric columns with 0 ---
  mutate(dplyr::across(all_of(expected_cols), ~ tidyr::replace_na(., 0L))) %>%
  # (Optional) if any non-expected numeric cols were created, you can also:
  # mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0L))) 
  arrange(factor(problem_type, levels = problem_type_levels)) %>%
  select(
    problem_type,
    all_of(expected_cols)
  )

print(subsequent_table)
}

generate_subsequent_table <- function(caselist) {
  
  library(dplyr)
  library(tidyr)
  
  # Canonical levels
  problem_type_levels <- c(
    "Injuries",
    "Illnesses",
    "Mental health problems"
  )
  
  recurrence_type_levels <- c(
    "early",
    "late",
    "delayed"
  )
  
  expected_cols <- c(
    "Index",
    "Exacerbation",
    "Unknown",
    "Recurrence (early)",
    "Recurrence (late)",
    "Recurrence (delayed)"
  )
  
  subsequent_table <- caselist %>%
    
    # --- Normalize labels to canonical values ---
    mutate(
      
      # problem type
      problem_type = case_when(
        tolower(problem_type) == "injury" ~ "Injuries",
        tolower(problem_type) == "illness" ~ "Illnesses",
        tolower(problem_type) == "mental health problem" ~ "Mental health problems",
        TRUE ~ problem_type
      ),
      
      # subsequent condition classification
      subsequent_cat = case_when(
        tolower(subsequent_cat) == "index" ~ "index",
        tolower(subsequent_cat) == "exacerbation" ~ "exacerbation",
        tolower(subsequent_cat) == "unknown" ~ "unknown",
        tolower(subsequent_cat) %in% c(
          "re-injury",
          "reinjury",
          "recurrent illness",
          "recurrence",
          "recurrent injury"
        ) ~ "recurrence",
        TRUE ~ NA_character_
      ),
      
      # recurrence timing
      recurrence_type = case_when(
        tolower(recurrence_type) %in% recurrence_type_levels ~
          tolower(recurrence_type),
        TRUE ~ NA_character_
      )
    ) %>%
    
    # --- Create display columns ---
    mutate(
      type_col = case_when(
        subsequent_cat == "index" ~ "Index",
        subsequent_cat == "exacerbation" ~ "Exacerbation",
        subsequent_cat == "unknown" ~ "Unknown",
        subsequent_cat == "recurrence" &
          recurrence_type == "early" ~ "Recurrence (early)",
        subsequent_cat == "recurrence" &
          recurrence_type == "late" ~ "Recurrence (late)",
        subsequent_cat == "recurrence" &
          recurrence_type == "delayed" ~ "Recurrence (delayed)",
        TRUE ~ NA_character_
      )
    ) %>%
    
    filter(
      !is.na(type_col),
      !is.na(problem_type),
      problem_type %in% problem_type_levels
    ) %>%
    
    # --- Count cases ---
    group_by(problem_type, type_col) %>%
    summarise(Cases = n(), .groups = "drop") %>%
    
    # --- Wide format ---
    pivot_wider(
      names_from = type_col,
      values_from = Cases,
      values_fill = 0
    ) %>%
    
    # --- Ensure all problem types appear ---
    complete(
      problem_type = problem_type_levels
    ) %>%
    
    # --- Add missing columns if absent ---
    {
      df <- .
      
      missing_cols <- setdiff(
        expected_cols,
        names(df)
      )
      
      if (length(missing_cols) > 0) {
        for (cn in missing_cols) {
          df[[cn]] <- 0L
        }
      }
      
      df
    } %>%
    
    # --- Replace remaining NAs ---
    mutate(
      across(
        all_of(expected_cols),
        ~ replace_na(., 0L)
      )
    ) %>%
    
    arrange(
      factor(
        problem_type,
        levels = problem_type_levels
      )
    ) %>%
    
    select(
      problem_type,
      all_of(expected_cols)
    )
  
  print(subsequent_table)
  
}

# 
# # Incidence analyses -----
# 
# # Exposure totals
# total_exposure <- sum(data_exposure$total)
# match_exposure <- data_exposure$match
# training_exposure <- data_exposure$training
# 
# 
# # Injury analyses (using hours as exposure)
# all_ma <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury"), total_exposure, "All injuries", "Medical attention")
# all_tl <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss"), total_exposure, "All injuries", "Time loss")
# 
# match_ma <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Match"), match_exposure, "Match injuries", "Medical attention")
# match_tl <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Match", timeloss_cat == "timeloss"), match_exposure, "Match injuries", "Time loss")
# 
# tr_ma <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Training"), training_exposure, "Training injuries", "Medical attention")
# tr_tl <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Training", timeloss_cat == "timeloss"), data_exposure$training_2, "Training injuries", "Time loss")
# 
# # Illness analyses (using player-days as exposure)
# all_ma_illness <- run_analysis_incidence(caselist %>% filter(problem_type == "Illness"), player_days, "All illnesses", "Medical attention")
# all_tl_illness <- run_analysis_incidence(caselist %>% filter(problem_type == "Illness", timeloss_cat == "timeloss"), player_days, "All illnesses", "Time loss")
# 
# # 4. Combine and Export Results
# 
# result_incidence <- bind_rows(
#   all_ma, match_ma, tr_ma,
#   all_tl, match_tl, tr_tl,
#   all_ma_illness, all_tl_illness
# ) %>% print()
# 
# 


# ## Burden analyses ------
# 
# # All injuries
# all_ma <- run_analysis_burden(caselist, sum(data_exposure$total), "All injuries", "Medical attention")
# all_tl <- run_analysis_burden(filter(caselist, timeloss_cat == "timeloss"), sum(data_exposure$total), "All injuries", "Time loss")
# 
# # Match injuries
# match_ma <- run_analysis_burden(filter(caselist, when_occurred == "Match"), sum(data_exposure$match), "Match injuries", "Medical attention")
# match_tl <- run_analysis_burden(filter(caselist, when_occurred == "Match", timeloss_cat == "timeloss"), sum(data_exposure$match), "Match injuries", "Time loss")
# 
# # Training injuries
# tr_ma <- run_analysis_burden(filter(caselist, when_occurred == "Training"), sum(data_exposure$training), "Training injuries", "Medical attention")
# tr_tl <- run_analysis_burden(filter(caselist, when_occurred == "Training", timeloss_cat == "timeloss"), sum(data_exposure$training), "Training injuries", "Time loss")
# 
# # Illnesses
# all_tl_illness <- run_analysis_burden(
#   filter(caselist, timeloss_cat == "timeloss", problem_type == "Illness"),
#   player_days,
#   "All illnesses",
#   "Time loss"
# )
# 
# # Combine and export results
# result_burden <- bind_rows(all_tl, match_tl, tr_tl, all_tl_illness)
# 
# result <- left_join(result_incidence, result_burden)

# Save overall results

save_overall_results_excel <- function(result_df, output_path) {
  
  # Define styles
  style_level_1 <- createStyle(textDecoration = "bold", border = "top")
  style_level_2 <- createStyle(indent = 5, halign = "left")
  style_level_3 <- createStyle(textDecoration = "italic", indent = 10)
  style_level_4 <- createStyle(textDecoration = "italic", indent = 15)
  
  # Create workbook and worksheet
  wb <- createWorkbook()
  addWorksheet(wb, "overall_results")
  writeData(wb, "overall_results", result_df)
  
  # Apply conditional formatting based on 'definition' column
  conditionalFormatting(wb, "overall_results", cols = 2:ncol(result_df), rows = 2:1000, rule = '$B2="Medical attention"', style = style_level_2)
  conditionalFormatting(wb, "overall_results", cols = 2:ncol(result_df), rows = 2:1000, rule = '$B2="Time loss"', style = style_level_3)
  
  # Apply bold header
  addStyle(wb, "overall_results", style = style_level_1, rows = 1, cols = 1:ncol(result_df), gridExpand = TRUE)
  
  # Auto-adjust column widths
  setColWidths(wb, "overall_results", cols = 1:ncol(result_df), widths = "auto")
  
  # Save workbook
  saveWorkbook(wb, file = output_path, overwrite = TRUE)
}

# 
# save_overall_results_excel(result, make_output_path("overall_results.xlsx"))
# write_csv(result, file = make_output_path("overall_results.csv"))


generate_basic_injury_table  <- function(caselist, data_exposure) {

    # ---- Exposure totals ----
    total_exposure    <- sum(data_exposure$total)
    match_exposure    <- data_exposure$match
    training_exposure <- data_exposure$training
    
    # ---- Incidence analyses ----
    all_tl <- run_analysis_incidence(
      caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss"),
      total_exposure, "All injuries", "Time loss"
    ) %>% 
      mutate(exposure = total_exposure)
  
   gradual_tl <- run_analysis_incidence(
      caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss", onset == "Gradual-onset"),
      total_exposure, "Gradual onset", "Time loss"
    ) %>% 
      mutate(exposure = total_exposure)
    
   sudden_tl <- run_analysis_incidence(
     caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss", onset == "Sudden-onset"),
     total_exposure, "Sudden onset", "Time loss"
   ) %>% 
     mutate(exposure = total_exposure)
      
    match_tl <- run_analysis_incidence(
      caselist %>% filter(problem_type == "Injury", when_occurred == "Match", timeloss_cat == "timeloss"),
      match_exposure, "Match injuries", "Time loss"
    ) %>% 
      mutate(exposure = match_exposure)
    
    tr_tl <- run_analysis_incidence(
      caselist %>% filter(problem_type == "Injury", when_occurred == "Training", timeloss_cat == "timeloss"),
      training_exposure, "Training injuries", "Time loss"
    )%>% 
      mutate(exposure = training_exposure)
    
    result_incidence <- bind_rows(all_tl, gradual_tl, sudden_tl,
                                match_tl, tr_tl)
    
    # ---- Burden analyses ----
    all_tl_b <- run_analysis_burden(
      filter(caselist, problem_type == "Injury", timeloss_cat == "timeloss"),
      total_exposure, "All injuries", "Time loss"
    )
    
    gradual_tl_b <- run_analysis_burden(
      filter(caselist, problem_type == "Injury", timeloss_cat == "timeloss", onset == "Gradual-onset"),
      total_exposure, "Gradual onset", "Time loss"
    )
    
    sudden_tl_b <- run_analysis_burden(
      filter(caselist, problem_type == "Injury", timeloss_cat == "timeloss", onset == "Sudden-onset"),
      total_exposure, "Sudden onset", "Time loss"
    )
    
    match_tl_b <- run_analysis_burden(
      filter(caselist, problem_type == "Injury", when_occurred == "Match", timeloss_cat == "timeloss"),
      match_exposure, "Match injuries", "Time loss"
    )
    
    tr_tl_b <- run_analysis_burden(
      filter(caselist, problem_type == "Injury", when_occurred == "Training", timeloss_cat == "timeloss"),
      training_exposure, "Training injuries", "Time loss"
    )
    
    result_burden <- bind_rows(all_tl_b, gradual_tl_b, sudden_tl_b, match_tl_b, tr_tl_b)
    
    # ---- Helper: format numbers ----
    format_num <- function(x) {
      ifelse(is.na(x), NA_character_,
             sub(" .?0+$", "", sprintf("%.2f", x)))
    }
    
    # ---- Final combined table ----
    injury_table <- left_join(result_incidence, result_burden) %>%
      mutate(
        "Incidence rate [95% CI]" = paste0(format_num(incidence_rate), " [",
                                           format_num(ci_lower), " to ", format_num(ci_upper), "]"),
        "Burden rate [95% CI]" = paste0(format_num(burden_rate), " [",
                                        format_num(lower_bound), " to ", format_num(upper_bound), "]")
      ) %>%
      select(outcome, exposure, n_cases,
             "Incidence rate [95% CI]", total_timeloss, "Burden rate [95% CI]") %>%
      rename("Exposure (h)" = exposure,
             "Cases (n)" = n_cases,
             "Time loss (days)" = total_timeloss)
    
    return(injury_table)
  }
  



generate_basic_health_problems_table <- function(caselist, data_exposure) {
  require(dplyr, quietly = TRUE)
  
  # ---- Exposure totals ----
  total_exposure    <- sum(data_exposure$total)
  match_exposure    <- data_exposure$match
  training_exposure <- data_exposure$training
  player_days < data_exposure$days
  
  # ---- Incidence analyses ----
  all_tl_injury <- run_analysis_incidence(
    caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss"),
    player_days, "Injuries", "Time loss"
  ) 
  all_tl_illness <- run_analysis_incidence(
    caselist %>% filter(problem_type == "Illness", timeloss_cat == "timeloss"),
    player_days, "Illnesses", "Time loss"
  )
  all_ma_mh <- run_analysis_incidence(
    caselist %>% filter(problem_type == "Mental health problem"),
    player_days, "Mental health problems", "Medical attention"
  )
  
  result_incidence <- bind_rows(all_tl_injury, all_tl_illness, all_ma_mh) %>%
    mutate(exposure = player_days)
  
  # ---- Burden analyses ----
  # Make outcome labels match incidence so the join works
  all_tl_injury_burden <- run_analysis_burden(
    filter(caselist, problem_type == "Injury", timeloss_cat == "timeloss"),
    player_days, "Injuries", "Time loss"
  )
  all_tl_illness_burden <- run_analysis_burden(
    filter(caselist, problem_type == "Illness", timeloss_cat == "timeloss"),
    player_days, "Illnesses", "Time loss"
  )
  all_ma_mh_burden <- run_analysis_burden(
    filter(caselist, problem_type == "Mental health problem"),
    player_days, "Mental health problems", "Time loss"
  )
  
  result_burden <- bind_rows(all_tl_injury_burden, all_tl_illness_burden, all_ma_mh_burden)
  
  # ---- Helper: format numbers (<= 2 dp; blank if NA) ----
  format_num <- function(x) {
    ifelse(is.na(x), "", sub(" .?0+$", "", sprintf("%.2f", x)))
  }
  
  # ---- Join incidence + burden (explicit key to avoid mismatches) ----
  joined <- left_join(result_incidence, result_burden, by = "outcome")
  
  # ---- Ensure rows exist for all types of health problems ----
  required_outcomes <- c("Injuries", "Illnesses", "Mental health problems")
  missing_outcomes  <- setdiff(required_outcomes, joined$outcome)
  
  if (length(missing_outcomes) > 0) {
    blank_rows <- tibble::tibble(
      outcome = missing_outcomes,
      exposure = NA_real_,
      n_cases = 0L,
      incidence_rate = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      total_timeloss = NA_real_,
      burden_rate = NA_real_,
      lower_bound = NA_real_,
      upper_bound = NA_real_
    )
    joined <- dplyr::bind_rows(joined, blank_rows)
  }
  
  # ---- Final combined table; hide incidence CI when cases == 0 ----
  health_probs_table <- joined %>%
    mutate(
      `Incidence rate [95% CI]` = if_else(
        n_cases == 0L,
        "",
        if_else(
          is.na(incidence_rate) | is.na(ci_lower) | is.na(ci_upper),
          "",
          paste0(
            format_num(incidence_rate), " [",
            format_num(ci_lower), " to ", format_num(ci_upper), "]"
          )
        )
      ),
      `Burden rate [95% CI]` = if_else(
        is.na(burden_rate) | is.na(lower_bound) | is.na(upper_bound),
        "",
        paste0(
          format_num(burden_rate), " [",
          format_num(lower_bound), " to ", format_num(upper_bound), "]"
        )
      )
    ) %>%
    select(
      outcome, exposure, n_cases,
      `Incidence rate [95% CI]`, total_timeloss, `Burden rate [95% CI]`
    ) %>%
    rename(
      `Exposure (player days)` = exposure,
      `Cases (n)`                  = n_cases,
      `Time loss (days)`         = total_timeloss
    ) %>%
    mutate(
      `Exposure (player days)` = format_num(`Exposure (player days)`),
      `Time loss (days)`         = format_num(`Time loss (days)`)
    )
  
  return(health_probs_table)
}






generate_table_1_3_4 <- function(caselist_input, exposure_input, output_path) {
  
  # ---- Build level specs: 1=Body area, 2=Tissue type, 3=Pathology type, 4=Diagnosis ----
  levels <- list(
    list(level = 1, grouping_vars = c("osiics_16_level_1"),
         fill = list(osiics_16_level_2 = "All", osiics_16_level_3 = "All", osiics_16_level_4 = "All")),
    list(level = 2, grouping_vars = c("osiics_16_level_1", "osiics_16_level_2"),
         fill = list(osiics_16_level_3 = "All", osiics_16_level_4 = "All")),
    list(level = 3, grouping_vars = c("osiics_16_level_1", "osiics_16_level_2", "osiics_16_level_3"),
         fill = list(osiics_16_level_4 = "All")),
    list(level = 4, grouping_vars = c("osiics_16_level_1", "osiics_16_level_2", "osiics_16_level_3", "osiics_16_level_4"),
         fill = list())
  )
  
  # ---- Incidence + severity tables across levels ----
  incidence_tables <- lapply(levels, function(lvl) {
    calculate_incidence_severity(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      dplyr::mutate(level = lvl$level) %>%
      dplyr::mutate(!!!lvl$fill) %>%
      dplyr::select(
        level,
        osiics_16_level_1, osiics_16_level_2, osiics_16_level_3, osiics_16_level_4,
        n_cases, incidence_rate, ci_lower, ci_upper,
        total_timeloss, median_timeloss, q1_timeloss, q3_timeloss
      )
  })
  table_1 <- dplyr::bind_rows(incidence_tables)
  
  # ---- Burden tables across levels ----
  burden_tables <- lapply(levels, function(lvl) {
    calculate_burden(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      dplyr::mutate(level = lvl$level) %>%
      dplyr::mutate(!!!lvl$fill) %>%
      dplyr::select(
        level,
        osiics_16_level_1, osiics_16_level_2, osiics_16_level_3, osiics_16_level_4,
        burden_rate, lower_bound, upper_bound
      )
  })
  table_burden <- dplyr::bind_rows(burden_tables)
  
  # ---- Join incidence/severity with burden (explicit keys) ----
  table_1 <- dplyr::left_join(
    table_1,
    table_burden,
    by = c("level", "osiics_16_level_1", "osiics_16_level_2", "osiics_16_level_3", "osiics_16_level_4")
  )
  
  # ---- Factor levels (ensure "All" is present where used) ----
  # NOTE: level_*_order should exist in your environment
  l1 <- unique(c(level_1_order))
  l2 <- unique(c(level_2_order, "All"))
  l3 <- unique(c(level_3_order, "All"))
  l4 <- unique(c(level_4_order, "All"))
  
  table_1 <- table_1 %>%
    dplyr::mutate(
      osiics_16_level_1 = factor(osiics_16_level_1, levels = l1),
      osiics_16_level_2 = factor(osiics_16_level_2, levels = l2),
      osiics_16_level_3 = factor(osiics_16_level_3, levels = l3),
      osiics_16_level_4 = factor(osiics_16_level_4, levels = l4)
    ) %>%
    dplyr::arrange(osiics_16_level_1, osiics_16_level_2, osiics_16_level_3, osiics_16_level_4) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        level == 1 ~ as.character(osiics_16_level_1),
        level == 2 ~ as.character(osiics_16_level_2),
        level == 3 ~ as.character(osiics_16_level_3),
        level == 4 ~ as.character(osiics_16_level_4),
        TRUE       ~ NA_character_
      ),
      sort_order = dplyr::row_number()
    ) %>%
    # Keep body area (1), pathology type (3), diagnosis (4);
    # and only show diagnosis rows when n_cases >= 5.
    dplyr::filter(level != 2) %>%
    dplyr::filter((level %in% c(1, 3)) | (level == 4 & n_cases >= 5)) %>%
    # ---------- SUPPRESSION ON NUMERICS (do this BEFORE formatting) ----------
  dplyr::mutate(
    # Time-loss rules:
    median_timeloss = dplyr::if_else(n_cases > 1,  median_timeloss, NA_real_),
    q1_timeloss     = dplyr::if_else(n_cases >= 5, q1_timeloss,     NA_real_),
    q3_timeloss     = dplyr::if_else(n_cases >= 5, q3_timeloss,     NA_real_)
    # If you also want to suppress incidence/burden at diagnosis below threshold,
    # you've already filtered diagnoses with n_cases < 5 above.
  ) %>%
    # ---------- FORMAT FOR DISPLAY (strings; safe because suppression done) ----------
  dplyr::mutate(
    incidence_rate = dplyr::if_else(is.na(incidence_rate), "", sprintf("%.2f", incidence_rate)),
    burden_rate    = dplyr::if_else(is.na(burden_rate),    "", sprintf("%.2f", burden_rate)),
    median_timeloss = dplyr::if_else(is.na(median_timeloss), "", sprintf("%.0f", median_timeloss)),
    incidence_ci   = dplyr::if_else(is.na(ci_lower) | is.na(ci_upper), "",
                                    paste0(" [", sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]")),
    median_iqr     = dplyr::if_else(is.na(q1_timeloss) | is.na(q3_timeloss), "",
                                    paste0(" (", sprintf("%.0f", q1_timeloss), ", ", sprintf("%.0f", q3_timeloss), ")")),
    burden_ci      = dplyr::if_else(is.na(lower_bound) | is.na(upper_bound), "",
                                    paste0(" [", sprintf("%.2f", lower_bound), ", ", sprintf("%.2f", upper_bound), "]"))
  ) %>%
    dplyr::select(
      level, label, n_cases,
      incidence_rate, incidence_ci,
      median_timeloss, median_iqr,
      burden_rate, burden_ci
    )
  
  # ---- Excel export ----
  style_level_1 <- openxlsx::createStyle(textDecoration = "bold", border = "top")
  style_level_2 <- openxlsx::createStyle(indent = 5, halign = "left")
  style_level_3 <- openxlsx::createStyle(textDecoration = "italic", indent = 10)
  style_level_4 <- openxlsx::createStyle(textDecoration = "italic", indent = 15)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "table_1")
  openxlsx::writeData(wb, "table_1", table_1)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2:9, rows = 2:1000, rule = "$A2=1", style = style_level_1)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2,    rows = 2:1000, rule = "$A2=2", style = style_level_2)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2,    rows = 2:1000, rule = "$A2=3", style = style_level_3)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2,    rows = 2:1000, rule = "$A2=4", style = style_level_4)
  openxlsx::setColWidths(wb, "table_1", cols = 1:9, widths = "auto")
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  
  # ---- CSV export ----
  readr::write_csv(table_1, file = make_output_path("table_injury_all_tl_1_3_4.csv"))
}

# 
# 
# generate_table_1_3_4 <- function(caselist_input, exposure_input, output_path) {
#   # ---- Helper: ensure UTF-8 across the data frame ----
#   sanitize_utf8_df <- function(df) {
#     # Convert column names to UTF-8
#     names(df) <- stringi::stri_enc_toutf8(names(df))
#     
#     # Convert character columns and factor levels to UTF-8
#     df[] <- lapply(df, function(x) {
#       if (is.character(x)) {
#         stringi::stri_enc_toutf8(x)
#       } else if (is.factor(x)) {
#         # Convert factor levels to UTF-8 but keep it a factor
#         lvls <- levels(x)
#         lvls_utf8 <- stringi::stri_enc_toutf8(lvls)
#         factor(x, levels = lvls, labels = lvls_utf8)
#       } else {
#         x
#       }
#     })
#     df
#   }
#   
#   # ---- Build level specs: 1=Body area, 2=Tissue type, 3=Pathology type, 4=Diagnosis ----
#   levels <- list(
#     list(level = 1, grouping_vars = c("osiics_16_level_1"),
#          fill = list(osiics_16_level_2 = "All", osiics_16_level_3 = "All", osiics_16_level_4 = "All")),
#     list(level = 2, grouping_vars = c("osiics_16_level_1", "osiics_16_level_2"),
#          fill = list(osiics_16_level_3 = "All", osiics_16_level_4 = "All")),
#     list(level = 3, grouping_vars = c("osiics_16_level_1", "osiics_16_level_2", "osiics_16_level_3"),
#          fill = list(osiics_16_level_4 = "All")),
#     list(level = 4, grouping_vars = c("osiics_16_level_1", "osiics_16_level_2", "osiics_16_level_3", "osiics_16_level_4"),
#          fill = list())
#   )
#   
#   # ---- Incidence + severity tables across levels ----
#   incidence_tables <- lapply(levels, function(lvl) {
#     calculate_incidence_severity(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
#       dplyr::mutate(level = lvl$level) %>%
#       dplyr::mutate(!!!lvl$fill) %>%
#       dplyr::select(
#         level,
#         osiics_16_level_1, osiics_16_level_2, osiics_16_level_3, osiics_16_level_4,
#         n_cases, incidence_rate, ci_lower, ci_upper,
#         total_timeloss, median_timeloss, q1_timeloss, q3_timeloss
#       )
#   })
#   table_1 <- dplyr::bind_rows(incidence_tables)
#   
#   # ---- Burden tables across levels ----
#   burden_tables <- lapply(levels, function(lvl) {
#     calculate_burden(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
#       dplyr::mutate(level = lvl$level) %>%
#       dplyr::mutate(!!!lvl$fill) %>%
#       dplyr::select(
#         level,
#         osiics_16_level_1, osiics_16_level_2, osiics_16_level_3, osiics_16_level_4,
#         burden_rate, lower_bound, upper_bound
#       )
#   })
#   table_burden <- dplyr::bind_rows(burden_tables)
#   
#   # ---- Join incidence/severity with burden (explicit keys) ----
#   table_1 <- dplyr::left_join(
#     table_1,
#     table_burden,
#     by = c("level", "osiics_16_level_1", "osiics_16_level_2", "osiics_16_level_3", "osiics_16_level_4")
#   )
#   
#   # ---- Factor levels (ensure "All" is present where used) ----
#   # NOTE: level_*_order should exist in your environment
#   l1 <- unique(c(level_1_order))
#   l2 <- unique(c(level_2_order, "All"))
#   l3 <- unique(c(level_3_order, "All"))
#   l4 <- unique(c(level_4_order, "All"))
#   
#   table_1 <- table_1 %>%
#     dplyr::mutate(
#       osiics_16_level_1 = factor(osiics_16_level_1, levels = l1),
#       osiics_16_level_2 = factor(osiics_16_level_2, levels = l2),
#       osiics_16_level_3 = factor(osiics_16_level_3, levels = l3),
#       osiics_16_level_4 = factor(osiics_16_level_4, levels = l4)
#     ) %>%
#     dplyr::arrange(osiics_16_level_1, osiics_16_level_2, osiics_16_level_3, osiics_16_level_4) %>%
#     dplyr::mutate(
#       label = dplyr::case_when(
#         level == 1 ~ as.character(osiics_16_level_1),
#         level == 2 ~ as.character(osiics_16_level_2),
#         level == 3 ~ as.character(osiics_16_level_3),
#         level == 4 ~ as.character(osiics_16_level_4),
#         TRUE       ~ NA_character_
#       ),
#       sort_order = dplyr::row_number()
#     ) %>%
#     # Keep body area (1), pathology type (3), diagnosis (4);
#     # and only show diagnosis rows when n_cases >= 5.
#     dplyr::filter(level != 2) %>%
#     dplyr::filter((level %in% c(1, 3)) | (level == 4 & n_cases >= 5)) %>%
#     # ---------- SUPPRESSION ON NUMERICS (do this BEFORE formatting) ----------
#   dplyr::mutate(
#     # Time-loss rules:
#     median_timeloss = dplyr::if_else(n_cases > 1,  median_timeloss, NA_real_),
#     q1_timeloss     = dplyr::if_else(n_cases >= 5, q1_timeloss,     NA_real_),
#     q3_timeloss     = dplyr::if_else(n_cases >= 5, q3_timeloss,     NA_real_)
#     # Note: incidence/burden for diagnosis already filtered by n_cases >= 5 above.
#   ) %>%
#     # ---------- FORMAT FOR DISPLAY (strings; safe because suppression done) ----------
#   dplyr::mutate(
#     incidence_rate = dplyr::if_else(is.na(incidence_rate), "", sprintf("%.2f", incidence_rate)),
#     burden_rate    = dplyr::if_else(is.na(burden_rate),    "", sprintf("%.2f", burden_rate)),
#     median_timeloss = dplyr::if_else(is.na(median_timeloss), "", sprintf("%.0f", median_timeloss)),
#     incidence_ci   = dplyr::if_else(is.na(ci_lower) | is.na(ci_upper), "",
#                                     paste0(" [", sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]")),
#     median_iqr     = dplyr::if_else(is.na(q1_timeloss) | is.na(q3_timeloss), "",
#                                     paste0(" (", sprintf("%.0f", q1_timeloss), ", ", sprintf("%.0f", q3_timeloss), ")")),
#     burden_ci      = dplyr::if_else(is.na(lower_bound) | is.na(upper_bound), "",
#                                     paste0(" [", sprintf("%.2f", lower_bound), ", ", sprintf("%.2f", upper_bound), "]"))
#   ) %>%
#     dplyr::select(
#       level, label, n_cases,
#       incidence_rate, incidence_ci,
#       median_timeloss, median_iqr,
#       burden_rate, burden_ci
#     )
#   
#   # ---- UTF-8 sanitize (AFTER formatting; BEFORE export) ----
#   table_1 <- sanitize_utf8_df(table_1)
#   
#   # ---- Excel export ----
#   style_level_1 <- openxlsx::createStyle(textDecoration = "bold", border = "top")
#   style_level_2 <- openxlsx::createStyle(indent = 5, halign = "left")
#   style_level_3 <- openxlsx::createStyle(textDecoration = "italic", indent = 10)
#   style_level_4 <- openxlsx::createStyle(textDecoration = "italic", indent = 15)
#   
#   wb <- openxlsx::createWorkbook()
#   openxlsx::addWorksheet(wb, "table_1")
#   openxlsx::writeData(wb, "table_1", table_1)
#   openxlsx::conditionalFormatting(wb, "table_1", cols = 2:9, rows = 2:1000, rule = "$A2=1", style = style_level_1)
#   openxlsx::conditionalFormatting(wb, "table_1", cols = 2,    rows = 2:1000, rule = "$A2=2", style = style_level_2)
#   openxlsx::conditionalFormatting(wb, "table_1", cols = 2,    rows = 2:1000, rule = "$A2=3", style = style_level_3)
#   openxlsx::conditionalFormatting(wb, "table_1", cols = 2,    rows = 2:1000, rule = "$A2=4", style = style_level_4)
#   openxlsx::setColWidths(wb, "table_1", cols = 1:9, widths = "auto")
#   openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
#   
#   # ---- CSV export ----
#   readr::write_csv(table_1, file = make_output_path("table_injury_all_tl_1_3_4.csv"))
# }
# 
# 









# version_new -----------
generate_table_1_3_4 <- function(caselist_input, exposure_input, output_path) {
  
  # ----------------------------- Helpers -----------------------------
  # UTF-8 sanitize (names + data)
  sanitize_utf8_df <- function(df) {
    names(df) <- stringi::stri_enc_toutf8(names(df))
    df[] <- lapply(df, function(x) {
      if (is.character(x)) {
        stringi::stri_enc_toutf8(x)
      } else if (is.factor(x)) {
        lvls <- levels(x)
        lvls_utf8 <- stringi::stri_enc_toutf8(lvls)
        factor(x, levels = lvls, labels = lvls_utf8)
      } else x
    })
    df
  }
  
  # Normalize labels for safe matching and ordering (handles UTF-8, spacing)
  normalize_label <- function(x) {
    x <- stringi::stri_enc_toutf8(as.character(x))
    x <- stringi::stri_trans_general(x, "NFKC")
    x <- stringi::stri_trim_both(x)
    x
  }
  
  # --------------------- Level sets (no Level 2) ---------------------
  # We never include L2 in computation or sorting.
  levels <- list(
    list(level = 1, grouping_vars = c("osiics_16_level_1")),
    list(level = 3, grouping_vars = c("osiics_16_level_1", "osiics_16_level_3")),
    list(level = 4, grouping_vars = c("osiics_16_level_1", "osiics_16_level_3", "osiics_16_level_4"))
  )
  
  # ----------------- Incidence + Severity by level ------------------
  incidence_tables <- lapply(levels, function(lvl) {
    calculate_incidence_severity(
      caselist_input, exposure_input,
      grouping_vars = lvl$grouping_vars
    ) %>%
      dplyr::mutate(level = lvl$level)
  })
  table_1 <- dplyr::bind_rows(incidence_tables)
  
  # -------------------------- Burden by level -----------------------
  burden_tables <- lapply(levels, function(lvl) {
    calculate_burden(
      caselist_input, exposure_input,
      grouping_vars = lvl$grouping_vars
    ) %>%
      dplyr::mutate(level = lvl$level)
  })
  table_burden <- dplyr::bind_rows(burden_tables)
  
  # ------------------------------ Join ------------------------------
  table_1 <- dplyr::left_join(
    table_1, table_burden,
    by = c("level", "osiics_16_level_1", "osiics_16_level_3", "osiics_16_level_4")
  )
  
  # ----------------- Build order maps (L1/L3 via factors) -----------
  # L4 will be alphabetical (within each L3), so no index map for L4.
  l1_order_norm <- normalize_label(level_1_order)
  l3_order_norm <- normalize_label(level_3_order)
  
  l1_map <- setNames(seq_along(l1_order_norm), l1_order_norm)
  l3_map <- setNames(seq_along(l3_order_norm), l3_order_norm)
  
  # ----------------- Normalize current labels + compute keys --------
  table_1 <- table_1 %>%
    dplyr::mutate(
      L1_norm = normalize_label(osiics_16_level_1),
      L3_norm = normalize_label(osiics_16_level_3),
      L4_norm = normalize_label(osiics_16_level_4),
      
      # Map to numeric keys based on predefined order (L1/L3).
      # Unknowns get pushed to the bottom of their L1 group with a warning.
      L1_key = unname(l1_map[L1_norm]),
      L3_key_raw = unname(l3_map[L3_norm])
    )
  
  # Diagnostics for unmatched L1/L3 labels.
  unmatched_l1 <- table_1 %>%
    dplyr::filter(is.na(L1_key)) %>%
    dplyr::distinct(osiics_16_level_1) %>% dplyr::pull()
  if (length(unmatched_l1) > 0) {
    warning("Unmatched L1 labels (not in level_1_order after normalization): ",
            paste0(unique(unmatched_l1), collapse = " | "))
  }
  
  unmatched_l3 <- table_1 %>%
    dplyr::filter(level %in% c(3,4), is.na(L3_key_raw)) %>%
    dplyr::distinct(osiics_16_level_3) %>% dplyr::pull()
  if (length(unmatched_l3) > 0) {
    warning("Unmatched L3 labels (not in level_3_order after normalization): ",
            paste0(unique(unmatched_l3), collapse = " | "))
  }
  
  # Fallbacks: put any unmatched at the end deterministically.
  l1_max <- length(l1_map)
  l3_max <- length(l3_map)
  
  table_1 <- table_1 %>%
    dplyr::mutate(
      L1_key = dplyr::coalesce(L1_key, l1_max + 1L),
      # For Level 1 rows, parent L3 group key = 0 (so header comes first)
      L3_key = dplyr::case_when(
        level == 1 ~ 0L,
        TRUE       ~ dplyr::coalesce(L3_key_raw, l3_max + 1L)
      ),
      # Within the (L1, L3) group: Level 3 header before Level 4 children
      level_key = dplyr::case_when(
        level == 1 ~ -1L,  # level-1 before everything in that L1
        level == 3 ~  0L,  # L3 header line
        level == 4 ~  1L   # then L4 diagnoses
      )
    )
  
  # ------------------ Filter (your suppression rules) ---------------
  table_1 <- table_1 %>%
    dplyr::filter(level %in% c(1, 3) | (level == 4 & n_cases >= 5))
  
  # ------------------ FINAL HIERARCHICAL SORT -----------------------
  # Order:
  # 1) L1 by factor order (via L1_key)
  # 2) L3 by factor order (via L3_key), but L1 header has L3_key = 0
  # 3) Within each (L1, L3): L3 header before its L4 children (via level_key)
  # 4) Within the L4 children: alphabetical by normalized L4 label (L4_norm)
  table_1 <- table_1 %>%
    dplyr::arrange(
      L1_key,
      L3_key,
      level_key,
      L4_norm
    )
  
  # (Optional) convert to factors for *display only* (no sorting reliance)
  table_1 <- table_1 %>%
    dplyr::mutate(
      osiics_16_level_1 = factor(osiics_16_level_1, levels = level_1_order),
      osiics_16_level_3 = factor(osiics_16_level_3, levels = level_3_order)
      # DO NOT force factor levels for L4 here; we already sorted alphabetically via L4_norm.
    )
  
  # ------------------ Labels + suppression + formatting -------------
  table_1 <- table_1 %>%
    dplyr::mutate(
      label = dplyr::case_when(
        level == 1 ~ as.character(osiics_16_level_1),
        level == 3 ~ as.character(osiics_16_level_3),
        level == 4 ~ as.character(osiics_16_level_4)
      ),
      sort_order = dplyr::row_number()
    ) %>%
    # Numeric suppression
    dplyr::mutate(
      median_timeloss = dplyr::if_else(n_cases > 1,  median_timeloss, NA_real_),
      q1_timeloss     = dplyr::if_else(n_cases >= 5, q1_timeloss,     NA_real_),
      q3_timeloss     = dplyr::if_else(n_cases >= 5, q3_timeloss,     NA_real_)
    ) %>%
    # String formatting (safe after suppression)
    dplyr::mutate(
      incidence_rate = dplyr::if_else(is.na(incidence_rate), "", sprintf("%.2f", incidence_rate)),
      burden_rate    = dplyr::if_else(is.na(burden_rate),    "", sprintf("%.2f", burden_rate)),
      median_timeloss = dplyr::if_else(is.na(median_timeloss), "", sprintf("%.0f", median_timeloss)),
      incidence_ci   = dplyr::if_else(is.na(ci_lower) | is.na(ci_upper), "",
                                      paste0(" [", sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]")),
      median_iqr     = dplyr::if_else(is.na(q1_timeloss) | is.na(q3_timeloss), "",
                                      paste0(" (", sprintf("%.0f", q1_timeloss), ", ", sprintf("%.0f", q3_timeloss), ")")),
      burden_ci      = dplyr::if_else(is.na(lower_bound) | is.na(upper_bound), "",
                                      paste0(" [", sprintf("%.2f", lower_bound), ", ", sprintf("%.2f", upper_bound), "]"))
    ) %>%
    dplyr::select(
      level, label, n_cases,
      incidence_rate, incidence_ci,
      median_timeloss, median_iqr,
      burden_rate, burden_ci
    )
  
  # ------------------ Final UTF-8 clean before export ---------------
  table_1 <- sanitize_utf8_df(table_1)
  
  # ------------------ Excel export ---------------------------------
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "table_1")
  openxlsx::writeData(wb, "table_1", table_1)
  
  style_level_1 <- openxlsx::createStyle(textDecoration = "bold", border = "top")
  style_level_3 <- openxlsx::createStyle(textDecoration = "italic", indent = 10)
  style_level_4 <- openxlsx::createStyle(textDecoration = "italic", indent = 15)
  
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2:9, rows = 2:2000,
                                  rule = "$A2=1", style = style_level_1)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2, rows = 2:2000,
                                  rule = "$A2=3", style = style_level_3)
  openxlsx::conditionalFormatting(wb, "table_1", cols = 2, rows = 2:2000,
                                  rule = "$A2=4", style = style_level_4)
  
  openxlsx::setColWidths(wb, "table_1", cols = 1:9, widths = "auto")
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  
  # ------------------ CSV export -----------------------------------
  readr::write_csv(table_1, file = make_output_path("table_injury_all_tl_1_3_4.csv"))
}






generate_table_2_3 <- function(caselist_input, exposure_input, output_path) {
  # Define levels for Table 2
  levels <- list(
    list(level = 2, grouping_vars = c("osiics_16_level_2"), fill = list(osiics_16_level_3 = "All")),
    list(level = 3, grouping_vars = c("osiics_16_level_2", "osiics_16_level_3"), fill = list())
  )
  
  # Incidence and severity
  incidence_tables <- lapply(levels, function(lvl) {
    calculate_incidence_severity(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      mutate(level = lvl$level) %>%
      mutate(!!!lvl$fill) %>%
      select(level, osiics_16_level_2, osiics_16_level_3,
             n_cases, incidence_rate, ci_lower, ci_upper,
             total_timeloss, median_timeloss, q1_timeloss, q3_timeloss)
  })
  
  table_2 <- bind_rows(incidence_tables)
  
  # Burden
  burden_tables <- lapply(levels, function(lvl) {
    calculate_burden(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      mutate(level = lvl$level) %>%
      mutate(!!!lvl$fill) %>%
      select(level, osiics_16_level_2, osiics_16_level_3,
             burden_rate, lower_bound, upper_bound)
  })
  
  table_burden <- bind_rows(burden_tables)
  
  # Join tables
  table_2 <- left_join(table_2, table_burden)
  
  # Apply factor levels
  table_2$osiics_16_level_2 <- factor(table_2$osiics_16_level_2, levels = level_2_order)
  table_2$osiics_16_level_3 <- factor(table_2$osiics_16_level_3, levels = level_3_order)
  
  table_2 <- table_2 %>%
    arrange(osiics_16_level_2, osiics_16_level_3) %>%
    mutate(
      label = case_when(
        level == 2 ~ as.character(osiics_16_level_2),
        level == 3 ~ as.character(osiics_16_level_3)
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
  
  
  # Save as csv
  write_csv(table_2, file = make_output_path("table_injury_all_tl_2_3.csv"))
  
}







generate_table_2_3 <- function(caselist_input, exposure_input, output_path) {
  # ---- Define levels: 2 = pathology type, 3 = subcategory (diagnosis group) ----
  levels <- list(
    list(level = 2, grouping_vars = c("osiics_16_level_2"),
         fill = list(osiics_16_level_3 = "All")),
    list(level = 3, grouping_vars = c("osiics_16_level_2", "osiics_16_level_3"),
         fill = list())
  )
  
  # ---- Incidence + severity tables across levels ----
  incidence_tables <- lapply(levels, function(lvl) {
    calculate_incidence_severity(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      dplyr::mutate(level = lvl$level) %>%
      dplyr::mutate(!!!lvl$fill) %>%
      dplyr::select(
        level, osiics_16_level_2, osiics_16_level_3,
        n_cases, incidence_rate, ci_lower, ci_upper,
        total_timeloss, median_timeloss, q1_timeloss, q3_timeloss
      )
  })
  table_2 <- dplyr::bind_rows(incidence_tables)
  
  # ---- Burden tables across levels ----
  burden_tables <- lapply(levels, function(lvl) {
    calculate_burden(caselist_input, exposure_input, grouping_vars = lvl$grouping_vars) %>%
      dplyr::mutate(level = lvl$level) %>%
      dplyr::mutate(!!!lvl$fill) %>%
      dplyr::select(
        level, osiics_16_level_2, osiics_16_level_3,
        burden_rate, lower_bound, upper_bound
      )
  })
  table_burden <- dplyr::bind_rows(burden_tables)
  
  # ---- Join incidence/severity with burden (explicit keys) ----
  table_2 <- dplyr::left_join(
    table_2,
    table_burden,
    by = c("level", "osiics_16_level_2", "osiics_16_level_3")
  )
  
  # ---- Factor levels (ensure "All" exists in level 3 order since we fill it) ----
  # level_2_order / level_3_order should exist in your environment
  l2 <- unique(c(level_2_order))
  l3 <- unique(c(level_3_order, "All"))
  
  table_2 <- table_2 %>%
    dplyr::mutate(
      osiics_16_level_2 = factor(osiics_16_level_2, levels = l2),
      osiics_16_level_3 = factor(osiics_16_level_3, levels = l3)
    ) %>%
    dplyr::arrange(osiics_16_level_2, osiics_16_level_3) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        level == 2 ~ as.character(osiics_16_level_2),
        level == 3 ~ as.character(osiics_16_level_3),
        TRUE       ~ NA_character_
      ),
      sort_order = dplyr::row_number()
    ) %>%
    # ---------- SUPPRESSION ON NUMERICS (BEFORE formatting) ----------
  # IQR rule: only report Q1–Q3 when n_cases >= 5 (applies to both levels)
  dplyr::mutate(
    q1_timeloss = dplyr::if_else(n_cases >= 5, q1_timeloss, NA_real_),
    q3_timeloss = dplyr::if_else(n_cases >= 5, q3_timeloss, NA_real_)
  ) %>%
    # Median rule: do NOT show median when there is only ONE case (either level)
    # Plus: For Level 3, only report median when n_cases >= 5 (subcategories rule)
    dplyr::mutate(
      median_timeloss = dplyr::if_else(
        n_cases == 1 | (level == 3 & n_cases < 5), NA_real_, median_timeloss
      )
    ) %>%
    # Level 3 (subcategory) suppression: show incidence & burden only when n_cases >= 5
    # Also suppress burden rate when n_cases == 1 at either level (global)
    dplyr::mutate(
      incidence_rate = dplyr::if_else(level == 3 & n_cases < 5, NA_real_, incidence_rate),
      burden_rate     = dplyr::if_else((level == 3 & n_cases < 5) | (n_cases == 1), NA_real_, burden_rate),
      # Blank CIs for suppressed metrics:
      ci_lower    = dplyr::if_else(level == 3 & n_cases < 5, NA_real_, ci_lower),
      ci_upper    = dplyr::if_else(level == 3 & n_cases < 5, NA_real_, ci_upper),
      lower_bound = dplyr::if_else((level == 3 & n_cases < 5) | (n_cases == 1), NA_real_, lower_bound),
      upper_bound = dplyr::if_else((level == 3 & n_cases < 5) | (n_cases == 1), NA_real_, upper_bound)
    ) %>%
    # ---------- FORMAT FOR DISPLAY (strings) ----------
  dplyr::mutate(
    incidence_rate  = dplyr::if_else(is.na(incidence_rate), "", sprintf("%.2f", incidence_rate)),
    burden_rate     = dplyr::if_else(is.na(burden_rate),    "", sprintf("%.2f", burden_rate)),
    median_timeloss = dplyr::if_else(is.na(median_timeloss), "", sprintf("%.0f", median_timeloss)),
    incidence_ci    = dplyr::if_else(is.na(ci_lower) | is.na(ci_upper), "",
                                     paste0(" [", sprintf("%.2f", ci_lower), ", ", sprintf("%.2f", ci_upper), "]")),
    median_iqr      = dplyr::if_else(is.na(q1_timeloss) | is.na(q3_timeloss), "",
                                     paste0(" (", sprintf("%.0f", q1_timeloss), ", ", sprintf("%.0f", q3_timeloss), ")")),
    burden_ci       = dplyr::if_else(is.na(lower_bound) | is.na(upper_bound), "",
                                     paste0(" [", sprintf("%.2f", lower_bound), ", ", sprintf("%.2f", upper_bound), "]"))
  ) %>%
    dplyr::select(
      level, label, n_cases,
      incidence_rate, incidence_ci,
      median_timeloss, median_iqr,
      burden_rate, burden_ci
    )
  
  # ---- Excel export ----
  style_level_2 <- openxlsx::createStyle(textDecoration = "bold", border = "top")
  style_level_3 <- openxlsx::createStyle(textDecoration = "italic", indent = 5)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "table_2")
  openxlsx::writeData(wb, "table_2", table_2)
  openxlsx::conditionalFormatting(wb, "table_2", cols = 2:9, rows = 2:1000, rule = "$A2=2", style = style_level_2)
  openxlsx::conditionalFormatting(wb, "table_2", cols = 2,    rows = 2:1000, rule = "$A2=3", style = style_level_3)
  openxlsx::setColWidths(wb, "table_2", cols = 1:9, widths = "auto")
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  
  # ---- CSV export ----
  readr::write_csv(table_2, file = make_output_path("table_injury_all_tl_2_3.csv"))
}

poisson.test(0, 1000, conf.level = 0.95)