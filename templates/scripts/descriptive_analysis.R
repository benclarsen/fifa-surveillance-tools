
# Assuming preprocessing is complete and caselist, player details, and exposure dataframes exist in environment

# Define output directory and project name

output_dir <- "03_Analysis/Results" 
project_name <- ""



## Load and prepare data -----


player_days <- data_exposure$days 

source("~/Library/CloudStorage/OneDrive-FIFA.org/Projects (FIFA)/Competition surveillance projects - current/fifa-surveillance-tools/03_Analysis/Code/analysis_functions.R")

osiics_16 <- load_osiics_16()

# Participation overview ----------

participation_table <- generate_participation_table(player_details)
write_csv(participation_table, file = make_output_path("participation_table.csv"))


# Description of cohort-------


#calculate players' age in years on first day of tournament
start_date <-  as.Date("2025-06-14") # First day of FCWC2025

player_details <- player_details %>% 
  mutate(age = sapply(date_birth, calculate_age, specific_day = start_date))


player_characteristics_table <- player_details %>%
  filter(consent == "yes") %>%
  summarise(
    Age = list(c(
      Median = round(median(age, na.rm = TRUE)),
      IQR = paste0(round(quantile(age, 0.25, na.rm = TRUE)), "–", round(quantile(age, 0.75, na.rm = TRUE))),
      Range = paste0(round(min(age, na.rm = TRUE)), "–", round(max(age, na.rm = TRUE)))
    )),
    "Height (cm)" = list(c(
      Median = round(median(height, na.rm = TRUE)),
      IQR = paste0(round(quantile(height, 0.25, na.rm = TRUE)), "–", round(quantile(height, 0.75, na.rm = TRUE))),
      Range = paste0(round(min(height, na.rm = TRUE)), "–", round(max(height, na.rm = TRUE)))
    )),
    "Body mass (kg)" = list(c(
      Median = round(median(weight, na.rm = TRUE)),
      IQR = paste0(round(quantile(weight, 0.25, na.rm = TRUE)), "–", round(quantile(weight, 0.75, na.rm = TRUE))),
      Range = paste0(round(min(weight, na.rm = TRUE)), "–", round(max(weight, na.rm = TRUE)))
    ))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Stats") %>%
  unnest_wider(Stats) %>%
  print()

write_csv(player_characteristics_table, file = make_output_path("player_characteristics_table.csv"))


# Exposure table -------
table_exposure <- data_exposure %>% 
  select(match, training, total, days) %>%
  rename(
  `Match exposure (h)` = match,
  `Training exposure (h)` = training,
  `Total exposure (h)` = total,
  `Player days` = days
) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  write_csv(, file = make_output_path("exposure_table.csv")) %>%
  print()


library(dplyr)
library(tidyr)
library(readr)

# Build the long table
table_exposure <- data_exposure %>%
  select(match, training, total, days) %>%
  rename(
    `Match exposure (h)`    = match,
    `Training exposure (h)` = training,
    `Total exposure (h)`    = total,
    `Player days`           = days
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

# Decide how to process 'value'
if (is.numeric(table_exposure$value)) {
  # Already numeric → just round
  table_exposure <- table_exposure %>%
    mutate(value = round(value))
  
} else {
  # Not numeric → coerce to character and parse, then round
  v_chr <- as.character(table_exposure$value)
  
  # Auto-detect decimal mark (e.g., "12,5")
  uses_comma_decimal <- any(grepl("\\d,\\d", v_chr))
  
  loc <- if (uses_comma_decimal) {
    readr::locale(decimal_mark = ",", grouping_mark = ".")
  } else {
    readr::locale(decimal_mark = ".", grouping_mark = ",")
  }
  
  table_exposure <- table_exposure %>%
    mutate(
      value = readr::parse_number(v_chr, locale = loc),
      value = round(value)
    )
}

# Write and print
write_csv(table_exposure, file = make_output_path("exposure_table.csv"))
print(table_exposure)

# Basic numbers -------





library(dplyr)
library(tidyr)

# Required categories (plural forms)
required_categories <- c("Injuries", "Illnesses", "Mental health problems")

basic_numbers_table <- caselist %>%
  # Normalize problem_type labels to plural forms
  mutate(problem_type = dplyr::recode(problem_type,
                                      "Injury" = "Injuries",
                                      "Illness" = "Illnesses",
                                      "Mental health problem" = "Mental health problems",
                                      .default = problem_type
  )) %>%
  group_by(problem_type) %>%
  summarise(
    Events = n_distinct(event_id[timeloss_cat == "timeloss"]),
    Cases  = n_distinct(case_id[timeloss_cat == "timeloss"]),
    `Affected players` = n_distinct(player_id),
    .groups = "drop"
  ) %>%
  # Ensure the three required categories exist; fill missing with zeros
  mutate(problem_type = as.character(problem_type)) %>%
  tidyr::complete(
    problem_type = required_categories,
    fill = list(Events = 0L, Cases = 0L, `Affected players` = 0L)
  ) %>%
  # Keep only the required categories (if others exist)
  filter(problem_type %in% required_categories) %>%
  # Order rows as specified
  arrange(factor(problem_type, levels = required_categories)) %>% 
  print()


write_csv(basic_numbers_table, file = make_output_path("basic_numbers_table.csv"))


### Number of cases by problem type and subsequent category -----

subsequent_table <- generate_subsequent_table(caselist)
write_csv(subsequent_table, file = make_output_path("subsequent_table.csv"))







# Incidence analyses -----

# Exposure totals
total_exposure <- sum(data_exposure$total)
match_exposure <- data_exposure$match
training_exposure <- data_exposure$training


# Injury analyses (using hours as exposure)
all_ma <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury"), total_exposure, "All injuries", "Medical attention")
all_tl <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss"), total_exposure, "All injuries", "Time loss")

match_ma <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Match"), match_exposure, "Match injuries", "Medical attention")
match_tl <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Match", timeloss_cat == "timeloss"), match_exposure, "Match injuries", "Time loss")

tr_ma <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Training"), training_exposure, "Training injuries", "Medical attention")
tr_tl <- run_analysis_incidence(caselist %>% filter(problem_type == "Injury", when_occurred == "Training", timeloss_cat == "timeloss"), data_exposure$training_2, "Training injuries", "Time loss")

# Illness analyses (using player-days as exposure)
all_ma_illness <- run_analysis_incidence(caselist %>% filter(problem_type == "Illness"), player_days, "All illnesses", "Medical attention")
all_tl_illness <- run_analysis_incidence(caselist %>% filter(problem_type == "Illness", timeloss_cat == "timeloss"), player_days, "All illnesses", "Time loss")

# 4. Combine and Export Results

result_incidence <- bind_rows(
  all_ma, match_ma, tr_ma,
  all_tl, match_tl, tr_tl,
  all_ma_illness, all_tl_illness
) %>% print()




## Burden analyses ------

# All injuries
all_ma <- run_analysis_burden(caselist, sum(data_exposure$total), "All injuries", "Medical attention")
all_tl <- run_analysis_burden(filter(caselist, timeloss_cat == "timeloss"), sum(data_exposure$total), "All injuries", "Time loss")

# Match injuries
match_ma <- run_analysis_burden(filter(caselist, when_occurred == "Match"), sum(data_exposure$match), "Match injuries", "Medical attention")
match_tl <- run_analysis_burden(filter(caselist, when_occurred == "Match", timeloss_cat == "timeloss"), sum(data_exposure$match), "Match injuries", "Time loss")

# Training injuries
tr_ma <- run_analysis_burden(filter(caselist, when_occurred == "Training"), sum(data_exposure$training), "Training injuries", "Medical attention")
tr_tl <- run_analysis_burden(filter(caselist, when_occurred == "Training", timeloss_cat == "timeloss"), sum(data_exposure$training), "Training injuries", "Time loss")

# Illnesses
all_tl_illness <- run_analysis_burden(
  filter(caselist, timeloss_cat == "timeloss", problem_type == "Illness"),
  player_days,
  "All illnesses",
  "Time loss"
)

# Combine and export results
result_burden <- bind_rows(all_tl, match_tl, tr_tl, all_tl_illness)

result <- left_join(result_incidence, result_burden)

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


save_overall_results_excel(result, make_output_path("overall_results.xlsx"))
write_csv(result, file = make_output_path("overall_results.csv"))




# Table time loss injuries showing exposure (per 1000h) -------

injury_table <- generate_basic_injury_table(caselist, data_exposure)

write_csv(injury_table, file = make_output_path("injury_table.csv"))



# Table all health problems showing exposure in athlete days injuries, illnesses and mental health problems

health_problems_table <- 
  generate_basic_health_problems_table(caselist, data_exposure)

write_csv(health_problems_table, file = make_output_path("health_problems_table.csv"))


# Table all health problems showing exposure in athlete days injuries, illnesses and mental health problems



# Injury table 1 (1-3-4)------------

caselist <- caselist %>%
  left_join(osiics_16)


# Time loss - All injuries
generate_table_1_3_4(
  caselist_input = caselist %>% filter(
    problem_type == "Injury", 
    timeloss_cat == "timeloss"),
  exposure_input = sum(data_exposure$total),
  output_path = make_output_path("table_injury_all_tl_1_3_4.xlsx")
)

# Time loss - Match injuries
generate_table_1_3_4(
  caselist_input = caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss", when_occurred == "Match"),
  exposure_input = sum(data_exposure$match),
  output_path = make_output_path("table_injury_match_tl_1_3_4.xlsx")
)

# Time loss - Training injuries
generate_table_1_3_4(
  caselist_input = caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss", when_occurred == "Training"),
  exposure_input = sum(data_exposure$training),
  output_path = make_output_path("table_injury_training_tl_1_3_4.xlsx")
)

# 
# # Medical attention - All injuries
# generate_table_1_3_4(
#   caselist_input = caselist %>% filter(problem_type == "Injury"),
#   exposure_input = sum(data_exposure$match),
#   output_path = make_output_path("table_injury_all_ma_1_3_4.xlsx")
# )
# 
# # Medical attention  - Match injuries
# generate_table_1_3_4(
#   caselist_input = caselist %>% filter(problem_type == "Injury",  when_occurred == "Match"),
#   exposure_input = sum(data_exposure$match),
#   output_path = make_output_path("table_injury_match_ma_1_3_4.xlsx")
# )

# # Medical attention  - Training injuries
# generate_table_1_3_4(
#   caselist_input = caselist %>% filter(problem_type == "Injury",  when_occurred == "Training"),
#   exposure_input = sum(data_exposure$training),
#   output_path = make_output_path("table_injury_training_ma_1_3_4.xlsx")
# )


# Injury table 2 (2-3)-----


generate_table_2_3(
  caselist_input = caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss"),
  exposure_input = sum(data_exposure$match),
  output_path = make_output_path("table_injury_all_tl_2_3.xlsx")
)


# Time loss - Match injuries
generate_table_2_3(
  caselist_input = caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss", when_occurred == "Match"),
  exposure_input = sum(data_exposure$match),
  output_path = make_output_path("table_injury_match_tl_2_3.xlsx")
)

# Time loss - Training injuries
generate_table_2_3(
  caselist_input = caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss", when_occurred == "Training"),
  exposure_input = sum(data_exposure$training),
  output_path = make_output_path("table_injury_training_tl_2_3.xlsx")
)


# Medical attention - All injuries
generate_table_2_3(
  caselist_input = caselist %>% filter(problem_type == "Injury"),
  exposure_input = sum(data_exposure$match),
  output_path = make_output_path("table_injury_all_ma_2_3.xlsx")
)

# Medical attention  - Match injuries
generate_table_2_3(
  caselist_input = caselist %>% filter(problem_type == "Injury",  when_occurred == "Match"),
  exposure_input = sum(data_exposure$match),
  output_path = make_output_path("table_injury_match_ma_2_3.xlsx")
)

# Medical attention  - Training injuries
generate_table_2_3(
  caselist_input = caselist %>% filter(problem_type == "Injury",  when_occurred == "Training"),
  exposure_input = sum(data_exposure$training),
  output_path = make_output_path("table_injury_training_ma_2_3.xlsx")
)


# Illness Table (1,3,4) ---------

generate_table_1_3_4(
  caselist_input = caselist %>% filter(problem_type == "Illness", timeloss_cat == "timeloss"),
  exposure_input = player_days,
  output_path = make_output_path("table_illness_all_tl_1_3_4.xlsx")
)



# Mental health problems Table (1,3,4) ---------

generate_table_1_3_4(
  caselist_input = caselist %>% filter(problem_type == "Mental health problem"),
  exposure_input = player_days,
  output_path = make_output_path("table_mh_all_ma_1_3_4.xlsx")
)







# Injury severity bins ----
severity_cat_injury <- 
  caselist %>% 
  #filter(when_occurred != "Other") %>%
  mutate(severity_cat = case_when(
    timeloss == 0 ~ "0 days",
    timeloss >0 & timeloss <4 ~ "1-3 days",
    timeloss >3 & timeloss <8 ~ "4-7 days",
    timeloss >7 & timeloss <29 ~ "8-28 days",
    timeloss >28 & timeloss <91 ~ "29-90 days",
    timeloss >90 & timeloss <181 ~ "91-180 days",
    timeloss >180  ~ ">180 days",
  )) %>% 
  filter(
    problem_type == "Injury", 
    subsequent_cat != "exacerbation"
  ) %>%
  group_by(severity_cat) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)*100) %>% 
  print()

severity_cat_injury$severity_cat <- 
  factor(
    severity_cat_injury$severity_cat,
    levels = c(
      # "0 days",
      "1-3 days",
       "4-7 days",
      "8-28 days",
      "29-90 days",
      "91-180 days"
      # ">180 days"
  ))

severity_cat_injury <- arrange(severity_cat_injury, severity_cat) %>% print()


# by body area
time_loss_table <- 
  caselist %>%
  filter(problem_type == "Injury", subsequent_cat != "exacerbation")  %>% 
  mutate(severity_cat = case_when(
    timeloss == 0 ~ "0 days",
    timeloss >0 & timeloss <4 ~ "1-3 days",
    timeloss >3 & timeloss <8 ~ "4-7 days",
    timeloss >7 & timeloss <29 ~ "8-28 days",
    timeloss >28 & timeloss <91 ~ "29-90 days",
    timeloss >90 & timeloss <181 ~ "91-180 days",
    timeloss >180  ~ ">180 days",
  )) %>%
  group_by(severity_cat, osiics_16_level_1) %>%
  summarise(n = n()) %>%
  mutate(n = replace_na(n, 0)) %>%
  pivot_wider(names_from = severity_cat, values_from = n) %>%
  select(
    osiics_16_level_1,
    "1-3 days",
     "4-7 days",
    "8-28 days",
    "29-90 days",
    "91-180 days"
    # ">180 days"
  ) %>%
  
  print()

unique(time_loss_table$osiics_16_level_1 )


time_loss_table$osiics_16_level_1 <- factor(time_loss_table$osiics_16_level_1, levels = c(
  "Head",
  "Neck",
  "Shoulder",
  "Upper arm",
  "Elbow",
  "Forearm",
  "Wrist",
  "Hand",
  "Chest",
  "Thoracic spine",
  "Lumbosacral",
  "Lumbar Spine",
  "Abdomen",
  "Hip/groin",
  "Groin/hip",
  "Hip",
  "Groin",
  "Thigh",
  "Knee",
  "Lower leg",
  "Ankle",
  "Foot",
  "Region unspecified",
  "Single injury crossing two or more regions"
))

time_loss_table <- arrange(time_loss_table, osiics_16_level_1)


totals <- caselist %>%
  filter(problem_type == "Injury", subsequent_cat != "exacerbation")  %>%  
  mutate(severity_cat = case_when(
    #timeloss == 0 ~ "0 days",
    timeloss >0 & timeloss <4 ~ "1-3 days",
    timeloss >3 & timeloss <8 ~ "4-7 days",
    timeloss >7 & timeloss <29 ~ "8-28 days",
    timeloss >28 & timeloss <91 ~ "29-90 days",
    timeloss >90 & timeloss <181 ~ "91-180 days",
    timeloss >180  ~ ">180 days",
  )) %>%
  group_by(severity_cat) %>%
  summarise(n = n()) %>%
  mutate(n = replace_na(n, 0)) %>%
  pivot_wider(names_from = severity_cat, values_from = n) %>%
  mutate(osiics_16_level_1 = "Total") %>%
  select(osiics_16_level_1, "1-3 days",  "4-7 days", "8-28 days", "29-90 days", "91-180 days") %>%
  print()

time_loss_table <- rbind(time_loss_table, totals) 

time_loss_table <- time_loss_table %>%
  mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  print()



# Create styles
style_header <- createStyle(textDecoration = "bold", border = "bottom", halign = "center")
style_body <- createStyle(halign = "left")

# Create workbook and add worksheet
wb <- createWorkbook()
addWorksheet(wb, "Time loss table")

# Write data
writeData(wb, "Time loss table", time_loss_table, headerStyle = style_header)

# Apply body style
addStyle(wb, "Time loss table", style_body, rows = 2:(nrow(participation_table)+1), cols = 1:2, gridExpand = TRUE)

# Auto-adjust column widths
setColWidths(wb, "Time loss table", cols = 1:2, widths = "auto")

# Save workbook
saveWorkbook(wb, file = make_output_path("time_loss_table.xlsx"), overwrite = TRUE)
write_csv(time_loss_table, file = make_output_path("time_loss_table.csv"))



# for report: body image labels ------



df <- caselist %>%
  filter(problem_type == "Injury", subsequent_cat != "exacerbation")  %>%  
  group_by(osiics_16_level_1) %>%
  summarise(n_tl = n(),
            tl = sum(timeloss_expected))%>% 
  arrange(desc(tl)) %>% print()



# Create styles
style_header <- createStyle(textDecoration = "bold", border = "bottom", halign = "center")
style_body <- createStyle(halign = "left")

# Create workbook and add worksheet
wb <- createWorkbook()
addWorksheet(wb, "Body image labels")

# Write data
writeData(wb, "Body image labels", df, headerStyle = style_header)

# Apply body style
addStyle(wb, "Body image labels", style_body, rows = 2:(nrow(participation_table)+1), cols = 1:2, gridExpand = TRUE)

# Auto-adjust column widths
setColWidths(wb, "Body image labels", cols = 1:2, widths = "auto")

# Save workbook
saveWorkbook(wb, file = make_output_path("body_image_labels.xlsx"), overwrite = TRUE)




