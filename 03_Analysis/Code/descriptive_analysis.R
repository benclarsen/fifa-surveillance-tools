
source("code/osiics_15.R")
# Define output directory and project name

output_dir <- "03_Analysis/Results" 
project_name <- "test"



## Load and prepare data -----
### Player data -------------
player_details <- read_delim("02_Data/example_player_details.csv", delim = ";") 

###  Case data --------
caselist <- read_delim("02_Data/example_caselist.csv", delim = ";") %>% filter(subsequent_cat != "exacerbation")

### Exposure data --------
data_exposure <- read_delim("02_Data/example_exposure_daily.csv", delim = ";") %>% 
  summarise(
    match = sum(match_minutes)/60,
    training = sum(training_minutes)/60) %>%
  mutate(total = match + training
  ) %>% print()

player_days <- read_delim("02_Data/example_exposure_daily.csv", delim = ";") %>% nrow()




# Participation overview ----------


participation_table <- player_details %>%
  summarise(
    `Total number of teams` = n_distinct(team),
    `Participating teams (consented)` = n_distinct(team[consent == "yes"]),
    `Proportion of teams included` = round(`Participating teams (consented)` / `Total number of teams`, 2),
    `Total number of players` = n(),
    `Consenting players` = n_distinct(player_id[consent == "yes"]),
    `Proportion of players included` = round(`Consenting players` / `Total number of players`, 2),
    `Proportion of players from consenting teams` = round(mean(consent == "yes"), 2)
  ) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Metric") %>%
  rename(Value = V1) %>%
  mutate(Value = ifelse(grepl("Proportion", Metric), round(as.numeric(Value), 2), as.integer(Value))) %>% 
  print()


# Create styles
style_header <- createStyle(textDecoration = "bold", border = "bottom", halign = "center")
style_body <- createStyle(halign = "left")

# Create workbook and add worksheet
wb <- createWorkbook()
addWorksheet(wb, "Participation Summary")

# Write data
writeData(wb, "Participation Summary", participation_table, headerStyle = style_header)

# Apply body style
addStyle(wb, "Participation Summary", style_body, rows = 2:(nrow(participation_table)+1), cols = 1:2, gridExpand = TRUE)

# Auto-adjust column widths
setColWidths(wb, "Participation Summary", cols = 1:2, widths = "auto")

# Save workbook
saveWorkbook(wb, file = make_output_path("participation_summary.xlsx"), overwrite = TRUE)



# Description of cohort-------


#calculate players' age in years on first day of tournament
start_date <-  as.Date("2024-07-19") # First day of OFT 2024

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
    height = list(c(
      Median = round(median(height, na.rm = TRUE)),
      IQR = paste0(round(quantile(height, 0.25, na.rm = TRUE)), "–", round(quantile(height, 0.75, na.rm = TRUE))),
      Range = paste0(round(min(height, na.rm = TRUE)), "–", round(max(height, na.rm = TRUE)))
    )),
    weight = list(c(
      Median = round(median(weight, na.rm = TRUE)),
      IQR = paste0(round(quantile(weight, 0.25, na.rm = TRUE)), "–", round(quantile(weight, 0.75, na.rm = TRUE))),
      Range = paste0(round(min(weight, na.rm = TRUE)), "–", round(max(weight, na.rm = TRUE)))
    ))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Stats") %>%
  unnest_wider(Stats)




# Create styles
style_header <- createStyle(textDecoration = "bold", border = "bottom", halign = "center")
style_body <- createStyle(halign = "left")

# Create workbook and add worksheet
wb <- createWorkbook()
addWorksheet(wb, "Player Characteristics")

# Write data
writeData(wb, "Player Characteristics", player_characteristics_table, headerStyle = style_header)

# Apply styles to body
addStyle(wb, "Player Characteristics", style_body, rows = 2:(nrow(player_characteristics_table)+1), cols = 1:4, gridExpand = TRUE)

# Auto-adjust column widths
setColWidths(wb, "Player Characteristics", cols = 1:4, widths = "auto")

# Save workbook
saveWorkbook(wb, file = make_output_path("player_characteristics_summary.xlsx"), overwrite = TRUE)



# Basic outcomes -------

### Number of cases by problem type and subsequent category -----
caselist %>%
  group_by(problem_type, subsequent_cat) %>%
  summarise(n_cases = n_distinct(case_id), .groups = "drop") %>%
  print()

### Number of time-loss cases by problem type ----------------
caselist %>%
  filter(timeloss_cat == "timeloss") %>%
  group_by(problem_type) %>%
  summarise(n_cases = n_distinct(case_id), .groups = "drop") %>%
  print()

### Number of involved players by problem type -----
caselist %>%
  group_by(problem_type) %>%
  summarise(n_players = n_distinct(player_id), .groups = "drop") %>%
  print()



# Incidence analyses -----

# Exposure totals
total_exposure <- sum(data_exposure$total)
match_exposure <- data_exposure$match
training_exposure <- data_exposure$training


# Injury analyses (using hours as exposure)
all_ma <- run_analysis(caselist %>% filter(problem_type == "Injury"), total_exposure, "All injuries", "Medical attention")
all_tl <- run_analysis(caselist %>% filter(problem_type == "Injury", timeloss_cat == "timeloss"), total_exposure, "All injuries", "Time loss")

match_ma <- run_analysis(caselist %>% filter(problem_type == "Injury", when_occurred == "Match"), match_exposure, "Match injuries", "Medical attention")
match_tl <- run_analysis(caselist %>% filter(problem_type == "Injury", when_occurred == "Match", timeloss_cat == "timeloss"), match_exposure, "Match injuries", "Time loss")

tr_ma <- run_analysis(caselist %>% filter(problem_type == "Injury", when_occurred == "Training"), training_exposure, "Training injuries", "Medical attention")
tr_tl <- run_analysis(caselist %>% filter(problem_type == "Injury", when_occurred == "Training", timeloss_cat == "timeloss"), training_exposure, "Training injuries", "Time loss")

# Illness analyses (using player-days as exposure)
all_ma_illness <- run_analysis(caselist %>% filter(problem_type == "Illness"), player_days, "All illnesses", "Medical attention")
all_tl_illness <- run_analysis(caselist %>% filter(problem_type == "Illness", timeloss_cat == "timeloss"), player_days, "All illnesses", "Time loss")

# 4. Combine and Export Results

result_incidence <- bind_rows(
  all_ma, match_ma, tr_ma,
  all_tl, match_tl, tr_tl,
  all_ma_illness, all_tl_illness
)




## Burden analyses ------

# All injuries
all_ma <- run_analysis(caselist, sum(data_exposure$total), "All injuries", "Medical attention")
all_tl <- run_analysis(filter(caselist, timeloss_cat == "timeloss"), sum(data_exposure$total), "All injuries", "Time loss")

# Match injuries
match_ma <- run_analysis(filter(caselist, when_occurred == "Match"), sum(data_exposure$match), "Match injuries", "Medical attention")
match_tl <- run_analysis(filter(caselist, when_occurred == "Match", timeloss_cat == "timeloss"), sum(data_exposure$match), "Match injuries", "Time loss")

# Training injuries
tr_ma <- run_analysis(filter(caselist, when_occurred == "Training"), sum(data_exposure$training), "Training injuries", "Medical attention")
tr_tl <- run_analysis(filter(caselist, when_occurred == "Training", timeloss_cat == "timeloss"), sum(data_exposure$training), "Training injuries", "Time loss")

# Illnesses
all_tl_illness <- run_analysis(
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





# Injury table 1 (1-3-4)------------

caselist <- left_join(caselist, osiics_15)


# Time loss - All injuries
generate_table_1_3_4(
  caselist_input = caselist %>% filter(
    problem_type == "Injury", 
    timeloss_cat == "timeloss"),
  exposure_input = sum(data_exposure$match),
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


# Medical attention - All injuries
generate_table_1_3_4(
  caselist_input = caselist %>% filter(problem_type == "Injury"),
  exposure_input = sum(data_exposure$match),
  output_path = make_output_path("table_injury_all_ma_1_3_4.xlsx")
)

# Medical attention  - Match injuries
generate_table_1_3_4(
  caselist_input = caselist %>% filter(problem_type == "Injury",  when_occurred == "Match"),
  exposure_input = sum(data_exposure$match),
  output_path = make_output_path("table_injury_match_ma_1_3_4.xlsx")
)

# Medical attention  - Training injuries
generate_table_1_3_4(
  caselist_input = caselist %>% filter(problem_type == "Injury",  when_occurred == "Training"),
  exposure_input = sum(data_exposure$training),
  output_path = make_output_path("table_injury_training_ma_1_3_4.xlsx")
)


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

