

# 
# # Load required libraries
# load_or_install(c("readr", "tidyverse", "stringr"))
# 
# 
# 
my_locale <- readr::locale(encoding = "UTF-8")
# osiics_16 <- readr::read_delim("02_Data/osiics_16.csv", delim = ";", locale = my_locale) 
# 
# osiics_16 <- osiics_16 %>% 
#   mutate(
#     osiics_16_level_1 = case_when(
#     `Body part` != "Medical" ~ `Body part`,
#     TRUE ~ `Medical System`),
#     osiics_16_level_3 = case_when(
#       `Body part` != "Medical" ~ `Injury type`,
#       TRUE ~ `Pathology type`
#   ))
# 
# 
# osiics_16 <- osiics_16 %>%
#   rename(
#   "osiics_16_code" = "Code",
#   "osiics_16_level_4" = "Diagnosis")
# # Clean whitespace from classification fields
# osiics_16 <- osiics_16 %>%
#   mutate(
#     osiics_16_level_1 = str_trim(osiics_16_level_1, side = "right"),
#     osiics_16_level_3 = str_trim(osiics_16_level_3, side = "right"),
#     osiics_16_level_4 = str_trim(osiics_16_level_4, side = "right")
#   )
# 
# 
# # align nomenclature of OSIICS to IOC consensus - note - all hip/groin cases in this dataset are groin!
# 
# unique(osiics_16$osiics_16_level_3)
# 
# osiics_16 <- osiics_16 %>% 
#   mutate(osiics_16_level_2 = case_when(
#     osiics_16_level_3 %in% c(
#       "Muscle injury",
#       "Muscle contusion",  
#       "Laceration",
#       "Contusion/vascular",
#       "Muscle compartment syndrome",
#       "Tendinopathy",
#       "Tendon rupture"
#     ) ~ "Muscle/Tendon",
#     osiics_16_level_3  == "Nerve injury"
#        ~ "Nervous",
#     osiics_16_level_3 %in% c(
#       "Fracture",
#       "Bone contusion",
#       "Bone stress injury",
#       "Physis injury",
#       "Avascular necrosis"
#     ) ~ "Bone",
#     osiics_16_level_3 %in% c(
#       "Cartilage",
#       "Arthritis",
#       "Synovitis / capsulitis",
#       "Bursitis"
#     ) ~ "Cartilage/Synovium/Bursa",
#     osiics_16_level_3 %in% c(
#       "Ligament",
#       "Joint sprain",
#       "Chronic instability"
#     ) ~ "Ligament/Joint capsule",
#     osiics_16_level_3 %in% c(
#       "Abrasion",
#       "Laceration"
#     ) ~ "Superficial tissues/skin",
#     osiics_16_level_3 %in% c(
#       "Contusion/vascular"
#     ) ~ "Vascular trauma",
#     osiics_16_level_3 %in% c(
#       "Stump injury" 
#     ) ~ "Stump",
#     osiics_16_level_3 %in% c(
#       "Organ trauma" 
#     ) ~ "Internal organs",
#     osiics_16_level_3 %in% c(
#       "Unknown",
#       "Pain without tissue type specified"
#     ) ~ "Non-specific"
#   ))
# 
# 
# 
# 
# # Level 1: Body Part and organ system
# level_1_order <- c(
#   "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
#   "Chest", "Thoracic spine", "Thoracic Spine", "Lumbosacral", "Lumbar Spine",
#   "Abdomen", "Hip/groin", "Groin/hip", "Hip", "Groin", "Thigh", "Knee",
#   "Lower leg", "Ankle", "Foot", "Region unspecified",
#   "Single injury crossing two or more regions", "Medical", "Cardiovascular",
#   "Dermatological", "Dental", "Endocrinological", "Gastrointestinal",
#   "Genitourinary", "Hematologic", "Musculoskeletal", "Neurological",
#   "Opthalmological", "Otological", "Psychiatric/psychological", "Respiratory",
#   "Thermoregulatory", "Multiple systems", "Multiple", "Unknown or not specified"
# )
# 
# # Level 2: Tissue Type
# level_2_order <- c(
#   "All", "Muscle/tendon", "Nervous", "Nervous system", "Bone",
#   "Cartilage/synovium/bursa", "Ligament/joint capsule", "Superficial tissues/skin",
#   "Vessels", "Stump", "Internal organs", "Non-specific", "Allergic",
#   "Environmental – exercise-related", "Environmental – non-exercise",
#   "Immunological/inflammatory", "Infection", "Neoplasm", "Metabolic/nutritional",
#   "Thrombotic/haemorrhagic", "Degenerative or chronic condition",
#   "Developmental anomaly", "Drug-related/poisoning", "Multiple",
#   "Unknown, or not specified"
# )
# 
# # Level 3: Pathology type and aetiology
# level_3_order <- c(
#   "All", "Muscle injury", "Muscle contusion", "Contusion/vascular",
#   "Muscle compartment syndrome", "Tendinopathy", "Tendon rupture",
#   "Brain & spinal cord injury", "Brain/Spinal cord injury", "Peripheral nerve injury",
#   "Nerve injury", "Fracture", "Bone stress injury", "Bone contusion",
#   "Avascular necrosis", "Physis injury", "Cartilage", "Cartilage injury",
#   "Arthritis", "Synovitis / capsulitis", "Synovitis/capsulitis", "Bursitis",
#   "Joint sprain (ligament tear or acute instability)", "Joint sprain",
#   "Chronic instability", "Contusion (superficial)", "Superficial contusion",
#   "Laceration", "Abrasion", "Vascular trauma", "Stump injury", "Organ trauma",
#   "Injury without tissue type specified", "Pain without tissue type specified","Allergic",
#   "Environmental – exercise-related", "Environmental – non-exercise",
#   "Immunological/inflammatory", "Infection", "Neoplasm", "Metabolic/nutritional",
#   "Thrombotic/haemorrhagic", "Degenerative or chronic condition",
#   "Developmental anomaly", "Drug-related/poisoning", "Multiple",
#   "Unknown, or not specified", "Unknown"
# )
# 
# # Level 4: Diagnosis
# level_4_order <- c("All", unique(osiics_16$osiics_16_level_4))
# 
# 
# 
# osiics_16 <- osiics_16 %>%
#   mutate(
#     osiics_16_level_1 = str_trim(osiics_16_level_1, side = "both"),
#     osiics_16_level_2 = str_trim(osiics_16_level_2, side = "both"),
#     osiics_16_level_3 = str_trim(osiics_16_level_3, side = "both"),
#     osiics_16_level_4 = str_trim(osiics_16_level_4, side = "both")
#   )
# 
# 
# 
# 
# osiics_16 <- osiics_16 %>%
#   mutate(
#     osiics_16_level_1 = case_match(
#       osiics_16_level_1,
#       "Lumbar Spine" ~ "Lumbosacral",
#       "Upper Arm" ~ "Upper arm",
#       "Multiple" ~ "Multiple systems",
#       "Multiple systems or not otherwise specified" ~ "Unknown or not specified",
#       "Not specific" ~ "Unknown or not specified",
#       "Unknown" ~ "Unknown or not specified",
#       "Ophthalmological" ~ "Opthalmological",
#       .default = osiics_16_level_1   # <--- This keeps all other values unchanged
#     ),
#     osiics_16_level_3 = case_match(
#       osiics_16_level_3,
#       "Ligament" ~ "Joint sprain",
#       "Nerve injury" ~ "Brain/Spinal cord injury",
#       "Synovitis / capsulitis" ~ "Synovitis/capsulitis" ,
#       .default = osiics_16_level_3   # <--- This keeps all other values unchanged
#     ))
# 
# 
# #"Groin/hip" = "Groin
# 
# 
# 
# 
# unique(osiics_16$osiics_16_level_1)
# unique(osiics_16$osiics_16_level_2)
# unique(osiics_16$osiics_16_level_3)
# 

# # Apply Factor Levels
# osiics_16 <- osiics_16 %>%
#   mutate(
#     osiics_16_level_1 = factor(osiics_16_level_1, levels = level_1_order),
#     osiics_16_level_2 = factor(osiics_16_level_2, levels = level_2_order),
#     osiics_16_level_3 = factor(osiics_16_level_3, levels = level_3_order),
#     osiics_16_level_4 = factor(osiics_16_level_4, levels = level_4_order)
#   ) 
# 
# osiics_16 <- osiics_16 %>%
#   select(
#     osiics_16_code,  osiics_16_level_1,osiics_16_level_2, osiics_16_level_3, osiics_16_level_4
#   )
# 
# 
# 
# unique(osiics_16$osiics_16_level_1)
# 
# write_csv(osiics_16, "02_Data/osiics_16_updated.csv")
osiics_16 <- readr::read_delim("02_Data/osiics_16_updated.csv", delim = ";", locale = my_locale) 


 unique(osiics_16$osiics_16_level_1)

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




 unique(osiics_16$osiics_16_level_3)

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


write_csv(osiics_16, "02_Data/osiics_16_fifa_version.csv")

