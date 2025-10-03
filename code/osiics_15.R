


# Load required libraries
load_or_install("readr", "tidyverse", "stringr")



my_locale <- readr::locale(encoding = "UTF-8")
osiics_15 <- readr::read_delim("data/osiics_15.csv", delim = ";", locale = my_locale) 

# Clean whitespace from classification fields
osiics_15 <- osiics_15 %>%
  mutate(
    osiics_15_body_part = str_trim(osiics_15_body_part, side = "right"),
    osiics_15_tissue = str_trim(osiics_15_tissue, side = "right"),
    osiics_15_pathology = str_trim(osiics_15_pathology, side = "both"),
    osiics_15_diagnosis = str_trim(osiics_15_diagnosis, side = "right")
  )


# align nomenclature of OSIICS to IOC consensus - note - all hip/groin cases in this dataset are groin!






osiics_15 <- osiics_15 %>%
  mutate(osiics_15_body_part = case_when(
    osiics_15_body_part == "Medical" ~ NA,
    TRUE ~ osiics_15_body_part
  )) %>%
  mutate(osiics_15_level_1 = case_when(
    !is.na(osiics_15_body_part) ~ osiics_15_body_part,
    TRUE ~ osiics_15_organ_system
  )) %>%
  mutate(osiics_15_level_2 = case_when(
    !is.na(osiics_15_tissue) ~ osiics_15_tissue,
    TRUE ~ osiics_15_eitiology
  )) %>%
  mutate(osiics_15_level_3 = osiics_15_pathology,
         osiics_15_level_4 = osiics_15_diagnosis
  )




# Level 1: Body Part and organ system
level_1_order <- c(
  "Head", "Neck", "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
  "Chest", "Thoracic spine", "Thoracic Spine", "Lumbosacral", "Lumbar Spine",
  "Abdomen", "Hip/groin", "Groin/hip", "Hip", "Groin", "Thigh", "Knee",
  "Lower leg", "Ankle", "Foot", "Region unspecified",
  "Single injury crossing two or more regions", "Medical", "Cardiovascular",
  "Dermatological", "Dental", "Endocrinological", "Gastrointestinal",
  "Genitourinary", "Hematologic", "Musculoskeletal", "Neurological",
  "Opthalmological", "Otological", "Psychiatric/psychological", "Respiratory",
  "Thermoregulatory", "Multiple systems", "Unknown or not specified"
)

# Level 2: Tissue Type and etiology
level_2_order <- c(
  "All", "Muscle/tendon", "Nervous", "Nervous system", "Bone",
  "Cartilage/synovium/bursa", "Ligament/joint capsule", "Superficial tissues/skin",
  "Vessels", "Stump", "Internal organs", "Non-specific", "Allergic",
  "Environmental – exercise-related", "Environmental – non-exercise",
  "Immunological/inflammatory", "Infection", "Neoplasm", "Metabolic/nutritional",
  "Thrombotic/haemorrhagic", "Degenerative or chronic condition",
  "Developmental anomaly", "Drug-related/poisoning", "Multiple",
  "Unknown, or not specified"
)

# Level 3: Pathology type
level_3_order <- c(
  "All", "Muscle injury", "Muscle contusion", "Contusion/vascular",
  "Muscle compartment syndrome", "Tendinopathy", "Tendon rupture",
  "Brain & spinal cord injury", "Brain/Spinal cord injury", "Peripheral nerve injury",
  "Nerve injury", "Fracture", "Bone stress injury", "Bone contusion",
  "Avascular necrosis", "Physis injury", "Cartilage", "Cartilage injury",
  "Arthritis", "Synovitis / capsulitis", "Synovitis/capsulitis", "Bursitis",
  "Joint sprain (ligament tear or acute instability)", "Joint sprain",
  "Chronic instability", "Contusion (superficial)", "Superficial contusion",
  "Laceration", "Abrasion", "Vascular trauma", "Stump injury", "Organ trauma",
  "Injury without tissue type specified", "Pain without tissue type specified",
  "Unknown"
)

# Level 4: Diagnosis
level_4_order <- c("All", unique(osiics_15$osiics_15_diagnosis))



osiics_15 <- osiics_15 %>%
  mutate(
    osiics_15_level_1 = str_trim(osiics_15_level_1, side = "both"),
    osiics_15_level_2 = str_trim(osiics_15_level_2, side = "both"),
    osiics_15_level_3 = str_trim(osiics_15_level_3, side = "both"),
    osiics_15_level_4 = str_trim(osiics_15_level_4, side = "both")
  )




osiics_15 <- osiics_15 %>%
  mutate(
    osiics_15_level_1 = case_match(
      osiics_15_level_1,
      "Lumbar Spine" ~ "Lumbosacral",
      "Upper Arm" ~ "Upper arm",
      "Multiple" ~ "Multiple systems",
      "Multiple systems or not otherwise specified" ~ "Unknown or not specified",
      "Not specific" ~ "Unknown or not specified",
      "Unknown" ~ "Unknown or not specified",
      "Ophthalmological" ~ "Opthalmological",
      .default = osiics_15_level_1   # <--- This keeps all other values unchanged
    ),
    osiics_15_level_3 = case_match(
      osiics_15_level_3,
      "Ligament" ~ "Joint sprain (ligament tear or acute instability)",
      "Nerve injury" ~ "Brain/Spinal cord injury",
      "Synovitis / capsulitis" ~ "Synovitis/capsulitis" ,
      .default = osiics_15_level_3   # <--- This keeps all other values unchanged
    ))


#"Groin/hip" = "Groin




unique(osiics_15$osiics_15_level_1)
unique(osiics_15$osiics_15_level_2)
unique(osiics_15$osiics_15_level_3)

setdiff(osiics_15$osiics_15_level_1, level_1_order)
setdiff(osiics_15$osiics_15_level_2, level_2_order)
setdiff(osiics_15$osiics_15_level_3, level_3_order)

# Apply Factor Levels
osiics_15 <- osiics_15 %>%
  mutate(
    osiics_15_level_1 = factor(osiics_15_level_1, levels = level_1_order),
    osiics_15_level_2 = factor(osiics_15_level_2, levels = level_2_order),
    osiics_15_level_3 = factor(osiics_15_level_3, levels = level_3_order),
    osiics_15_level_4 = factor(osiics_15_level_4, levels = level_4_order)
  ) 

osiics_15 <- osiics_15 %>%
  select(
    osiics_15_code, osiics_15_diagnosis, osiics_15_level_1:osiics_15_level_4
  )
