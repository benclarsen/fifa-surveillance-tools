# OSIICS 16 reference table ----
#
# The FIFA version of OSIICS 16 is the single source of truth for the injury
# and illness taxonomy. It is a semicolon-delimited file in data/, maintained
# by hand. dev/osiics_16.R records how it was originally derived but no longer
# reproduces it - the current file has been edited since.
#
# Levels:
#   1  body area or organ system
#   2  tissue type or aetiology
#   3  pathology type
#   4  specific diagnosis
#
# The orderings below are the display order used in every table and figure.
# They are defined here once. Do not copy them into analysis scripts.


## Category orderings ----

problem_type_order <- c(
  "Injury",
  "Illness",
  "Mental health problem"
)


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


level_2_order <- c(
  "All", "Muscle/tendon", "Nervous", "Bone",
  "Cartilage/synovium/bursa", "Ligament/joint capsule", "Superficial tissues/skin",
  "Vessels", "Stump", "Internal organs", "Non-specific"
)


level_3_order <- c(
  "All", "Muscle injury", "Muscle contusion",
  "Muscle compartment syndrome", "Tendinopathy", "Tendon rupture",
  "Brain/spinal cord injury", "Peripheral nerve injury",
  "Fracture", "Bone stress injury", "Bone contusion",
  "Avascular necrosis", "Physis injury", "Cartilage injury",
  "Arthritis", "Synovitis/capsulitis", "Bursitis", "Joint sprain",
  "Chronic instability", "Contusion (superficial)",
  "Laceration", "Abrasion", "Vascular trauma", "Stump injury", "Organ trauma",
  "Injury without tissue type specified", "Allergic",
  "Environmental - exercise-related", "Environmental - non-exercise",
  "Immunological/inflammatory", "Infection", "Neoplasm", "Metabolic/nutritional",
  "Thrombotic/haemorrhagic", "Degenerative or chronic condition",
  "Developmental anomaly", "Drug-related/poisoning", "Multiple",
  "Unknown/not specified", "Unknown"
)


## Load the reference table ----

# Read the OSIICS reference table, apply the display orderings, and warn about
# any label in the file that the orderings do not cover.
# `file` defaults to osiics_file, which load_tools.R defines.
load_osiics <- function(file = osiics_file) {
  
  if (!file.exists(file)) {
    stop("OSIICS reference file not found: ", file)
  }
  
  osiics <- readr::read_delim(
    file,
    delim = ";",
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
  
  required <- c("osiics_16_code", "problem_type", paste0("osiics_16_level_", 1:4))
  absent   <- setdiff(required, names(osiics))
  
  if (length(absent) > 0) {
    stop("OSIICS reference file is missing columns: ", paste(absent, collapse = ", "))
  }
  
  check_labels(osiics$problem_type,      problem_type_order, "problem_type")
  check_labels(osiics$osiics_16_level_1, level_1_order,      "level 1")
  check_labels(osiics$osiics_16_level_2, level_2_order,      "level 2")
  check_labels(osiics$osiics_16_level_3, level_3_order,      "level 3")
  
  osiics$problem_type      <- factor(osiics$problem_type,      levels = problem_type_order)
  osiics$osiics_16_level_1 <- factor(osiics$osiics_16_level_1, levels = level_1_order)
  osiics$osiics_16_level_2 <- factor(osiics$osiics_16_level_2, levels = level_2_order)
  osiics$osiics_16_level_3 <- factor(osiics$osiics_16_level_3, levels = level_3_order)
  
  dplyr::arrange(
    osiics,
    problem_type,
    osiics_16_level_1,
    osiics_16_level_2,
    osiics_16_level_3
  )
}


## Body area recoding ----

# Reassign the body area for particular OSIICS codes.
#
# OSIICS 16 places some diagnoses in a combined "Hip/groin" body area, while the
# football-specific extension of the IOC consensus statement (Waldén et al.,
# 2023) recommends reporting hip and groin separately. A competition can
# therefore assign individual codes by hand, in config.R, based on the clinical
# information available for each case.
#
# This changes the analysis for that competition only. It does not alter the
# OSIICS reference table.
#
# `recode` is a data frame with columns osiics_16_code and osiics_16_level_1.
apply_body_area_recode <- function(caselist, recode) {
  
  if (is.null(recode) || nrow(recode) == 0) {
    return(caselist)
  }
  
  unknown <- setdiff(recode$osiics_16_level_1, level_1_order)
  
  if (length(unknown) > 0) {
    stop("body_area_recode assigns body areas that are not in level_1_order: ",
         paste(unknown, collapse = ", "))
  }
  
  matched <- match(caselist$osiics_16_code, recode$osiics_16_code)
  rows    <- which(!is.na(matched))
  
  if (length(rows) == 0) {
    warning("None of the codes in body_area_recode appear in this caselist.",
            call. = FALSE)
    return(caselist)
  }
  
  caselist$osiics_16_level_1[rows] <- recode$osiics_16_level_1[matched[rows]]
  
  attr(caselist, "n_recoded") <- length(rows)
  
  caselist
}


## Validation ----

# Warn when a column contains labels the ordering does not cover. Those labels
# become NA when the column is turned into a factor, so cases carrying them
# would otherwise disappear from tables without comment.
check_labels <- function(values, ordering, label) {
  
  unmatched <- setdiff(unique(as.character(values)), c(ordering, NA))
  
  if (length(unmatched) > 0) {
    warning(
      "OSIICS ", label, " contains labels not in the ordering: ",
      paste(unmatched, collapse = " | "),
      call. = FALSE
    )
  }
  
  invisible(unmatched)
}