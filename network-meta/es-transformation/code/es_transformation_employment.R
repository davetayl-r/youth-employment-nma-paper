## OUTCOME: EMPLOYMENT
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(googlesheets4)
library(tidyverse)
library(esc)
library(meta)

## READ DATA

google_sheet_location <- "https://docs.google.com/spreadsheets/d/1wdBGAF1I1H0LegEoEIIuo2I5pktl6UbDF5Qd37PxLsc/edit#gid=364556614"

raw_employment_completion_data <- read_sheet(
  ss = google_sheet_location,
  sheet = "esc_input_employment")

quality_appraisal_location <- "./nma/context-data/quality_appraisal_information.RDS"
quality_appraisal_information <- readRDS(quality_appraisal_location)

included_studies_location <- "./nma/context-data/employment_included_studies.RDS"
included_studies_information <- readRDS(included_studies_location)

## CLEAN ES DATA

clean_employment_completion_data <- raw_employment_completion_data %>%
  select(
    "Study Reference",
    esc_type,                                              
    prop1event,
    grp1n,
    prop2event,
    grp2n,
    or,
    se,
    totaln,
    beta,                                                  
    sdy,
    chisq,
    t,
    f,
    te,
    "Reported TE 95 per cent CI (upper)",	
    "Reported TE 95 per cent CI (lower)") %>%
  rename(
    study_ref = "Study Reference",
    te_ci_low = "Reported TE 95 per cent CI (lower)",
    te_ci_high = "Reported TE 95 per cent CI (upper)",
    ) 

## SUBSET STUDY LOCATION AND QUALITY APPRAISAL DATA

quality_appraisal_employment_subset <- quality_appraisal_information %>%
  rename(
    study_ref = study_reference
  ) %>%
  dplyr::filter(study_ref %in% clean_employment_completion_data$study_ref) 

study_location_design_employment_subset <- included_studies_information %>%
  select(
    -`Primary reference`
  ) %>%
  rename(
    study_ref = `Study reference`,
    study_design = `Study design`,
    study_location = Location) %>%
  mutate(study_design = case_when(
    study_design == "RCT" ~ "Randomised",
    study_design == "QED" ~ "Non-randomised"
  )) %>%
  dplyr::filter(study_ref %in% clean_employment_completion_data$study_ref)
  
## MERGE WITH EMPLOYMENT ES DATA

subset_employment_completion_data <- clean_employment_completion_data %>%
  left_join(quality_appraisal_employment_subset, by = "study_ref") %>%
  left_join(study_location_design_employment_subset, by = "study_ref") 
  
## CONVERT TO COMMON EFFECT SIZE

employment_binary_proportions_data <- subset_employment_completion_data %>%
  filter(esc_type == "binary_proportion") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_bin_prop",                                              
    prop1event = prop1event,
    grp1n = grp1n,
    prop2event = prop2event,
    grp2n = grp2n,
    es.type = "g")

employment_unstandard_regression_data <- subset_employment_completion_data %>%
  filter(esc_type == "unstandard_reg_coef") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_B",                                              
    b = beta,
    sdy = sdy,
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g")

employment_standard_regression_data <- subset_employment_completion_data %>%
  filter(esc_type == "standard_reg_coef") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_beta",                                              
    beta = beta,
    sdy = sdy,
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g")

employment_odds_ratio_data <- subset_employment_completion_data %>%
  filter(esc_type == "or") %>%
  convert_or2d(
    study = .$study_ref,
    or = .$or,
    se = .$se,
    totaln = .$totaln,
    es.type = "g") %>%
  as.data.frame()

employment_t_stat_data <- subset_employment_completion_data %>%
  filter(esc_type == "esc_t") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_t",                                              
    t = t,
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g")

employment_f_stat_data <- subset_employment_completion_data %>%
  filter(esc_type == "esc_f") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_f",                                              
    f = f,
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g")

employment_chi_square_data <- subset_employment_completion_data %>%
  filter(esc_type == "chi_square") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_chisq",                                              
    chisq = chisq,
    totaln = totaln,
    es.type = "g")

employment_mean_diff_data <- subset_employment_completion_data %>%
  filter(esc_type == "mean_diff") %>%
  mutate(
    es = te/sdy,
    weight = NA_real_,
    var = NA_real_,
    measure = "g",
    ci.lo = te_ci_low/sdy,
    ci.hi = te_ci_high/sdy,
    se = (te_ci_high - te_ci_low)/3.92
    ) %>%
  mutate(
    es = hedges_g(
      d = es,
      totaln = totaln)
  ) %>%
  rename(
    study = study_ref,
    sample.size = totaln
    ) %>%
  select(
    study,
    es,
    weight,
    sample.size,
    se,
    var,
    ci.lo,
    ci.hi,
    measure
  )

## MERGE DIFFERENT ES DATA

merged_employment_es_data <- bind_rows(
  employment_binary_proportions_data,
  employment_odds_ratio_data,
  employment_unstandard_regression_data,
  employment_standard_regression_data,
  employment_t_stat_data,
  employment_f_stat_data,
  employment_chi_square_data,
  employment_mean_diff_data)

## COMBINE RESULTS FROM STUDIES THAT REPORT SUBGROUPS SEPERATELY

preparatory_training_2011_germany_seperate <- employment_unstandard_regression_data %>%
  filter(study %in% c(
    "7A_Preparatory_Training_2011_Germany"))

preparatory_training_2011_germany_meta <- metagen(
  TE = es,
  seTE = se,
  data = preparatory_training_2011_germany_seperate,
  studlab = paste(study),
  prediction = FALSE,
  sm = "SMD")

preparatory_training_2011_germany_combined <- data.frame(
  study = preparatory_training_2011_germany_seperate$study[1],
  es = preparatory_training_2011_germany_meta$TE.random,
  weight = NA_real_,
  sample.size = preparatory_training_2011_germany_seperate$sample.size[1] + preparatory_training_2011_germany_seperate$sample.size[2],
  se = preparatory_training_2011_germany_meta$seTE.random,
  var = NA_real_,
  ci.lo = preparatory_training_2011_germany_meta$lower.random,
  ci.hi = preparatory_training_2011_germany_meta$upper.random,
  measure = "g")
  
short_term_training_2011_germany_seperate <- employment_unstandard_regression_data %>%
  filter(study %in% c(
    "7B_Short-term_Training_2011_Germany"))

short_term_training_2011_germany_meta <- metagen(
  TE = es,
  seTE = se,
  data = short_term_training_2011_germany_seperate,
  studlab = paste(study),
  prediction = FALSE,
  sm = "SMD")

short_term_training_2011_germany_combined <- data.frame(
  study = short_term_training_2011_germany_seperate$study[1],
  es = short_term_training_2011_germany_meta$TE.random,
  weight = NA_real_,
  sample.size = short_term_training_2011_germany_seperate$sample.size[1] + short_term_training_2011_germany_seperate$sample.size[2],
  se = short_term_training_2011_germany_meta$seTE.random,
  var = NA_real_,
  ci.lo = short_term_training_2011_germany_meta$lower.random,
  ci.hi = short_term_training_2011_germany_meta$upper.random,
  measure = "g")

job_search_2011_germany_seperate <- employment_unstandard_regression_data %>%
  filter(study %in% c(
    "7C_Job_Search_2011_Germany"))

job_search_2011_germany_meta <- metagen(
  TE = es,
  seTE = se,
  data = job_search_2011_germany_seperate,
  studlab = paste(study),
  prediction = FALSE,
  sm = "SMD")

job_search_2011_germany_combined <- data.frame(
  study = job_search_2011_germany_seperate$study[1],
  es = job_search_2011_germany_meta$TE.random,
  weight = NA_real_,
  sample.size = job_search_2011_germany_seperate$sample.size[1] + job_search_2011_germany_seperate$sample.size[2],
  se = job_search_2011_germany_meta$seTE.random,
  var = NA_real_,
  ci.lo = job_search_2011_germany_meta$lower.random,
  ci.hi = job_search_2011_germany_meta$upper.random,
  measure = "g")

job_creation_2011_germany_seperate <- employment_unstandard_regression_data %>%
  filter(study %in% c(
    "7D_Job_Creation_Schemes_2011_Germany"))

job_creation_2011_germany_meta <- metagen(
  TE = es,
  seTE = se,
  data = job_creation_2011_germany_seperate,
  studlab = paste(study),
  prediction = FALSE,
  sm = "SMD")

job_creation_2011_germany_combined <- data.frame(
  study = job_creation_2011_germany_seperate$study[1],
  es = job_creation_2011_germany_meta$TE.random,
  weight = NA_real_,
  sample.size = job_creation_2011_germany_seperate$sample.size[1] + job_creation_2011_germany_seperate$sample.size[2],
  se = job_creation_2011_germany_meta$seTE.random,
  var = NA_real_,
  ci.lo = job_creation_2011_germany_meta$lower.random,
  ci.hi = job_creation_2011_germany_meta$upper.random,
  measure = "g")

further_training_2011_germany_seperate <- employment_unstandard_regression_data %>%
  filter(study %in% c(
    "7E_Further_Training_Measures_2011_Germany"))

further_training_2011_germany_meta <- metagen(
  TE = es,
  seTE = se,
  data = further_training_2011_germany_seperate,
  studlab = paste(study),
  prediction = FALSE,
  sm = "SMD")

further_training_2011_germany_combined <- data.frame(
  study = further_training_2011_germany_seperate$study[1],
  es = further_training_2011_germany_meta$TE.random,
  weight = NA_real_,
  sample.size = further_training_2011_germany_seperate$sample.size[1] + further_training_2011_germany_seperate$sample.size[2],
  se = further_training_2011_germany_meta$seTE.random,
  var = NA_real_,
  ci.lo = further_training_2011_germany_meta$lower.random,
  ci.hi = further_training_2011_germany_meta$upper.random,
  measure = "g")

yei_2020_portugal_seperate <- employment_standard_regression_data %>%
  filter(study %in% c(
    "30_YEI_2020_Portugal"))

yei_2020_portugal_meta <- metagen(
  TE = es,
  seTE = se,
  data = yei_2020_portugal_seperate,
  studlab = paste(study),
  prediction = FALSE,
  sm = "SMD")

yei_2020_portugal_combined <- data.frame(
  study = yei_2020_portugal_seperate$study[1],
  es = yei_2020_portugal_meta$TE.random,
  weight = NA_real_,
  sample.size = yei_2020_portugal_seperate$sample.size[1] + yei_2020_portugal_seperate$sample.size[2],
  se = yei_2020_portugal_meta$seTE.random,
  var = NA_real_,
  ci.lo = yei_2020_portugal_meta$lower.random,
  ci.hi = yei_2020_portugal_meta$upper.random,
  measure = "g")

ndyp_2004_uk_seperate <- employment_standard_regression_data %>%
  filter(study %in% c(
    "59F_NDYP_2004_UK"))

ndyp_2004_uk_meta <- metagen(
  TE = es,
  seTE = se,
  data = ndyp_2004_uk_seperate,
  studlab = paste(study),
  prediction = FALSE,
  sm = "SMD")

ndyp_2004_uk_combined <- data.frame(
  study = ndyp_2004_uk_seperate$study[1],
  es = ndyp_2004_uk_meta$TE.random,
  weight = NA_real_,
  sample.size = ndyp_2004_uk_seperate$sample.size[1] + ndyp_2004_uk_seperate$sample.size[2],
  se = ndyp_2004_uk_meta$seTE.random,
  var = NA_real_,
  ci.lo = ndyp_2004_uk_meta$lower.random,
  ci.hi = ndyp_2004_uk_meta$upper.random,
  measure = "g")

## MERGE SEPERATE DATA FRAMES WITH COMBINED ES DATA

merged_combined_es_data <- bind_rows(
  preparatory_training_2011_germany_combined,
  short_term_training_2011_germany_combined,
  job_search_2011_germany_combined,
  job_creation_2011_germany_combined,
  further_training_2011_germany_combined,
  yei_2020_portugal_combined,
  ndyp_2004_uk_combined)

## REMOVE STUDIES FROM ES DATA THAT WERE REPORTED SEPERATELY, THEN ADD BACK THE COMBINED ONES

clean_employment_es_data <- merged_employment_es_data %>%
  dplyr::filter(!study %in% merged_combined_es_data$study) %>%
  bind_rows(merged_combined_es_data)

## CHECKS:
## 1. Count of original es: 69
## 2. Individual records requiring synthesis: 14
## 3. Count of original es, less records requiring synthesis (1-3): 55
## 4. Count of merged es records: 7
## 5. Unique records (3+4): 62

## MERGE WITH CLEAN DATA TO GET INFORMATION FOR SUBGROUP ANALYSIS

subgroup_info_employment_es_data <- subset_employment_completion_data %>%
  select(
    study_ref,
    study_design,
    study_location,
    complex_population,
    overall_rating
  ) %>%
  rename(
    study = study_ref,
    study_quality = overall_rating
  ) %>%
  distinct()

export_employment_es_data <- subgroup_info_employment_es_data %>%
  left_join(clean_employment_es_data, by = "study") %>%
  filter(study %in% included_studies_information$`Study reference`)

## EXPORT DATA FOR NMA

export_employment_es_data %>%
  saveRDS(
    "./es-transformation/output/employment_es_data.RDS")