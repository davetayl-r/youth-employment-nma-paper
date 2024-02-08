## Effect size transformation
## OUTCOME: EDUCATION COMPLETION — HIGH SCHOOL OR EQUIVALENT


## LOAD REQUIRED PACKAGES

library(googlesheets4)
library(tidyverse)
library(esc)

## READ DATA

google_sheet_location <- "https://docs.google.com/spreadsheets/d/1m5r4IvU-TruXkBH2w2Z88xQYHRNGFSYuMWppr19f7U8/edit#gid=42105740"

raw_education_completion_data <- read_sheet(
  ss = google_sheet_location,
  sheet = "esc_input_hs_grad")

quality_appraisal_location <- "./network-meta/nma/context-data/quality_appraisal_information.RDS"

quality_appraisal_information <- readRDS(quality_appraisal_location)

included_studies_location <- "./network-meta/nma/context-data/education_included_studies.RDS"

included_studies_information <- readRDS(included_studies_location)

## CLEAN ES DATA

clean_education_completion_data <- raw_education_completion_data %>%
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
    #chisq,
    #t,
    #f,
    #te,
    "Reported TE 95 per cent CI (upper)",	
    "Reported TE 95 per cent CI (lower)") %>%
  rename(
    study_ref = "Study Reference",
    te_ci_low = "Reported TE 95 per cent CI (lower)",
    te_ci_high = "Reported TE 95 per cent CI (upper)"
    ) %>%
  filter(
    study_ref %in% included_studies_information$`Study reference`)

## SUBSET STUDY LOCATION AND QUALITY APPRAISAL DATA

quality_appraisal_education_subset <- quality_appraisal_information %>%
  rename(
    study_ref = study_reference
  ) %>%
  dplyr::filter(study_ref %in% clean_education_completion_data$study_ref) 

study_location_design_education_subset <- included_studies_information %>%
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
  dplyr::filter(study_ref %in% clean_education_completion_data$study_ref)

## MERGE WITH EDUCATION ES DATA

subset_education_completion_data <- clean_education_completion_data %>%
  left_join(quality_appraisal_education_subset, by = "study_ref") %>%
  left_join(study_location_design_education_subset, by = "study_ref") 

## CONVERT TO COMMON EFFECT SIZE

education_binary_proportions_data <- subset_education_completion_data %>%
  filter(esc_type == "binary_proportion") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_bin_prop",                                              
    prop1event = prop1event,
    grp1n = grp1n,
    prop2event = prop2event,
    grp2n = grp2n,
    es.type = "g")

education_regression_data <- subset_education_completion_data %>%
  filter(esc_type == "standard_reg_coef") %>%
  effect_sizes(
    study = study_ref,
    fun = "esc_beta",                                              
    beta = beta,
    sdy = sdy,
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g")

education_odds_ratio_data <- subset_education_completion_data %>%
  filter(esc_type == "or") %>%
  convert_or2d(
    study = .$study_ref,
    or = .$or,
    se = .$se,
    totaln = .$totaln,
    es.type = "g") %>%
  as.data.frame()

## MERGE ES DATA

merged_education_es_data <- bind_rows(
  education_binary_proportions_data,
  education_regression_data,
  education_odds_ratio_data) 

## MERGE WITH CLEAN DATA TO GET INFORMATION FOR SUBGROUP ANALYSIS

subgroup_info_education_es_data <- subset_education_completion_data %>%
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

export_education_es_data <- subgroup_info_education_es_data %>%
  left_join(merged_education_es_data, by = "study") %>%
  saveRDS(
    "./es-transformation/output/education_es_data.RDS")

