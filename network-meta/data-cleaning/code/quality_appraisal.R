## QUALITY APPRAISAL

## CLEAN AND PROCESS DATA FOR WRITEUP

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(stringr)
library(googlesheets4)

## READ LIST OF INCLUDED STUDIES 

included_studies_data_location <- "https://docs.google.com/spreadsheets/d/1YGbkWKGf9Afc5OtBKanbAP7HtJyC2-Tx9NeGychCwaY/edit#gid=0"

included_studies_data <- read_sheet(
  ss = included_studies_data_location,
  sheet = "included_studies")

## READ QA DATA

quality_appraisal_data_location <- "https://docs.google.com/spreadsheets/d/1aEnTOhqPC4pwo7teDS6TZe6gQ-euLLKm1WkQAP-5gOM/edit#gid=519540450"

quality_appraisal_raw <- read_sheet(
  ss = quality_appraisal_data_location,
  sheet = "Study Quality Appraisal")
  
## REMOVE EXCLUDED STUDIES, CLEAN UP AND EXPORT

quality_appraisal_clean <- quality_appraisal_raw %>%
  rename(
    study_reference = `Study Reference #`,
    qa_domain_1_study_design = 4,
    qa_domain_2_sample_size = 6,
    qa_domain_3_attrition = 8,
    qa_domain_4_intervention_description = 10,
    qa_domain_5_outcome_definition = 12,
    qa_domain_6_baseline_balance = 14,
    overall_rating = 16
    ) %>%
  select(
    study_reference,
    qa_domain_1_study_design,
    qa_domain_2_sample_size,
    qa_domain_3_attrition,
    qa_domain_4_intervention_description,
    qa_domain_5_outcome_definition,
    qa_domain_6_baseline_balance,
    overall_rating
    ) %>%
  mutate(
    qa_domain_3_attrition = case_when(
      qa_domain_3_attrition == "N/A" ~ NA_character_,
      qa_domain_3_attrition == "NA" ~ NA_character_,
      TRUE ~ qa_domain_3_attrition)
  ) %>%
  filter(
    study_reference %in% included_studies_data$study_reference
  )

saveRDS(
  quality_appraisal_clean,
  file = "./nma/context-data/quality_appraisal_information.RDS") 
