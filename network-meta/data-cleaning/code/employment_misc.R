## OUTCOME: EDUCATION COMPLETION — HIGH SCHOOL OR EQUIVALENT
## CLEAN AND PROCESS DATA FOR WRITEUP

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(googlesheets4)

## READ DATA

key_decisions_data_location <- "https://docs.google.com/spreadsheets/d/1wdBGAF1I1H0LegEoEIIuo2I5pktl6UbDF5Qd37PxLsc/edit#gid=364556614"

key_decisions_data <- read_sheet(
  ss = key_decisions_data_location,
  sheet = "key_decisions_employment")

employment_studies_data_location <- "https://docs.google.com/spreadsheets/d/1wdBGAF1I1H0LegEoEIIuo2I5pktl6UbDF5Qd37PxLsc/edit#gid=364556614"

employment_studies_data <- read_sheet(
  ss = employment_studies_data_location,
  sheet = "esc_input_employment")

included_studies_data_location <- "https://docs.google.com/spreadsheets/d/1YGbkWKGf9Afc5OtBKanbAP7HtJyC2-Tx9NeGychCwaY/edit#gid=0"

included_studies_data <- read_sheet(
  ss = included_studies_data_location,
  sheet = "included_studies")

final_list_included_studies_data_location <- "./nma/context-data/employment_include_studies_list_final.RDS"
final_list_included_studies <- readRDS(final_list_included_studies_data_location)

## CLEAN & EXPORT DATA: KEY DECISIONS

clean_key_decisions_data <- key_decisions_data %>%
  filter(`Study Ref` %in% final_list_included_studies$study) %>%
  select(
    `Primary Reference`,
    `Identified Issue`,
    Action) %>%
  mutate(
    `Primary Reference` = str_c("@", `Primary Reference`, sep = "")) %>%
  saveRDS("./nma/context-data/employment_key_decisions.RDS")  
  
## CLEAN & EXPORT DATA: INCLUDED STUDIES

clean_included_studies_data <- included_studies_data %>%
  select(
    study_reference,
    study_name,
    study_design,
    location,
    primary_reference,
    population_with_disabilities,
    high_risk_population) %>%
  filter(study_reference %in% final_list_included_studies$study) %>%
  mutate(
    primary_reference = str_c("@", primary_reference, sep = "")) %>%
  filter(
    study_reference %in% employment_studies_data$`Study Reference`) %>%
  mutate(
    complex_population = case_when(
      high_risk_population == "Yes" ~ "Yes",
      population_with_disabilities == "Yes" ~ "Yes",
      TRUE ~ "No")
  ) %>%
  select(
    -high_risk_population,
    -population_with_disabilities
  ) %>%
  rename(
    `Study reference` = study_reference,
    `Study name` = study_name, 
    `Study design` = study_design,
    Location = location,
    `Primary reference` = primary_reference
  ) %>%
  select(
    `Study reference`,
    `Study name`,
    `Primary reference`,
    `Study design`,
    Location,
    complex_population
  ) %>%
  saveRDS("./nma/context-data/employment_included_studies.RDS")
