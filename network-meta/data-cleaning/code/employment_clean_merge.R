## OUTCOME: EMPLOYMENT
## MERGE AND CLEAN DATA FOR NMA FOR SAU HETEROGENEITY SPECIFICATION

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(googlesheets4)

## READ DATA

component_data_location <- "https://docs.google.com/spreadsheets/d/1IlYzDoBuMwCw_AkRILwYB8gYLZWVDGLumYFMmrl7tOk/edit#gid=698569653"

component_data <- read_sheet(
  ss = component_data_location,
  sheet = "component_combinations")

employment_es_data_location <- "./network-meta/es-transformation/output/employment_es_data.RDS"

employment_es_data <- readRDS(employment_es_data_location)

## GET LIST OF STUDY REFS FROM ES DATA

included_studies_employment <- employment_es_data$study

## FILTER COMPONENTS FOR INCLUDED STUDIES REPORTING EMPLOYMENT OUTCOMES

employment_component_data <- component_data %>%
  filter(study_reference %in% included_studies_employment) 

## RENAME COMPONENT DATA

named_employment_component_data <- employment_component_data %>%
  rename(study = study_reference) %>%
  mutate(intervention_basic_skills = case_when(
    intervention_basic_skills == "Yes" ~ "BS")) %>%
  mutate(intervention_life_skills = case_when(
    intervention_life_skills == "Yes" ~ "LS")) %>%
  mutate(intervention_off_job_training = case_when(
    intervention_off_job_training == "Yes" ~ "OFF-JT")) %>%
  mutate(intervention_on_the_job_training = case_when(
    intervention_on_the_job_training == "Yes" ~ "ON-JT")) %>%
  mutate(intervention_apprenticeships = case_when(
    intervention_apprenticeships == "Yes" ~ "APP")) %>%
  mutate(intervention_coaching_mentoring = case_when(
    intervention_coaching_mentoring == "Yes" ~ "C&M")) %>%
  mutate(intervention_other = case_when(
    intervention_other == "Yes" ~ "OTH")) %>%
  mutate(intervention_case_management = case_when(
    intervention_case_management == "Yes" ~ "C-MGMT")) %>%
  mutate(intervention_job_placement_assistance = case_when(
    intervention_job_placement_assistance == "Yes" ~ "JOB-PLC")) %>%
  mutate(intervention_paid_work_experience = case_when(
    intervention_paid_work_experience == "Yes" ~ "P-WRKEXP")) %>%
  mutate(intervention_unpaid_work_experience = case_when(
    intervention_unpaid_work_experience == "Yes" ~ "U-WRKEXP")) %>%
  mutate(intervention_supported_paid_employment = case_when(
    intervention_supported_paid_employment == "Yes" ~ "SUP-EMP")) %>%
  mutate(intervention_volunteer_work_unpaid = case_when(
    intervention_volunteer_work_unpaid == "Yes" ~ "VOL")) %>%
  mutate(intervention_counseling = case_when(
    intervention_counseling == "Yes" ~ "COUNSL")) %>%
  mutate(intervention_group_peer_support = case_when(
    intervention_group_peer_support == "Yes" ~ "PEER")) %>%
  mutate(intervention_entrepreneurship_business_skills = case_when(
    intervention_entrepreneurship_business_skills == "Yes" ~ "ENTRE")) %>%
  mutate(intervention_behavioural_psychological = case_when(
    intervention_behavioural_psychological == "Yes" ~ "BEHAV")) %>%
  mutate(intervention_wage_subsidies = case_when(
    intervention_wage_subsidies == "Yes" ~ "WG-SUB")) %>%
  mutate(intervention_transportation_to_work = case_when(
    intervention_transportation_to_work == "Yes" ~ "TRANS")) %>%
  mutate(intervention_sports_club_activity = case_when(
    intervention_sports_club_activity == "Yes" ~ "S&R")) %>%
  mutate(intervention_program_access = case_when(
    intervention_program_access == "Yes" ~ "ACC")) %>%
  mutate(intervention_residential = case_when(
    intervention_residential == "Yes" ~ "RESI")) %>%
  mutate(intervention_referrals_brokerage = case_when(
    intervention_referrals_brokerage == "Yes" ~ "REF")) %>%
  mutate(intervention_remediation = case_when(
    intervention_remediation == "Yes" ~ "REM-ED")) %>%
  mutate(intervention_accreditation = case_when(
    intervention_accreditation == "Yes" ~ "ACCRED")) %>%
  mutate(comparison = case_when(
    comparison == "Services as Usual" ~ "SAU")) %>%
  mutate(comparison_services_as_usual_only = case_when(
    comparison_services_as_usual_only == "Yes" ~ "SAU")) %>%
  mutate(comparison_basic_skills = case_when(
    comparison_basic_skills == "Yes" ~ "BS")) %>%
  mutate(comparison_life_skills = case_when(
    comparison_life_skills == "Yes" ~ "LS")) %>%
  mutate(comparison_off_job_training = case_when(
    comparison_off_job_training == "Yes" ~ "OFF-JT")) %>%
  mutate(comparison_on_the_job_training = case_when(
    comparison_on_the_job_training == "Yes" ~ "ON-JT")) %>%
  mutate(comparison_apprenticeships = case_when(
    comparison_apprenticeships == "Yes" ~ "APP")) %>%
  mutate(comparison_coaching_mentoring = case_when(
    comparison_coaching_mentoring == "Yes" ~ "C&M")) %>%
  mutate(comparison_other = case_when(
    comparison_other == "Yes" ~ "OTH")) %>%
  mutate(comparison_case_management = case_when(
    comparison_case_management == "Yes" ~ "C-MGMT")) %>%
  mutate(comparison_job_placement_assistance = case_when(
    comparison_job_placement_assistance == "Yes" ~ "JOB-PLC")) %>%
  mutate(comparison_paid_work_experience = case_when(
    comparison_paid_work_experience == "Yes" ~ "P-WRKEXP")) %>%
  mutate(comparison_unpaid_work_experience = case_when(
    comparison_unpaid_work_experience == "Yes" ~ "U-WRKEXP")) %>%
  mutate(comparison_supported_paid_employment = case_when(
    comparison_supported_paid_employment == "Yes" ~ "SUP-EMP")) %>%
  mutate(comparison_volunteer_work_unpaid = case_when(
    comparison_volunteer_work_unpaid == "Yes" ~ "VOL")) %>%
  mutate(comparison_counseling = case_when(
    comparison_counseling == "Yes" ~ "COUNSL")) %>%
  mutate(comparison_group_peer_support = case_when(
    comparison_group_peer_support == "Yes" ~ "PEER")) %>%
  mutate(comparison_entrepreneurship_business_skills = case_when(
    comparison_entrepreneurship_business_skills == "Yes" ~ "ENTRE")) %>%
  mutate(comparison_behavioural_psychological = case_when(
    comparison_behavioural_psychological == "Yes" ~ "BEHAV")) %>%
  mutate(comparison_wage_subsidies = case_when(
    comparison_wage_subsidies == "Yes" ~ "WG-SUB")) %>%
  mutate(comparison_transportation_to_work = case_when(
    comparison_transportation_to_work == "Yes" ~ "TRANS")) %>%
  mutate(comparison_sports_club_activity = case_when(
    comparison_sports_club_activity == "Yes" ~ "S&R")) %>%
  mutate(comparison_program_access = case_when(
    comparison_program_access == "Yes" ~ "ACC")) %>%
  mutate(comparison_residential = case_when(
    comparison_residential == "Yes" ~ "RESI")) %>%
  mutate(comparison_referrals_brokerage = case_when(
    comparison_referrals_brokerage == "Yes" ~ "REF")) %>%
  mutate(comparison_remediation = case_when(
    comparison_remediation == "Yes" ~ "REM-ED")) %>%
  mutate(comparison_accreditation = case_when(
    comparison_accreditation == "Yes" ~ "ACCRED"))

# export component data for plotting
subset_employment_component_data %>%
  saveRDS("./network-meta/visualisation/input/employment_component_breakdown.RDS")

# prepare specification for analysis
employment_treatment_comparison_core_components <- named_employment_component_data %>%
  select(
    study,                              
    intervention_basic_skills,                    
    intervention_life_skills,                     
    intervention_off_job_training,                
    intervention_on_the_job_training,              
    intervention_apprenticeships,                
    intervention_coaching_mentoring,               
    intervention_other,                               
    comparison_services_as_usual_only,            
    comparison_basic_skills,
    comparison_life_skills,                       
    comparison_off_job_training,
    comparison_on_the_job_training,               
    comparison_apprenticeships,
    comparison_coaching_mentoring,                
    comparison_other) %>%
  unite(intervention,
        starts_with("intervention"), 
        sep = "+", 
        na.rm = TRUE, 
        remove = TRUE) %>%
  unite(comparison,
        starts_with("comparison"), 
        sep = "+", 
        na.rm = TRUE, 
        remove = TRUE) %>%
  ## Remove studies with no active components in treatment group
  filter(
    !intervention == "OTH"
  )

# export data
employment_included_studies <- employment_treatment_comparison_core_components %>%
  select(study) %>%
  saveRDS("./network-meta/nma/context-data/employment_include_studies_list_final.RDS")

employment_treatment_comparison_core_components %>%
  left_join(
    employment_es_data,
    by = "study") %>%
  saveRDS("./network-meta/nma/meta-data/employment_analysis_data.RDS")