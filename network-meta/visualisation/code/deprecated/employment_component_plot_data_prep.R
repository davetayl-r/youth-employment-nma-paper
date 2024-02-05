## PLOT HISTOGRAMS OF COMPONENTS IN EACH SPECIFICATION FOR EMPLOYMENT

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(ggplot2)

## READ DATA

employment_raw_data_location <- "./visualisation/input/employment_component_breakdown.RDS"
employment_raw_data <- readRDS(employment_raw_data_location)

final_list_included_studies_data_location <- "./nma/context-data/employment_include_studies_list_final.RDS"
final_list_included_studies <- readRDS(final_list_included_studies_data_location)

## FILTER STUIES FOR FINAL INCLUSION

employment_clean_data <- employment_raw_data %>%
  filter(study %in% final_list_included_studies$study)

## CLEAN DATA FOR SPECIFICATION ONE: Intervention components + consolidated “other” component vs. all SAU

clean_data_specification_one <- employment_clean_data %>%
  select(
    study,                              
    intervention_basic_skills,                    
    intervention_life_skills,                     
    intervention_off_job_training,                
    intervention_on_the_job_training,              
    intervention_apprenticeships,                
    intervention_coaching_mentoring,               
    intervention_other,                               
    comparison) %>%
  pivot_longer(
    cols = (-study),
    values_drop_na = TRUE) %>%
  mutate(group = case_when(
    str_detect(name, "intervention") ~ "Intervention",
    str_detect(name, "comparison") ~ "Comparison")) %>%
  group_by(group, value) %>%
  tally() %>%
  mutate(component_group = case_when(
    value == "BS" ~ "Component of interest",
    value == "LS" ~ "Component of interest",
    value == "OFF-JT" ~ "Component of interest",
    value == "ON-JT" ~ "Component of interest",
    value == "APP" ~ "Component of interest",
    value == "C&M" ~ "Component of interest",
    value == "SAU" ~ "Comparison",
    TRUE ~ "Other component")) %>%
  mutate(value = factor(value,
    levels = c(
      "BS",
      "LS",
      "OFF-JT",
      "ON-JT",
      "APP",
      "C&M",
      "OTH",
      "C-MGMT",
      "P-WRKEXP",
      "REF",
      "COUNSL",
      "ACC",
      "SAU"
    ),
    ordered = TRUE))

## CLEAN DATA FOR SPECIFICATION TWO: Intervention components + component “other” vs. all SAU

clean_data_specification_two <- employment_clean_data %>%
  select(
    study,                              
    intervention_basic_skills,                    
    intervention_life_skills,                     
    intervention_off_job_training,                
    intervention_on_the_job_training,              
    intervention_apprenticeships,                
    intervention_coaching_mentoring,               
    intervention_case_management,                  
    intervention_paid_work_experience,             
    intervention_counseling,                       
    intervention_program_access,                
    intervention_referrals_brokerage,                               
    comparison) %>%
  pivot_longer(
    cols = (-study),
    values_drop_na = TRUE) %>%
  mutate(group = case_when(
    str_detect(name, "intervention") ~ "Intervention",
    str_detect(name, "comparison") ~ "Comparison")) %>%
  group_by(group, value) %>%
  tally() %>%
  mutate(component_group = case_when(
    value == "BS" ~ "Component of interest",
    value == "LS" ~ "Component of interest",
    value == "OFF-JT" ~ "Component of interest",
    value == "ON-JT" ~ "Component of interest",
    value == "APP" ~ "Component of interest",
    value == "C&M" ~ "Component of interest",
    value == "SAU" ~ "Comparison",
    TRUE ~ "Other component")) %>%
  mutate(value = factor(value,
    levels = c(
      "BS",
      "LS",
      "OFF-JT",
      "ON-JT",
      "APP",
      "C&M",
      "OTH",
      "C-MGMT",
      "P-WRKEXP",
      "REF",
      "COUNSL",
      "ACC",
      "SAU"
      ),
    ordered = TRUE))

## CLEAN DATA FOR SPECIFICATION THREE: Intervention components + consolidated “other” vs. SAU components + consolidated “other”

clean_data_specification_three <- employment_clean_data %>%
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
  pivot_longer(
    cols = (-study),
    values_drop_na = TRUE) %>%
  mutate(group = case_when(
    str_detect(name, "intervention") ~ "Intervention",
    str_detect(name, "comparison") ~ "Comparison")) %>%
  group_by(group, value) %>%
  tally() %>%
  mutate(component_group = case_when(
    value == "BS" ~ "Component of interest",
    value == "LS" ~ "Component of interest",
    value == "OFF-JT" ~ "Component of interest",
    value == "ON-JT" ~ "Component of interest",
    value == "APP" ~ "Component of interest",
    value == "C&M" ~ "Component of interest",
    value == "SAU" ~ "Comparison",
    TRUE ~ "Other component")) %>%
  mutate(value = factor(value,
    levels = c(
      "BS",
      "LS",
      "OFF-JT",
      "ON-JT",
      "APP",
      "C&M",
      "OTH",
      "C-MGMT",
      "P-WRKEXP",
      "REF",
      "COUNSL",
      "ACC",
      "SAU"
      ),
    ordered = TRUE))

## CLEAN DATA FOR SPECIFICATION FOUR: Intervention components + component “other” vs SAU components + component “other”

clean_data_specification_four <- employment_clean_data %>%
  select(
    study,                              
    intervention_basic_skills,                    
    intervention_life_skills,                     
    intervention_off_job_training,                
    intervention_on_the_job_training,              
    intervention_apprenticeships,                
    intervention_coaching_mentoring,               
    intervention_case_management,                  
    intervention_paid_work_experience,             
    intervention_counseling,                       
    intervention_program_access,                  
    intervention_referrals_brokerage,                               
    comparison_services_as_usual_only,            
    comparison_basic_skills,                       
    comparison_life_skills,                       
    comparison_off_job_training,                   
    comparison_on_the_job_training,               
    comparison_apprenticeships,                   
    comparison_coaching_mentoring,                                      
    comparison_case_management,               
    comparison_paid_work_experience,             
    comparison_counseling,   
    comparison_referrals_brokerage,
    comparison_program_access) %>%
  pivot_longer(
    cols = (-study),
    values_drop_na = TRUE) %>%
  mutate(group = case_when(
    str_detect(name, "intervention") ~ "Intervention",
    str_detect(name, "comparison") ~ "Comparison")) %>%
  group_by(group, value) %>%
  tally() %>%
  mutate(component_group = case_when(
    value == "BS" ~ "Component of interest",
    value == "LS" ~ "Component of interest",
    value == "OFF-JT" ~ "Component of interest",
    value == "ON-JT" ~ "Component of interest",
    value == "APP" ~ "Component of interest",
    value == "C&M" ~ "Component of interest",
    value == "SAU" ~ "Comparison",
    TRUE ~ "Other component")) %>%
  mutate(value = factor(value,
    levels = c(
      "BS",
      "LS",
      "OFF-JT",
      "ON-JT",
      "APP",
      "C&M",
      "OTH",
      "C-MGMT",
      "P-WRKEXP",
      "REF",
      "COUNSL",
      "ACC",
      "SAU"
      ),
    ordered = TRUE))

## EXPORT PLOT DATA

clean_data_specification_one %>%
  saveRDS("./visualisation/input/employment_specification_one_components_plot_data.RDS")

clean_data_specification_two %>%
  saveRDS("./visualisation/input/employment_specification_two_components_plot_data.RDS")

clean_data_specification_three %>%
  saveRDS("./visualisation/input/employment_specification_three_components_plot_data.RDS")

clean_data_specification_four %>%
  saveRDS("./visualisation/input/employment_specification_four_components_plot_data.RDS")
