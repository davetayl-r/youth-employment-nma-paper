## PLOT HISTOGRAMS OF COMPONENTS IN EACH MODEL FOR EDUCATION COMPLETION

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(ggplot2)

## READ DATA

raw_data_location <- "./visualisation/input/education_completion_component_breakdown.RDS"
raw_data <- readRDS(raw_data_location)

## DROP THREE STUDIES THAT WERE SUBSEQUENTLY EXCLUDED

excluded_studies <- c(
  "73_YTD_Colorado_2014_USA",
  "49_Independent Living Employment Services Programme_2014_USA",
  "42B_Young_Adult_Internship_Program_2018_USA"
  )

clean_data <- raw_data %>%
  filter(
    !study %in% excluded_studies
  )

## CLEAN DATA FOR MODEL ONE: Intervention components + consolidated “other” component vs. all SAU

clean_data_model_one <- clean_data %>%
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
      "C&M",
      "OTH",
      "C-MGMT",
      "JOB-PLC",
      "P-WRKEXP",
      "U-WRKEXP",
      "SUP-EMP",
      "VOL",
      "COUNSL",
      "PEER",
      "ENTRE",
      "BEHAV",
      "WG-SUB",
      "TRANS",
      "S&R",
      "ACC",
      "RESI",
      "REF",
      "SAU"
    ),
    ordered = TRUE))

## CLEAN DATA FOR MODEL TWO: Intervention components + component “other” vs. all SAU

clean_data_model_two <- clean_data %>%
  select(
    study,                              
    intervention_basic_skills,                    
    intervention_life_skills,                     
    intervention_off_job_training,                
    intervention_on_the_job_training,              
    intervention_apprenticeships,                
    intervention_coaching_mentoring,               
    intervention_case_management,                  
    intervention_job_placement_assistance,        
    intervention_paid_work_experience,             
    intervention_unpaid_work_experience,        
    intervention_supported_paid_employment,        
    intervention_volunteer_work_unpaid,          
    intervention_counseling,                       
    intervention_group_peer_support,             
    intervention_entrepreneurship_business_skills,
    intervention_behavioural_psychological,       
    intervention_wage_subsidies,
    intervention_transportation_to_work,
    intervention_sports_club_activity,
    intervention_program_access,                  
    intervention_residential,
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
      "C&M",
      "OTH",
      "C-MGMT",
      "JOB-PLC",
      "P-WRKEXP",
      "U-WRKEXP",
      "SUP-EMP",
      "VOL",
      "COUNSL",
      "PEER",
      "ENTRE",
      "BEHAV",
      "WG-SUB",
      "TRANS",
      "S&R",
      "ACC",
      "RESI",
      "REF",
      "SAU"
      ),
    ordered = TRUE))

## CLEAN DATA FOR MODEL THREE: Intervention components + consolidated “other” vs. SAU components + consolidated “other”

clean_data_model_three <- clean_data %>%
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
      "C&M",
      "OTH",
      "C-MGMT",
      "JOB-PLC",
      "P-WRKEXP",
      "U-WRKEXP",
      "SUP-EMP",
      "VOL",
      "COUNSL",
      "PEER",
      "ENTRE",
      "BEHAV",
      "WG-SUB",
      "TRANS",
      "S&R",
      "ACC",
      "RESI",
      "REF",
      "SAU"
      ),
    ordered = TRUE))

## CLEAN DATA FOR MODEL FOUR: Intervention components + component “other” vs SAU components + component “other”

clean_data_model_four <- clean_data %>%
  select(
    study,                              
    intervention_basic_skills,                    
    intervention_life_skills,                     
    intervention_off_job_training,                
    intervention_on_the_job_training,              
    intervention_apprenticeships,                
    intervention_coaching_mentoring,               
    intervention_case_management,                  
    intervention_job_placement_assistance,        
    intervention_paid_work_experience,             
    intervention_unpaid_work_experience,        
    intervention_supported_paid_employment,        
    intervention_volunteer_work_unpaid,          
    intervention_counseling,                       
    intervention_group_peer_support,             
    intervention_entrepreneurship_business_skills,
    intervention_behavioural_psychological,       
    intervention_wage_subsidies,
    intervention_transportation_to_work,
    intervention_sports_club_activity,
    intervention_program_access,                  
    intervention_residential,
    intervention_referrals_brokerage,                               
    comparison_services_as_usual_only,            
    comparison_basic_skills,                       
    comparison_life_skills,                       
    comparison_off_job_training,                   
    comparison_on_the_job_training,               
    comparison_apprenticeships,                   
    comparison_coaching_mentoring,                                      
    comparison_case_management,               
    comparison_job_placement_assistance,           
    comparison_paid_work_experience,             
    comparison_unpaid_work_experience,             
    comparison_supported_paid_employment,         
    comparison_volunteer_work_unpaid,              
    comparison_counseling,                        
    comparison_group_peer_support,                 
    comparison_entrepreneurship_business_skills,  
    comparison_behavioural_psychological,          
    comparison_wage_subsidies,               
    comparison_transportation_to_work,             
    comparison_sports_club_activity,              
    comparison_program_access,                     
    comparison_residential,                       
    comparison_referrals_brokerage) %>%
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
      "C&M",
      "OTH",
      "C-MGMT",
      "JOB-PLC",
      "P-WRKEXP",
      "U-WRKEXP", 
      "SUP-EMP",
      "VOL",
      "COUNSL",
      "PEER",
      "ENTRE",
      "BEHAV",
      "WG-SUB",
      "TRANS",
      "S&R",
      "ACC",
      "RESI",
      "REF",
      "SAU"
      ),
    ordered = TRUE))

clean_data_model_four %>%
  ungroup() %>%
  filter(group == "Comparison") %>%
  select(value)

## EXPORT PLOT DATA

clean_data_model_one %>%
  saveRDS("./visualisation/input/education_completion_model_one_components_plot_data.RDS")

clean_data_model_two %>%
  saveRDS("./visualisation/input/education_completion_model_two_components_plot_data.RDS")

clean_data_model_three %>%
  saveRDS("./visualisation/input/education_completion_model_three_components_plot_data.RDS")

clean_data_model_four %>%
  saveRDS("./visualisation/input/education_completion_model_four_components_plot_data.RDS")
