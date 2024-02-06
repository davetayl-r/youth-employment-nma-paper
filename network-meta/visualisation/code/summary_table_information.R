## SUMMARY FIGURES FOR REPORT

## LOAD REQUIRED PACKAGES

library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(scales)

## READ DATA

data_location <- "https://docs.google.com/spreadsheets/d/1YGbkWKGf9Afc5OtBKanbAP7HtJyC2-Tx9NeGychCwaY/edit#gid=0"

study_data <- read_sheet(
  ss = data_location,
  sheet = "included_studies")

component_data_location <- "https://docs.google.com/spreadsheets/d/1IlYzDoBuMwCw_AkRILwYB8gYLZWVDGLumYFMmrl7tOk/edit#gid=698569653"

component_data <- read_sheet(
  ss = component_data_location,
  sheet = "component_combinations")

## COMPONENT BREAKDOWN

total <- 58

## APPRENTICESHIPS

apprenticeships_count <- component_data %>%
  select(
    ends_with("apprenticeships")
  ) %>%
  pivot_longer(
    ends_with("apprenticeships")
  ) %>%
  select(
    -name
  ) %>%
  mutate(value = case_when(
    value == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(value == 1) %>%
  tally()

apprenticeships_count
round((apprenticeships_count/total)*100, 1)

## BASIC SKILLS

basic_skills_count <- component_data %>%
  select(
    ends_with("basic_skills")
    ) %>%
  pivot_longer(
    ends_with("basic_skills")
    ) %>%
  select(
    -name
  ) %>%
  mutate(value = case_when(
    value == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(value == 1) %>%
  tally()

basic_skills_count
round((basic_skills_count/total)*100, 1)

## COACHING & MENTORING

coaching_mentoring_count <- component_data %>%
  select(
    ends_with("coaching_mentoring")
  ) %>%
  pivot_longer(
    ends_with("coaching_mentoring")
  ) %>%
  select(
    -name
  ) %>%
  mutate(value = case_when(
    value == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(value == 1) %>%
  tally()

coaching_mentoring_count
round((coaching_mentoring_count/total)*100, 1)

## LIFE SKILLS

life_skills_count <- component_data %>%
  select(
    ends_with("life_skills")
  ) %>%
  pivot_longer(
    ends_with("life_skills")
  ) %>%
  select(
    -name
  ) %>%
  mutate(value = case_when(
    value == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(value == 1) %>%
  tally()

life_skills_count
round((life_skills_count/total)*100, 1)

## ON-THE-JOB TRAINING

on_the_job_training_count <- component_data %>%
  select(
    ends_with("on_the_job_training")
  ) %>%
  pivot_longer(
    ends_with("on_the_job_training")
  ) %>%
  select(
    -name
  ) %>%
  mutate(value = case_when(
    value == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(value == 1) %>%
  tally()

on_the_job_training_count
round((on_the_job_training_count/total)*100, 1)

## OFF-THE-JOB TRAINING

off_job_training_count <- component_data %>%
  select(
    ends_with("off_job_training")
  ) %>%
  pivot_longer(
    ends_with("off_job_training")
  ) %>%
  select(
    -name
  ) %>%
  mutate(value = case_when(
    value == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(value == 1) %>%
  tally()

off_job_training_count
round((off_job_training_count/total)*100, 1)

## OTHER

other_count <- component_data %>%
  select(
    intervention_other,
    comparison_other) %>%
  pivot_longer(
    ends_with("other")
  ) %>%
  select(
    -name
  ) %>%
  mutate(value = case_when(
    value == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(value == 1) %>%
  tally()

other_count
round((other_count/total)*100, 1)

## STUDY DESIGN

study_data %>%
  select(study_design) %>%
  mutate(study_design = case_when(
    study_design == "RCT" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(study_design == 1) %>%
  tally()

study_data %>%
  select(study_design) %>%
  mutate(study_design = case_when(
    study_design == "QED" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(study_design == 1) %>%
  tally()

## PUBLICATION SOURCE

study_data %>%
  select(peer_reviewed_publication) %>%
  mutate(peer_reviewed_publication = case_when(
    peer_reviewed_publication == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(peer_reviewed_publication == 1) %>%
  tally()

study_data %>%
  select(peer_reviewed_publication) %>%
  mutate(peer_reviewed_publication = case_when(
    peer_reviewed_publication == "No" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(peer_reviewed_publication == 1) %>%
  tally()

## YEAR OF PUBLICATION

study_data %>%
  select(year_of_publication) %>%
  mutate(
    `1990-1994` = case_when(year_of_publication %in% c(1990:1994) ~ 1, TRUE ~ 0),
    `1995-1999` = case_when(year_of_publication %in% c(1995:1999) ~ 1, TRUE ~ 0),
    `2000-2004` = case_when(year_of_publication %in% c(2000:2004) ~ 1, TRUE ~ 0),
    `2005-2009` = case_when(year_of_publication %in% c(2005:2009) ~ 1, TRUE ~ 0),
    `2010-2014` = case_when(year_of_publication %in% c(2010:2014) ~ 1, TRUE ~ 0),
    `2015-2019` = case_when(year_of_publication %in% c(2015:2019) ~ 1, TRUE ~ 0),
    `2020-current` = case_when(year_of_publication %in% c(2020:2023) ~ 1, TRUE ~ 0)
    ) %>%
  select(-year_of_publication) %>%
  summarise(
    `1990-1994` = sum(`1990-1994`), 
    `1995-1999` = sum(`1995-1999`),
    `2000-2004` = sum(`2000-2004`),
    `2005-2009` = sum(`2005-2009`),
    `2010-2014` = sum(`2010-2014`),
    `2015-2019` = sum(`2015-2019`),
    `2020-current` = sum(`2020-current`)
  )
  
## POPULATION

additional_barriers <- study_data %>%
  select(
    population_with_disabilities,
    high_risk_population) %>%
  mutate(additional_barriers = case_when(
    population_with_disabilities == "Yes" ~ 1,
    high_risk_population == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  select(additional_barriers) %>%
  summarise(
    additional_barriers = sum(additional_barriers))

additional_barriers
round((additional_barriers/total*100), 1)

total - additional_barriers
round(((total-additional_barriers)/total*100), 1)

## CROSS TAB X COUNTRY

cross_tab_data <- left_join(
  study_data,
  component_data) %>%
  select(
    location,
    intervention_basic_skills, 
    intervention_life_skills, 
    intervention_off_job_training, 
    intervention_on_the_job_training, 
    intervention_apprenticeships,
    intervention_coaching_mentoring,
    intervention_other
  ) %>%
  rename(
    basic_skills = intervention_basic_skills, 
    life_skills = intervention_life_skills, 
    off_job_training = intervention_off_job_training, 
    on_the_job_training = intervention_on_the_job_training, 
    apprenticeships = intervention_apprenticeships,
    coaching_mentoring = intervention_coaching_mentoring,
    other = intervention_other
  ) %>%
  mutate(location_group = case_when(
    location == "United States" ~ "United States",
    TRUE ~ "Other"
  )) %>%
  mutate(
    count = 1,
    total = sum(count)
  ) %>%
  select(
    -count,
    -location
  ) %>%
  pivot_longer(
    -c(location_group,
       total)
  ) %>%
  filter(
    value == "Yes"
  ) %>%
  mutate(
    n = 1
  ) %>%
  select(
    -value
  )
    
xtabs(n ~ name + location_group, data = cross_tab_data)

