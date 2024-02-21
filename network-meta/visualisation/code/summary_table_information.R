## Produce summary table for paper

# load required packages
library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(scales)

# read data about included studies
included_studies_data_location <- "https://docs.google.com/spreadsheets/d/1YGbkWKGf9Afc5OtBKanbAP7HtJyC2-Tx9NeGychCwaY/edit#gid=0"

included_studies_data <- read_sheet(
  ss = included_studies_data_location,
  sheet = "included_studies")

# read data about components of included studies
component_data_location <- "https://docs.google.com/spreadsheets/d/1IlYzDoBuMwCw_AkRILwYB8gYLZWVDGLumYFMmrl7tOk/edit#gid=698569653"

component_data <- read_sheet(
  ss = component_data_location,
  sheet = "component_combinations")

# read data about employment outcome counts
employment_outcome_count_location <- "https://docs.google.com/spreadsheets/d/1wdBGAF1I1H0LegEoEIIuo2I5pktl6UbDF5Qd37PxLsc/edit#gid=364556614"

# employment es data
employment_outcome_data <- read_sheet(
  ss = employment_outcome_count_location,
  sheet = "esc_input_employment")

# hours worked es data
hours_worked_outcome_data <- read_sheet(
  ss = employment_outcome_count_location,
  sheet = "esc_input_hours_worked")

# wages es data
wages_outcome_data <- read_sheet(
  ss = employment_outcome_count_location,
  sheet = "esc_input_wages_earnings")

# read data about education outcome counts
education_outcome_count_location <- "https://docs.google.com/spreadsheets/d/1m5r4IvU-TruXkBH2w2Z88xQYHRNGFSYuMWppr19f7U8/edit#gid=42105740"

# hs grad es data
hs_grad_outcome_data <- read_sheet(
  ss = education_outcome_count_location,
  sheet = "esc_input_hs_grad")

# vet commencement es data
vet_commence_outcome_data <- read_sheet(
  ss = education_outcome_count_location,
  sheet = "esc_input_vet_commence")

# university commencement es data
uni_commence_outcome_data <- read_sheet(
  ss = education_outcome_count_location,
  sheet = "esc_input_uni_commence")  

# set total number of studies
total <- dim(included_studies_data)[1]

# apprenticeships
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
  tally() %>%
  pull(n) 

apprenticeships_per_cent <- round((apprenticeships_count/total)*100, 1)

# basic skills
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
  tally() %>%
  pull(n)

basic_skills_per_cent <- round((basic_skills_count/total)*100, 1)

# life skills
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
  tally() %>%
  pull(n)

life_skills_per_cent <- round((life_skills_count/total)*100, 1)

# coaching and mentoring
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
  tally() %>%
  pull(n)

coaching_mentoring_per_cent <- round((coaching_mentoring_count/total)*100, 1)

# on-the-job training
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
  tally() %>%
  pull(n)

on_the_job_training_per_cent <- round((on_the_job_training_count/total)*100, 1)

# off-the-job training
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
  tally() %>%
  pull(n)

off_job_training_per_cent <- round((off_job_training_count/total)*100, 1)

# other
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
  tally() %>%
  pull(n)

other_per_cent <- round((other_count/total)*100, 1)

# summarise component-level information
component_summary <- tibble(
  category = c(
    "Apprenticeships", 
    "Basic Skills", 
    "Life Skills", 
    "Coaching and Mentoring", 
    "On-the-job training", 
    "Off-the-job training", 
    "Other"
    ),
  n = c(
    apprenticeships_count, 
    basic_skills_count, 
    life_skills_count, 
    coaching_mentoring_count,
    on_the_job_training_count, 
    off_job_training_count, 
    other_count
    ),
  per_cent = c(
    apprenticeships_per_cent,
    basic_skills_per_cent,
    life_skills_per_cent,
    coaching_mentoring_per_cent,
    on_the_job_training_per_cent, 
    off_job_training_per_cent, 
    other_per_cent
    ) 
) 

# employment status
employment_status_count <- employment_outcome_data %>%
  filter(`Study Reference` %in% included_studies_data$study_reference) %>%
  distinct(`Study Reference`) %>%
  tally() %>%
  pull(n)

employment_status_per_cent <- round((employment_status_count/total)*100, 1)

# hours worked
hours_worked_count <- hours_worked_outcome_data %>%
  filter(`Study Reference` %in% included_studies_data$study_reference) %>%
  distinct(`Study Reference`) %>%
  tally() %>%
  pull(n)

hours_worked_per_cent <- round((hours_worked_count/total)*100, 1)

# wages or earnings
wages_count <- wages_outcome_data %>%
  filter(`Study Reference` %in% included_studies_data$study_reference) %>%
  distinct(`Study Reference`) %>%
  tally() %>%
  pull(n)

wages_per_cent <- round((wages_count/total)*100, 1)

# high school or equivalent completion
hs_grad_count <- hs_grad_outcome_data %>% 
  filter(`Study Reference` %in% included_studies_data$study_reference) %>%
  distinct(`Study Reference`) %>%
  tally() %>%
  pull(n)

hs_grad_per_cent <- round((hs_grad_count/total)*100, 1)

# vet commencement
vet_commence_count <- vet_commence_outcome_data %>% 
  filter(`Study Reference` %in% included_studies_data$study_reference) %>%
  distinct(`Study Reference`) %>%
  tally() %>%
  pull(n)

vet_commence_per_cent <- round((vet_commence_count/total)*100, 1)

# university commencement
uni_commence_count <- uni_commence_outcome_data %>% 
  filter(`Study Reference` %in% included_studies_data$study_reference) %>%
  distinct(`Study Reference`) %>%
  tally() %>%
  pull(n)

uni_commence_per_cent <- round((uni_commence_count/total)*100, 1)

# summarise outcome information
outcome_summary <- tibble(
  category = c(
    "Employment status", 
    "Hours worked", 
    "Wages or Earnings", 
    "High school or equivalent completion", 
    "Vocational education commencement", 
    "University commencement"
    ),
  n = c(
    employment_status_count, 
    hours_worked_count, 
    wages_count, 
    hs_grad_count,
    vet_commence_count, 
    uni_commence_count
    ),
  per_cent = c(
    employment_status_per_cent,
    hours_worked_per_cent,
    wages_per_cent,
    hs_grad_per_cent,
    vet_commence_per_cent, 
    uni_commence_per_cent
    ) 
) 

# randomised studies
randomised_studies_count <- included_studies_data %>%
  select(study_design) %>%
  mutate(study_design = case_when(
    study_design == "RCT" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(study_design == 1) %>%
  tally() %>%
  pull(n)

randomised_studies_per_cent <- round((randomised_studies_count/total)*100, 1)

# non-randomised studies
non_randomised_studies_count <- included_studies_data %>%
  select(study_design) %>%
  mutate(study_design = case_when(
    study_design == "QED" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(study_design == 1) %>%
  tally() %>%
  pull(n)

non_randomised_studies_per_cent <- round((non_randomised_studies_count/total)*100, 1)

# study design summary
study_design_summary <- tibble(
  category = c(
    "Randomised studies", 
    "Non-randomised studies"
    ),
  n = c(
    randomised_studies_count, 
    non_randomised_studies_count
    ),
  per_cent = c(
    randomised_studies_per_cent,
    non_randomised_studies_per_cent
    ) 
)

# peer-reviewed publications
peer_reviewed_publication_count <- included_studies_data %>%
  select(peer_reviewed_publication) %>%
  mutate(peer_reviewed_publication = case_when(
    peer_reviewed_publication == "Yes" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(peer_reviewed_publication == 1) %>%
  tally() %>%
  pull(n)

peer_reviewed_publication_per_cent <- round((peer_reviewed_publication_count/total)*100, 1)

# grey lit
grey_literature_count <- included_studies_data %>%
  select(peer_reviewed_publication) %>%
  mutate(peer_reviewed_publication = case_when(
    peer_reviewed_publication == "No" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(peer_reviewed_publication == 1) %>%
  tally() %>%
  pull(n)

grey_literature_per_cent <- round((grey_literature_count/total)*100, 1)

# publication source summary
publication_source_summary <- tibble(
  category = c(
    "Peer reviewed published study", 
    "Grey literature"
    ),
  n = c(
    peer_reviewed_publication_count, 
    grey_literature_count
    ),
  per_cent = c(
    peer_reviewed_publication_per_cent,
    grey_literature_per_cent
    ) 
)

# year of publication
year_publication_summary <- included_studies_data %>%
  select(year_of_publication) %>%
  mutate(
    `1990-1994` = case_when(
      year_of_publication %in% c(1990:1994) ~ 1, 
      TRUE ~ 0),
    `1995-1999` = case_when(
      year_of_publication %in% c(1995:1999) ~ 1, 
      TRUE ~ 0),
    `2000-2004` = case_when(
      year_of_publication %in% c(2000:2004) ~ 1, 
      TRUE ~ 0),
    `2005-2009` = case_when(
      year_of_publication %in% c(2005:2009) ~ 1, 
      TRUE ~ 0),
    `2010-2014` = case_when(
      year_of_publication %in% c(2010:2014) ~ 1, 
      TRUE ~ 0),
    `2015-2019` = case_when(
      year_of_publication %in% c(2015:2019) ~ 1, 
      TRUE ~ 0),
    `2020-current` = case_when(
      year_of_publication %in% c(2020:2023) ~ 1, 
      TRUE ~ 0)
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
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "n"
  ) %>%
  mutate(
    per_cent = round((n/total)*100,1)
  )

# population facing additional barriers
additional_barriers <- included_studies_data %>%
  select(
    population_with_disabilities,
    high_risk_population
    ) %>%
  mutate(additional_barriers = case_when(
    population_with_disabilities == "Yes" ~ 1,
    high_risk_population == "Yes" ~ 1,
    TRUE ~ 0)
    ) %>%
  select(
    additional_barriers
    ) %>%
  summarise(
    additional_barriers = sum(additional_barriers)
    ) %>%
  pull(additional_barriers)

additional_barriers_per_cent <- round((additional_barriers/total*100), 1)

# population not facing additional barriers
non_additional_barriers <- total - additional_barriers
non_additional_barriers_per_cent <- round((non_additional_barriers/total*100), 1)

# population summary
population_summary <- tibble(
  category = c(
    "Reported facing additional barriers", 
    "Reported not facing additional barriers"
    ),
  n = c(
    additional_barriers, 
    non_additional_barriers
    ),
  per_cent = c(
    additional_barriers_per_cent,
    non_additional_barriers_per_cent
    ) 
)

# study location
location_summary <- included_studies_data %>%
  select(
    location
  ) %>%
  count(
    location,
    name = "n"
  ) %>%
  arrange(
    desc(
      n)
  ) %>%
  mutate(
    per_cent = round((n/total*100),1)
  ) %>%
  rename(
    category = location
  )

# combine all data for export
export_table <- bind_rows(
  component_summary,
  outcome_summary,
  study_design_summary,
  publication_source_summary,
  year_publication_summary,
  population_summary,
  location_summary
)

write_csv(
  export_table,
  "./network-meta/visualisation/output/included_studies_summary.csv"
)