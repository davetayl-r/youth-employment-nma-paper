## SAMPLE SIZE NUMBERS FOR YFF

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(googlesheets4)

## READ DATA

employment_component_specification_three_es_data_location <- "./nma/meta-data/employment_analysis_data_specification_three.RDS"
employment_component_specification_three_es_data <- readRDS(employment_component_specification_three_es_data_location)

education_component_specification_three_es_data_location <- "./nma/meta-data/education_analysis_data_specification_three.RDS"
education_component_specification_three_es_data <- readRDS(education_component_specification_three_es_data_location)

## COUNT SAMPLE SIZE

employment_nam_sample_size <- employment_component_specification_three_es_data %>%
  summarise(sum(sample.size))

## 676664

education_nma_sample_size <- education_component_specification_three_es_data %>%
  summarise(sum(sample.size))

## 46800