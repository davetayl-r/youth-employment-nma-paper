## COMPONENT DEFINITIONS
## CLEAN AND PROCESS DATA FOR WRITEUP

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(stringr)
library(googlesheets4)

## READ & EXPORT DATA

component_definitions_data_location <- "https://docs.google.com/spreadsheets/d/1IlYzDoBuMwCw_AkRILwYB8gYLZWVDGLumYFMmrl7tOk/edit#gid=779599844"

component_definitions <- read_sheet(
  ss = component_definitions_data_location,
  sheet = "definition") %>%
  mutate(
    `Definition from protocol` = str_replace_all(
      `Definition from protocol`, 
      "•", 
      "\n*")) %>%
  mutate(
    `Operationalisting the definition` = str_replace_all(
      `Operationalisting the definition`, 
      "•", 
      "\n*")
  )

saveRDS(
  component_definitions,
  file = "./nma/context-data/component_definitions.RDS") 
