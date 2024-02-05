## NMA: EMPLOYMENT OUTCOME

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(ggplot2)
library(scales)

## READ DATA

component_data_location <- "https://docs.google.com/spreadsheets/d/1IlYzDoBuMwCw_AkRILwYB8gYLZWVDGLumYFMmrl7tOk/edit#gid=698569653"

component_data <- read_sheet(
  ss = component_data_location,
  sheet = "component_combinations")

## SELECT RELEVANT COMPONENTS

clean_data <- component_data %>%
  select(
    study_reference,
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
    comparison_other
  ) %>%
  rename(
    study = study_reference
  ) %>%
  pivot_longer(
    cols = (-study),
    values_drop_na = TRUE) %>%
  mutate(group = case_when(
    str_detect(name, "intervention_") ~ "Intervention",
    str_detect(name, "comparison_") ~ "Comparison")) %>%
  group_by(group, name) %>%
  tally() %>%
  mutate(
    name = str_remove_all(name, "comparison_"),
    name = str_remove_all(name, "intervention_"),
    name = str_remove_all(name, "_only"),
    ) %>%
  mutate(
    component = case_when(
      name == "basic_skills" ~ "Basic Skills",
      name == "life_skills" ~ "Life Skills",
      name == "off_job_training" ~ "Off-the-job training",
      name == "on_the_job_training" ~ "On-the-job training",
      name == "apprenticeships" ~ "Apprenticeships",
      name == "coaching_mentoring" ~ "Coaching & Mentoring",
      name == "other" ~ "Other",
      name == "services_as_usual" ~ "Services as Usual")) %>%
  mutate(component_type = case_when(
    name == "other" ~  "Other component",
    name == "services_as_usual" ~  "Comparison",
    TRUE ~ "Component of interest"))

## PLOT FIGURE

specification_three_plot <- clean_data %>%
  ggplot() +
  aes(
    x = component,
    y = n,
    fill = component_type,
    label = n) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    vjust = -1
  ) +
  facet_grid(
    group ~ .,
    scales = "free",
    shrink = FALSE) +
  lims(
    y = c(0,60)
  ) +
  labs(
    y = "Frequency component/comparison type appears in specification sample",
    x = "Component/comparison type"
  ) +
  scale_x_discrete(labels = wrap_format(11)) +
  scale_fill_manual(
    values = c(
      "Comparison" = "#69C2C9",
      "Component of interest" = "#BFB800",
      "Other component" = "#7D2248"
    )
  ) +
  theme(
    plot.background = element_rect(
      colour = "#FFFFFF"),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    #axis.text.x = element_text(
    #  angle = 45,
    #  vjust = 1,
    #  hjust = 1
    #),
    strip.text = element_text(
      face = "bold",
      hjust = 0,
      size = 11
    )
  ) 

## EXPORT PLOT

ggsave(
  file = "./visualisation/output/nma_component_breakdown.png",
  plot = specification_three_plot, 
  width = 10, 
  height = 6, 
  type = "cairo-png")