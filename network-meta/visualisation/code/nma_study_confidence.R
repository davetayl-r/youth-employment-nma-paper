## NMA
## STUDY QUALITY PLOT

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(ggplot2)

## READ DATA

included_studies_confidence_data_location <- "./network-meta/nma/context-data/quality_appraisal_information.RDS"
included_studies_confidence_data <- readRDS(included_studies_confidence_data_location)

## CLEAN DATA

study_confidence_plot_data <- included_studies_confidence_data %>%
  rename(
    `Study design` = qa_domain_1_study_design,
    `Sample size` = qa_domain_2_sample_size,  
    `Attrition` = qa_domain_3_attrition,
    `Intervention description` = qa_domain_4_intervention_description,
    `Outcome definition` = qa_domain_5_outcome_definition,
    `Baseline balance` = qa_domain_6_baseline_balance,
    `Overall confidence` = overall_rating
  ) %>%
  pivot_longer(
    -study_reference,
    names_to = "domain",
    values_to = "rating"
  ) %>% 
  mutate(
    domain = factor(
      domain,
      levels = c(
        "Study design", 
        "Sample size",  
        "Attrition",
        "Intervention description",
        "Outcome definition",
        "Baseline balance",
        "Overall confidence"),
      ordered = TRUE)
  ) %>%
  filter(!is.na(rating)) %>%
  mutate(
    rating = factor(
      rating,
      levels = c(
        "Low confidence", 
        "Medium confidence",  
        "High confidence"),
      ordered = TRUE)
  ) %>%
  mutate(
    n = 1
  ) %>%
  group_by(
    domain
  ) %>%
  mutate(
    total = sum(n)
  ) %>%
  ungroup() %>%
  group_by(
    domain,
    rating,
    total
  ) %>%
  tally() %>%
  mutate(
    per_cent = (n/total) * 100
  ) %>%
  mutate(
    domain_group = case_when(
      domain == "Overall confidence" ~ "total",
      TRUE ~ "other") 
  ) 

## PREPARE PLOT

study_confidence_plot <- study_confidence_plot_data %>% 
  ggplot() +
  aes(
    x = rating,
    y = per_cent,
    fill = domain,
    label = n) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    vjust = -.5
  ) +
  facet_wrap(
    domain ~ .,
    shrink = FALSE,
    ncol = 2) +
  lims(
    y = c(0,110)
  ) +
  labs(
    y = "Per cent",
    x = "Assessment of study confidence"
  ) +
  scale_fill_manual(
    values = c(
      "Study design" = "#E52E36",
      "Sample size" = "#F8AA3D",
      "Attrition" = "#69C2C9",
      "Intervention description" = "#736E01",
      "Outcome definition" = "#318187",
      "Baseline balance" = "#BFB800",
      "Overall confidence" = "#7D2248"
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
    strip.text = element_text(
      face = "bold",
      hjust = 0,
      size = 11
    )
  ) 

ggsave(
  file = "./network-meta/visualisation/output/nma_study_confidence.png",
  plot = study_confidence_plot, 
  width = 10, 
  height = 8, 
  type = "cairo-png")
