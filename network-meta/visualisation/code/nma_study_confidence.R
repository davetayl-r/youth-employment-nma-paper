# Study confidence figure

# load required packages
library(tidyverse)
library(ggplot2)
library(scales)

# read study confidence data
included_studies_confidence_data_location <- "./network-meta/nma/context-data/quality_appraisal_information.RDS"
included_studies_confidence_data <- readRDS(included_studies_confidence_data_location)

# clean data
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

# prepare plot
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
    ncol = 4) +
  lims(
    y = c(0,110)
  ) +
  labs(
    y = "Per cent of total studies (n=58)",
    x = "Assessment of study confidence"
  ) +
  scale_x_discrete(
    labels = wrap_format(10)
  ) +
  scale_fill_manual(
    values = c(
      "Study design" = "#5A5A5A",
      "Sample size" = "#5A5A5A",
      "Attrition" = "#5A5A5A",
      "Intervention description" = "#5A5A5A",
      "Outcome definition" = "#5A5A5A",
      "Baseline balance" = "#5A5A5A",
      "Overall confidence" = "#006DAE"
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

# export plot
ggsave(
  file = "./network-meta/visualisation/output/nma_study_confidence.png",
  plot = study_confidence_plot, 
  width = 10, 
  height = 6, 
  type = "cairo-png")