## PLOT HISTOGRAMS SHOWING COMPONENT BREAKDOWN FOR EACH MODEL: EDUCATION COMPLETION

## LOAD REQURIED PACKAGES

library(tidyverse)
library(ggplot2)
library(hrbrthemes)

## LOAD DATA

model_one_plot_data_location <- "./visualisation/input/education_completion_model_one_components_plot_data.RDS"
model_one_plot_data <- readRDS(model_one_plot_data_location)

model_two_plot_data_location <- "./visualisation/input/education_completion_model_two_components_plot_data.RDS"
model_two_plot_data <- readRDS(model_two_plot_data_location)

model_three_plot_data_location <- "./visualisation/input/education_completion_model_three_components_plot_data.RDS"
model_three_plot_data <- readRDS(model_three_plot_data_location)

model_four_plot_data_location <- "./visualisation/input/education_completion_model_four_components_plot_data.RDS"
model_four_plot_data <- readRDS(model_four_plot_data_location)

## PLOT AND EXPORT MODEL ONE HISTOGRAM

education_completion_model_one_component_plot <- model_one_plot_data %>%
  ggplot() +
  aes(
    x = value,
    y = n,
    fill = component_group,
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
    y = c(0,30)
  ) +
  labs(
    y = "Frequency component/comparison type appears in model sample",
    x = "Component/comparison type"
  ) +
  scale_fill_ipsum() +
  theme_ipsum() +
  theme(
    plot.background = element_rect(
      colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) 
  
ggsave(
  filename = "./visualisation/output/education_completion_model_one_component_plot.png",
  plot = education_completion_model_one_component_plot,
  type = "cairo-png")

## PLOT AND EXPORT MODEL TWO HISTOGRAM

education_completion_model_two_component_plot <- model_two_plot_data %>%
  ggplot() +
  aes(
    x = value,
    y = n,
    fill = component_group,
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
    y = c(0,30)
  ) +
  labs(
    y = "Frequency component/comparison type appears in model sample",
    x = "Component/comparison type"
  ) +
  scale_fill_ipsum() +
  theme_ipsum() +
  theme(
    plot.background = element_rect(
      colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1)) 

ggsave(
  filename = "./visualisation/output/education_completion_model_two_component_plot.png",
  plot = education_completion_model_two_component_plot,
  type = "cairo-png")

## PLOT AND EXPORT MODEL THREE HISTOGRAM

education_completion_model_three_component_plot <- model_three_plot_data %>%
  ggplot() +
  aes(
    x = value,
    y = n,
    fill = component_group,
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
    y = c(0,30)
  ) +
  labs(
    y = "Frequency component/comparison type appears in model sample",
    x = "Component/comparison type"
  ) +
  scale_fill_ipsum() +
  theme_ipsum() +
  theme(
    plot.background = element_rect(
      colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) 

ggsave(
  filename = "./visualisation/output/education_completion_model_three_component_plot.png",
  plot = education_completion_model_three_component_plot,
  type = "cairo-png")

## PLOT AND EXPORT MODEL FOUR HISTOGRAM

education_completion_model_four_component_plot <- model_four_plot_data %>%
  ggplot() +
  aes(
    x = value,
    y = n,
    fill = component_group,
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
    y = c(0,30)
  ) +
  labs(
    y = "Frequency component/comparison type appears in model sample",
    x = "Component/comparison type"
  ) +
  scale_fill_ipsum() +
  theme_ipsum() +
  theme(
    plot.background = element_rect(
      colour = "white"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1))  

ggsave(
  filename = "./visualisation/output/education_completion_model_four_component_plot.png",
  plot = education_completion_model_four_component_plot,
  type = "cairo-png")