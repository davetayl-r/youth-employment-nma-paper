## NMA
## STUDY LOCATION PLOT

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(ggplot2)

## READ DATA

included_studies_data_location <- "https://docs.google.com/spreadsheets/d/1YGbkWKGf9Afc5OtBKanbAP7HtJyC2-Tx9NeGychCwaY/edit#gid=0"

included_studies_data <- read_sheet(
  ss = included_studies_data_location,
  sheet = "included_studies")

## CLEAN DATA

study_location_plot_data <- included_studies_data %>%
  group_by(
    location) %>%
    tally() %>%
    mutate(
      total = sum(n)
    ) %>%
    mutate(
      per_cent = (n/total) * 100
    ) %>%
    arrange(
      per_cent
    ) %>%
  mutate(
    study_location_continent = case_when(
      location == "United States" ~ "North America",
      location == "Australia" ~ "Oceania",
      TRUE ~ "Europe")
    )

study_location_plot <- study_location_plot_data %>%
    ggplot() +
    aes(
      x = reorder(location, -per_cent),
      y = per_cent,
      fill = study_location_continent,
      label = n) +
    geom_bar(
      stat = "identity"
    ) +
    geom_text(
      vjust = -.5
    ) +
    lims(
      y = c(0,70)
    ) +
    labs(
      y = "Per cent of included studies",
      x = "Location of included study"
    ) +
  scale_fill_manual(
    values = c(
      "North America" = "#7D2248",
      "Oceania" = "#BFB800",
      "Europe" = "#69C2C9"
    ),
    name = "Region study was conducted:"
  ) +
  theme(
    plot.background = element_rect(
      colour = "#FFFFFF"),
    legend.position = "bottom",
    #legend.title = element_blank(),
    legend.direction = "horizontal",
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold",
      hjust = 0,
      size = 11
    )
  ) 

ggsave(
  file = "./visualisation/output/nma_study_location.png",
  plot = study_location_plot, 
  width = 10, 
  height = 5, 
  type = "cairo-png")