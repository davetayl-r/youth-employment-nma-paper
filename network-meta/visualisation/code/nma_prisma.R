## Create PRISMA 2020 figure for publication

# load required packages
library(tidyverse)
library(readxl)
library(PRISMA2020)

# read data
raw_prisma_data_location <- "./network-meta/visualisation/input/nma_prisma_data.csv"
raw_prisma_data <- read.csv(raw_prisma_data_location)

# convert raw data to PRISMA chart form
prisma_data <- PRISMA_data(raw_prisma_data)

# create figure
prisma_figure <- PRISMA_flowdiagram(
  data = prisma_data,
  interactive = FALSE,
  detail_databases = TRUE,
  previous = FALSE,
  other = TRUE,
  fontsize = 10,
  font = "Helvetica",
  title_colour = "Goldenrod1",
  greybox_colour = "Gainsboro",
  main_colour = "Black",
  arrow_colour = "Black",
  arrow_head = "normal",
  arrow_tail = "none",
  side_boxes = TRUE
)

# view figure
prisma_figure

# export figure
PRISMA_save(
  plotobj = prisma_figure,
  filename = "./network-meta/visualisation/output/nma_prisma_flowchart.png",
  filetype = "png",
  overwrite = TRUE)