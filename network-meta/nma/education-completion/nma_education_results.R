## Network meta analysis: education completion

# load required packages
library(tidyverse)
library(readxl)
library(meta)
library(netmeta)
library(dmetar)
library(ggplot2)
library(scales)
library(viscomp)

# load required data
education_component_es_data_location <- "./network-meta/nma/meta-data/education_analysis_data.RDS"
education_component_es_data <- readRDS(education_component_es_data_location)

# inspect network structure
education_connections <- netconnection(
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_component_es_data, 
  nchar.trts = 30
  )

# fit standard (full interaction) component NMA
education_network_meta_random_effects <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_component_es_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

component_combination_labels <- c(
  "On-the-job Training + Other",
  "Life Skills + Off-the-job Training + Other",
  "Basic Skills + Other",
  "Basic Skills + Life Skills + Off-the-job Training + Other",
  "Basic Skills + Life Skills + Other",
  "Basic Skills + Life Skills + Coaching & Mentoring + Other",
  "Basic Skills + Off-the-job Training + Other",
  "Coaching & Mentoring + Other",
  "Basic Skills + Coaching & Mentoring + Other",
  "Off-the-job Training + On-the-job Training + Other",
  "Life Skills + Other",
  "Life Skills + Coaching & Mentoring + Other",
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_education_standard.png",
  width = 29,
  height = 10,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  )

forest(
  education_network_meta_random_effects,
  studlab = component_combination_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-1, 1.5),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7,
  leftcols = c("studlab", "prop.direct"),
  equal.size = FALSE,
  sortvar = -"TE",
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE)

dev.off()

# visualise network structure
set.seed(368)

png(
  file = "./network-meta/visualisation/output/nma_education_netgraph.png",
  width = 1000,
  height = 600,
  type = "cairo-png"
  )

network_labels <- c(
  "Basic Skills + Coaching & Mentoring + Other",
  "Basic Skills + Life Skills + Coaching & Mentoring + Other",
  "Basic Skills + Life Skills + Off-the-job Training + Other",
  "Basic Skills + Life Skills + Other",
  "Basic Skills + Off-the-job Training + Other",
  "Basic Skills + Other",
  "Coaching & Mentoring + Other",
  "Life Skills + Coaching & Mentoring + Other",
  "Life Skills + Off-the-job Training + Other",
  "Life Skills + Other",
  "Off-the-job Training + On-the-job Training + Other",
  "On-the-job Training + Other",
  "Other",
  "Services as usual")
  
netgraph(
  education_network_meta_random_effects,
  plastic = FALSE,
  col ="#006DAE",
  number.of.studies = TRUE,
  cex.number = 1.5,
  pos.number.of.studies = 0.7,
  labels = network_labels
  )

dev.off()

# visualise components included in NMA
education_nma_component_visualisation <- compdesc(
  model = education_network_meta_random_effects,
  sep = "+")

education_nma_component_heatmap <- education_nma_component_visualisation$heatmat +
  theme(
    plot.background = element_rect(fill = "white")
  )

# export plot
ggsave(
  file = "./network-meta/visualisation/output/nma_education_component_frequency.png",
  plot = education_nma_component_heatmap, 
  width = 10,
  height = 7,
  type = "cairo-png"
  )

# count number of high confidence studies in sample 
education_component_es_data %>%
  filter(
    study_quality == "High confidence"
  )

# print table of results
netleague(
  x = education_network_meta_random_effects,
  direct = TRUE,
  digits = 2,
  overwrite = TRUE,
  path = "./network-meta/visualisation/output/nma_education_results.xlsx"
  )

# assess publication bias

# adjusted funnel plot
png(
  file = "./network-meta/visualisation/output/nma_education_standard_nma_publication_bias.png",
  width = 26,
  height = 16,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

order = c("SAU")

funnel(
  x = education_network_meta_random_effects,
  order = order,
  pooled = "random",
  lump.comparator = TRUE,
  text.comparator = "SAU",
  pch = c(11:21), 
  col = c(11:21),
  text.linreg = "(Egger)",
  method.bias = c("Egger"), digits.pval = 2
  )

dev.off()

# egger's test of the intercept
input_eggers <- funnel(
  x = education_network_meta_random_effects,
  order = order,
  pooled = "random",
  lump.comparator = TRUE,
  text.comparator = "SAU"
  )

metabias(metagen(TE.adj, seTE, data = input_eggers))

# evaluate local incoherence through using node splitting

# Note: there is no mixed evidence for this outcome, so node splitting is redundant

# evaluate global incoherence

# Note: there is no mixed evidence in this specification, so this  is redundant

# Fit Additive CNMA
education_additive_component_network_meta <- netcomb(
  x = education_network_meta_random_effects, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

education_additive_component_network_meta_plot_data <- netcomplex(
  education_additive_component_network_meta,  
  education_additive_component_network_meta$comps
  )

png(
  file = "./network-meta/visualisation/output/nma_education_additive.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

forest(
  education_additive_component_network_meta_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Education completion"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -education_additive_component_network_meta_plot_data$Comb.random
  )

dev.off()

# Fit interaction component NMAs

# Interaction: ON-JT+OTH
education_interaction_matrix_ojt_oth <- cbind(
  education_additive_component_network_meta$C.matrix,
  `ON-JT+OTH` = 0)
education_interaction_matrix_ojt_oth["ON-JT+OTH", "ON-JT+OTH"] <- 1

education_interaction_component_network_meta_ojt_oth <- netcomb(
  education_network_meta_random_effects, 
  C.matrix = education_interaction_matrix_ojt_oth, 
  inactive = "SAU"
  )

education_interaction_component_network_meta_ojt_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_ojt_oth,  
  education_interaction_component_network_meta_ojt_oth$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_education_interaction_onjt_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "On-the-job Training x Other")

forest(
  education_interaction_component_network_meta_ojt_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Education completion"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_ojt_oth_plot_data$Comb.random
  ) 

dev.off()

# Interaction: BS+OTH
education_interaction_matrix_bs_oth <- cbind(
  education_additive_component_network_meta$C.matrix,
  `BS+OTH` = 0)
education_interaction_matrix_bs_oth["BS+OTH", "BS+OTH"] <- 1

education_interaction_component_network_meta_bs_oth <- netcomb(
  education_network_meta_random_effects, 
  C.matrix = education_interaction_matrix_bs_oth, 
  inactive = "SAU")

education_interaction_component_network_meta_bs_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_bs_oth,  
  education_interaction_component_network_meta_bs_oth$comps)

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_education_interaction_bs_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "Basic Skills x Other")

forest(
  education_interaction_component_network_meta_bs_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Education completion"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_bs_oth_plot_data$Comb.random
  ) 

dev.off()

# Interaction: LS+OTH
education_interaction_matrix_ls_oth <- cbind(
  education_additive_component_network_meta$C.matrix,
  `LS+OTH` = 0)
education_interaction_matrix_ls_oth["LS+OTH", "LS+OTH"] <- 1

education_interaction_component_network_meta_ls_oth <- netcomb(
  education_network_meta_random_effects, 
  C.matrix = education_interaction_matrix_ls_oth, 
  inactive = "SAU"
  )

education_interaction_component_network_meta_ls_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_ls_oth,  
  education_interaction_component_network_meta_ls_oth$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_education_interaction_ls_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "Life Skills x Other")

forest(
  education_interaction_component_network_meta_ls_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Education completion"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_ls_oth_plot_data$Comb.random
  ) 

dev.off()

# Interaction: C&M+OTH 
education_interaction_matrix_cm_oth <- cbind(
  education_additive_component_network_meta$C.matrix,
  `C&M+OTH` = 0)
education_interaction_matrix_cm_oth["C&M+OTH", "C&M+OTH"] <- 1

education_interaction_component_network_meta_cm_oth <- netcomb(
  education_network_meta_random_effects, 
  C.matrix = education_interaction_matrix_cm_oth, 
  inactive = "SAU"
  )

education_interaction_component_network_meta_cm_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_cm_oth,  
  education_interaction_component_network_meta_cm_oth$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_education_interaction_cm_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "Coaching & Mentoring x Other")

forest(
  education_interaction_component_network_meta_cm_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Education completion"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_cm_oth_plot_data$Comb.random
  ) 

dev.off()

# Compare results between different NMA specifications

# prep colour palette
colour_palette <- viridis_pal(
    begin = 0, 
    end = 0.8,
    direction = 1,
    option = "inferno"
  )(6)

# prepare plot data
education_forest_plot_data <- netbind(
  random = TRUE, 
  common = FALSE, 
  education_network_meta_random_effects, 
  education_additive_component_network_meta, 
  education_interaction_component_network_meta_ojt_oth,
  education_interaction_component_network_meta_bs_oth,
  education_interaction_component_network_meta_ls_oth,
  education_interaction_component_network_meta_cm_oth,
  name = c(
    "Standard NMA", 
    "Additive cNMA", 
    "On-the-job training x Other Interaction cNMA",
    "Basic Skills x Other Interaction cNMA",
    "Life Skills x Other Interaction cNMA",
    "Coaching & Mentoring x Other Interaction cNMA"
    ), 
  col.study = colour_palette, 
  col.square = colour_palette
  )

png(
  file = "./network-meta/visualisation/output/nma_education_results_comparison.png",
  width = 18,
  height = 36,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

# plot different nma specifications
forest(
  education_forest_plot_data,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.75, 1.25),
  addrow.subgroups = FALSE,
  fontsize = 10,
  spacing = 0.7,
  squaresize = 0.9,
  smlab = paste("Employment Program Components vs. Usual Services: \n", "Education completion"), 
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  drop.reference.group = TRUE
  )

dev.off()

# subgroup analysis

# subgroup analysis by: study confidence

# subset data by study confidence
education_network_meta_random_effects_high_confidence_data <- education_component_es_data %>%
  filter(
    study_quality == "High confidence"
  )

education_network_meta_random_effects_low_med_confidence_data <- education_component_es_data %>%
  mutate(
    relevant_studies = case_when(
      str_detect(study_quality, "Low confidence") ~ "Low or Medium confidence",
      str_detect(study_quality, "Medium confidence") ~ "Low or Medium confidence")
  ) %>%
  filter(
    relevant_studies == "Low or Medium confidence"
  ) 

# Stop, there are only nine studies that use high-quality designs. This analysis is not possible. 

# subgroup analysis by: study location

# subset data by study location
education_network_meta_random_effects_location_usa_data <- education_component_es_data %>%
  filter(
    study_location == "United States"
  ) 

education_network_meta_random_effects_location_other_data <- education_component_es_data %>%
  filter(
    !study_location == "United States"
  ) 

# Stop, there is only one study from outside the United States. This analysis is not possible. 

# subgroup analysis by: study population

# subset data by study population
education_network_meta_random_effects_population_complex_needs_data <- education_component_es_data %>%
  filter(
    complex_population == "Yes"
  ) 

education_network_meta_random_effects_population_without_complex_needs_data <- education_component_es_data %>%
  filter(
    complex_population == "No"
  ) 

# specify nma
education_network_meta_random_effects_population_without_complex_needs <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_network_meta_random_effects_population_without_complex_needs_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

# specify additive nma
education_additive_component_network_meta_population_complex_needs <- discomb(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_network_meta_random_effects_population_complex_needs_data, 
  inactive = "SAU",
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

education_additive_component_network_meta_population_without_complex_needs <- netcomb(
  x = education_network_meta_random_effects_population_without_complex_needs, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

# view results
education_additive_component_network_meta_population_complex_needs_plot_data <- netcomplex(
  education_additive_component_network_meta_population_complex_needs,  
  education_additive_component_network_meta_population_complex_needs$comps
  )

education_additive_component_network_meta_population_without_complex_needs_plot_data <- netcomplex(
  education_additive_component_network_meta_population_without_complex_needs,  
  education_additive_component_network_meta_population_without_complex_needs$comps
  )

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  #"On-the-job Training", 
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_education_results_subgroup_population_complex.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

forest(
  education_additive_component_network_meta_population_complex_needs_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies involving young people\nwith reported complex needs"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -education_additive_component_network_meta_population_complex_needs_plot_data$Comb.random
  )

dev.off()

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_education_results_subgroup_population_not_complex.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  )

forest(
  education_additive_component_network_meta_population_without_complex_needs_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies involving young people\nwithout reported complex needs"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -education_additive_component_network_meta_population_without_complex_needs_plot_data$Comb.random
  )

dev.off()

# sensitivity analysis

# sensitivity analysis by: study design

# subset data by study design
education_network_meta_random_effects_randomised_data <- education_component_es_data %>%
  filter(
    study_design == "Randomised"
  ) 

education_network_meta_random_effects_non_randomised_data <- education_component_es_data %>%
  filter(
    study_design == "Non-randomised"
  ) 

# Stop, there are only three studies that use non-randomised designs. This analysis is not possible. 