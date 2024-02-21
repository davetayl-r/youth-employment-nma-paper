## Network meta analysis: employment status

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
employment_component_es_data_location <- "./network-meta/nma/data/employment_analysis_data.RDS"
employment_component_es_data <- readRDS(employment_component_es_data_location)

# inspect network structure
employment_connections <- netconnection(
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_component_es_data, 
  nchar.trts = 30
  )

# fit standard (full interaction) component NMA
employment_network_meta_random_effects <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_component_es_data,
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

component_combination_labels <- c(
  "On-the-job Training + Other",
  "Basic Skills + Off-the-job Training + Other",
  "On-the-job Training",
  "Apprenticeships",
  "Life Skills + Coaching & Mentoring + Other",
  "Off-the-job Training",
  "Basic Skills + Life Skills + Off-the-job Training + Other",
  "Life Skills + Off-the-job Training + Other",
  "Basic Skills + Life Skills + Coaching & Mentoring + Other",
  "Basic Skills",
  "Off-the-job Training + On-the-job Training + Other",
  "Coaching & Mentoring + Other",
  "Basic Skills + Life Skills + Other",
  "Other",
  "Basic Skills + Coaching & Mentoring + Other",
  "Basic Skills + Life Skills + Off-the-job Training + Coaching & Mentoring + Other",
  "Life Skills + Other",
  "Basic Skills + Off-the-job Training",
  "Basic Skills + Other"
  )

png(
  file = "./network-meta/visualisation/output/nma_employment_standard.png",
  width = 29,
  height = 12,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

forest(employment_network_meta_random_effects,
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
set.seed(268)

png(
  file = "./network-meta/visualisation/output/nma_employment_netgraph.png",
  width = 1000,
  height = 750,
  type = "cairo-png"
  ) 

network_labels <- c(
  "Apprenticeships",
  "Basic Skills",
  "Basic Skills + Coaching & Mentoring + Other",
  "Basic Skills + Life Skills + Coaching & Mentoring + Other",
  "Basic Skills + Life Skills + Off-the-job Training + Coaching & Mentoring + Other",
  "Basic Skills + Life Skills + Off-the-job Training + Other",
  "Basic Skills + Life Skills + Other",
  "Basic Skills + Off-the-job Training",
  "Basic Skills + Off-the-job Training + Other",
  "Basic Skills + Other",
  "Coaching & Mentoring + Other",
  "Life Skills + Coaching & Mentoring + Other",
  "Life Skills + Off-the-job Training + Other",
  "Life Skills + Other",
  "Off-the-job Training",
  "Off-the-job Training + On-the-job Training + Other",
  "On-the-job Training",
  "On-the-job Training + Other",
  "Other",
  "Services as usual")

netgraph(
  employment_network_meta_random_effects,
  plastic = FALSE,
  col ="#006DAE",
  number.of.studies = TRUE,
  cex.number = 1.5,
  pos.number.of.studies = 0.7,
  labels = network_labels
  )

dev.off()

# visualise components included in NMA
employment_nma_component_visualisation <- compdesc(
  model = employment_network_meta_random_effects,
  sep = "+"
  ) 

employment_nma_component_heatmap <- employment_nma_component_visualisation$heatmat +
  theme(
    plot.background = element_rect(fill = "white")
  )

# export plot
ggsave(
  file = "./network-meta/visualisation/output/nma_employment_component_frequency.png",
  plot = employment_nma_component_heatmap, 
  width = 10, 
  height = 7, 
  type = "cairo-png"
  )

# count number of high confidence studies in sample
employment_component_es_data %>%
  filter(
    study_quality == "High confidence"
  )

# print table of results
netleague(
  x = employment_network_meta_random_effects,
  direct = TRUE,
  digits = 2,
  overwrite = TRUE,
  path = "./network-meta/visualisation/output/nma_employment_results.xlsx"
  )

# assess publication bias

# adjusted funnel plot
png(
  file = "./network-meta/visualisation/output/nma_employment_standard_nma_publication_bias.png",
  width = 26,
  height = 16,
  units = "cm",
  #pointsize = 10,
  res = 1200,
  #bg = "white",
  type = "cairo-png"
  )

order = c("SAU")

funnel(
  x = employment_network_meta_random_effects,
  order = order,
  pooled = "random",
  lump.comparator = TRUE,
  text.comparator = "SAU",
  pch = c(8:24), 
  col = c(8:24),
  text.linreg = "(Egger)",
  method.bias = c("Egger"), digits.pval = 2
  )

dev.off()

# egger's test of the intercept
input_eggers <- funnel(
  x = employment_network_meta_random_effects,
  order = order,
  pooled = "random",
  lump.comparator = TRUE,
  text.comparator = "SAU")

metabias(metagen(TE.adj, seTE, data = input_eggers))

# evaluate local incoherence through using node splitting
employment_network_meta_random_effects_node_split <- netsplit(
  employment_network_meta_random_effects)

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_employment_side.png",
  width = 21,
  height = 31,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

forest(
  employment_network_meta_random_effects_node_split,
  sortvar = -employment_network_meta_random_effects_node_split$k,
  show = "both", # only show combinations with both direct and indirect evidence,
  leftlabs = c("Component \ncombinations", "Number \nof Studies", "Prop. direct\n evidence", "I\n squared")
  )

dev.off()

# evaluate global incoherence
employment_network_meta_random_effects_decomp <- decomp.design(employment_network_meta_random_effects)

employment_network_meta_random_effects_decomp$Q.inc.random %>%
  rownames_to_column() %>%
  mutate(
    Q = round(Q, 3),
    pval = round(pval, 3),
    tau.within = round(tau.within, 3)
  ) %>%
  rename(
    Specification = rowname,
    `p-value` = pval,
    `Square-root of between study variance` = tau.within
  )

# Fit Additive CNMA
employment_additive_component_network_meta <- netcomb(
  x = employment_network_meta_random_effects, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

employment_additive_component_network_meta_plot_data <- netcomplex(
  employment_additive_component_network_meta,  
  employment_additive_component_network_meta$comps
  )

png(
  file = "./network-meta/visualisation/output/nma_employment_additive.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

forest(
  employment_additive_component_network_meta_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Employment status"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -employment_additive_component_network_meta_plot_data$Comb.random
  )

dev.off()

# Fit interaction component NMAs

# Interaction: BS+OFF-JT
employment_interaction_matrix_bs_ofjt <- cbind(
  employment_additive_component_network_meta$C.matrix,
  `BS+OFF-JT` = 0)
employment_interaction_matrix_bs_ofjt["BS+OFF-JT", "BS+OFF-JT"] <- 1

employment_interaction_component_network_meta_bs_ofjt <- netcomb(
  employment_network_meta_random_effects, 
  C.matrix = employment_interaction_matrix_bs_ofjt, 
  inactive = "SAU"
  )

employment_interaction_component_network_meta_bs_ofjt_plot_data <- netcomplex(
  employment_interaction_component_network_meta_bs_ofjt,  
  employment_interaction_component_network_meta_bs_ofjt$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_employment_interaction_bs_offjt.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "Basic Skills x Off-the-job Training")

forest(
  employment_interaction_component_network_meta_bs_ofjt_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Employment status"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -employment_interaction_component_network_meta_bs_ofjt_plot_data$Comb.random
  )

dev.off()

# Interaction: ON-JT+OTH
employment_interaction_matrix_ojt_oth <- cbind(
  employment_additive_component_network_meta$C.matrix,
  `ON-JT+OTH` = 0)
employment_interaction_matrix_ojt_oth["ON-JT+OTH", "ON-JT+OTH"] <- 1

employment_interaction_component_network_meta_ojt_oth <- netcomb(
  employment_network_meta_random_effects, 
  C.matrix = employment_interaction_matrix_ojt_oth, 
  inactive = "SAU"
  )

employment_interaction_component_network_meta_ojt_oth_plot_data <- netcomplex(
  employment_interaction_component_network_meta_ojt_oth,  
  employment_interaction_component_network_meta_ojt_oth$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_employment_interaction_onjt_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "On-the-job Training x Other")

forest(
  employment_interaction_component_network_meta_ojt_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Employment status"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -employment_interaction_component_network_meta_ojt_oth_plot_data$Comb.random
  ) 

dev.off()

# Interaction: BS+OTH
employment_interaction_matrix_bs_oth <- cbind(
  employment_additive_component_network_meta$C.matrix,
  `BS+OTH` = 0)
employment_interaction_matrix_bs_oth["BS+OTH", "BS+OTH"] <- 1

employment_interaction_component_network_meta_bs_oth <- netcomb(
  employment_network_meta_random_effects, 
  C.matrix = employment_interaction_matrix_bs_oth, 
  inactive = "SAU"
  )

employment_interaction_component_network_meta_bs_oth_plot_data <- netcomplex(
  employment_interaction_component_network_meta_bs_oth,  
  employment_interaction_component_network_meta_bs_oth$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_employment_interaction_bs_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "Basic Skills x Other")

forest(
  employment_interaction_component_network_meta_bs_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Employment status"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -employment_interaction_component_network_meta_bs_oth_plot_data$Comb.random
  ) 

dev.off()

# Interaction: LS+OTH
employment_interaction_matrix_ls_oth <- cbind(
  employment_additive_component_network_meta$C.matrix,
  `LS+OTH` = 0)
employment_interaction_matrix_ls_oth["LS+OTH", "LS+OTH"] <- 1

employment_interaction_component_network_meta_ls_oth <- netcomb(
  employment_network_meta_random_effects, 
  C.matrix = employment_interaction_matrix_ls_oth, 
  inactive = "SAU"
  )

employment_interaction_component_network_meta_ls_oth_plot_data <- netcomplex(
  employment_interaction_component_network_meta_ls_oth,  
  employment_interaction_component_network_meta_ls_oth$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_employment_interaction_ls_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "Life Skills x Other")

forest(
  employment_interaction_component_network_meta_ls_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Employment status"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -employment_interaction_component_network_meta_ls_oth_plot_data$Comb.random
  )

dev.off()

# Interaction: C&M+OTH 
employment_interaction_matrix_cm_oth <- cbind(
  employment_additive_component_network_meta$C.matrix,
  `C&M+OTH` = 0)
employment_interaction_matrix_cm_oth["C&M+OTH", "C&M+OTH"] <- 1

employment_interaction_component_network_meta_cm_oth <- netcomb(
  employment_network_meta_random_effects, 
  C.matrix = employment_interaction_matrix_cm_oth, 
  inactive = "SAU"
  )

employment_interaction_component_network_meta_cm_oth_plot_data <- netcomplex(
  employment_interaction_component_network_meta_cm_oth,  
  employment_interaction_component_network_meta_cm_oth$comps
  )

# export forest plot
png(
  file = "./network-meta/visualisation/output/nma_employment_interaction_cm_oth.png",
  width = 17,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other",
  "Coaching & Mentoring x Other")

forest(
  employment_interaction_component_network_meta_cm_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Program Components vs. Usual Services: \n",                   "Employment status"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -employment_interaction_component_network_meta_cm_oth_plot_data$Comb.random
  )

dev.off()

# Compare results between different NMA specifications

# prep colour palette
colour_palette <- viridis_pal(
    begin = 0, 
    end = 0.8,
    direction = 1,
    option = "inferno"
  )(7)

png(
  file = "./network-meta/visualisation/output/nma_employment_results_comparison.png",
  width = 18,
  height = 58,
  units = "cm",
  res = 800,
  type = "cairo-png"
  ) 

# prepare plot data
employment_forest_plot_data <- netbind(
  random = TRUE, 
  common = FALSE, 
  employment_network_meta_random_effects, 
  employment_additive_component_network_meta, 
  employment_interaction_component_network_meta_bs_ofjt,
  employment_interaction_component_network_meta_ojt_oth,
  employment_interaction_component_network_meta_bs_oth,
  employment_interaction_component_network_meta_ls_oth,
  employment_interaction_component_network_meta_cm_oth,
  name = c(
    "Standard NMA", 
    "Additive cNMA", 
    "Basic Skills x Off-the-job training Interaction cNMA",
    "On-the-job training x Other Interaction cNMA",
    "Basic Skills x Other Interaction cNMA",
    "Life Skills x Other Interaction cNMA",
    "Coaching & Mentoring x Other Interaction cNMA"
    ), 
  col.study = colour_palette,
  col.square = colour_palette
  )

# plot different nma specifications
forest(employment_forest_plot_data,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.75, 1.25),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Employment Program Components vs. Usual Services: \n",                   "Employment status"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  drop.reference.group = TRUE
  )

dev.off()

# subgroup analysis

# subgroup analysis by: study confidence

# subset data by study confidence
employment_network_meta_random_effects_high_confidence_data <- employment_component_es_data %>%
  filter(
    study_quality == "High confidence"
  )

employment_network_meta_random_effects_low_med_confidence_data <- employment_component_es_data %>%
  mutate(
    relevant_studies = case_when(
      str_detect(study_quality, "Low confidence") ~ "Low or Medium confidence",
      str_detect(study_quality, "Medium confidence") ~ "Low or Medium confidence")
  ) %>%
  filter(
    relevant_studies == "Low or Medium confidence"
  ) 

# specify NMA
employment_network_meta_random_effects_subgroup_high_confidence <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_high_confidence_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

employment_network_meta_random_effects_subgroup_low_med_confidence <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_low_med_confidence_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

# specify additive NMA
employment_additive_component_network_meta_subgroup_high_confidence <- netcomb(
  x = employment_network_meta_random_effects_subgroup_high_confidence, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

employment_additive_component_network_meta_subgroup_low_med_confidence <- netcomb(
  x = employment_network_meta_random_effects_subgroup_low_med_confidence, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

# view results
employment_additive_component_network_meta_subgroup_high_confidence_plot_data <- netcomplex(
  employment_additive_component_network_meta_subgroup_high_confidence,  
  employment_additive_component_network_meta_subgroup_high_confidence$comps
  )

employment_additive_component_network_meta_subgroup_low_med_confidence_plot_data <- netcomplex(
  employment_additive_component_network_meta_subgroup_low_med_confidence,  
  employment_additive_component_network_meta_subgroup_low_med_confidence$comps
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
  file = "./network-meta/visualisation/output/nma_employment_results_subgroup_high_confidence.png",
  width = 14,
  height = 4,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

forest(employment_additive_component_network_meta_subgroup_high_confidence_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.75, 1.25),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies in which we have high confidence"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -employment_additive_component_network_meta_subgroup_high_confidence_plot_data$Comb.random
  )

dev.off()

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_employment_results_subgroup_low_med_confidence.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

forest(
  employment_additive_component_network_meta_subgroup_low_med_confidence_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.75, 1.25),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies in which we have \nlow or medium confidence"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -employment_additive_component_network_meta_subgroup_low_med_confidence_plot_data$Comb.random
  )

dev.off()

# subgroup analysis by: study location

# subset data by study location
employment_network_meta_random_effects_location_usa_data <- employment_component_es_data %>%
  filter(
    study_location == "United States"
  )

employment_network_meta_random_effects_location_other_data <- employment_component_es_data %>%
  filter(
    !study_location == "United States"
  )

# specify NMA
employment_network_meta_random_effects_subgroup_location_usa <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_location_usa_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

employment_network_meta_random_effects_subgroup_location_other <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_location_other_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

# specify additive NMA
employment_additive_component_network_meta_subgroup_location_usa <- netcomb(
  x = employment_network_meta_random_effects_subgroup_location_usa, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

employment_additive_component_network_meta_subgroup_location_other <- netcomb(
  x = employment_network_meta_random_effects_subgroup_location_other, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

# view results
employment_additive_component_network_meta_subgroup_location_usa_plot_data <- netcomplex(
  employment_additive_component_network_meta_subgroup_location_usa,  
  employment_additive_component_network_meta_subgroup_location_usa$comps
  )

employment_additive_component_network_meta_subgroup_location_other_plot_data <- netcomplex(
  employment_additive_component_network_meta_subgroup_location_other,  
  employment_additive_component_network_meta_subgroup_location_other$comps
  )

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_employment_results_subgroup_location_usa.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  )

forest(
  employment_additive_component_network_meta_subgroup_location_usa_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.75, 1.25),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies conducted in the United States"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -employment_additive_component_network_meta_subgroup_location_usa_plot_data$Comb.random
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
  file = "./network-meta/visualisation/output/nma_employment_results_subgroup_location_other.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

forest(
  employment_additive_component_network_meta_subgroup_location_other_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.75, 1.25),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies conducted in other locations"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -employment_additive_component_network_meta_subgroup_location_other_plot_data$Comb.random
  )

dev.off()

# subgroup analysis by: study population

# subset data by study population
employment_network_meta_random_effects_population_complex_needs_data <- employment_component_es_data %>%
  filter(
    complex_population == "Yes"
  ) 

employment_network_meta_random_effects_population_without_complex_needs_data <- employment_component_es_data %>%
  filter(
    complex_population == "No"
  ) 

# specify NMA
employment_network_meta_random_effects_population_without_complex_needs <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_population_without_complex_needs_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

# specify additive NMA
employment_additive_component_network_meta_population_complex_needs <- discomb(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_population_complex_needs_data, 
  inactive = "SAU",
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

employment_additive_component_network_meta_population_without_complex_needs <- netcomb(
  x = employment_network_meta_random_effects_population_without_complex_needs, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

# view results
employment_additive_component_network_meta_population_complex_needs_plot_data <- netcomplex(
  employment_additive_component_network_meta_population_complex_needs,  
  employment_additive_component_network_meta_population_complex_needs$comps
  )

employment_additive_component_network_meta_population_without_complex_needs_plot_data <- netcomplex(
  employment_additive_component_network_meta_population_without_complex_needs,  
  employment_additive_component_network_meta_population_without_complex_needs$comps
  )

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_employment_results_subgroup_population_complex.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  ) 

forest(
  employment_additive_component_network_meta_population_complex_needs_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.5, 2.5),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies involving young people\nwith reported additional barriers"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -employment_additive_component_network_meta_population_complex_needs_plot_data$Comb.random
  )

dev.off()

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_employment_results_subgroup_population_not_complex.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  )

forest(
  employment_additive_component_network_meta_population_without_complex_needs_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.5, 2.5),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Studies involving young people\nwithout reported additional barriers"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -employment_additive_component_network_meta_population_without_complex_needs_plot_data$Comb.random
  )

dev.off()

# sensitivity analysis

# sensitivity analysis by: study design

# subset data by study design
employment_network_meta_random_effects_randomised_data <- employment_component_es_data %>%
  filter(study_design == "Randomised")

employment_network_meta_random_effects_non_randomised_data <- employment_component_es_data %>%
  filter(study_design == "Non-randomised")

# specify NMA
employment_network_meta_random_effects_subgroup_randomised <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_randomised_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

employment_network_meta_random_effects_subgroup_non_randomised <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = employment_network_meta_random_effects_non_randomised_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE
  )

# specify additive NMA
employment_additive_component_network_meta_subgroup_randomised <- netcomb(
  x = employment_network_meta_random_effects_subgroup_randomised, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

employment_additive_component_network_meta_subgroup_non_randomised <- netcomb(
  x = employment_network_meta_random_effects_subgroup_non_randomised, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE
  )

# view results
employment_additive_component_network_meta_subgroup_randomised_plot_data <- netcomplex(
  employment_additive_component_network_meta_subgroup_randomised,  
  employment_additive_component_network_meta_subgroup_randomised$comps
  )

employment_additive_component_network_meta_subgroup_non_randomised_plot_data <- netcomplex(
  employment_additive_component_network_meta_subgroup_non_randomised,  
  employment_additive_component_network_meta_subgroup_non_randomised$comps
  )

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other"
  )

png(
  file = "./network-meta/visualisation/output/nma_employment_results_sensitivity_randomised.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  )

forest(
  employment_additive_component_network_meta_subgroup_randomised_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Randomised studies"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  drop.reference.group = TRUE,
  sortvar = -employment_additive_component_network_meta_subgroup_randomised_plot_data$Comb.random
  )

dev.off()

component_labels <- c(
  "Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

png(
  file = "./network-meta/visualisation/output/nma_employment_results_sensitivity_non_randomised.png",
  width = 14,
  height = 6,
  units = "cm",
  res = 1200,
  type = "cairo-png"
  )

forest(
  employment_additive_component_network_meta_subgroup_non_randomised_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 7, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Non-randomised studies"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  drop.reference.group = TRUE,
  sortvar = -employment_additive_component_network_meta_subgroup_non_randomised_plot_data$Comb.random
  )

dev.off()