## NMA EDUCATION RESULTS 

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(readxl)
library(meta)
library(netmeta)
library(dmetar)
library(ggplot2)
library(scales)
library(viscomp)

## RUN CODE THAT PRODUCES INPUT FOR THIS ANALYSIS

#source("./data-cleaning/code/component_definitions.R")
#source("./data-cleaning/code/quality_appraisal.R")
#source("./data-cleaning/code/education_misc.R")
#source("./es-transformation/code/es_transformation_education.R")
#source("./data-cleaning/code/education_clean_merge.R")

## LOAD REQUIRED DATA

education_component_specification_three_es_data_location <- "./nma/meta-data/education_analysis_data_specification_three.RDS"
education_component_specification_three_es_data <- readRDS(education_component_specification_three_es_data_location)

## INSPECT NETWORK STRUCTURE

education_connections_specification_three <- netconnection(
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_component_specification_three_es_data, 
  nchar.trts = 30)

## FIT STANDARD NMA

education_network_meta_random_effects_specification_three <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_component_specification_three_es_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE)

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

png(file = "./visualisation/output/nma_education_standard_nma_specification_three.png",
    width = 29,
    height = 10,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

forest(education_network_meta_random_effects_specification_three,
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

## VISUALISE NETWORK STRUCTURE

set.seed(368)

png(file = "./visualisation/output/nma_education_netgraph_specification_three.png",
    width = 1000,
    height = 600,
    type = "cairo-png") 

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
  education_network_meta_random_effects_specification_three,
  plastic = FALSE,
  col = "#7D2248",
  number.of.studies = TRUE,
  cex.number = 1,
  pos.number.of.studies = 0.7,
  labels = network_labels)

dev.off()

## VISUALISE COMPONENTS INCLUDED IN NMA

education_nma_component_visualisation <- compdesc(
  model = education_network_meta_random_effects_specification_three,
  sep = "+") 

education_nma_component_heatmap <- education_nma_component_visualisation$heatmat +
  theme(
    plot.background = element_rect(fill = "white")
  )

## EXPORT PLOT

ggsave(
  file = "./visualisation/output/nma_education_component_frequency.png",
  plot = education_nma_component_heatmap, 
  width = 10, 
  height = 7, 
  type = "cairo-png")

## COUNT NUMBER OF HIGH CONFIDENCE STUDIES IN SAMPLE

education_component_specification_three_es_data %>%
  filter(study_quality == "High confidence")

## PRINT TABLE OF RESULTS

netleague(
  x = education_network_meta_random_effects_specification_three,
  direct = TRUE,
  digits = 2,
  overwrite = TRUE,
  path = "./visualisation/output/nma_education_results.xlsx")

## ASSESSING PUBLICATION BIAS

png(file = "./visualisation/output/nma_education_standard_nma_specification_three_publication_bias.png",
    width = 26,
    height = 16,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

order = c("SAU")

funnel(
  x = education_network_meta_random_effects_specification_three,
  order = order,
  pooled = "random",
  lump.comparator = TRUE,
  text.comparator = "SAU",
  pch = c(11:21), 
  col = c(11:21),
  text.linreg = "(Egger)",
  method.bias = c("Egger"), digits.pval = 2)

dev.off()

## RUN EGGER'S TEST

input_eggers <- funnel(
  x = education_network_meta_random_effects_specification_three,
  order = order,
  pooled = "random",
  lump.comparator = TRUE,
  text.comparator = "SAU")

metabias(metagen(TE.adj, seTE, data = input_eggers))

## EVALUATING LOCAL INCOHERENCE

## IMPLEMENT NODE SPLITTING

## Note: there is no mixed evidence in this specification, so node splitting is redundant

# education_network_meta_random_effects_specification_three_node_split <- netsplit(
#   education_network_meta_random_effects_specification_three) 

## EXPORT FOREST PLOT
# 
# png(file = "./visualisation/output/nma_education_side.png",
#     width = 21,
#     height = 31,
#     units = "cm",
#     res = 1200,
#     type = "cairo-png") 
# 
# forest(
#   education_network_meta_random_effects_specification_three_node_split,
#   sortvar = -education_network_meta_random_effects_specification_three_node_split$k,
#   show = "all", # only show combinations with both direct and indirect evidence,
#   leftlabs = c("Component \ncombinations", "Number \nof Studies", "Prop. direct\n evidence", "I\n squared")
#   )
# 
# dev.off()

## EVALUATING GLOBAL INCOHERENCE 

## Note: there is no mixed evidence in this specification, so this  is redundant

# education_network_meta_random_effects_specification_three_decomp <- decomp.design(education_network_meta_random_effects_specification_three)
# 
# education_network_meta_random_effects_specification_three_decomp$Q.inc.random %>%
#   rownames_to_column() %>%
#   mutate(
#     Q = round(Q, 3),
#     pval = round(pval, 3),
#     tau.within = round(tau.within, 3)
#   ) %>%
#   rename(
#     Specification = rowname,
#     `p-value` = pval,
#     `Square-root of between study variance` = tau.within)
                                   
## FIT ADDITIVE MODEL

education_additive_component_network_meta_specification_three <- netcomb(
  x = education_network_meta_random_effects_specification_three, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE)

education_additive_component_network_meta_specification_three_plot_data <- netcomplex(
  education_additive_component_network_meta_specification_three,  
  education_additive_component_network_meta_specification_three$comps)

png(file = "./visualisation/output/nma_education_additive.png",
    width = 17,
    height = 6,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

forest(
  education_additive_component_network_meta_specification_three_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.5, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Programme Components vs. Usual Services \n",                   "(High school (or equiv.) completion)"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  drop.reference.group = TRUE,
  leftlabs = "Intervention component",
  sortvar = -education_additive_component_network_meta_specification_three_plot_data$Comb.random
)

dev.off()

## FIT INTERACTION COMPONENT NMAs

## INTERACTION: ON-JT+OTH

education_interaction_matrix_specification_three_ojt_oth <- cbind(
  education_additive_component_network_meta_specification_three$C.matrix,
  `ON-JT+OTH` = 0)
education_interaction_matrix_specification_three_ojt_oth["ON-JT+OTH", "ON-JT+OTH"] <- 1

education_interaction_component_network_meta_specification_three_ojt_oth <- netcomb(
  education_network_meta_random_effects_specification_three, 
  C.matrix = education_interaction_matrix_specification_three_ojt_oth, 
  inactive = "SAU")

education_interaction_component_network_meta_specification_three_ojt_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_specification_three_ojt_oth,  
  education_interaction_component_network_meta_specification_three_ojt_oth$comps)

## EXPORT FOREST PLOT

png(file = "./visualisation/output/nma_education_interaction_onjt_oth.png",
    width = 17,
    height = 6,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

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
  education_interaction_component_network_meta_specification_three_ojt_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Programme Components vs. Usual Services \n",                   "(High school (or equiv.) completion)"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_specification_three_ojt_oth_plot_data$Comb.random
) 

dev.off()

## INTERACTION: BS+OTH

education_interaction_matrix_specification_three_bs_oth <- cbind(
  education_additive_component_network_meta_specification_three$C.matrix,
  `BS+OTH` = 0)
education_interaction_matrix_specification_three_bs_oth["BS+OTH", "BS+OTH"] <- 1

education_interaction_component_network_meta_specification_three_bs_oth <- netcomb(
  education_network_meta_random_effects_specification_three, 
  C.matrix = education_interaction_matrix_specification_three_bs_oth, 
  inactive = "SAU")

education_interaction_component_network_meta_specification_three_bs_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_specification_three_bs_oth,  
  education_interaction_component_network_meta_specification_three_bs_oth$comps)

## EXPORT FOREST PLOT

png(file = "./visualisation/output/nma_education_interaction_bs_oth.png",
    width = 17,
    height = 6,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

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
  education_interaction_component_network_meta_specification_three_bs_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Programme Components vs. Usual Services \n",                   "(High school (or equiv.) completion)"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_specification_three_bs_oth_plot_data$Comb.random
) 

dev.off()

## INTERACTION: LS+OTH

education_interaction_matrix_specification_three_ls_oth <- cbind(
  education_additive_component_network_meta_specification_three$C.matrix,
  `LS+OTH` = 0)
education_interaction_matrix_specification_three_ls_oth["LS+OTH", "LS+OTH"] <- 1

education_interaction_component_network_meta_specification_three_ls_oth <- netcomb(
  education_network_meta_random_effects_specification_three, 
  C.matrix = education_interaction_matrix_specification_three_ls_oth, 
  inactive = "SAU")

education_interaction_component_network_meta_specification_three_ls_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_specification_three_ls_oth,  
  education_interaction_component_network_meta_specification_three_ls_oth$comps)

## EXPORT FOREST PLOT

png(file = "./visualisation/output/nma_education_interaction_ls_oth.png",
    width = 17,
    height = 6,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

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
  education_interaction_component_network_meta_specification_three_ls_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Programme Components vs. Usual Services \n",                   "(High school (or equiv.) completion)"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_specification_three_ls_oth_plot_data$Comb.random
) 

dev.off()

## INTERACTION: C&M+OTH 

education_interaction_matrix_specification_three_cm_oth <- cbind(
  education_additive_component_network_meta_specification_three$C.matrix,
  `C&M+OTH` = 0)
education_interaction_matrix_specification_three_cm_oth["C&M+OTH", "C&M+OTH"] <- 1

education_interaction_component_network_meta_specification_three_cm_oth <- netcomb(
  education_network_meta_random_effects_specification_three, 
  C.matrix = education_interaction_matrix_specification_three_cm_oth, 
  inactive = "SAU")

education_interaction_component_network_meta_specification_three_cm_oth_plot_data <- netcomplex(
  education_interaction_component_network_meta_specification_three_cm_oth,  
  education_interaction_component_network_meta_specification_three_cm_oth$comps)

## EXPORT FOREST PLOT

png(file = "./visualisation/output/nma_education_interaction_cm_oth.png",
    width = 17,
    height = 6,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

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
  education_interaction_component_network_meta_specification_three_cm_oth_plot_data,
  studlab = component_labels,
  reference.group = "SAU",
  drop.reference.group = TRUE,
  col.by = "black",
  xlim = c(-1, 1),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Programme Components vs. Usual Services \n",                   "(High school (or equiv.) completion)"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  sortvar = -education_interaction_component_network_meta_specification_three_cm_oth_plot_data$Comb.random
) 

dev.off()

## Compare results between different NMA specifications

png(file = "./visualisation/output/nma_education_results_comparison.png",
    width = 16,
    height = 26,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

education_specification_three_forest_plot_data <- netbind(
  random = TRUE,
  common = FALSE,
  education_network_meta_random_effects_specification_three, 
  education_additive_component_network_meta_specification_three, 
  education_interaction_component_network_meta_specification_three_ls_oth, 
  name = c("Standard NMA", "Additive cNMA", "Life Skills x Other Interaction cNMA"),
  col.study = c("#7D2248", "#BFB800", "#69C2C9"),
  col.square = c("#7D2248", "#BFB800", "#69C2C9"))

forest(education_specification_three_forest_plot_data,
  reference.group = "SAU",
  col.by = "black",
  xlim = c(-0.75, 1.25),
  addrow.subgroups = FALSE,
  fontsize = 10, 
  spacing = 0.7, 
  squaresize = 0.9,
  smlab = paste("Education Programme Components vs. Usual Services \n",                   "(High school (or equiv.) completion)"),
  label.left = "Favours Usual Services",
  label.right = "Favours Intervention",
  leftlabs = "Intervention component",
  drop.reference.group = TRUE)

dev.off()

## SUBGROUP ANALYSIS

### STUDY CONFIDENCE

## SUBSET DATA BY STUDY CONFIDENCE

education_network_meta_random_effects_specification_three_high_confidence_data <- education_component_specification_three_es_data %>%
  filter(study_quality == "High confidence")

education_network_meta_random_effects_specification_three_low_med_confidence_data <- education_component_specification_three_es_data %>%
  mutate(
    relevant_studies = case_when(
      str_detect(study_quality, "Low confidence") ~ "Low or Medium confidence",
      str_detect(study_quality, "Medium confidence") ~ "Low or Medium confidence")
  ) %>%
  filter(relevant_studies == "Low or Medium confidence") 

# Stop, there are only nine studies that use high-quality designs. This analysis is not possible. 

### STUDY LOCATION

## SUBSET DATA BY STUDY LOCATION

education_network_meta_random_effects_specification_three_location_usa_data <- education_component_specification_three_es_data %>%
  filter(study_location == "United States") 

education_network_meta_random_effects_specification_three_location_other_data <- education_component_specification_three_es_data %>%
  filter(!study_location == "United States") 

# Stop, there is only one study from outside the United States. This analysis is not possible. 

### STUDY POPULATION

## SUBSET DATA BY STUDY POPULATION

education_network_meta_random_effects_specification_three_population_complex_needs_data <- education_component_specification_three_es_data %>%
  filter(complex_population == "Yes") 

education_network_meta_random_effects_specification_three_population_without_complex_needs_data <- education_component_specification_three_es_data %>%
  filter(complex_population == "No") 

## SPECIFY NMA

education_network_meta_random_effects_specification_three_population_without_complex_needs <- netmeta(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_network_meta_random_effects_specification_three_population_without_complex_needs_data, 
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE)

## SPECIFY ADDITIVE NMA

education_additive_component_network_meta_specification_three_population_complex_needs <- discomb(
  TE = es,
  seTE = se,
  treat1 = intervention, 
  treat2 = comparison, 
  studlab = study,
  data = education_network_meta_random_effects_specification_three_population_complex_needs_data, 
  inactive = "SAU",
  reference.group = "SAU",
  sm = "SMD", 
  nchar.trts = 30,
  common = FALSE,
  drop.reference.group = TRUE)

education_additive_component_network_meta_specification_three_population_without_complex_needs <- netcomb(
  x = education_network_meta_random_effects_specification_three_population_without_complex_needs, 
  inactive = "SAU",
  sep.comps = "+",
  random = TRUE)

## VIEW RESULTS

education_additive_component_network_meta_specification_three_population_complex_needs_plot_data <- netcomplex(
  education_additive_component_network_meta_specification_three_population_complex_needs,  
  education_additive_component_network_meta_specification_three_population_complex_needs$comps)

education_additive_component_network_meta_specification_three_population_without_complex_needs_plot_data <- netcomplex(
  education_additive_component_network_meta_specification_three_population_without_complex_needs,  
  education_additive_component_network_meta_specification_three_population_without_complex_needs$comps)

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  #"On-the-job Training", 
  "Other")

png(file = "./visualisation/output/nma_education_results_subgroup_population_complex.png",
    width = 14,
    height = 6,
    units = "cm",
    res = 1200,
    type = "cairo-png") 

forest(education_additive_component_network_meta_specification_three_population_complex_needs_plot_data,
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
       sortvar = -education_additive_component_network_meta_specification_three_population_complex_needs_plot_data$Comb.random)

dev.off()

component_labels <- c(
  #"Apprenticeships", 
  "Basic Skills", 
  "Coaching & Mentoring", 
  "Life Skills", 
  "Off-the-job Training", 
  "On-the-job Training", 
  "Other")

png(file = "./visualisation/output/nma_education_results_subgroup_population_not_complex.png",
    width = 14,
    height = 6,
    units = "cm",
    res = 1200,
    type = "cairo-png")

forest(education_additive_component_network_meta_specification_three_population_without_complex_needs_plot_data,
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
       sortvar = -education_additive_component_network_meta_specification_three_population_without_complex_needs_plot_data$Comb.random)

dev.off()

## SENSITIVITY ANALYSIS

## STUDY DESIGN

## SUBSET DATA BY STUDY DESIGN

education_network_meta_random_effects_specification_three_randomised_data <- education_component_specification_three_es_data %>%
  filter(study_design == "Randomised") 

education_network_meta_random_effects_specification_three_non_randomised_data <- education_component_specification_three_es_data %>%
  filter(study_design == "Non-randomised") 

# Stop, there are only three studies that use non-randomised designs. This analysis is not possible. 
