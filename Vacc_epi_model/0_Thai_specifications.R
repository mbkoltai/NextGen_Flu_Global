# UK model specifications
#set the file path to the main folder <- change here and all other should be read in. 
setwd("~/Documents/GitHub/NextGen_Flu_Thai/")
library(here)
library(fluEvidenceSynthesis)
library(tidyverse)


####### VARIABLES #####

###### these are for the economics and have not yet been adapted to Thailand
base_scenario_to_use <- 2 # for the economics
vacc_delivery_price <- 15.85#20#12#15.85 # for the economics. 
discount_rate <- 0.035
qaly_discount_rate <- 0.015
threshold <- 20000

# the rest

location <- "Thailand"
posterior_sample_size <- 1000
set.seed(100887)
target_scenarios <- c(1,2,3,4,5,6) # change to required

use_presampled <- F # this is always false for UK - as only have 1000 samples of each
save_samples <- F # this is always false for UK - as only have 1000 samples of each
change_susceptibility_switch <- "OFF" # whether to take account reduction of infection from previous year
name_run <- "default"
create_plots <- "Yes"

num_parameters_posteriors <- 4
transmisibility_location <- 2 # position in posterior of the transmissibility parameter
infection_delays <- c(0.8,1.8)
initial_infection_location <- 4

age_groups_model <- c(2, 6, 12, 18, 60) 
no_risk_groups <- 1
max_age <- 73 # average life expectancy - used only for ageing out of the last compartment
num_age_groups <- 6
end_first_year_vaccination <- as.Date("2006-04-01")
#THESE NEED EXTRACTING
risk_ratios_input <- matrix(c(0, 0, 0, 0, 0, 0, 
                              0, 0, 0, 0, 0, 0),
                            ncol = num_age_groups , byrow = T)
susceptibility_pattern <- c(NA, 3,3,3,3,3)
calculate_FOI <- "Yes" # this should always be No for the UK (as no inter-epidemic period)
num_years <- 4
years <- c(2005:2009)
# load in the inference results from the UK
contact_ids_input <- as.matrix(inference.results.2013[[1]][[1]]$contact.ids[1,])
#THIS NEEDS EXTRACTING
data(polymod_uk)

relevant_polymod <- polymod_uk
age_group_labels <- data.frame(id =c(1:21), 
                               label =rep(c("Age0","Age1-4","Age5-14","Age15-24",
                                        "Age25-44","Age45-64", "Age65+"),3))
risk_group_labels <- data.frame(id =c(1:21), 
                               label =c(rep("Risk_group1",num_age_groups),
                               rep("Risk_group2",num_age_groups),
                               rep("Risk_group3",num_age_groups)))
vaccine_scenario_names <- c("Seasonal (2013)","Seasonal (2019)","Improved (minimal)","Improved (efficacy)","Improved (breadth)",
"Universal")


##### SOURCE AND RUN ######Í

# The vaccination scenario 
source(here::here("Vacc_epi_model","1_0_vaccination_scenario_list_Thai_LSHTM.R"))
#The list of epidemics in Thailand
source(here::here("Vacc_epi_model","2_0_epidemics_list.R"))# needs updating from UK 

# Run all the modelling!
source(here::here("Vacc_epi_model", "0_Main_NextGen.R"))
# Run all the economics!

source(here::here("Economics", "5_0_run_economics.R"))

