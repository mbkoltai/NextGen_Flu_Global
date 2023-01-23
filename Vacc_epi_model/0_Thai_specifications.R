# UK model specifications
#set the file path to the main folder <- change here and all other should be read in. 
setwd("~/Documents/GitHub/NextGen_Flu_Thai/")
library(here)
library(fluEvidenceSynthesis)
library(tidyverse)
library(data.table)
library(MASS)
library(ggplot2)
library(fluEvidenceSynthesis)
library(plyr)
library(reshape2)
library(ISOweek)
library(qs)
library(gridExtra)
library(grid)
library(wpp2019)
if("tictoc" %in% (.packages())){
  detach("package:tictoc", unload=TRUE) 
}



####### VARIABLES #####

###### these are for the economics and have not yet been adapted to Thailand
base_scenario_to_use <- 2 # for the economics
vacc_delivery_price <- 15.85#20#12#15.85 # for the economics. 
discount_rate <- 0.035
qaly_discount_rate <- 0.015
threshold <- 20000

# the rest

location <- "Thailand"
posterior_sample_size <- 10
set.seed(100887)
target_scenarios <- c(1,4,28,53,75,122) # change to required

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
max_age <- 77 # average life expectancy - used only for ageing out of the last compartment
num_age_groups <- 6
end_first_year_vaccination <- as.Date("2006-04-01") #CHANGE
high_risk <- rep(0,6)
risk_ratios_input <- matrix(c(high_risk,0,0,0,0,0,0),
                            ncol = num_age_groups , byrow = T)
susceptibility_pattern <- c(NA, 3,3,3,3,3)
calculate_FOI <- "Yes" # this should always be No for the UK (as no inter-epidemic period)
num_years <- 4
years <- c(2005:2010)
#     v3:<2 yr,  v4:2-5 yrs,  v5:6-11 yrs,  v6:12-17 yrs, v7:18-59 yrs, v8:>=60 yrs
# load in contacts etc. 
source(here::here("Fitting", "creating_contacts.R"))
contact_ids_input <-NA # not required as using a fixed contact matrix
relevant_polymod <- polymod.thai

age_group_labels <- data.frame(id =c(1:18), 
                               label =rep(c("Age0-1","Age2-5","Age6-11","Age12-17",
                                        "Age18-59","Age60+"),3))
risk_group_labels <- data.frame(id =c(1:18), 
                               label =c(rep("Risk_group1",num_age_groups),
                               rep("Risk_group2",num_age_groups),
                               rep("Risk_group3",num_age_groups)))
vaccine_scenario_names <- c("No vaccine","Current seasonal","Improved (minimal)","Improved (efficacy)","Improved (breadth)",
"Universal")


##### SOURCE AND RUN ######Í

# The vaccination scenario 
source(here::here("Vacc_epi_model","1_0_vaccination_scenario_list_Thai_LSHTM.R"))
#The list of epidemics in Thailand
source(here::here("Vacc_epi_model","2_0_epidemics_list_thai.R"))# needs updating from UK 

# Run all the modelling!
source(here::here("Vacc_epi_model", "0_Main_NextGen.R"))
# Run all the economics!

source(here::here("Economics", "5_0_run_economics.R"))

