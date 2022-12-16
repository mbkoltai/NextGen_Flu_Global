## FOI for Thailand background

# load in the data
source(here::here("Fitting", "data_for_fitting.R"))
FOI_subset <- all_input_data[, c("date", "fluAH1", "fluAH3", "fluB")]

#specify the dates needed
fluAH1_epidemics[, actual_inclusion := 0]
