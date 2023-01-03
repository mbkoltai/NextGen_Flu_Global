## FOI for Thailand background

# load in the data
source(here::here("Fitting", "data_for_fitting.R"))
FOI_subset <- all_input_data[, c("date", "fluAH1", "fluAH3", "fluB")]

#specify the dates needed
fluAH1_epidemics[, FOI_inclusion := 1]
fluAH3_epidemics[, FOI_inclusion := 1]
fluB_epidemics[, FOI_inclusion := 1]

print("Note: hard-coded at the epidemics at this point - needs altering if epidemics change")


fluAH1_epidemics[date >= as.Date(epidemics_to_fit[[1]]$start) &
                   date <= epidemics_to_fit[[1]]$end, 
                 FOI_inclusion := 0]

fluAH1_epidemics[date >= as.Date(epidemics_to_fit[[2]]$start) &
                   date <= epidemics_to_fit[[2]]$end, 
                 FOI_inclusion := 0]

fluAH3_epidemics[date >= as.Date(epidemics_to_fit[[3]]$start) &
                   date <= epidemics_to_fit[[3]]$end, 
                 FOI_inclusion := 0]

fluAH3_epidemics[date >= as.Date(epidemics_to_fit[[4]]$start) &
                   date <= epidemics_to_fit[[4]]$end, 
                 FOI_inclusion := 0]

fluAH3_epidemics[date >= as.Date(epidemics_to_fit[[5]]$start) &
                   date <= epidemics_to_fit[[5]]$end, 
                 FOI_inclusion := 0]

fluB_epidemics[date >= as.Date(epidemics_to_fit[[6]]$start) &
                   date <= epidemics_to_fit[[6]]$end, 
                 FOI_inclusion := 0]

fluAH1_epidemics[, type := "AH1N1"]
fluAH3_epidemics[, type := "AH3N2"]
fluB_epidemics[, type := "B"]

FOI_data <- rbind(fluAH1_epidemics[FOI_inclusion == 1, c("date", "flu", "type")],
                  fluAH3_epidemics[FOI_inclusion == 1, c("date", "flu", "type")],
                  fluB_epidemics[FOI_inclusion == 1, c("date", "flu", "type")])



lambda_estimates <- data.frame(matrix(nrow = 3, ncol = 2))

for(subtype_num in 1:3){
  subtype <-  c("AH1N1", "AH3N2", "B")[subtype_num]

    lambda <- fitdistr(unlist(FOI_data[type == subtype, "flu"]), densfun="poisson")
    lambda_estimates[subtype_num,] <- c(lambda$estimate, subtype)
  
}

colnames(lambda_estimates) <- c("foi", "virus_type")

# ascertainment rates for later multiplication
H1_multiplier <- mean(exp(ascertainment_H1$posterior_subset...1.))
H3_multiplier <- mean(exp(ascertainment_H3$posterior_subset...1.))
B_multiplier <- mean(exp(ascertainment_B$posterior_subset...1.))
