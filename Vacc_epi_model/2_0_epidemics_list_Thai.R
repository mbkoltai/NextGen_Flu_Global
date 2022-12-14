# epidemics_list.R 
source(here::here("Fitting", "data_for_fitting.R"))
source(here::here("Fitting", "epidemics.R"))

no_epidemics <- 6
epidemics_list <- list()

for(i in 1:no_epidemics){
  
  epidemics_list[[i]] <- list( 
    start_date = epidemics_to_fit[[i]]$start, 
    end_date = epidemics_to_fit[[i]]$end,
    year = year(epidemics_to_fit[[i]]$start), 
    flutype = epidemics_to_fit[[i]]$type, 
    previous_epi = NA)
  
}

print("manually assining previous epidemic - needs changing if epidemics change")

epidemics_list[[2]]$previous_epi <- 1
epidemics_list[[4]]$previous_epi <- 3
epidemics_list[[5]]$previous_epi <- 4
