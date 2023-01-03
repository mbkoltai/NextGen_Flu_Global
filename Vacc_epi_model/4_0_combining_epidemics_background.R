### combining background and epidemics components

colnames(overall_store)[which(colnames(overall_store) == "Vacc_scenario")] <- "scenario"
colnames(total_cases_time)[which(colnames(total_cases_time) == "Virus")] <- "virus_type"
overall_store <- overall_store[, c("Date", "scenario", "virus_type", "variable", "infections")]
total_cases_time$scenario <- as.numeric(as.character(total_cases_time$scenario))

temp_list_store <- list()
for(i in 1:n_samples){
  
  overall_store[,sample := i]
  temp_list_store[[i]] <- copy(overall_store)
  
}

overall_store <- rbindlist(temp_list_store)

overall_store[total_cases_time, on = c("Date", "scenario", "virus_type", "sample"), epidemic_cases := i.total_cases]
overall_store[is.na(epidemic_cases), epidemic_cases := 0]
overall_store[, total_infections := infections + epidemic_cases]

#check it's combined and looks cageuly appropiate. 
# ggplot(overall_store, aes(x = Date, y = total_infections, group = sample)) + 
#   geom_line() + 
#   facet_grid(variable~virus_type)

# Need to store it in the way the economics expects
overall_store[variable == "rel_sus_1", variable := "X1"]
overall_store[variable == "rel_sus_2", variable := "X2"]
overall_store[variable == "rel_sus_3", variable := "X3"]
overall_store[variable == "rel_sus_4", variable := "X4"]
overall_store[variable == "rel_sus_5", variable := "X5"]
overall_store[variable == "rel_sus_6", variable := "X6"]
                 
total_infections_all <- dcast.data.table(overall_store, 
                                         Date + scenario + virus_type + sample ~ variable, 
                                         value.var = "total_infections")

total_infections_all[, Year := year(Date)]
colnames(total_infections_all)[which(colnames(total_infections_all) == "virus_type")] <- "Virus"
