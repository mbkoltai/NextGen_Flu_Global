# FOI calculations with data. 

## need the proportion immune over time
# the vaccination at each point
vaccination_ratio_store2 <- as.data.table(vaccination_ratio_store2)

# calculate teh proportion immune
vaccination_ratio_store2[, prop_immune_1 := prop_v1*prop_Rv1]
vaccination_ratio_store2[, prop_immune_2 := prop_v2*prop_Rv2]
vaccination_ratio_store2[, prop_immune_3 := prop_v3*prop_Rv3]
vaccination_ratio_store2[, prop_immune_4 := prop_v4*prop_Rv4]
vaccination_ratio_store2[, prop_immune_5 := prop_v5*prop_Rv5]
vaccination_ratio_store2[, prop_immune_6 := prop_v6*prop_Rv6]

# # calcuate the relative susceptibility in the 3 age groups of question
vaccination_ratio_store2[, rel_sus_1 := (1-prop_immune_1)]
vaccination_ratio_store2[, rel_sus_2 := (1-prop_immune_2)]
vaccination_ratio_store2[, rel_sus_3 := (1-prop_immune_3)]
vaccination_ratio_store2[, rel_sus_4 := (1-prop_immune_4)]
vaccination_ratio_store2[, rel_sus_5 := (1-prop_immune_5)]
vaccination_ratio_store2[, rel_sus_6 := (1-prop_immune_6)]

# variable is which age group it is
overall_store <- melt.data.table(vaccination_ratio_store2, id.vars = c("Date", "Vacc_scenario", "virus_type" , "week_all" , "week"), 
                                 measure.vars = c("rel_sus_1", "rel_sus_2", "rel_sus_3", "rel_sus_4", "rel_sus_5", "rel_sus_6"))
overall_store[, Date := as.Date(Date, origin = "1970-01-01")]
overall_store[, how_many_weeks := month(Date)]
overall_store[, year := year(Date)]
overall_store[, month_year := paste0(how_many_weeks, "_", year)]
num_weeks_mothnth <- overall_store[, .N, by = c("Vacc_scenario", "virus_type", "variable", "how_many_weeks", "year", "month_year")]
overall_store[num_weeks_mothnth, on = c("Vacc_scenario", "virus_type", "variable", "how_many_weeks", "year", "month_year"), 
              num_weeks := N]


# add the relevant lambda estimate to each row of overall store (with some name accounting included)
overall_store[lambda_estimates, on = "virus_type", foi_poisson := as.numeric(foi)/num_weeks]

# for each subtype and age group, lambda * proportion susceptle / relevant ascenrtainmentr ates (stored in multipier)

# this is subtypes. 
overall_store[virus_type == "AH1N1", infections := (as.numeric(foi_poisson)*value) / H1_multiplier]
overall_store[virus_type == "AH3N2", infections := (as.numeric(foi_poisson)*value) / H3_multiplier]
overall_store[virus_type == "B", infections := (as.numeric(foi_poisson)*value) / B_multiplier]

# now split across age groups acording to size. 
tot_pop <- sum(population2[1,1:6])
overall_store[variable == "rel_sus_1", infections := infections*(population2[1,1]/tot_pop)]
overall_store[variable == "rel_sus_2", infections := infections*(population2[1,2]/tot_pop)]
overall_store[variable == "rel_sus_3", infections := infections*(population2[1,3]/tot_pop)]
overall_store[variable == "rel_sus_4", infections := infections*(population2[1,4]/tot_pop)]
overall_store[variable == "rel_sus_5", infections := infections*(population2[1,5]/tot_pop)]
overall_store[variable == "rel_sus_6", infections := infections*(population2[1,6]/tot_pop)]

FOI_summary <- overall_store[, sum(infections, na.rm = T), by = c("Vacc_scenario","virus_type", "variable")]



