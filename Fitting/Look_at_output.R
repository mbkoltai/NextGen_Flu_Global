library(fluEvidenceSynthesis)
library(ggplot2)

# sepcify how many samples to take for spaghetti plots
n_samples <- 100
# can use this to load one
#load(file = here::here("Fitting", "Fits", "mcmc_1_1000_2022-11-28 13:41:41 GMT"))

colnames(output_list$posterior$batch) <- c("reporting", "transmissibility", "sus1",
                                       "infected", "blank", "blank")
post_samples <- data.table(output_list$posterior$batch)
# remove the blank ones
post_samples[, blank := NULL ]
post_samples[, blank := NULL ]
# add the likelihoods
post_samples[, ll := output_list$posterior$llikelihoods]
post_samples$timestep <- 1:nrow(post_samples)
post_samples_m <- melt.data.table(post_samples, id.vars = "timestep")

DENSITY <- ggplot(post_samples_m) + 
  facet_wrap( ~ variable, ncol=3, scales="free" ) +
  geom_histogram(aes(x=value), bins=25)
DENSITY

TRACE_THINNED <- ggplot(post_samples_m, aes(x = timestep, y = value)) + 
  facet_grid(variable~., scales = "free_y") + 
  geom_line()

TRACE_THINNED

# Function that runs the model given a set of parameters. Most of the function
# has todo with loading the correct inputs for the ODE model
ode.results <- function( pars, input_demography = pop_by_age ) 
{
  
  age.group.limits <- c(2,6,12,18,60)
 
  contacts <- fluEvidenceSynthesis::contact_matrix(as.matrix(polymod.thai),
                                                   input_demography, age.group.limits ) 
  
  age.groups <- stratify_by_age(input_demography, age.group.limits)
  # Ppulation size initially infected by age and risk group
  initial.infected <- rep( 10^pars[4], 6 ) 

  # Run simulation
  # Note that to reduce complexity 
  # by using the same susceptibility parameter for multiple age groups
  odes <- infectionODEs(
    population = age.groups,
    vaccine_calendar = vaccine_calendar,
    contact_matrix = contacts,
    susceptibility = c(1, pars[3], pars[3],
                       pars[3], pars[3], pars[3]),
    transmissibility = pars[2]/100, 
    infection_delays = c(0.8,1.8),
    initial_infected = initial.infected,
    interval = 7)

  # For simplicity we sum the low and high risk group

  odes <- data.table(odes)
  # Ignore times row
  odes[,Month := month(Time)]
  odes[,Time := NULL]
  odes[, lapply(.SD, sum, na.rm=TRUE), by="Month" ]
  monthly_cases <-  odes[, sum(.SD, na.rm=TRUE), by="Month" ]

  return( monthly_cases )}


sample_no<- sample(c(1:nrow(post_samples)), 1)
out_run <- list()

for(i in 1:n_samples){
  
  test_params <- post_samples[sample_no[i],]
  temp_out <- ode.results(unlist(test_params))
  temp_out$timestep <- 1:nrow(temp_out)
  temp_out$V1 <-temp_out$V1* exp(post_samples[sample_no[i],1])
  out_run[[i]] <- temp_out 
  
}

 spaghetti_out <- rbindlist(out_run, use.names = T, idcol = "sample")

 data_fitted <- data.frame(V1 = epidemics_to_fit[[output_list$epidemic_ran]]$data_points)
 data_fitted$timestep <- 1:nrow(data_fitted)                    
 
print(
  ggplot(spaghetti_out, aes(x = timestep, y = V1, group = sample)) + 
  geom_line(alpha = 0.5)+ 
    geom_point(data = data_fitted,aes(x = timestep, y = V1, group =NULL), colour = "red")
  )


ggplot(data = data_fitted) + 
  geom_point(data = data_fitted,aes(x = timestep, y = V1))

# c(-7, 0.07, 0.9, 0.9, 0.9, 0.4)# gives an outbreak
