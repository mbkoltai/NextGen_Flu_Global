library(fluEvidenceSynthesis)
library(beepr)
# The custom inference function. In this example the custom inference function 
# performs exactly the same inference as the original C++ function (above). 
# It is up to the user to change this in a way that works for their analysis.

source(here::here("Fitting","epidemics.R"))
source(here::here("Fitting","inference_function.R"))
source(here::here("Fitting","data_for_fitting.R"))
source(here::here("Fitting","creating_contacts.R"))

epidemic_to_run <- 1
post_size <- 10000
thinning_steps <- 100
burn_in <- 250000
seed_to_use <- 70
save <- T

set.seed(seed_to_use)

dates_to_run <- c(epidemics_to_fit[[epidemic_to_run]]$start,epidemics_to_fit[[epidemic_to_run]]$end)

vaccine_calendar <- as_vaccination_calendar(efficacy = c(0,0,0,0,0,0), 
                                            dates = dates_to_run,
                                            coverage = matrix(0, nrow = 3,#length(dates_to_run), 
                                                              ncol = 6), 
                                            no_age_groups = 6, no_risk_groups = 1)

# for some reason the function seems to require at least 
initial_parameters <- epidemics_to_fit[[epidemic_to_run]]$initial_params
names(initial_parameters) <- c("reporting", "transmissibility",
                               "susceptibility",
                               "initial_infected")
# ninitial infected to the  power of, reporting is on log scale


output <- custom_inference(input_demography = pop_by_age, 
                 vaccine_calendar = vaccine_calendar, 
                 input_polymod = polymod.thai, 
                 ili = NULL, 
                 mon_pop = NULL, 
                 n_pos = epidemics_to_fit[[epidemic_to_run]]$data_points,
                 n_samples = NULL,
                 initial = initial_parameters,
                 mapping = NULL, 
                 nbatch = post_size, 
                 nburn = burn_in, 
                 blen = thinning_steps)


output_list <- list (
  epidemic_ran = epidemic_to_run,
  posterior = output, 
  post_size = post_size, 
  thinning_steps = thinning_steps, 
  burn_in = burn_in, 
  seed = seed_to_use
)

if(save == T){
saveRDS(output_list, file = here::here("Fitting", "Fits", 
                                        paste0("mcmc_", epidemic_to_run,
                                               "_", post_size, "_", Sys.time(),".Rdata")))
}

beep(4)