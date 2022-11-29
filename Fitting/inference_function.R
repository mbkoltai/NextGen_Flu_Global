
custom_inference <- function(input_demography, vaccine_calendar, input_polymod, ili = NULL, 
                             mon_pop = NULL, n_pos, n_samples, initial, mapping,
                             nbatch, nburn, blen) {
  
  current.contact.ids <- seq(1,nrow(polymod_uk))
  proposed.contact.ids <- current.contact.ids
  
  # Seven age groups used in the model
  age.group.limits <- c(2,6,12,18,60)
  
  # Sum all populations with a certain age into their corresponding age group
  age.group.sizes.5 <- stratify_by_age(demography, age.group.limits)
  
  # if (missing(mapping))
  #   mapping <- age_group_mapping(age.group.limits, c(5,15,45,65))
  
  # Define the actual log likelihood function
  llikelihood <- function( pars ) {
    
    contacts <- fluEvidenceSynthesis::contact_matrix(as.matrix(input_polymod[]),
                                                     input_demography, age.group.limits ) 
    
    age.groups <- stratify_by_age(input_demography, 
                                  age.group.limits )
    
    
    # Population size initially infected by age and risk group
    initial.infected <- rep( 10^pars[4], 6 ) 
    # initial.infected <- stratify_by_risk(
    #   initial.infected, risk.ratios );
    
    # Run simulation
    # Note that to reduce complexity 
    
    # we are using the same susceptibility parameter for multiple age groups
    odes <- infectionODEs(
      population = age.groups,
      vaccine_calendar = vaccine_calendar,
      contact_matrix = contacts,
      susceptibility = c(1, pars[3], pars[3],
                         pars[3], pars[3], pars[3]),
      transmissibility =  pars[2]/100, 
      infection_delays = c(0.8,1.8),
      initial_infected = initial.infected,
      interval = 7)
    
    odes <- data.table(odes)
    # Ignore times row
    odes[,Month := month(Time)]
    odes[,Time := NULL]
    odes[, lapply(.SD, sum, na.rm=TRUE), by="Month" ]
    monthly_cases <-  odes[, sum(.SD, na.rm=TRUE), by="Month" ]
    
    total_ll <- 0
    
    for(i in 1:length(n_pos)){
      
      month_ll <-  dbinom(x = n_pos[i], size = round(as.numeric(monthly_cases[i,"V1"])),
                          prob = exp(pars[1]), log = T)
      
      
      total_ll <- total_ll + month_ll  
      
    }
    return(total_ll)
  }
  llprior <- function(pars) {
    if (any(pars[2:4] < 0) || any(pars[c(3)] > 1)
        || pars[4] < log(0.00001) || pars[4] > 29.5 ) # 29.5 as 0.01% of population
      return(-Inf)
    lprob <- 0
    # lprob <- dnorm(pars[5], 0.1653183, 0.02773053, 1)
    # lprob <- lprob + dlnorm(pars[1], -4.493789, 0.2860455, 1)
    # lprob <- lprob + dlnorm(pars[2], -4.117028, 0.4751615, 1)
    # lprob <- lprob + dlnorm(pars[3], -2.977965, 1.331832, 1)
    
    return(lprob)
  }
  
  # Store the contact ids used during inference
  contact.ids <- list()
  
  # Run adaptive.mcmc
  mcmc.result <- adaptive.mcmc(lprior = llprior, llikelihood = llikelihood, 
                               nburn = nburn, 
                               initial = initial,
                               nbatch = nbatch, blen = blen)
  
  mcmc.result$contact.ids <- t(data.frame(contact.ids))
  mcmc.result
}

