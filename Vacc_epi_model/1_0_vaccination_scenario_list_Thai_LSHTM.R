# List of the vccination scenarios required

# NOTE: All the dates are releative to a year start date of 2005-01-01

# specify the number of days per year (leap years!) (2005 - 2009)
year_days <- c(365,365,365,366,365)

#find values from literature
# from https://www.cdc.gov/flu/vaccines-work/past-seasons-estimates.html
# assuming SH and NH the same, and differetn viral subtypes the saem

H1_matches <- c(
  "2005_SH" = "NOMATCH",
  "2005_NH" = "NOMATCH",
  "2006_SH" = "MATCH",
  "2006_NH" = "MATCH",
  "2007_SH" = "NOMATCH",
  "2007_NH" = "NOMATCH",
  "2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "MATCH", 
  "2009_NH" = "MATCH",
  "20010_SH" = NA
)
# 
H3_matches <- c(
  "2005_SH" = "NOMATCH",
  "2005_NH" = "NOMATCH",
  "2006_SH" = "MATCH",
  "2006_NH" = "MATCH",
  "2007_SH" = "NOMATCH",
  "2007_NH" = "NOMATCH",
  "2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "MATCH", 
  "2009_NH" = "MATCH",
  "20010_SH" = NA
)
# 
B_matches <- c(
  "2005_SH" = "NOMATCH",
  "2005_NH" = "NOMATCH",
  "2006_SH" = "MATCH",
  "2006_NH" = "MATCH",
  "2007_SH" = "NOMATCH",
  "2007_NH" = "NOMATCH",
  "2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "MATCH", 
  "2009_NH" = "MATCH",
  "20010_SH" = NA
)


efficacies <- list(
  efficacy_constant_0.9 = c(0.9,0.9),
  efficacy_constant_0.7 = c(0.6,0.6),
  efficacy_NOMATCH_0.4 = c(0.6,0.3),
  efficacy_NOMATCH_0.7 = c(0.9,0.6)
)

# start in march as this is the "year end" / "birthday"
campaigns <- list(
  dates_campaign = c("-05-01","-07-31"), # May to July (Making it -07-31, so that 90 days generated in model)
  dates_yearround = c("-03-02") # all year round
)

# this is the coverage over the dates period
# i.e. over one year in the year round scenario, and over the 3 months in the 3 month scenario
coverages <- list(
  coverage_low = c(0.25, 0.25,rep(0,16)),
  coverage_mid = c(0.5, 0.5,rep(0,16)),
  coverage_high = c(0.75, 0.75,rep(0,16))
)

durations <- c(
  1/(365.25*0.5),
  1/(365.25*1),
  1/(365.25*2),
  1/(365.25*3),
  1/(365.25*4),
  1/(365.25*5)
)

vaccine_scenarios <- list()
i <- 1


efficacy_H3 <- matrix(nrow= 18, ncol = 11)
efficacy_H3[1,] <- efficacy_H3[2,] <- efficacy_H3[3,] <- efficacy_H3[4,] <- 
  efficacy_H3[5,] <- efficacy_H3[6,] <- efficacy_H3[7,] <- efficacy_H3[8,] <- 
  efficacy_H3[9,] <- efficacy_H3[10,] <- efficacy_H3[11,] <- efficacy_H3[12,] <- 
  efficacy_H3[13,] <- efficacy_H3[14,] <- efficacy_H3[15,] <- efficacy_H3[16,] <- 
  efficacy_H3[17,] <- efficacy_H3[18,]  <- H3_matches
efficacy_H3_copy <- matrix(nrow= 18, ncol = 11)

efficacy_H1 <- matrix(nrow= 18, ncol = 11)
efficacy_H1[1,] <- efficacy_H1[2,] <- efficacy_H1[3,] <- efficacy_H1[4,] <- 
  efficacy_H1[5,] <- efficacy_H1[6,] <- efficacy_H1[7,] <- efficacy_H1[8,] <- 
  efficacy_H1[9,] <- efficacy_H1[10,] <- efficacy_H1[11,] <- efficacy_H1[12,] <- 
  efficacy_H1[13,] <- efficacy_H1[14,] <- efficacy_H1[15,] <- efficacy_H1[16,] <- 
  efficacy_H1[17,] <- efficacy_H1[18,]  <- H1_matches
efficacy_H1_copy <- matrix(nrow= 18, ncol = 11)

efficacy_B <- matrix(nrow= 18, ncol = 11)
efficacy_B[1,] <- efficacy_B[2,] <- efficacy_B[3,] <- efficacy_B[4,] <- 
  efficacy_B[5,] <- efficacy_B[6,] <- efficacy_B[7,] <- efficacy_B[8,] <- 
  efficacy_B[9,] <- efficacy_B[10,] <- efficacy_B[11,] <- efficacy_B[12,] <- 
  efficacy_B[13,] <- efficacy_B[14,] <- efficacy_B[15,] <- efficacy_B[16,] <- 
  efficacy_B[17,] <- efficacy_B[18,]  <- B_matches
efficacy_B_copy <- matrix(nrow= 18, ncol = 11)

vaccine_scenarios[[1]] <- list(waning_rate = 1, 
                      efficacy_H3 = matrix(0,nrow=18,ncol=10), 
                      efficacy_H1 = matrix(0,nrow=18,ncol=10),
                      efficacy_B = matrix(0,nrow=18,ncol=10), 
                      dates = c("-03-02"),  # all year round
                      coverage = rep(0,18), 
                      prop_group_vacc = rep(0,6))



for(scen_duration in durations){
  for(scen_coverage  in coverages){
    for(scen_campaign in campaigns){
      for(scen_efficacy in efficacies){
        
       efficacy_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(scen_efficacy[1])
       efficacy_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(scen_efficacy[2])
       
       efficacy_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(scen_efficacy[1])
       efficacy_H3_copy[which(efficacy_H3=="NOMATCH")] <- as.numeric(scen_efficacy[2])
       
       efficacy_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(scen_efficacy[1])
       efficacy_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(scen_efficacy[2])
        
       waning_years <- 1/(scen_duration*365.25)
       if(waning_years <1 ) {prop_group_vacc = rep(1, 6)} else{
      prop_group_vacc = c(1/waning_years,1/waning_years,0,0,0,0)}
       
        vaccine_scenarios[[i+1]] = list(
          waning_rate = scen_duration, 
          efficacy_H3 = efficacy_H3_copy,
          efficacy_H1 = efficacy_H1_copy,
          efficacy_B = efficacy_B_copy,
          dates = scen_campaign, 
          coverage = scen_coverage,
          prop_group_vacc = prop_group_vacc
        )
        i <- i+1
      }
    }
  }
}

lookup_year <- names(B_matches)

# if(exact_efficacies==T){
#   source("current_efficacy_sensitivity.R")
# }
