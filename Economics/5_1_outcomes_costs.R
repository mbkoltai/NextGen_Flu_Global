#### Economic analysis - calculating the basics

# load in saved epi model output file if want to run econ alone
#load(file = here::here("UK_output", "total_cases_time.Rdata"))

#### sections in this script are:
# - calculating annual number of deaths, hospitalizations and GP consultations cromer
# - calculating annual number of illness cases (non-febrile (ARI-fev) and fever (fev - death - hosp))
# - calulating annual non-death YLDs
# - calculating annual costs

# Conversion of costs from Meeyai et al
# Cost inputs are given in I$ so convert tto lcu using 2012 IMF PPP factor of 17.76 reported in the paper <----- this is incorrect, the inputs in the supplement are actualy in Baht
# Inflate using GDP deflator to 2021 lcu
# Convert to USD using exchange rate for 2021
inflator <- 1 * 156/139 * 1/31.98   # WB PPP conversion factor for 2021 is 12.04
# inflator <- 1

seasons <- c("2005to2006","2006to2007","2007to2008","2008to2009")
ages <- c("[0,2)","[2,6)","[6,12)","[12,18)","[18,60)","[60,+)")
viruses <- c("AH1N1","AH3N2","B")

popthai.grouped = as.data.table(cbind(
  age = ages,
  pop = as.vector(unlist(population2[1,1:6]))
))

####### Annual infections ######
total_cases_time <- total_infections_all
total_cases_time[Date>="2005-03-01" & Date<"2006-03-01",Season:="2005to2006"]
total_cases_time[Date>="2006-03-01" & Date<"2007-03-01",Season:="2006to2007"]
total_cases_time[Date>="2007-03-01" & Date<"2008-03-01",Season:="2007to2008"]
total_cases_time[Date>="2008-03-01" & Date<"2009-03-01",Season:="2008to2009"]
total_cases_time[, Date := NULL]


cases_sample_year <- total_cases_time[, lapply(.SD, sum, na.rm=T), by = c("sample", "scenario", "Season", "Virus")]
cases_sample_year[, Year := NULL]
cases_sample_year <- cases_sample_year[!is.na(Season)] # drop data not in seasons of interest
cases_sample_year_m <- data.table(melt(cases_sample_year, value.name="infections", id.vars = c("sample", "scenario", "Season", "Virus")))
cases_sample_year_m[variable =="X1", age := "[0,2)"]
cases_sample_year_m[variable =="X2", age := "[2,6)"]
cases_sample_year_m[variable =="X3", age := "[6,12)"]
cases_sample_year_m[variable =="X4", age := "[12,18)"]
cases_sample_year_m[variable =="X5", age := "[18,60)"]
cases_sample_year_m[variable =="X6", age := "[60,+)"]
cases_sample_year_m[,variable := NULL]

###### Annual deaths per case #######
# rates per 100,000 person years 

# # these were modeled as normal distributions with the negative values treated as zero
# deaths.B2005to2006.0to18 <- rnorm(n_samples, 1.40, 3.20)
# deaths.B2006to2007.0to18 <- rnorm(n_samples, 1.37, 2.89)
# deaths.B2007to2008.0to18 <- rnorm(n_samples, 4.19, 5.17)
# deaths.B2008to2009.0to18 <- rnorm(n_samples, 2.95, 3.86)
# 
# deaths.B2005to2006.18to59 <- rnorm(n_samples, -1.21, 1.54)
# deaths.B2006to2007.18to59 <- rnorm(n_samples, -0.84, 0.94)
# deaths.B2007to2008.18to59 <- rnorm(n_samples, -2.84, 1.99)
# deaths.B2008to2009.18to59 <- rnorm(n_samples, -1.71, 1.73)
# 
# deaths.B2005to2006.60plus <- rnorm(n_samples, 61.42, 32.82)
# deaths.B2006to2007.60plus <- rnorm(n_samples, 31.64, 25.48)
# deaths.B2007to2008.60plus <- rnorm(n_samples, 65.34, 58.03)
# deaths.B2008to2009.60plus <- rnorm(n_samples, 37.66, 58.69)
# 
# deaths.AH1N12005to2006.0to18 <- rnorm(n_samples, 0.16, 0.40)
# deaths.AH1N12006to2007.0to18 <- rnorm(n_samples, 0.80, 2.88)
# deaths.AH1N12007to2008.0to18 <- rnorm(n_samples, 0.33, 2.04)
# deaths.AH1N12008to2009.0to18 <- rnorm(n_samples, 0.43, 2.39)
# 
# deaths.AH1N12005to2006.18to59 <- rnorm(n_samples, 0.24, 0.27)
# deaths.AH1N12006to2007.18to59 <- rnorm(n_samples, 1.83, 1.10)
# deaths.AH1N12007to2008.18to59 <- rnorm(n_samples, 1.03, 0.89)
# deaths.AH1N12008to2009.18to59 <- rnorm(n_samples, 2.33, 1.21)
# 
# deaths.AH1N12005to2006.60plus <- rnorm(n_samples, 11.58, 10.97)
# deaths.AH1N12006to2007.60plus <- rnorm(n_samples, 44.95, 41.68)
# deaths.AH1N12007to2008.60plus <- rnorm(n_samples, 134.00, 61.71)
# deaths.AH1N12008to2009.60plus <- rnorm(n_samples, 1.11, 58.69)   #co-incidence or typo in table?
# 
# deaths.AH3N22005to2006.0to18 <- rnorm(n_samples, -1.45, 3.66)
# deaths.AH3N22006to2007.0to18 <- rnorm(n_samples, 2.21, 2.57)
# deaths.AH3N22007to2008.0to18 <- rnorm(n_samples, -0.33, 2.92)
# deaths.AH3N22008to2009.0to18 <- rnorm(n_samples, 1.79, 3.25)
# 
# deaths.AH3N22005to2006.18to59 <- rnorm(n_samples, 0.39, 1.60)
# deaths.AH3N22006to2007.18to59 <- rnorm(n_samples, 1.68, 1.08)
# deaths.AH3N22007to2008.18to59 <- rnorm(n_samples, 0.64, 1.59)
# deaths.AH3N22008to2009.18to59 <- rnorm(n_samples, 2.49, 1.72)
# 
# deaths.AH3N22005to2006.60plus <- rnorm(n_samples, 2.72, 34.32)
# deaths.AH3N22006to2007.60plus <- rnorm(n_samples, 21.46, 27.41)
# deaths.AH3N22007to2008.60plus <- rnorm(n_samples, -3.15, 34.22)
# deaths.AH3N22008to2009.60plus <- rnorm(n_samples, 15.89, 33.15)
# 
# 
# deaths <- rbind(
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[0,2)", Season="2005to2006", death.rate = deaths.B2005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[0,2)", Season="2006to2007", death.rate = deaths.B2006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[0,2)", Season="2007to2008", death.rate = deaths.B2007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[0,2)", Season="2008to2009", death.rate = deaths.B2008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[2,6)", Season="2005to2006", death.rate = deaths.B2005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[2,6)", Season="2006to2007", death.rate = deaths.B2006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[2,6)", Season="2007to2008", death.rate = deaths.B2007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[2,6)", Season="2008to2009", death.rate = deaths.B2008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[6,12)", Season="2005to2006", death.rate = deaths.B2005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[6,12)", Season="2006to2007", death.rate = deaths.B2006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[6,12)", Season="2007to2008", death.rate = deaths.B2007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[6,12)", Season="2008to2009", death.rate = deaths.B2008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[12,18)", Season="2005to2006", death.rate = deaths.B2005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[12,18)", Season="2006to2007", death.rate = deaths.B2006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[12,18)", Season="2007to2008", death.rate = deaths.B2007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[12,18)", Season="2008to2009", death.rate = deaths.B2008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[18,60)", Season="2005to2006", death.rate = deaths.B2005to2006.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[18,60)", Season="2006to2007", death.rate = deaths.B2006to2007.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[18,60)", Season="2007to2008", death.rate = deaths.B2007to2008.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[18,60)", Season="2008to2009", death.rate = deaths.B2008to2009.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[60,+)", Season="2005to2006", death.rate = deaths.B2005to2006.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[60,+)", Season="2006to2007", death.rate = deaths.B2006to2007.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[60,+)", Season="2007to2008", death.rate = deaths.B2007to2008.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "B", age = "[60,+)", Season="2008to2009", death.rate = deaths.B2008to2009.60plus),
#   
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[0,2)", Season="2005to2006", death.rate   = deaths.AH1N12005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[0,2)", Season="2006to2007", death.rate   = deaths.AH1N12006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[0,2)", Season="2007to2008", death.rate   = deaths.AH1N12007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[0,2)", Season="2008to2009", death.rate   = deaths.AH1N12008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[2,6)", Season="2005to2006", death.rate   = deaths.AH1N12005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[2,6)", Season="2006to2007", death.rate   = deaths.AH1N12006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[2,6)", Season="2007to2008", death.rate   = deaths.AH1N12007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[2,6)", Season="2008to2009", death.rate   = deaths.AH1N12008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[6,12)", Season="2005to2006", death.rate  = deaths.AH1N12005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[6,12)", Season="2006to2007", death.rate  = deaths.AH1N12006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[6,12)", Season="2007to2008", death.rate  = deaths.AH1N12007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[6,12)", Season="2008to2009", death.rate  = deaths.AH1N12008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[12,18)", Season="2005to2006", death.rate = deaths.AH1N12005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[12,18)", Season="2006to2007", death.rate = deaths.AH1N12006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[12,18)", Season="2007to2008", death.rate = deaths.AH1N12007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[12,18)", Season="2008to2009", death.rate = deaths.AH1N12008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[18,60)", Season="2005to2006", death.rate = deaths.AH1N12005to2006.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[18,60)", Season="2006to2007", death.rate = deaths.AH1N12006to2007.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[18,60)", Season="2007to2008", death.rate = deaths.AH1N12007to2008.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[18,60)", Season="2008to2009", death.rate = deaths.AH1N12008to2009.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[60,+)", Season="2005to2006", death.rate  = deaths.AH1N12005to2006.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[60,+)", Season="2006to2007", death.rate  = deaths.AH1N12006to2007.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[60,+)", Season="2007to2008", death.rate  = deaths.AH1N12007to2008.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "AH1N1", age = "[60,+)", Season="2008to2009", death.rate  = deaths.AH1N12008to2009.60plus),
#   
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[0,2)", Season="2005to2006", death.rate   = deaths.AH3N22005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[0,2)", Season="2006to2007", death.rate   = deaths.AH3N22006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[0,2)", Season="2007to2008", death.rate   = deaths.AH3N22007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[0,2)", Season="2008to2009", death.rate   = deaths.AH3N22008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[2,6)", Season="2005to2006", death.rate   = deaths.AH3N22005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[2,6)", Season="2006to2007", death.rate   = deaths.AH3N22006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[2,6)", Season="2007to2008", death.rate   = deaths.AH3N22007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[2,6)", Season="2008to2009", death.rate   = deaths.AH3N22008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[6,12)", Season="2005to2006", death.rate  = deaths.AH3N22005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[6,12)", Season="2006to2007", death.rate  = deaths.AH3N22006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[6,12)", Season="2007to2008", death.rate  = deaths.AH3N22007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[6,12)", Season="2008to2009", death.rate  = deaths.AH3N22008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[12,18)", Season="2005to2006", death.rate = deaths.AH3N22005to2006.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[12,18)", Season="2006to2007", death.rate = deaths.AH3N22006to2007.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[12,18)", Season="2007to2008", death.rate = deaths.AH3N22007to2008.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[12,18)", Season="2008to2009", death.rate = deaths.AH3N22008to2009.0to18),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[18,60)", Season="2005to2006", death.rate = deaths.AH3N22005to2006.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[18,60)", Season="2006to2007", death.rate = deaths.AH3N22006to2007.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[18,60)", Season="2007to2008", death.rate = deaths.AH3N22007to2008.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[18,60)", Season="2008to2009", death.rate = deaths.AH3N22008to2009.18to59),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[60,+)", Season="2005to2006", death.rate  = deaths.AH3N22005to2006.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[60,+)", Season="2006to2007", death.rate  = deaths.AH3N22006to2007.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[60,+)", Season="2007to2008", death.rate  = deaths.AH3N22007to2008.60plus),
#   cbind(sample=seq(1,n_samples), Virus = "AH3N2", age = "[60,+)", Season="2008to2009", death.rate  = deaths.AH3N22008to2009.60plus)
# )

###### Annual deaths per case #######
# rates per 100,000 person years from Ben's repo
# note this is different to table S4 in supplement of Meeyai et al.


deaths.data <- list(
  # 2005-06
  list(
    MeanH1DeathsPer100k=c(0.03, 0.03, 0.03, 0.03, 0.34, 2.76), 
    VarH1DeathsPer100k=c(0.002, 0.002, 0.002, 0.002, 0.18, 7.11),
    MeanH3DeathsPer100k=c(0.95, 0.95, 0.95 , 0.95, 1.74, 6.39), 
    VarH3DeathsPer100k=c(0.42, 0.42, 0.42 , 0.42, 2.46, 531.44),
    MeanBDeathsPer100k=c(-0.87, -0.87, -0.87, -0.87 , -1.03,  21.82),
    VarBDeathsPer100k=c(0.41, 0.41, 0.41, 0.41, 2.95, 933.97)
  ),
  # 2006-07
  list(
    MeanH1DeathsPer100k=c(0.44 ,0.44 ,0.44 ,0.44 , 2.24, 34.87),
    VarH1DeathsPer100k=c(0.26 , 0.26 , 0.26 , 0.26 ,1.23 ,221.16), 
    MeanH3DeathsPer100k=c(0.30,0.30, 0.30 ,0.30,1.22, 36.99),
    VarH3DeathsPer100k=c(0.04,0.04,0.04 ,0.04, 0.81, 667.26),
    MeanBDeathsPer100k=c(-0.40 ,-0.40 ,-0.40 ,-0.40, -1.17, 11.12),
    VarBDeathsPer100k=c(0.09 ,0.09 ,0.09 , 0.09 , 1.36 , 973.77)
  ),
  # 2007-08
  list(
    MeanH1DeathsPer100k=c(0.21 ,0.21 , 0.21 , 0.21 , 1.08, 20.16),
    VarH1DeathsPer100k=c(0.06,0.06 ,0.06, 0.06 , 1.04 , 552.51), 
    MeanH3DeathsPer100k=c(0.50,0.50,0.50 ,0.50 ,0.90 ,13.60),
    VarH3DeathsPer100k=c(0.12, 0.12 ,0.12 ,0.12 , 1.89, 935.75),
    MeanBDeathsPer100k=c(-1.24 ,-1.24, -1.24, -1.24 ,-3.46, -37.79),
    VarBDeathsPer100k=c(0.85 ,0.85,0.85 ,0.85 , 3.33 , 1857.14)
  ),
  # 2008-09
  list(
    MeanH1DeathsPer100k=c(0.45 ,0.45 ,0.45 ,0.45, 2.30, 34.96),
    VarH1DeathsPer100k=c(0.27,0.27 , 0.27, 0.27, 1.40 , 537.32), 
    MeanH3DeathsPer100k=c(0.62,0.62,0.62 ,0.62 ,2.44 ,29.69),
    VarH3DeathsPer100k=c(0.18, 0.18, 0.18, 0.18, 2.53,816.24),
    MeanBDeathsPer100k=c(-0.88 ,-0.88,-0.88, -0.88, -2.35 ,-26.54),
    VarBDeathsPer100k=c(0.42 ,0.42 ,0.42 ,0.42, 12, 3.81 , 1846.46)
  )
)

deaths <- vector("list",3*6*4)
i = 0
for (s in 1:length(seasons)){
  for (a in 1:length(ages)){
    for (v in 1:length(viruses)){
    i=i+1
    deaths[[i]] <- as.data.frame(cbind(
      sample=seq(1,n_samples),
      Virus=viruses[v],
      age=ages[a],
      Season=seasons[s],
      death.rate=rnorm(n_samples,deaths.data[[s]][[2*(v-1)+1]][[a]],(deaths.data[[s]][[2*(v-1)+2]][[a]])^0.5)
    ))
    }
  }
}
deaths <- rbindlist(deaths)
deaths <- as.data.table(deaths)
deaths[
  , sample := as.numeric(sample)
][
  , death.rate.per100k := as.numeric(death.rate)
][
  death.rate.per100k < 0, death.rate.per100k := 0
][
  , death.rate := NULL
]

# merge cases with population and death data
deaths <- merge(cases_sample_year_m[scenario==1],deaths,by=c("sample","Season","age","Virus"))
deaths <- merge(deaths, popthai.grouped,by=c("age"))
deaths[, pop := as.numeric(pop)]
deaths[, fluDeaths := death.rate.per100k * pop/100000]

###### Annual symptomatic and fever cases #######
#From the Carrat paper N.B. Meeyai et al. used single average value from
# the same paper: probsymptoms~dbeta(85.78626, 43.0219) 

carrat_symptoms <- data.table(
  c("symptoms", "symptoms", "symptoms", "fever", "fever", "fever"),
  c("AH1N1", "AH3N2", "B", "AH1N1", "AH3N2", "B"), 
           c(0.708, 0.645, 0.574, 0.370, 0.406, 0.075),
           c(0.504, 0.546, 0.352, 0.246, 0.309, 0.032),
           c(0.852, 0.733, 0.769, 0.513 ,0.511, 0.169)
           )

colnames(carrat_symptoms) <- c("outcome","Virus", "median", "lower", "upper")

# fit a beta distribution to each of the rows. 
##### Estimating alpha and beta from quantiles and mean
quantiles_to_fit <- c(0.5,0.025, 0.975)

# output the comparison measures.
f.beta <- function(alpha, beta, x, lower=0, upper=1) {
  p <- pbeta(x[1:3], alpha, beta)
  # return both
  return(c(p))
}

# Sums of squares - for comparing estimated to real. 
delta <- function(fit, actual) sum((fit-actual)^2)

#calculate for the theta and probs what is needed
objective <- function(theta, x, prob, ...) {
  ab <- exp(theta) # Parameters are the *logs* of alpha and beta
  #work out what the logit of the quanitle is for these parameters
  fit <- f.beta(ab[1], ab[2], x=as.numeric(x),...)
  # return the sum of squares. 
  return (delta(fit, prob))
}


start <-log(c(10,10))  
# for each of the samples. 

for(i in 1:nrow(carrat_symptoms)){
  x <- carrat_symptoms[i,c("median", "lower", "upper")]
sol <- optim(f=objective,p=start,
             method="L-BFGS-B", 
             x=x,
             prob=c(quantiles_to_fit)
)
parms <- exp(sol$par)           # Estimates of alpha and beta

carrat_symptoms[i,"alpha"] <- parms[1]
carrat_symptoms[i,"beta"] <- parms[2]
}

# check the outputs look reasonable
# j <- 6
# x <- seq(0,1, 0.01)
# y <- dbeta(shape1 = as.numeric(carrat_symptoms[j, "alpha"]), 
#       shape2 = as.numeric(carrat_symptoms[j, "beta"]), 
#       x = x)
# plot(x,y)

# take n samples from each distribution
n_samples <- length(unique(cases_sample_year_m$sample))
H1N1_symptoms <- rbeta(n =n_samples, 
                      shape1 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "symptoms", "alpha"]), 
                      shape2 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "symptoms", "beta"]) )
H3N2_symptoms <- rbeta(n =n_samples, 
                      shape1 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "symptoms", "alpha"]), 
                      shape2 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "symptoms", "beta"]) )
B_symptoms <- rbeta(n =n_samples, 
                      shape1 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "symptoms", "alpha"]), 
                      shape2 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "symptoms", "beta"]))
H1N1_fever <- rbeta(n =n_samples, 
                       shape1 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "fever", "alpha"]), 
                       shape2 = as.numeric(carrat_symptoms[Virus =="AH1N1" & outcome == "fever", "beta"]) )
H3N2_fever <- rbeta(n =n_samples, 
                       shape1 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "fever", "alpha"]), 
                       shape2 = as.numeric(carrat_symptoms[Virus =="AH3N2" & outcome == "fever", "beta"]) )
B_fever <- rbeta(n =n_samples, 
                    shape1 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "fever", "alpha"]), 
                    shape2 = as.numeric(carrat_symptoms[Virus =="B" & outcome == "fever", "beta"]))

# combine into table
carrat_samples <- data.table(
  "sample" =  rep((1:n_samples),3), 
  "Virus" =  rep(c("AH1N1", "AH3N2", "B"), each = n_samples), 
  "propSymptomatic" = c(H1N1_symptoms, H3N2_symptoms, B_symptoms),
  "propFever" = c(H1N1_fever, H3N2_fever, B_fever)
)

### merge with deaths data to calculate probability of death per symptomatic case

deaths <- merge(deaths, carrat_samples,by=c("sample","Virus"))
deaths[, Symptomatic := infections * propSymptomatic]
deaths[, prob.death.if.symptomatic := fluDeaths/Symptomatic]
deaths <- deaths[,.(sample,Virus,age,Season,propSymptomatic,propFever,prob.death.if.symptomatic)]

### merge with infections data for different scenarios
cases_sample_year_m <- merge(cases_sample_year_m,deaths,by=c("sample","Virus","age","Season"))

# cases_sample_year_m <- merge(
#   cases_sample_year_m,
#   carrat_samples,
#   by=c("sample","Virus")
# )

cases_sample_year_m[,Symptomatic:=infections*propSymptomatic][,propSymptomatic:=NULL]
cases_sample_year_m[,Fever:=infections*propFever][,propFever:=NULL]
cases_sample_year_m[,Deaths:=Symptomatic*prob.death.if.symptomatic][,prob.death.if.symptomatic:=NULL]

# collapse infection data across virus types since IPD/ILI data is non-specific
cases_sample_year_m <- cases_sample_year_m[
  ,
  .(
    infections = sum(infections),
    Symptomatic = sum(Symptomatic),
    Fever = sum(Fever),
    Deaths = sum(Deaths)
  ),
  by = .(sample, scenario, Season, age) # removed Month
][!is.na(Season)]

####### Outpatient and Inpatient Cases ######
# Use monthy ILI and IPD data from Meehai et al. and follow their assumption
# that minimum monthly reported ILI/IPD by age group in a given season reflects
# the non-flu baseline. (ILI = IPD + OPD)
# Compare this data to our baseline (scenario==1) simulation to estimate the 
# IP and OP risk per symptomatic case.

outcomes <- c()
# iterate over seasons
for (i in 1:4){
  mILI <- data[[i]][["ILIbyage"]]
  mIPD <- data[[i]][["IPDbyage"]]
  # rownames(m) <- c("[0,2)","[2,6)","[6,12)","[12,18)","[18,60)","[60,+)")
  # colnames(m) <- c(seq(3,12),seq(1,2))
  # subtract minimum "non-flu" baseline from each age band
  for ( j in 1:6){
    mILI[j,]=mILI[j,]-min(mILI[j,])
    mIPD[j,]=mIPD[j,]-min(mIPD[j,])
  }

  # yearly aggregated
  outcomes <- rbind(
    cbind(
      Season=names(data[i]),
      age=c("[0,2)","[2,6)","[6,12)","[12,18)","[18,60)","[60,+)"),
      ILI=rowSums(mILI),
      IPD=rowSums(mIPD)
    ),
    outcomes
  )
  
  # monthly 
  # for (j in 1:12){
  #   outcomes <- rbind(
  #     cbind(
  #       Season=names(data[i]),
  #       age=c("[0,2)","[2,6)","[6,12)","[12,18)","[18,60)","[60,+)"),
  #       ILI=mILI[,j],
  #       IPD=mIPD[,j]
  #     ),
  #     outcomes
  #   )
  # }
    
}
outcomes <- as.data.table(outcomes)
outcomes[,ILI:=as.numeric(ILI)]
outcomes[,IPD:=as.numeric(IPD)]
outcomes[,OPD:=ILI-IPD]

# combine outcomes with cases from baseline scenario
outcomes <- merge(cases_sample_year_m[scenario==1],outcomes,by=c("Season","age"))
outcomes[,probInpatient := IPD/Symptomatic]
outcomes[,probOutpatient := OPD/Symptomatic]
outcomes[age=="[0,2)",age.numeric:=0]
outcomes[age=="[2,6)",age.numeric:=2]
outcomes[age=="[6,12)",age.numeric:=6]
outcomes[age=="[12,18)",age.numeric:=12]
outcomes[age=="[18,60)",age.numeric:=18]
outcomes[age=="[60,+)",age.numeric:=60]

RISK_INPATIENT <- ggplot(data=outcomes[Season!='2007to2008'],aes(x=as.factor(age.numeric),y=probInpatient,color=age))+
  geom_violin() +
  # facet_grid(.~Season) +
  ggtitle("Probability of symptomatic case becoming an inpatient")

RISK_OUTPATIENT <- ggplot(data=outcomes[Season!='2007to2008'],aes(x=as.factor(age.numeric),y=probOutpatient,color=age))+
  geom_violin() +
  # facet_grid(.~Season) +
  ggtitle("Probability of symptomatic case becoming an outpatient")

# Fit beta distributions for IP/OP risk for each age group
# Currently dropping 2007to2008 season due to data issues
probInpatientParams <- data.frame()
probOutpatientParams <- data.frame()
# age_grps <- c(0,2,6,12,18,60)
age_grps <-  c("[0,2)","[2,6)","[6,12)","[12,18)","[18,60)","[60,+)")
for(i in 1:length(age_grps)){
  print(c(i,age_grps[i]))
  IP <- outcomes[age==age_grps[i] & Season!='2007to2008',probInpatient]  
  OP <- outcomes[age==age_grps[i] & Season!='2007to2008',probOutpatient] 
  
  f <- fitdistr(IP, dbeta, start=list(shape1=1.5,shape2=10), lower=c(1E-8,1E-8)) # lower bound to supress optim warning
  probInpatientParams[i,"alpha"] <- f$estimate[[1]]
  probInpatientParams[i,"beta"] <- f$estimate[[2]]
  
  f <- fitdistr(OP, dbeta, start=list(shape1=1.5,shape2=10), lower=c(1E-8,1E-8)) # lower bound to supress optim warning
  probOutpatientParams[i,"alpha"] <- f$estimate[[1]]
  probOutpatientParams[i,"beta"] <- f$estimate[[2]]
}

# Now sample from these distributions
outcomes <- data.table()
for(i in 1:length(age_grps)){
 outcomes <- rbind(
    outcomes,
    cbind(
      sample = seq(1:n_samples),
      age = rep(age_grps[[i]],n_samples),
      probInpatient = rbeta(
        n_samples, 
        shape1 = probInpatientParams$alpha[[i]],
        shape2 = probInpatientParams$beta[[i]]
      ),
      probOutpatient = rbeta(
        n_samples, 
        shape1 = probOutpatientParams$alpha[[i]],
        shape2 = probOutpatientParams$beta[[i]]
      )
    )
  )
}
outcomes[,sample:=as.numeric(sample)]
outcomes[,probInpatient:=as.numeric(probInpatient)]
outcomes[,probOutpatient:=as.numeric(probOutpatient)]
outcomes <- cases_sample_year_m[outcomes,on=.(sample,age)]

# calculate expected number of IP and OP cases
outcomes[, IPcases := Symptomatic * probInpatient][, probInpatient:=NULL]
outcomes[, OPcases := Symptomatic * probOutpatient][, probOutpatient:=NULL]

### YLDs per non-fatal outcome from Meehai et al.

# YLDS associated with a non-hospitalized and surviving symptomatic case 
YLDpersym <- rgamma(n_samples,7.72, 1543.2) 

# YLDS associated with an outpatient case 
# See Lugner 2012 
YLDperOP <- rgamma(n_samples,1,125) #mean=0.008, var=.000064

# YLDS associated with an inpatient case 
# See Lugner 2012 
YLDperIP <- rgamma(n_samples,1, 46.083)

# DALYs from vaccine adverse events
# HundredsofDALYs.per.vax~dgamma(1.5544, 503793) 
# DALYs.per.vax<-HundredsofDALYs.per.vax/100   

YLDs <- as.data.table(cbind(
  sample=seq(1,n_samples),
  YLDpersym,
  YLDperOP,
  YLDperIP
))

outcomes <- outcomes[YLDs,on=.(sample)]

###### Costs #######

#### Disease costs from Meehai et al. ####

# Cost for symptomatic case
cost.per.symptomatic.0to2    <- rgamma(n_samples,shape=208.2, rate=10.3)  
cost.per.symptomatic.2to5    <- rgamma(n_samples,shape= 142.0, rate=9.9) 
cost.per.symptomatic.6to11   <- rgamma(n_samples,shape=86.7, rate=3.2) # mean=27, var=8.4
cost.per.symptomatic.12to17  <- rgamma(n_samples,shape=110.25, rate=26.25 ) # mean =4.2 var = 0.16
cost.per.symptomatic.18to59  <- cost.per.symptomatic.12to17 #rgamma(n_samples,shape=110.25, rate=26.25 ) 
cost.per.symptomatic.60plus  <- cost.per.symptomatic.12to17 #rgamma(n_samples,shape=110.25, rate=26.25 ) 

# Direct med costs of OPD visit
# Distribution Meehai et al. are v. similar so sample as a single parameter
cILI.OP.0to2   <- rgamma(n_samples,shape=23.3, rate=0.062)  # mean =373, var = 5991
cILI.OP.2to5   <- rgamma(n_samples,shape=23.6, rate=0.063)  # mean =374, var = 5914
cILI.OP.6to11  <- rgamma(n_samples,shape=23.35, rate=0.063) # mean =373, var = 5944
cILI.OP.12to17 <- rgamma(n_samples,shape=23.29, rate=0.063)  # mean =373, var = 5960
cILI.OP.18to59 <- rgamma(n_samples,shape=23.66, rate=0.064 ) # mean =373, var =5868
cILI.OP.60plus <- rgamma(n_samples,shape=23.47, rate=0.063) # mean =373, var =5929

# Direct non-med costs of OPD visit

if (societal_perspective) {
  cDNM.ILI.OP.0to2   <- rgamma(n_samples,shape=499.3, rate=3.66) #mean=136.3, var=37.21
  cDNM.ILI.OP.2to5   <- rgamma(n_samples,shape=516.05, rate=3.79 ) #mean=136.3, var=6
  cDNM.ILI.OP.6to11  <- cDNM.ILI.OP.2to5
  cDNM.ILI.OP.12to17 <- cDNM.ILI.OP.2to5
  cDNM.ILI.OP.18to59 <- cDNM.ILI.OP.2to5
  cDNM.ILI.OP.60plus <- cDNM.ILI.OP.0to2
} else {
  cDNM.ILI.OP.0to2   <- 0 
  cDNM.ILI.OP.2to5   <- 0 
  cDNM.ILI.OP.6to11  <- 0 
  cDNM.ILI.OP.12to17 <- 0 
  cDNM.ILI.OP.18to59 <- 0 
  cDNM.ILI.OP.60plus <- 0 
}

# time costs of OPD visit
if (societal_perspective) {
  cTimeILI.OP.0to2   <- rgamma(n_samples,shape=1.71, rate=0.0007)  #mean=2442, var=3492787
  cTimeILI.OP.2to5   <- rgamma(n_samples,shape=1.68, rate=0.00069) #mean= 2430, var=3522754
  cTimeILI.OP.6to11  <- rgamma(n_samples,shape=1.64, rate=0.00067) #mean=2428, var=3597472
  cTimeILI.OP.12to17 <- rgamma(n_samples,shape=1.72, rate=0.0007)  #mean=2461 ,var=3517125
  cTimeILI.OP.18to59 <- rgamma(n_samples,shape=3.08, rate=0.0018)  #mean=1668,var=902120
  cTimeILI.OP.60plus <- rgamma(n_samples,shape=3.20, rate=0.0019)  #mean=1660,var=859885
} else {
  cTimeILI.OP.0to2   <- 0 
  cTimeILI.OP.2to5   <- 0 
  cTimeILI.OP.6to11  <- 0 
  cTimeILI.OP.12to17 <- 0 
  cTimeILI.OP.18to59 <- 0 
  cTimeILI.OP.60plus <- 0 
}

# Direct med costs of IPD visit
cILI.IP.0to2   <- rgamma(n_samples,shape=35.58, rate=0.0048) # mean =7448, var =1559144
cILI.IP.2to5   <- rgamma(n_samples,shape=345.97, rate=0.049) # mean =7122, var = 146608 
cILI.IP.6to11  <- rgamma(n_samples,shape=35.58, rate=0.0055) # mean =6466, var = 1175025
cILI.IP.12to17 <- rgamma(n_samples,shape=872.02, rate=0.18)  # mean =4756, var = 25937
cILI.IP.18to59 <- rgamma(n_samples,shape=809.47, rate=0.060) # mean =13563, var =227241
cILI.IP.60plus <- rgamma(n_samples,shape=67.47, rate=0.0076) # mean =8923, var =1180176

# Direct non-med costs of IPD visit

if (societal_perspective) {
  cDNM.ILI.IP.0to2   <- rgamma(n_samples,shape=151.6, rate=0.14) #mean=1081, var=7713
  cDNM.ILI.IP.2to5   <- rgamma(n_samples,shape=290.9, rate=0.26) #mean=1101, var=4168
  cDNM.ILI.IP.6to11  <- rgamma(n_samples,shape=283.4, rate=0.29) #mean=980, var=3389
  cDNM.ILI.IP.12to17 <- rgamma(n_samples,shape=408.7, rate=0.45) #mean=913, var=2039
  cDNM.ILI.IP.18to59 <- rgamma(n_samples,shape=448.8, rate=0.51) #mean=872, var=1693
  cDNM.ILI.IP.60plus <- rgamma(n_samples,shape=296.9, rate=0.27) #mean=1102, var=4092
} else {
  cDNM.ILI.IP.0to2   <- 0 
  cDNM.ILI.IP.2to5   <- 0 
  cDNM.ILI.IP.6to11  <- 0 
  cDNM.ILI.IP.12to17 <- 0 
  cDNM.ILI.IP.18to59 <- 0 
  cDNM.ILI.IP.60plus <- 0 
}

# time costs of IPD visit
if (societal_perspective) {
cTimeILI.IP.0to2   <- rgamma(n_samples,shape=488.1, rate=0.1)    #mean=4776, var=46729
cTimeILI.IP.2to5   <- rgamma(n_samples,shape=1821.4, rate=0.38)  #mean=4834, var=12830
cTimeILI.IP.6to11  <- rgamma(n_samples,shape=1690.2, rate=0.38)  #mean=4470, var=11822
cTimeILI.IP.12to17 <- rgamma(n_samples,shape=9577.6, rate=2.24)  #mean=4269, var=1903
cTimeILI.IP.18to59 <- rgamma(n_samples,shape=25961.8, rate=6.26) #mean=4144, var=662
cTimeILI.IP.60plus <- rgamma(n_samples,shape=1938.0, rate=0.4)   #mean=4839, var=12083
} else {
  cTimeILI.IP.0to2   <- 0 # for now set to zero
  cTimeILI.IP.2to5   <- 0 # for now set to zero
  cTimeILI.IP.6to11  <- 0 # for now set to zero
  cTimeILI.IP.12to17 <- 0 # for now set to zero
  cTimeILI.IP.18to59 <- 0 # for now set to zero
  cTimeILI.IP.60plus <- 0 # for now set to zero
}
  
# Cost per death
c.Death <- 0

#### Vaccine costs from Meehai et al. ####
# vaccine costs in Thai Baht per dose (from table 3 of report)

# c.VacPrice <- 201/31 #USD
c.VacPrice <- 201/31.98 #Int$

#logistics (see table 3 WHO report)
cVacLog.2to5   <- rgamma(n_samples, shape= 26.298, rate=1.421)  #mean=18.5, 95% (CIs 12.1,26.2
cVacLog.6to11  <- cVacLog.2to5 #rgamma(n_samples, shape= 26.298, rate=1.421)  #mean=18.5, 95% (CIs 12.1,26.2
cVacLog.12to17 <- cVacLog.2to5 #rgamma(n_samples, shape= 26.298, rate=1.421)  #mean=18.5, 95% (CIs 12.1,26.2)
cVacLog.60plus <- cVacLog.2to5 #rgamma(n_samples, shape= 26.298, rate=1.421)  #mean=18.5, 95% (CIs 12.1,26.2)

cVacLog.18to59 <- cVacLog.60plus # my assumption
cVacLog.0to2   <- cVacLog.2to5  # my assumption

# administration costs per dose (see table 3 of submitted report)
cVacAdmin.2to5   <- rgamma(n_samples, shape= 44.74, rate=0.382)  #mean=117.1 , 95% CIs (85.3,153.8)
cVacAdmin.6to11  <- rgamma(n_samples, shape= 1, rate=0.0149 )  #mean= 67, 95% CIs (1.7,247.6)
cVacAdmin.12to17 <- cVacAdmin.6to11 #rgamma(n_samples, shape= 1, rate=0.0149 )  #mean= 67, 95% CIs (1.7,247.2)
cVacAdmin.60plus <- cVacAdmin.2to5 #rgamma(n_samples, shape= 44.74, rate=0.382)  #mean=117.1 , 95% CIs (85.3,153.8)

cVacAdmin.18to59 <- cVacAdmin.60plus # my assumption
cVacAdmin.0to2   <- cVacAdmin.2to5 # my assumption

# time costs from vaccine administration

# if (societal_perspective) {
#   cTimeVac.0to2   <- 0
#   cTimeVac.2to5   <- rgamma(n_samples,shape=14.198,rate=0.471) #mean=30.172, se=8.007
#   cTimeVac.6to11  <- rgamma(n_samples,shape=14.08,rate=1.858)  #mean=7.58,   se=2.02
#   cTimeVac.12to17 <- rgamma(n_samples,shape=13.94,rate=1.838)  #mean=7.6,    se=2.03
#   cTimeVac.18to59 <- 0
#   cTimeVac.60plus <- rgamma(n_samples,shape=14.198,rate=0.471) #mean=30.172, se= 8.007
# } else {
  cTimeVac.0to2   <- 0
  cTimeVac.2to5   <- 0
  cTimeVac.6to11  <- 0
  cTimeVac.12to17 <- 0
  cTimeVac.18to59 <- 0
  cTimeVac.60plus <- 0
# }

# Adverse event costs per dose. *NOT INCLUDING FOR NOW*
# cAETIV.2to5<-rgamma(n_samples, shape=1.941, rate=3.70) # mean =0.524, var = .376 
# cAETIV.6to11<-rgamma(n_samples, shape= 1.296, rate=3.97) # mean =.326, var =  .287	
# cAETIV.12to17<-rgamma(n_samples, shape= 1.217, rate=13.733) # mean =.089, var = .08
# cAETIV.60plus<-rgamma(n_samples, shape= 1.217, rate=13.733) # mean =.089, var = .08

# cAELAIV.2to5<-rgamma(n_samples, shape=1.941, rate=3.70) # mean =0.524, var = .376 
# cAELAIV.6to11<-rgamma(n_samples, shape= 1.296, rate=3.97) # mean =.326, var =  .287	
# cAELAIV.12to17<-rgamma(n_samples, shape= 1.217, rate=13.733) # mean =.089, var = .08

# Direct non-med costs of vaccine administration (preschool only)
# cDNMVac.2to5   <- rgamma(n_samples, shape=494.16, rate=1.81) #mean=272.76, var=150.6
# cDNMVac.6to11  <-0
# cDNMVac.12to17 <-0

# AE related direct non-medical costs
# cDNMVacAE.2to5<-rgamma(n_samples, shape=3.48, rate=2.51)  #mean=1.385, se=0.742
# cDNMVacAE.6to11<-rgamma(n_samples, shape=1.47, rate=2.147)  #mean=0.685, se=0.565
# cDNMVacAE.12to17<-rgamma(n_samples, shape=1.187, rate=2.675)  #mean=0.444, se=0.407
# cDNMVacAE.60plus<-rgamma(n_samples, shape=1.187, rate=2.675)  #mean=0.444, se=0.407

# AE related time from seeking tx
# cTimeVacAE.2to5<-rgamma(n_samples, shape=2.652, rate=8.595) #mean = .309 se= .189
# cTimeVacAE.6to11<-rgamma(n_samples, shape=1.241, rate=8.031) #mean = .154 se= .139
# cTimeVacAE.12to17<-rgamma(n_samples, shape=1.036, rate=10.214) #mean = .101  se=.1 
# cTimeVacAE.60plus<-rgamma(n_samples, shape=1.036, rate=10.214) #mean = .101  se=.1 

# combine cost data and join with outcomes data
costs <- rbind(
  cbind(
    age = rep("[0,2)",n_samples),
    sample = seq(1,n_samples),
    cost.per.symptomatic = cost.per.symptomatic.0to2,
    cost.per.IP = cILI.IP.0to2   + cDNM.ILI.IP.0to2   + cTimeILI.IP.0to2,
    cost.per.OP = cILI.OP.0to2   + cDNM.ILI.OP.0to2   + cTimeILI.OP.0to2,
    costDelivery.per.dose = cVacLog.0to2 + cVacAdmin.0to2 + cTimeVac.0to2,
    cVacLog = cVacLog.0to2,
    cVacAdmin = cVacAdmin.0to2,
    cVacAdmin = cTimeVac.0to2,
    cost.per.death = c.Death,
    cost.VacPrice = c.VacPrice 
  ),
  cbind(
    age = rep("[2,6)",n_samples),
    sample = seq(1,n_samples),
    cost.per.symptomatic = cost.per.symptomatic.2to5,
    cost.per.IP = cILI.IP.2to5   + cDNM.ILI.IP.2to5   + cTimeILI.IP.2to5,
    cost.per.OP = cILI.OP.2to5   + cDNM.ILI.OP.2to5   + cTimeILI.OP.2to5,
    costDelivery.per.dose = cVacLog.2to5 + cVacAdmin.2to5 + cTimeVac.2to5,
    cVacLog = cVacLog.2to5,
    cVacAdmin = cVacAdmin.2to5,
    cVacAdmin = cTimeVac.2to5,
    cost.per.death = c.Death,
    cost.VacPrice = c.VacPrice 
  ),
  cbind(
    age = rep("[6,12)",n_samples),
    sample = seq(1,n_samples),
    cost.per.symptomatic = cost.per.symptomatic.6to11,
    cost.per.IP = cILI.IP.6to11  + cDNM.ILI.IP.6to11  + cTimeILI.IP.6to11,
    cost.per.OP = cILI.OP.6to11  + cDNM.ILI.OP.6to11  + cTimeILI.OP.6to11,
    costDelivery.per.dose = cVacLog.6to11 + cVacAdmin.6to11 + cTimeVac.6to11,
    cVacLog = cVacLog.6to11,
    cVacAdmin = cVacAdmin.6to11,
    cVacAdmin = cTimeVac.6to11,
    cost.per.death = c.Death,
    cost.VacPrice = c.VacPrice 
  ),
  cbind(
    age = rep("[12,18)",n_samples),
    sample = seq(1,n_samples),
    cost.per.symptomatic = cost.per.symptomatic.12to17,
    cost.per.IP = cILI.IP.12to17 + cDNM.ILI.IP.12to17 + cTimeILI.IP.12to17,
    cost.per.OP = cILI.OP.12to17 + cDNM.ILI.OP.12to17 + cTimeILI.OP.12to17,
    costDelivery.per.dose = cVacLog.12to17 + cVacAdmin.12to17 + cTimeVac.12to17,
    cVacLog = cVacLog.12to17,
    cVacAdmin = cVacAdmin.12to17,
    cVacAdmin = cTimeVac.12to17,
    cost.per.death = c.Death,
    cost.VacPrice = c.VacPrice 
  ),
  cbind(
    age = rep("[18,60)",n_samples),
    sample = seq(1,n_samples),
    cost.per.symptomatic = cost.per.symptomatic.18to59,
    cost.per.IP = cILI.IP.18to59 + cDNM.ILI.IP.18to59 + cTimeILI.IP.18to59,
    cost.per.OP = cILI.OP.18to59 + cDNM.ILI.OP.18to59 + cTimeILI.OP.18to59,
    costDelivery.per.dose = cVacLog.18to59 + cVacAdmin.18to59 + cTimeVac.18to59,
    cVacLog = cVacLog.18to59,
    cVacAdmin = cVacAdmin.18to59,
    cVacAdmin = cTimeVac.18to59,
    cost.per.death = c.Death,
    cost.VacPrice = c.VacPrice 
  ),
  cbind(
    age = rep("[60,+)",n_samples),
    sample = seq(1,n_samples),
    cost.per.symptomatic = cost.per.symptomatic.60plus,
    cost.per.IP = cILI.IP.60plus + cDNM.ILI.IP.60plus + cTimeILI.IP.60plus,
    cost.per.OP = cILI.OP.60plus + cDNM.ILI.OP.60plus + cTimeILI.OP.60plus,
    costDelivery.per.dose = cVacLog.60plus + cVacAdmin.60plus + cTimeVac.60plus,
    cVacLog = cVacLog.60plus,
    cVacAdmin = cVacAdmin.60plus,
    cVacAdmin = cTimeVac.60plus,
    cost.per.death = c.Death,
    cost.VacPrice = c.VacPrice 
  )
)

costs <- as.data.table(costs)
costs[
  ,sample := as.numeric(sample)
][
  ,cost.per.symptomatic := as.numeric(cost.per.symptomatic) * inflator
][
  ,cost.per.IP := as.numeric(cost.per.IP) * inflator
][
  ,cost.per.OP := as.numeric(cost.per.OP) * inflator
][
  ,costDelivery.per.dose := as.numeric(costDelivery.per.dose) * inflator
][
  ,cost.per.death := as.numeric(cost.per.death) * inflator
][
    ,cost.VacPrice := as.numeric(cost.VacPrice)
]

outcomes <- outcomes[costs, on=.(age,sample)]

# calculate totals # check fever vs symptoms
outcomes[
  ,YLDs := (
    Symptomatic * YLDpersym + 
    IPcases     * YLDperIP + 
    OPcases     * YLDperOP
  )
][
  ,costs.outcomes := (
    Symptomatic * cost.per.symptomatic +
    IPcases     * cost.per.IP +
    OPcases     * cost.per.OP +
    Deaths      * cost.per.death
  )
]

# # now need the number of vaccinations


temp_c[,scenario := as.factor(Vacc_scenario)]
age_group_names <- unique(temp_c$age_group)
temp_c[age_group==age_group_names[1],age := ages[1]]
temp_c[age_group==age_group_names[2],age := ages[2]]
temp_c[age_group==age_group_names[3],age := ages[3]]
temp_c[age_group==age_group_names[4],age := ages[4]]
temp_c[age_group==age_group_names[5],age := ages[5]]
temp_c[age_group==age_group_names[6],age := ages[6]]

# outcomes <- outcomes[
#   , scenario := as.numeric(scenario)
# ][
#   , Season := as.character(Season)
# ]
outcomes <- merge(outcomes,temp_c,by=c("scenario","Season","age"))

outcomes[
  , costs.vacc_purchase := Vaccinations * cost.VacPrice
][
  , costs.vacc := Vaccinations * (cost.VacPrice + costDelivery.per.dose)
][
  , costs.total := costs.vacc + costs.outcomes
]




