

##### Load in all the data #####
# Data from Ben's github repo https://github.com/BenSCooper/fluvaxmod/
data <- list()

# 2005/06 data
# https://github.com/BenSCooper/fluvaxmod/blob/189e0ea7b01fbb61307d1e4d6211b3bd133e2b10/data%202005to6%20%20in%20multinomial%20format%206%20age%20groups.txt
data[["2005to2006"]] <- list(
  Numtestedbymonth=c(38,84,142,192,264,226,109,139,165,196,146,194),
  fluAH1posbymonth=c(0,0,0,0,1,0,3,2,3,0,1,3),  fluBposbymonth=c(1,0,5,6,18,14,13,15,21,26,17,5),
  ILIbyage=matrix(
    c(48,63,67,84,650,121,78,100,161,137,849,138,131,276,274,246,1280,206,142,236,213,189,1119,233,141,177,209,165,1120,194,84,163,218,172,1063,177,76,123,140,141,1080,211,101,145,173,134,1143,175,55,114,114,86,713,151,95,155,158,89,882,204,49,84,99,59,609,152,69,82,76,49,547,115),
    nrow=6, ncol=12
  ),
  IPDbyage=matrix(
    c(11,16,14,27,81,22,8,21,39,21,104,26,20,26,33,34,134,30,21,27,29,36,110,30,21,29,32,17,129,22,20,26,49,40,103,31,11,26,25,27,129,23,13,22,35,17,134,22,6,13,14,12,64,13,13,9,14,10,85,16,8,10,15,11,54,18,9,10,12,4,70,18),
    nrow=6, ncol=12
  )
)

# 2006/07 data
# https://github.com/BenSCooper/fluvaxmod/blob/189e0ea7b01fbb61307d1e4d6211b3bd133e2b10/data%202006to7%20%20in%20multinomial%20format%206%20age%20groups.txt
data[["2006to2007"]] <- list(
  Numtestedbymonth=c(138,183,402,455,430,374,349,379,241,416,444,353),
  fluAH1posbymonth=c(0,22,103,62,64,37,15,10,1,0,0,0),
  fluAH3posbymonth=c(0,0,0,1,1,2,10,21,12,40,62,39),
  fluBposbymonth=c(3,1,3,6,4,8,18,35,15,20,16,5),
  ILIbyage=matrix(
    c(44,60,61,74,783,127,36,81,111,100,633,137,69,157,201,143,881,182,77,289,418,279,1343,214,88,237,398,321,1244,218,50,137,181,155,873,168,58,90,118,99,752,157,44,79,87,114,671,131,27,40,70,97,511,99,69,142,196,167,1154,225,79,220,261,194,1045,232,52,134,122,92,712,166),
    nrow=6, ncol=12
  ),
  IPDbyage=matrix(
    c(7,6,14,16,58,18,8,16,21,20,74,17,9,22,44,31,118,16,17,102,153,92,233,50,29,90,155,90,240,37,12,36,34,28,106,23,9,16,25,21,90,19,7,19,16,17,93,26,8,9,13,11,62,12,27,51,62,52,181,48,32,116,88,57,207,65,22,55,37,21,132,36),
    nrow=6, ncol=12
  )
)

# 2007/08 data
# https://github.com/BenSCooper/fluvaxmod/blob/189e0ea7b01fbb61307d1e4d6211b3bd133e2b10/data%202007to8%20%20in%20multinomial%20format%206%20age%20groups.txt
data[["2007to2008"]] <- list(
  Numtestedbymonth=c(180,265,373,402,332,367,367,426,390,556,341,314),
  fluAH1posbymonth=c(0,0,5,9,8,10,23,44,33,7,5,4),
  fluAH3posbymonth=c(6,17,26,21,27,39,29,27,11,37,5,8),
  fluBposbymonth=c(3,5,1,6,22,46,51,59,84,148,48,32),
  ILIbyage=matrix(
    c(42,47,66,65,481,138,56,67,113,123,621,107,78,143,190,175,845,155,97,180,216,187,902,165,74,173,172,169,810,148,76,204,252,168,996,144,86,127,190,127,928,190,56,147,243,165,911,150,43,123,172,122,495,99,71,152,203,180,828,169,72,204,239,206,829,158,75,152,142,104,735,146),
    nrow=6, ncol=12
  ),
  IPDbyage=matrix( # *** Note this is identical to ILIbyage ***
    c(42,47,66,65,481,138,56,67,113,123,621,107,78,143,190,175,845,155,97,180,216,187,902,165,74,173,172,169,810,148,76,204,252,168,996,144,86,127,190,127,928,190,56,147,243,165,911,150,43,123,172,122,495,99,71,152,203,180,828,169,72,204,239,206,829,158,75,152,142,104,735,146),
    nrow=6, ncol=12
  )
)

# 2008/09 data

data[["2008to2009"]] <- list(
  Numtestedbymonth=c(211,224,338,331,276,270,296,307,272,283,237,194),
  fluAH1posbymonth=c(8,8,3,8,11,3,6,10,9,6,7,11),
  fluAH3posbymonth=c(15,16,41,37,31,22,15,8,12,14,7,3),
  fluBposbymonth=c(11,7,15,19,22,27,34,20,6,11,14,5),
  ILIbyage=matrix(
    c(37,76,72,84,550,110,41,113,127,114,810,122,97,313,355,268,1347,225,77,260,313,221,1264,230,63,169,220,175,857,152,104,167,226,179,970,174,78,128,170,133,1054,161,74,203,275,249,1282,161,54,114,162,108,899,130,71,122,165,134,883,176,43,103,123,111,604,135,58,95,133,120,693,124),
    nrow=6, ncol=12
  ),
  IPDbyage=matrix( 
    c(7,17,9,9,70,13,12,29,25,23,137,20,17,58,51,38,143,31,22,52,46,34,149,37,19,39,51,33,88,28,29,38,46,31,92,21,16,21,33,23,119,22,17,33,49,39,137,14,16,20,22,11,86,17,18,34,36,27,129,28,14,23,31,28,90,21,10,21,24,21,91,17),
    nrow=6, ncol=12
  ))
  

##### Reformat into one and make some investigative plots! ##### 
### Want to create a data.table with all the data in, so we cna plot it over tiem
  
  all_input_data <- data.table(
    month = rep(c(4:12,1,2,3),4),
    year = c(rep(2005,9), rep(2006,12), rep(2007,12), rep(2008, 12), rep(2009,3)), 
    tested = c(data[[1]]$Numtestedbymonth, data[[2]]$Numtestedbymonth,
               data[[3]]$Numtestedbymonth, data[[4]]$Numtestedbymonth) , 
    fluAH1 = c(data[[1]]$fluAH1posbymonth, data[[2]]$fluAH1posbymonth,
             data[[3]]$fluAH1posbymonth, data[[4]]$fluAH1posbymonth) , 
    fluAH3 = c(data[[1]]$fluAH3posbymonth, data[[2]]$fluAH3posbymonth,
               data[[3]]$fluAH3posbymonth, data[[4]]$fluAH3posbymonth) , 
    fluB = c(data[[1]]$fluBposbymonth, data[[2]]$fluBposbymonth,
               data[[3]]$fluBposbymonth, data[[4]]$fluBposbymonth), 
    IlI_age_0_2 = c(data[[1]]$ILIbyage[1,], data[[2]]$ILIbyage[1,],
                 data[[3]]$ILIbyage[1,], data[[4]]$ILIbyage[1,]),
    IlI_age_2_5 = c(data[[1]]$ILIbyage[2,], data[[2]]$ILIbyage[2,],
                 data[[3]]$ILIbyage[2,], data[[4]]$ILIbyage[2,]),
    IlI_age_6_11 = c(data[[1]]$ILIbyage[3,], data[[2]]$ILIbyage[3,],
                 data[[3]]$ILIbyage[3,], data[[4]]$ILIbyage[3,]),
    IlI_age_12_17 = c(data[[1]]$ILIbyage[4,], data[[2]]$ILIbyage[4,],
                 data[[3]]$ILIbyage[4,], data[[4]]$ILIbyage[4,]),
    IlI_age_18_59 = c(data[[1]]$ILIbyage[5,], data[[2]]$ILIbyage[5,],
                      data[[3]]$ILIbyage[5,], data[[4]]$ILIbyage[5,]),
    IlI_age_60 = c(data[[1]]$ILIbyage[6,], data[[2]]$ILIbyage[6,],
                      data[[3]]$ILIbyage[6,], data[[4]]$ILIbyage[6,])
  )
  
all_input_data[, date := as.Date(paste0(year, "-", month,"-01"))]
all_input_data[,all_ILI := IlI_age_0_2 + IlI_age_2_5 + 
                 IlI_age_6_11 + IlI_age_12_17 + IlI_age_18_59 + 
                 IlI_age_60]

ili_by_age <- melt.data.table(all_input_data, 
                              id.vars = c("date"), 
                              measure.vars = c(
                                "IlI_age_0_2", 
                                "IlI_age_2_5",
                                "IlI_age_6_11",
                                "IlI_age_12_17", 
                                "IlI_age_18_59", 
                                "IlI_age_60"
                              ))


positives_time <- melt.data.table(all_input_data, id.vars = c("date"), measure.vars = c("tested",
                                                                          "fluAH1", 
                                                                          "fluAH3", 
                                                                          "fluB", 
                                                                          "all_ILI"))
ALL_TESTED <- ggplot(positives_time, aes(x = date, y =value, colour = variable )) + 
  geom_line() + 
  theme_linedraw() + 
  geom_point() +
  facet_grid(variable~., scales = "free_y") + 
  labs(x = "Month", y = "Number positive or Number tested") + 
  theme(legend.position = "none")
  
SUBTYPES_TIME <- ggplot(positives_time[variable != "tested", ], aes(x = date, y =value, colour = variable )) + 
  geom_line() + 
  theme_linedraw() + 
  geom_point()+
  labs(x = "Month", y = "Number positive or Number tested", 
       colour = "subtype")

ILI_BY_AGE <- ggplot(ili_by_age, aes (x = date, 
                                      y = value, 
                                      colour = variable)) + 
  geom_line() + 
  theme_linedraw() + 
  facet_grid(variable~., scales = "free_y") + 
  labs(x = "Month", y = "ILI", 
       colour = "age")
  
#### defining the epidemics ####

# start = two months subtype specific going up 
# end = two months subtype specific going down
# and over median at all time points in the epidmic

# ignore if only one month out of sync (either up or down)

identify_seasons <- function(input_data){

  input_data$flu_peak <- input_data[,quantile(flu, probs= 0.9)]
  input_data$over_peak <- F
  input_data[flu_peak< flu, over_peak := T]
  
  input_data$flu_included <- input_data[,quantile(flu, probs= 0.6)]
  input_data$over_inclusion<- F
  input_data[flu_included< flu, over_inclusion := T]
  
  tmp <- rle(input_data$over_inclusion)
  input_data$Seq <- rep(tmp$lengths >= 2,times = tmp$lengths)
  
  # add the sequence number to each
  seq_to_add <- c()
  start_seq <- 1
  for(i in 1:length(tmp$lengths)){
    
     length_run <- tmp$lengths[i]
    tester <- input_data[sum(tmp$lengths[1:i]),"over_inclusion"]
    if(tester == F){ 
      seq_to_add <- c(seq_to_add, rep(0,length_run))
    } else {
      seq_to_add <- c(seq_to_add, rep(start_seq,length_run))
      start_seq <- start_seq + 1
      }
  }

  input_data$seq <- seq_to_add
  input_data$epidem_inclusion <- 0
  for(j in 1:start_seq){
    
    if(any(input_data[seq == j,"over_peak"]==T)){
      input_data[seq == j, epidem_inclusion := 1 ]
    }
  }

  return(input_data)
}

fluAH1_epidemics <- all_input_data[, c("date","tested", "fluAH1")]
colnames(fluAH1_epidemics) <- c("date", "tested", "flu")
fluAH1_epidemics <- identify_seasons(fluAH1_epidemics)

fluAH3_epidemics <- all_input_data[, c("date","tested", "fluAH3")]
colnames(fluAH3_epidemics) <- c("date", "tested", "flu")
fluAH3_epidemics <- identify_seasons(fluAH3_epidemics)

fluB_epidemics <- all_input_data[, c("date","tested", "fluB")]
colnames(fluB_epidemics) <- c("date", "tested", "flu")
fluB_epidemics <- identify_seasons(fluB_epidemics)

fluAH1_epidemics[, visualiser := epidem_inclusion*max(fluAH1_epidemics$flu)*1.1]
fluAH1_epidemics[, visualiser_peak := over_peak*max(fluAH1_epidemics$flu)*1.1]

AH1_EPIS <-ggplot(fluAH1_epidemics, aes(x = date)) + 
  geom_line(aes(y=flu), colour = "red") + 
 geom_line(aes(y = visualiser)) + 
  geom_point(aes(y = visualiser)) + 
  theme_linedraw() + 
  labs(y = "AH1") + 
  lims(y = c(0,163))

fluAH3_epidemics[, visualiser := epidem_inclusion*max(fluAH3_epidemics$flu)*1.1]
fluAH3_epidemics[, visualiser_peak := over_peak*max(fluAH3_epidemics$flu)*1.1]

AH3_EPIS <-ggplot(fluAH3_epidemics, aes(x = date)) + 
  geom_line(aes(y=flu), colour = "red") + 
  geom_line(aes(y = visualiser)) +
  geom_point(aes(y = visualiser)) +
  theme_linedraw() + 
  labs(y = "AH3")+ 
  lims(y = c(0,163))

fluB_epidemics[, visualiser := epidem_inclusion*max(fluB_epidemics$flu)*1.1]
fluB_epidemics[, visualiser_peak := over_peak*max(fluB_epidemics$flu)*1.1]

B_EPIS <-ggplot(fluB_epidemics, aes(x = date)) + 
  geom_line(aes(y=flu), colour = "red") + 
  geom_line(aes(y = visualiser)) +
  geom_point(aes(y = visualiser)) +
  theme_linedraw() + 
  labs(y = "B")+ 
  lims(y = c(0,163))
  
#grid.arrange(AH1_EPIS, AH3_EPIS, B_EPIS, ncol= 1)



data(popM)
popM <- data.table(popM)
male_age <- unlist(popM[name == "Thailand", "2015"])
data(popF)
popF <- data.table(popF)
female_age <- unlist(popF[name == "Thailand", "2015"])

pop_by_age <- female_age+male_age
pop_by_age <- rep(pop_by_age/5, each = 5)*1000

  