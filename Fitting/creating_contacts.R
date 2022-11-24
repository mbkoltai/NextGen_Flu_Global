#formatting Thai contact matrix data for the epi model, to match POLYMOD UK data
#downloaded from https://zenodo.org/record/4739777

# This is the key of the Thai contact survey data created at the end: 
#     v1:age of subject 
#     v2:data taken in the weekend (1) or during the week (0)
#     v3:<2 yr,  v4:2-5 yrs,  v5:6-11 yrs,  v6:12-17 yrs, v7:18-59 yrs, v8:>=60 yrs
#      
# At the end, the Thai contact survey data will be in the same format as polymod_uk

library(tidyverse)
#Read in participant information
part.info<- read.csv(here::here("Epi/Data/SMILI_Thailand_2015", "2015_Meeyai_Thailand_participant_common.csv"))
participant <- part.info %>% dplyr::select(part_id, part_age)

#Read in contact information
cont.info<- read.csv(here::here("Epi/Data/SMILI_Thailand_2015", "2015_Meeyai_Thailand_contact_common.csv"))
contact <- cont.info %>% dplyr::select(part_id, cnt_age_exact)
#Note: there are 119 unique participant_ids in the contact df who do not appear in the 
#participant df and so we have not data on their ages. These participants are excluded from the 
#final dataframe created

#Read in data on when diary was filled 
sday.info <- read.csv(here::here("Epi/Data/SMILI_Thailand_2015", "2015_Meeyai_Thailand_sday.csv"))

table(sday.info$dayofweek, sday.info$holiday)
#Note: this shows that each day of the week may be a working day or holiday, depending on who 
#filled in the diary. So use data from the 'holiday' column
sday <- sday.info %>% dplyr::select(part_id, holiday)

df <- contact %>% 
  left_join(participant, by = "part_id") %>% 
  left_join(sday, by = "part_id")

df2 <- df %>% filter(is.na(part_age))
length(unique(df2$part_id))
summary(df2)
rm(df2)

df <- df %>% filter(!is.na(part_age))
#age groups used in Thai model are <2, 2-5, 6-11, 12-17, 18-59, 60 and above

df <- df %>% 
  mutate(age_class_cont = case_when(
    cnt_age_exact < 2 ~ "Below_2",
    between(cnt_age_exact, 2, 5) ~ "2_5",
    between(cnt_age_exact, 6, 11) ~ "6_11",
    between(cnt_age_exact, 12, 17) ~ "12_17",
    between(cnt_age_exact, 18, 59) ~ "18_59",
    TRUE ~ "60_and_above"
  )) %>% 
  dplyr::select(part_id, part_age, age_class_cont, holiday)

names(df) <- c("csid", "age_years_part", "age_class_cont", "day")

df1  <- df[ ,c(1, 2, 4, 3)]
df2  <- df[ ,c(1, 2, 4, 3)]
df2  <- df[ ,c(1, 2, 3, 4)]
df2  <- df[ ,c(1, 2, 4)]
df2  <- unique(df2)
df3  <- tally(group_by(df1, csid, age_class_cont))
df3  <- merge(df3,df2,by.x = "csid", by.y = "csid", all.x=T)
df3  <- reshape(df3, 
                timevar = "age_class_cont",
                idvar = c("csid", "age_years_part", "day"),
                direction = "wide")

df3                            <- df3[order(df3$age_years_part), ]
df3$age_years_part             <- round(df3$age_years_part, 0)
df3[is.na(df3)]       <- 0

df4    <- plyr::rename(df3, c(age_years_part= "v1", day="v2", 
                              n.Below_2="v3", n.2_5 ="v4", n.6_11 = "v5",
                              n.12_17 ="v6", n.18_59 ="v7", n.60_and_above ="v8"))
df5    <- df4[ ,c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8")]
df6    <- df5
df7    <- data.frame(matrix(0, ncol = 8, nrow = nrow(df6)))

df7$X1 <- as.integer(df6$v1)
df7$X2 <- as.integer(df6$v2)
df7$X3 <- as.integer(df6$v3)
df7$X4 <- as.integer(df6$v4)
df7$X5 <- as.integer(df6$v5)
df7$X6 <- as.integer(df6$v6)
df7$X7 <- as.integer(df6$v7)
df7$X8 <- as.integer(df6$v8)

df7    <- plyr::rename(df7, c(X1= "V1", X2="V2", X3 = "V3", 
                              X4="V4", X5="V5", X6="V6", 
                              X7="V7", X8="V8"))

polymod.thai     <- df7

rm(df, df1, df2, df3, df4, df5, df6, df7, sday, sday.info, part.info, participant, 
   contact, cont.info)

#Format Thailand population
#age_groups <- c(2, 6, 12, 18, 60) 

popthai.to.use <- c(1637070, 4000962, 6131617, 6227226, 38377518, 5792970)
popthai.split <- c(rep(popthai.to.use[1]/2, 2), rep(popthai.to.use[2]/4, 4), rep(popthai.to.use[3]/6, 6), 
                   rep(popthai.to.use[4]/6, 6), rep(popthai.to.use[5]/42, 42), popthai.to.use[6])

#stratify_by_age(popthai.split, age_groups)
popthai <- data.frame(X = 1:61,
                      X2005 = popthai.split,
                      X2006 = popthai.split,
                      X2007 = popthai.split,
                      X2008 = popthai.split,
                      X2009 = popthai.split
)

rm(popthai.to.use, popthai.split)