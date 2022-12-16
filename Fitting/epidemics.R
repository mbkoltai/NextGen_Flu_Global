# # list the epidemics
# 
# # for each epidemic, need 
# # - start date
# # - end date
# # initial parameters (rep, trans, sus, init, blank, blank)
# # data points
# 

epidemics_to_fit <- list(
  
  list(start = as.Date("2006-04-01"), 
       end = as.Date("2006-12-31"),
       initial_params = c(-10.8, 10, 0.6, 3, 0, 0), 
       data_points = unlist(fluAH1_epidemics[13:21,"flu"]), 
       type = "AH1N1"), 
  
  list(start = as.Date("2007-08-01"), 
       end = as.Date("2008-01-31"),
       initial_params = c(-11.5, 10, 0.6, 3, 0, 0), 
       data_points = unlist(fluAH1_epidemics[29:34,"flu"]), 
       type = "AH1N1"),
  
  list(start = as.Date("2005-04-01"), 
       end = as.Date("2005-10-31"),
       initial_params = c(-10.8, 10, 0.8, 2, 0, 0), 
       data_points = unlist(fluAH3_epidemics[1:7,"flu"]), 
       type = "AH3N2"),
  
  list(start = as.Date("2006-12-31"), 
       end = as.Date("2007-04-29"),
       initial_params = c(-10.8, 11, 0.8, 5, 0, 0), 
       data_points = unlist(fluAH3_epidemics[21:25,"flu"]), 
       type = "AH3N2"),
  
  list(start = as.Date("2008-05-01"), 
       end = as.Date("2008-10-31"),
       initial_params = c(-10, 9.5, 0.6, 3, 0, 0), 
       data_points = unlist(fluAH3_epidemics[38:43,"flu"]), 
       type = "AH3N2"),
  
  list(start = as.Date("2007-07-01"), 
       end = as.Date("2008-04-30"),
       initial_params = c(-10.8, 10, 0.6, 1, 0, 0), 
       data_points = unlist(fluB_epidemics[28:37,"flu"]), 
       type = "B"),
  
  
  
  
  list(start = as.Date("2006-04-01"), 
       end = as.Date("2006-12-31"),
       initial_params = c(-10, 8, 0.8, 1, 0, 0), 
       data_points = unlist(fluAH1_epidemics[13:21,"flu"]), 
       type = "AH1"), 
  
  list(start = as.Date("2007-08-01"), 
       end = as.Date("2008-01-31"),
       initial_params = c(-12, 10, 0.8, 1, 0, 0), 
       data_points = unlist(fluAH1_epidemics[29:34,"flu"]), 
       type = "AH1"),
  
  list(start = as.Date("2005-04-01"), 
       end = as.Date("2005-10-31"),
       initial_params = c(-12, 10, 0.9, 1, 0, 0), 
       data_points = unlist(fluAH3_epidemics[1:7,"flu"]), 
       type = "AH3"),
  
  list(start = as.Date("2006-12-31"), 
       end = as.Date("2007-04-29"),
       initial_params = c(-12, 7, 0.8, 2, 0, 0), 
       data_points = unlist(fluAH3_epidemics[21:25,"flu"]), 
       type = "AH3"),
  
  list(start = as.Date("2008-05-01"), 
       end = as.Date("2008-10-31"),
       initial_params = c(-11, 12, 0.9, 1, 0, 0), 
       data_points = unlist(fluAH3_epidemics[38:43,"flu"]), 
       type = "AH3"),
  
  list(start = as.Date("2007-07-01"), 
       end = as.Date("2008-04-30"),
       initial_params = c(-12, 10, 0.6, 2, 0, 0), 
       data_points = unlist(fluB_epidemics[28:37,"flu"]), 
       type = "B")
)





