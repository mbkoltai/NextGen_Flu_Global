# list the epidemics

# for each epidemic, need 
# - start date
# - end date
# initial parameters (rep, trans, sus, init, blank, blank)
# data points

epidemics_to_fit <- list(
  
  list(start = as.Date("2006-03-01"), 
       end = as.Date("2006-11-30"),
       initial_params = c(-10.8, 1, 0.6, 1, 0, 0), 
       data_points = unlist(fluAH1_epidemics[12:20,"flu"]), 
       type = "AH1"), 
  
  list(start = as.Date("2007-06-01"), 
       end = as.Date("2008-02-29"),
       initial_params = c(-12.5, 8, 0.82, -4, 0, 0), 
       data_points = unlist(fluAH1_epidemics[27:35,"flu"]), 
       type = "AH1"),
  
  list(start = as.Date("2008-07-01"), 
       end = as.Date("2009-01-31"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluAH1_epidemics[40:46,"flu"]), 
       type = "AH1"),
  
  list(start = as.Date("2005-04-01"), 
       end = as.Date("2005-09-30"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluAH3_epidemics[2:6,"flu"]), 
       type = "AH3"),
  
  list(start = as.Date("2006-10-01"), 
       end = as.Date("2006-03-31"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluAH3_epidemics[19:24,"flu"]), 
       type = "AH3"),
  
  list(start = as.Date("2007-04-01"), 
       end = as.Date("2007-11-30"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluAH3_epidemics[25:32,"flu"]), 
       type = "AH3"),
  
  list(start = as.Date("2008-03-01"), 
       end = as.Date("2009-02-28"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluAH3_epidemics[36:47,"flu"]), 
       type = "AH3"),
  
  list(start = as.Date("2005-08-01"), 
       end = as.Date("2006-02-28"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluB_epidemics[5:11,"flu"]), 
       type = "B"),

  list(start = as.Date("2006-09-01"), 
       end = as.Date("2007-02-28"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluB_epidemics[18:23,"flu"]), 
       type = "B"),
  
  list(start = as.Date("2007-08-01"), 
       end = as.Date("2008-04-30"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluB_epidemics[29:37,"flu"]), 
       type = "B"),
  
  list(start = as.Date("2008-05-01"), 
       end = as.Date("2008-11-30"),
       initial_params = c(-10.8, 0.01, 0.6, 1, 0, 0), 
       data_points = unlist(fluB_epidemics[38:44,"flu"]), 
       type = "B")
)
