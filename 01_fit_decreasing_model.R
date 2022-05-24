setwd(paste0(Sys.getenv("HOME"), "/US-cities-COVID-mobility/"))
rm(list=ls())


library(TMB)


####compile model####
model_folder <- "TMB_code/"
model_name <- "decreasing_model"

model_path <- paste0(model_folder, model_name)
tryCatch(dyn.unload(dynlib(model_path)),
         error = function(e) print(e))
compile(paste0(model_path, ".cpp"))
dyn.load(dynlib(model_path))


####load data####
city_i <- 1

load("city_names_df.RData") #match city names to number
#Boston MA (city #4) was excluded from all analysis


#Objects loaded:
# trips_model_input_age
#   response data. Matrices that give the number of trips traveled on each
#   route in each week, each row is a route and each column is a week.
#   There is one matrix per age group
#
# X_route_age
#   design matrices of variables that are associated with each route
#   (pair of zip codes). In this case this is an indicator variable for 
#   distance quantile. One matrix for each age group, which allows for 
#   e.g. different routes being excluded from analysis in different age 
#   groups (though in practice these matrices are often the same)
#
# X_loc_age
#   design matrices of variables that are associated with each zip code.
#   In this case this is (normalised) proportion of high income subscribers
#   and median household income in the zip code. Again one matrix for each age
#   group (in this case in practice likely to vary due to the different
#   subscribers by age group).
#
# X_temp_list
#   design matrices of variables that vary over time, in this case relative 
#   COVID case incidence. Each entry is a matrix representing a specific week.
#   The time-varying variables are assumed not to be associated with specific
#   age groups
#
# zip_pop
#   matrix representing the number of subscribers of each age group in each 
#   zip code. Each row is an age group, column is a zip code
#
# origin_index
#   Each route is an unordered pair of zip codes (i, j), where i, j,
#   represent the ordinals of the zip codes (not the actual 5 digit code). 
#   This vector gives the first code associated with each route. E.g. 
#   if the first route is the pair (i, j) then the first entry of 
#   origin_index will be i.
#
# dest_index
#   As above but with the second zip code of each pair, so the first entry 
#   would be j.
#
# n_weeks_use
#   How many weeks were chosen to fit too. This value can be chosen to cover
#   the time period where travel was decreasing



####create model object and fit####

city_i <- 1
#uncomment loop below to run all models

for(city_i in setdiff(1:27, 4)){
  print(city_i)
  
  city_name <- city_names_df$city_names[city_names_df$city_no == city_i]
  load(paste0("decreasing_model_data/city_",city_i,
              "_", city_name, ".RData"))
  
  
  model_silent <- TRUE
  m <- MakeADFun(
    data = list(design_matrix_route=X_route_age,
                design_matrix_loc=X_loc_age,
                design_matrix_temporal=X_temp_list[1:(n_weeks_use - 1)],
                Y_trips_all=trips_model_input_age,
                trips_censored=matrix(0, 1, 1),
                pops=t(zip_pop),
                origin_index_all=origin_index,
                dest_index_all=dest_index,
                time_varying_effects=1
    ),
    parameters = list(log_trip_rate1=rep(-3, dim(trips_model_input_age[[1]])[1]),
                      log_trip_rate2=rep(-3, dim(trips_model_input_age[[2]])[1]),
                      log_trip_rate3=rep(-3, dim(trips_model_input_age[[3]])[1]),
                      beta_route=rep(0, dim(X_route_age[[1]])[2]),
                      beta_loc=rep(0, dim(X_loc_age[[1]])[2]),
                      beta_temporal=rep(0, 1),
                      beta_age=rep(0, 3), 
                      log_time_effects = rep(-3, n_weeks_use-1)
    ),
    DLL = model_name,
    silent=model_silent,
    random=c("log_trip_rate1",
             "log_trip_rate2",
             "log_trip_rate3")
  )
  
  ptm <- proc.time()
  fit <- nlminb(m$par, m$fn, m$gr, control=list(iter.max=500,eval.max=500))
  ptm2 <- proc.time()
  print("Time to fit")
  print(ptm2 - ptm)
  
  pars <- fit$par
  
  save(pars, file = paste0("decreasing_model_fits/pars_city_",
                           city_i, ".RData"))
  
}