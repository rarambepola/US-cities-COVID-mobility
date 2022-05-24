setwd(paste0(Sys.getenv("HOME"), "/US-cities-COVID-mobility/"))
rm(list=ls())


library(TMB)


####compile model####
model_folder <- "TMB_code/"
model_name <- "summer_model"


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
# Y_trips_list
#   response data. Vectors that give the mean trips traveled on each
#   route per week on each route over summer period.
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
#
# baseline_trip_rates
#   Baseline trip rates for each route learned in the decreasing model.
#   For this mock example this is one vector but in practice it would usually
#   be different for each age group (see section creating the model 
#   object below)



city_i <- 1
#uncomment loop below to run all models

# for(city_i in setdiff(1:27, 4)){
  city_name <- city_names_df$city_names[city_names_df$city_no == city_i]
  load(paste0("summer_model_data/city_",city_i,
              "_", city_name, ".RData"))
  
  
  
  model_silent <- TRUE
  m <- MakeADFun(
    data = list(design_matrix_route=X_route_age,
                design_matrix_loc=X_loc_age,
                Y_trips_all=Y_trips_list,
                pops=t(zip_pop),
                origin_index_all=origin_index,
                dest_index_all=dest_index,
                trip_rate1=baseline_trip_rates,
                trip_rate2=baseline_trip_rates,
                trip_rate3=baseline_trip_rates
    ),
    parameters = list(beta_0=-3,
                      beta_route=rep(0, dim(X_route_age[[1]])[2]),
                      beta_loc=rep(0, dim(X_loc_age[[1]])[2]),
                      beta_age=rep(0, 3)
    ),
    DLL = model_name,
    silent=model_silent
  )
  
  ptm <- proc.time()
  fit <- nlminb(m$par, m$fn, m$gr, control=list(iter.max=500,eval.max=500))
  ptm2 <- proc.time()
  print("Time to fit")
  print(ptm2 - ptm)
  
  # print(fit)
  
  pars <- fit$par
  
  save(pars, file = paste0("summer_model_fits/pars_city_",
                           city_i, ".RData"))

# }
