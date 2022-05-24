#include <TMB.hpp>
#include <math.h>
#include <stdio.h>

//define container for list of matrices - LOM type
template <class Type>
struct LOM_t : vector<matrix<Type> > {
  //constructor
  LOM_t(SEXP x){ /*x is list from R*/
    (*this).resize(LENGTH(x)); /*resize pointer to be the right size*/
    for(int i=0; i<LENGTH(x); i++){ 
      SEXP m = VECTOR_ELT(x, i); /*get ith element of list */
      (*this)(i) = asMatrix<Type>(m); /*put it as ith element*/
    }
  }
};

template <class Type>
struct LOIV_t : vector<vector<int> > {
  //constructor
  LOIV_t(SEXP x){ /*x is list from R*/
    (*this).resize(LENGTH(x)); /*resize pointer to be the right size*/
    for(int i=0; i<LENGTH(x); i++){ 
      SEXP m = VECTOR_ELT(x, i); /*get ith element of list */
      (*this)(i) = asVector<int>(m); /*put it as ith element*/
    }
  }
};



template<class Type>
Type objective_function<Type>::operator() ()
{
  
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;
  
  
  DATA_STRUCT(design_matrix_route, LOM_t);
  DATA_STRUCT(design_matrix_loc, LOM_t);
  // DATA_MATRIX(design_matrix_route);
  // DATA_MATRIX(design_matrix_loc);
  DATA_STRUCT(design_matrix_temporal, LOM_t);
  DATA_STRUCT(Y_trips_all, LOM_t);
  // DATA_MATRIX(Y_trips);
  DATA_IMATRIX(trips_censored);
  DATA_MATRIX(pops);
  DATA_STRUCT(origin_index_all, LOIV_t);
  DATA_STRUCT(dest_index_all, LOIV_t);
  DATA_INTEGER(time_varying_effects);



  // PARAMETER(beta_0);
  PARAMETER_VECTOR(log_trip_rate1);
  PARAMETER_VECTOR(log_trip_rate2);
  PARAMETER_VECTOR(log_trip_rate3);
  // PARAMETER_VECTOR(log_trip_rate_back);
  PARAMETER_VECTOR(beta_route);
  PARAMETER_VECTOR(beta_loc);
  PARAMETER_VECTOR(beta_temporal);
  PARAMETER_VECTOR(beta_age);
  PARAMETER_VECTOR(log_time_effects);

  int n_loc = pops.rows();
  int i=0;
  // matrix<Type> Y_trips = Y_trips_all(i);
  // std::cout<<Y_trips_all.size();
  int n_t=Y_trips_all((int) 0).cols();


  int n_age=beta_age.size();
  //priors
  Type beta_mean = 0;
  Type beta_sd = 1;
  Type f=0;
  
  for(int i=0; i<log_trip_rate1.size(); i++){
    f -= dnorm(log_trip_rate1(i), (Type) -3, (Type) 0.5);
  }

  for(int i=0; i<log_trip_rate2.size(); i++){
    f -= dnorm(log_trip_rate2(i), (Type) -3, (Type) 0.5);
  }

  for(int i=0; i<log_trip_rate3.size(); i++){
    f -= dnorm(log_trip_rate3(i), (Type) -3, (Type) 0.5);
  }



  for(int i=1; i<(n_t-1); i++){
    f -= dnorm(log_time_effects(i), log_time_effects(i-1), (Type) 0.1, true);
  }

  for(int i=1; i<beta_route.size(); i++){
    f -= dnorm(beta_route(i), beta_mean, beta_sd, true);
  }

  for(int i=1; i<beta_loc.size(); i++){
    f -= dnorm(beta_loc(i), beta_mean, beta_sd, true);
  }

  for(int i=1; i<beta_temporal.size(); i++){
    f -= dnorm(beta_temporal(i), beta_mean, beta_sd, true);
  }

  for(int i=1; i<beta_age.size(); i++){
    f -= dnorm(beta_age(i), beta_mean, beta_sd, true);
  }



  vector<Type> time_effects = exp(log_time_effects);

  // vector<Type> trip_rate_back = exp(log_trip_rate_back);



//
//
//   vector<Type> baseline_route_h(n_trips);
//   vector<Type> baseline_loc_h(n_loc);
//   
  vector<Type> log_trip_rate;
  for(int age_i=0; age_i<n_age; age_i++){
    
    if(age_i==0){
      log_trip_rate = log_trip_rate1;
    }else if(age_i==1){
      log_trip_rate = log_trip_rate2;
    }else{
      log_trip_rate = log_trip_rate3;
    }
    vector<Type> trip_rate = exp(log_trip_rate);

    vector<Type> log_baseline_route_h = design_matrix_route(age_i) * beta_route;
    vector<Type> log_baseline_loc_h = design_matrix_loc(age_i) * beta_loc;

    vector<int> origin_index = origin_index_all(age_i);
    vector<int> dest_index = origin_index_all(age_i);
    matrix<Type> Y_trips = Y_trips_all(age_i);
    int n_trips=Y_trips.rows();
    vector<Type> H_t(n_trips);
    vector<Type> H_t_back(n_trips);


    for(int i=0; i<n_trips; i++){
      H_t(i) = 0;
      H_t_back(i) = 0;
    }

    // vector<Type> prob_t;
    vector<Type> trip_ratio;
    vector<Type> trip_ratio_back;
    vector<Type> log_baseline_temporal_h;

    int a, b; // origin/destination indices

    matrix<Type> exp_trips(n_trips, n_t);
    int reached=0;

    for(int i=0; i<n_trips; i++){
      for(int j=0; j<n_t; j++){
        exp_trips(i, j) = 0;
      }
    }

    for(int i=0; i<(n_t-1); i++){
    // for(int i=0; i<1; i++){
      trip_ratio = exp(-H_t);
      trip_ratio_back = exp(-H_t_back);

      if(time_varying_effects){
        log_baseline_temporal_h = design_matrix_temporal(i) * beta_temporal;
      }
      for(int j=0; j<n_trips; j++){
        a = origin_index(j);
        b = dest_index(j);

        f -= dpois(Y_trips(j, i),
               2 * (pops(a, age_i) * trip_rate(j) * trip_ratio(j) +
                 pops(b, age_i) * trip_rate(j) * trip_ratio_back(j)),
                 true);

        // exp_trips(j, i) = 2 * (pops(a) * trip_rate(j) * trip_ratio(j) +
        //   pops(b) * trip_rate(j) * trip_ratio_back(j));

        // exp_trips(j, i) = 2 * (pops(a) * trip_rate(j) * trip_ratio(j) +
        //   pops(b) * trip_rate(j) * trip_ratio_back(j));
        // reached=1;

        // //update H_t and H_t_back
        // H_t(j) += time_effects(i) * exp(log_baseline_route_h(j) +
        //   log_baseline_loc_h(a));

        if(time_varying_effects){
          H_t(j) += time_effects(i) * exp(log_baseline_route_h(j) +
            log_baseline_temporal_h(a) + log_baseline_loc_h(a) + beta_age(age_i));

          H_t_back(j) += time_effects(i) * exp(log_baseline_route_h(j) +
            log_baseline_temporal_h(b) + log_baseline_loc_h(b) + beta_age(age_i));
        }else{
          H_t(j) += time_effects(i) * exp(log_baseline_route_h(j) +
            log_baseline_loc_h(a) + beta_age(age_i));

          H_t_back(j) += time_effects(i) * exp(log_baseline_route_h(j) +
            log_baseline_loc_h(b) + beta_age(age_i));
        }

      }
    }

  //
    trip_ratio = exp(-H_t);

    for(int j=0; j<n_trips; j++){
      a = origin_index(j);
      b = dest_index(j);

      f -= dpois(Y_trips(j, n_t-1),
                 2 * (pops(a, age_i) * trip_rate(j) * trip_ratio(j) +
                   pops(b, age_i) * trip_rate(j) * trip_ratio_back(j)),
                   true);
    }

  }


  // REPORT(exp_trips);
  // REPORT(n_t);
  // REPORT(reached);
  return(f);
}
