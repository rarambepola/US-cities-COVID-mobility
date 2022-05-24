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

template <class Type>
struct LOV_t : vector<vector<Type> > {
  //constructor
  LOV_t(SEXP x){ /*x is list from R*/
    (*this).resize(LENGTH(x)); /*resize pointer to be the right size*/
    for(int i=0; i<LENGTH(x); i++){ 
      SEXP m = VECTOR_ELT(x, i); /*get ith element of list */
      (*this)(i) = asVector<Type>(m); /*put it as ith element*/
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
  DATA_STRUCT(Y_trips_all, LOV_t);
  DATA_MATRIX(pops);
  DATA_STRUCT(origin_index_all, LOIV_t);
  DATA_STRUCT(dest_index_all, LOIV_t);
  DATA_VECTOR(trip_rate1);
  DATA_VECTOR(trip_rate2);
  DATA_VECTOR(trip_rate3);

  PARAMETER(beta_0);
  PARAMETER_VECTOR(beta_route);
  PARAMETER_VECTOR(beta_loc);
  PARAMETER_VECTOR(beta_age);

  int n_age=beta_age.size();
  //priors
  Type beta_mean = 0;
  Type beta_sd = 0.4;
  Type f=0;
  


  for(int i=0; i<beta_route.size(); i++){
    f -= dnorm(beta_route(i), beta_mean, beta_sd, true);
  }

  for(int i=0; i<beta_loc.size(); i++){
    f -= dnorm(beta_loc(i), beta_mean, beta_sd, true);
  }


  for(int i=0; i<beta_age.size(); i++){
    f -= dnorm(beta_age(i), beta_mean, beta_sd, true);
  }



  
  vector<Type> trip_rate;
  vector<Type>log_starting_fraction;
  vector<Type>log_starting_fraction_back;


  for(int age_i=0; age_i<n_age; age_i++){
    if(age_i==0){
      trip_rate = trip_rate1;
    }else if(age_i==1){
      trip_rate = trip_rate2;
    }else{
      trip_rate = trip_rate3;
    }

    vector<Type> log_baseline_route_h = design_matrix_route(age_i) * beta_route;
    vector<Type> log_baseline_loc_h = design_matrix_loc(age_i) * beta_loc;

    vector<int> origin_index = origin_index_all(age_i);
    vector<int> dest_index = origin_index_all(age_i);
    vector<Type> Y_trips = Y_trips_all(age_i);
    int n_trips=Y_trips.size();
    


    int a, b; // origin/destination indices

    Type rate_frac, rate_frac_back;
    for(int j=0; j<n_trips; j++){
      a = origin_index(j);
      b = dest_index(j);
      
      rate_frac = exp(beta_0 + log_baseline_loc_h(a) + 
        log_baseline_route_h(j) + beta_age(age_i));
      rate_frac_back = exp(beta_0 + log_baseline_loc_h(b) + 
        log_baseline_route_h(j) + beta_age(age_i));
      

      if(Y_trips(j) > 0){
        f -= dpois(Y_trips(j),
                   2 * (pops(a, age_i) * trip_rate(j) * rate_frac +
                     pops(b, age_i) * trip_rate(j) * rate_frac_back),
                     true);
      }else if(Y_trips(j) == 0){
      for(int k=0; k<100; k++){
        f -= dpois((Type) k,
                   2 * (pops(a, age_i) * trip_rate(j) * rate_frac +
                     pops(b, age_i) * trip_rate(j) * rate_frac_back),
                     true);
        }
      }
    }
  }
  


  return(f);
}
