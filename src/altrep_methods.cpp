#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

// because we need to initialize the altrep class
#include <R_ext/Rdynload.h>

#if R_VERSION < R_Version(3, 6, 0)

// workaround because R's <R_ext/Altrep.h> not so conveniently uses `class`
// as a variable name, and C++ is not happy about that
//
// SEXP R_new_altrep(R_altrep_class_t class, SEXP data1, SEXP data2);
//
#define class klass

// Because functions declared in <R_ext/Altrep.h> have C linkage
extern "C" {
#include <R_ext/Altrep.h>
}

// undo the workaround
#undef class

#else
#include <R_ext/Altrep.h>
#endif


bool is_compact_seq(SEXP x) {

  if(!ALTREP(x)){

    return false;
  }
  R_xlen_t info_l=::Rf_xlength(R_altrep_data1(x));
  if(info_l!=3){

    return false;
  }

  return true;
}



bool is_compact_forward(SEXP x){
  PROTECT(x);
  if(!ALTREP(x)){

    return false;
  }
  R_xlen_t info_l=::Rf_xlength(R_altrep_data1(x));
  if(info_l!=3){
    UNPROTECT(1);
    return false;
  }
  if(((int) REAL0(info)[2])!=1){
    return false;
  }
  return true;
}

std::pair<int,int> altrep_pair(SEXP x){
  if(!is_compact_seq(x)){
    Rcpp::stop("x is not a compact sequence!");
  }
  const auto ard = R_altrep_data1(x);
  const int seq_start =INT(ard[0])-1;
  const int seq_size= INT(ard[1])-seq_start;

  return(std::make_pair(seq_start,seq_size));
}


boost::variant<std::pair<int,int>,Rcpp::IntegerVector> dispatch_subset(SEXP x){

  auto my_t = TYPEOF(x);
  if(my_t!=INTSXP){
    Rcpp::stop("cannot dispatch non-integer types (yet)");
  }
  R_xlen_t info_l=::Rf_xlength(x);
  if(info_l==0){
    return(std::make_pair(0,-1));
  }
  if(is_compact_forward(x)){
    return(altrep_pair(x));
  }
  return(Rcpp::as<Rcpp::IntegerVector>(x));
}
