// [[Rcpp::depends(BH)]]
#include <cstdbool>
#include <variant>
#include <Rcpp.h>
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


template<typename T>
constexpr std::string_view sexp_f(const T tr) noexcept{

  if  (tr == 0){ return "NILSXP";}    /* nil = NULL */
  if  (tr == 1){ return "SYMSXP";}    /* symbols */
  if  (tr == 2){ return "LISTSXP";}    /* lists of dotted pairs */
  if  (tr == 3){ return "CLOSXP";}    /* closures */
  if  (tr == 4){ return "ENVSXP";}    /* environments */
  if  (tr == 5){ return "PROMSXP";}    /* promises: [un]evaluated closure arguments */
  if  (tr == 6){ return "LANGSXP";}    /* language constructs (special lists) */
  if  (tr == 7){ return "SPECIALSXP";}    /* special forms */
  if  (tr == 8){ return "BUILTINSXP";}    /* builtin non-special forms */
  if  (tr == 9){ return "CHARSXP";}    /* "scalar" string type (internal only)*/
  if  (tr == 10){ return "LGLSXP";}   /* logical vectors */
  if  (tr == 13){ return "INTSXP";}   /* integer vectors */
  if  (tr == 14){ return "REALSXP";}   /* real variables */
  if  (tr == 15){ return "CPLXSXP";}   /* complex variables */
  if  (tr == 16){ return "STRSXP";}   /* string vectors */
  if  (tr == 17){ return "DOTSXP";}   /* dot-dot-dot object */
  if  (tr == 18){ return "ANYSXP";}   /* make "any" args work */
  if  (tr == 19){ return "VECSXP";}   /* generic vectors */
  if  (tr == 20){ return "EXPRSXP";}   /* expressions vectors */
  if  (tr == 21){ return "BCODESXP";}   /* byte code */
  if  (tr == 22){ return "EXTPTRSXP";}   /* external pointer */
  if  (tr == 23){ return "WEAKREFSXP";}   /* weak reference */
  if  (tr == 24){ return "RAWSXP";}   /* raw bytes */
  if  (tr == 25){ return "S4SXP";}
  if  (tr == 30){ return "NEWSXP";}   /* fresh node creaed in new page */
  if  (tr == 31){ return "FREESXP";}   /* node released by GC */
  if  (tr == 99){ return "FUNSXP";}
  return "Don't Know";
};



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
  if(!ALTREP(x)){

    return false;
  }
  const auto info = R_altrep_data1(x);
  R_xlen_t info_l=::Rf_xlength(info);
  if(info_l!=3){
    return false;
  }
  if(((int) REAL0(info)[2])!=1){
    return false;
  }
  return true;
}

std::pair<int,std::optional<int>> altrep_pair(SEXP x){
  if(!is_compact_seq(x)){
    Rcpp::stop("x is not a compact sequence!");
  }
  const auto ard = R_altrep_data1(x);
  const auto ardd = REAL0(ard);
  const int seq_size =(int)ardd[0];
  const int seq_start= (int)ardd[1]-1;

  return(std::make_pair(seq_start,seq_size));
}


std::variant<std::pair<int,std::optional<int>> ,Rcpp::IntegerVector> dispatch_subset(SEXP x){

  auto my_t = TYPEOF(x);
  if(my_t!=INTSXP){
    Rcpp::stop("cannot dispatch non-integer types (yet)");
  }
  R_xlen_t info_l=::Rf_xlength(x);
  if(info_l==0){
    return(std::make_pair(0,std::nullopt));
  }
  if(is_compact_forward(x)){
    auto ap= altrep_pair(x);
    return(ap);
  }
  Rcpp::IntegerVector av=Rcpp::as<Rcpp::IntegerVector>(x);
  return(av);
}
