#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp11)]]
#ifdef USE_BLOSC
#include <blosc_filter.h>
#endif
#include <Rcpp.h>


//[[Rcpp::export]]
std::string openFileHandleRead(const std::string filepath){
  return(std::to_string(H5Fopen(filepath.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT)));
}


//[[Rcpp::export]]
size_t closeFileHandle(const std::string fh){
  return(H5Fclose( std::stoull(fh) ));
}



// [[Rcpp::interfaces(r,cpp)]]
//[[Rcpp::export]]
void start_blosc(){
  Rcpp::Environment env = Rcpp::Environment::global_env();
  #ifdef USE_BLOSC
  auto r = register_blosc(nullptr,nullptr);
  #else
  auto r =0;
  #endif
  Rcpp::LogicalVector bv(1);
  bv[0]=r==1;
  env["..blosc"]=bv;
#ifdef USE_LZF
  auto rr = register_lzf();
  bv[0]=rr==1;
   env["..lzf"]=bv;
#endif
  auto nr = register_zstd();
  bv[0]=nr==1;
  env["..zstd"]=bv;

}





//[[Rcpp::export]]
bool check_blosc(){
  Rcpp::Environment env = Rcpp::Environment::global_env();
  return(env["..blosc"]);
}

