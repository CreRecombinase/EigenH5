#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include <blosc_filter.h>
#include <Rcpp.h>


//[[Rcpp::export]]
std::string openFileHandleRead(const std::string filepath){
  namespace fs = stdx::filesystem;
  fs::path dp(filepath);
  return(std::to_string(H5Fopen(dp.c_str(), H5F_ACC_RDONLY, NULL)));
}


//[[Rcpp::export]]
size_t closeFileHandle(const std::string fh){
  return(H5Fclose( std::stoull(fh) ));
}



// [[Rcpp::interfaces(r,cpp)]]
//[[Rcpp::export]]
void start_blosc(){
  Rcpp::Environment env = Rcpp::Environment::global_env();
  auto r = register_blosc(nullptr,nullptr);
  Rcpp::LogicalVector bv(1);
  bv[0]=r==1;
  env["..blosc"]=bv;
  auto rr = register_lzf();
  bv[0]=rr==1;
   env["..lzf"]=bv;
  auto nr = register_zstd();
  bv[0]=nr==1;
  env["..zstd"]=bv;

}





//[[Rcpp::export]]
bool check_blosc(){
  Rcpp::Environment env = Rcpp::Environment::global_env();
  return(env["..blosc"]);
}




//[[Rcpp::export]]
bool is_transposed(const std::string filename,
                   const std::string groupname,
                   const std::string dataname){
  return(HighFive::File(filename,HighFive::File::ReadOnly).getGroup(groupname).getDataSet(dataname).isTransposed());
}
