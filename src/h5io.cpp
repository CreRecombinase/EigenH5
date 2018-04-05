#include <EigenH5.h>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include <blosc_filter.h>


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


  // Rcpp::LogicalVector bv = env["..blosc"];
  //Rcout << "Stooge Nb 2 is: " << v[1] << std::endl;



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
