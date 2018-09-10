#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]


// RCPP_MODULE(mod_vec) {
// using namespace Rcpp;
//  using namespace HighFive;
// // we expose class std::vector<double>
// // as "vec" on the R side
//  class_<File>("H5File");
// // exposing constructors
//  .constructor<std::string,>()

//[[Rcpp::export]]
SEXP open_file_ro(const std::string filename){
  Rcpp::XPtr<HighFive::File> p(new HighFive::File(filename,HighFive::File::ReadOnly));
  return(p);
}

//[[Rcpp::export]]
SEXP open_file_rw(const std::string filename){
  Rcpp::XPtr<HighFive::File> p(new HighFive::File(filename,HighFive::File::ReadWrite |HighFive::File::Create));
  return(p);
}



//[[Rcpp::export]]
void  release_file( Rcpp::XPtr<HighFive::File> p){
  p.release();
}

//[[Rcpp::export]]
void  release_dataset( Rcpp::XPtr<HighFive::DataSet> p){
  p.release();
}

//[[Rcpp::export]]
void  release_group( Rcpp::XPtr<HighFive::Group> p){
  p.release();
}




//[[Rcpp::export]]
SEXP get_file_object(Rcpp::XPtr<HighFive::File> f, const std::string object_name){
  using obj_var=std::variant<HighFive::DataSet,HighFive::Group>;
  return(Rcpp::XPtr<obj_var>(new obj_var(f->getObject(object_name))));
}
//[[Rcpp::export]]
SEXP get_dataset(Rcpp::XPtr<HighFive::File> f, const std::string object_name){
  return(Rcpp::XPtr<HighFive::DataSet>(new HighFive::DataSet(f->getDataSet(object_name))));
}
//[[Rcpp::export]]
SEXP get_group(Rcpp::XPtr<HighFive::File> f, const std::string object_name){
  return(Rcpp::XPtr<HighFive::Group>(new HighFive::Group(f->getGroup(object_name))));
}




  
  








// bool xptr_release( XPtr< std::vector<int> > p) {
//     p.release();
//     return !p;
// }
