#include "EigenH5.h"
#include "highfive/highfive.hpp"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
// [[Rcpp::interfaces(cpp)]]
#include <optional>
#include <array>




// 
// 
// 
// 
// RCPP_MODULE(FileManager) {
//   using namespace Rcpp;
// 
//   class_<FileManager>( "FileManager")
//     .default_constructor("Default constructor") // This exposes the default constructor
//     .method("print", &FileManager::print)
//     .method("get_file", &FileManager::get_file)
//     // This exposes the print method
//     //    .property("Bender", &MyClass::getBender, &MyClass::setBender) // and this shows how we set up a property
//   ;
// }
