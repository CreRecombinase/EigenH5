#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include <experimental/filesystem>
#include <optional>
#include <array>

// [[Rcpp::interfaces(r, cpp)]]


std::shared_ptr<HighFive::File> file_r(const std::string filename){
  try{
    HighFive::SilenceHDF5 silence;
    return(std::make_shared<HighFive::File>(HighFive::File(filename,HighFive::File::ReadOnly)));
  }catch(HighFive::Exception & err){
    return(nullptr);
  }
}
std::shared_ptr<HighFive::File> file_cw(const std::string filename){
  HighFive::SilenceHDF5 silence;
  try{
    return(std::make_shared<HighFive::File>(HighFive::File(filename,HighFive::File::ReadWrite| HighFive::File::Create)));
  }catch(HighFive::Exception & err){
    return(nullptr);
  }
}
std::shared_ptr<HighFive::File> file_w(const std::string filename){
  HighFive::SilenceHDF5 silence;
  try{
    return(std::make_shared<HighFive::File>(HighFive::File(filename,HighFive::File::ReadWrite)));
  }catch(HighFive::Exception & err){
    return(nullptr);
  }
}
