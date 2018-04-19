#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include <experimental/filesystem>
#include <optional>
#include <array>

// [[Rcpp::interfaces(r, cpp)]]
//std::unordered_map<int,char>
// Rcpp::XPtr<  std::unordered_map<fs::path,HighFive::File> > generate_fmap(){
//   std::unordered_map<fs::path,HighFive::File>* v = new std::unordered_map<fs::path,HighFive::File> ;
//   
//   
//   /* wrap the pointer as an external pointer */
//   /* this automatically protected the external pointer from R garbage
//    collection until p goes out of scope. */
//   XPtr< std::unordered_map<fs::path,HighFive::File> > p(v) ;
//   
//   /* return it back to R, since p goes out of scope after the return
//    the external pointer is no more protected by p, but it gets
//    protected by being on the R side */
//   return( p ) ;
// }

std::shared_ptr<HighFive::File> file_rp(const std::string filename){
  try{
    HighFive::SilenceHDF5 silence;
    return(std::make_shared<HighFive::File>(HighFive::File(filename,HighFive::File::ReadOnly)));
  }catch(HighFive::Exception & err){
    return(nullptr);
  }
}



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









