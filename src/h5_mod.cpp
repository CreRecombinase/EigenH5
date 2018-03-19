#include <EigenH5.h>
#include <Rcpp.h>



using namespace Rcpp;


RCPP_MODULE(File_module) {
  class_<H5File>( "H5File" )
  
    .constructor<std::string,bool>()

    .field( "filename", &H5File::filename )
    .field( "readOnly", &H5File::readOnly )
    ;
}

RCPP_EXPOSED_CLASS(H5File)

RCPP_MODULE(Group_module){
  class_<H5Group>( "H5Group" )
  
    .constructor<H5File,std::string>()

    .field( "groupname", &H5Group::groupname )
    ;
}
  
