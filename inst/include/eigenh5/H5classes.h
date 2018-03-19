#ifndef RCPP_CLASSES_H
#define RCPP_CLASSES_H

//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp11)]]

class H5File{
 public:

  std::string filename;
  bool readOnly;
  HighFive::File File;
 H5File(const std::string filename_,const bool readOnly_):filename(filename_),
    readOnly(readOnly_),
    File(filename,readOnly ? HighFive::File::ReadOnly : HighFive::File::ReadWrite | HighFive::File::Create){}
};




class H5Group{
 public:
  std::string groupname;
  HighFive::Group Group;
 H5Group(H5File file):groupname("/"),Group(file.File.getGroup(groupname)){
  }
 H5Group(H5File file,std::string groupname_):groupname("/"),Group(file.File.getGroup(groupname)){
  }
};



#endif
