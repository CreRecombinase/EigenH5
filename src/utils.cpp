#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
#include <experimental/filesystem>
#include <optional>
#include <array>

// [[Rcpp::interfaces(r, cpp)]]









//[[Rcpp::export]]
Rcpp::IntegerVector guess_chunks(const std::vector<int> dimsize){
  std::vector<size_t> tdims(dimsize.begin(),dimsize.end());
  return(Rcpp::wrap(HighFive::Filter::guess_chunk(tdims)));
}


//[[Rcpp::export]]
bool exists_h5(const std::string filename,
	       const std::string groupname="/",
	       const std::string dataname=""){
  using namespace HighFive;
  bool ret = false;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(!file.exist(groupname)){
    ret = false;
  }
  if(dataname==""){
    ret = true;
  }else{
    ret = file.getGroup(groupname).exist(dataname);
  }

  return(ret);
}

//[[Rcpp::export]]
bool isObject(const std::string filename, std::string dataname){
  namespace fs = std::experimental::filesystem;
  bool ret = false;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  ret = file.exist(dataname);
  return(ret);
}


//[[Rcpp::export]]
bool isDataSet(const std::string filename, std::string dataname){

  namespace fs = std::experimental::filesystem;
  fs::path d_path = dataname;

  bool ret = false;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(file.exist(dataname)){
    ret = file.isDataSet(dataname);
    return(ret);
  }else{
      Rcpp::stop("Object does not exist!");
  }
  Rcpp::stop("unable to open file for reading!");
}


//[[Rcpp::export]]
bool isGroup(const std::string filename, std::string dataname){

  namespace fs = std::experimental::filesystem;
  fs::path d_path = dataname;

  bool ret = false;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(file.exist(dataname)){
    ret = file.isGroup(dataname);
  }else{
    Rcpp::stop("Object does not exist!");
  }
  return(ret);
}

//[[Rcpp::export]]
Rcpp::StringVector ls_h5(const std::string filename,Rcpp::CharacterVector groupname = Rcpp::CharacterVector::create("/"),
                         bool full_names=false){

  HighFive::File file(filename,HighFive::File::ReadOnly);
  HighFive::Group grp;
  namespace fs = std::experimental::filesystem;
  std::string tgroupname = Rcpp::as<std::string>(groupname(0));
  if(tgroupname=="."){
    tgroupname="/";
  }
  //fs::path d_path = dataname;
  fs::path g_path(tgroupname);

  //HDF5ErrMapper::ToException<GroupException>(
  grp = file.getGroup(g_path);

  const size_t num_cols = grp.getNumberObjects();
  Rcpp::StringVector retvec(num_cols);
  for(int i=0; i<num_cols;i++){
    fs::path tpath = full_names ? g_path : fs::path();
    tpath/=fs::path(grp.getObjectName(i));
    std::string tst = tpath;
    retvec[i]=tst;
  }

  return(retvec);
}
//[[Rcpp::export]]
Rcpp::StringVector typeof_h5(const std::string &filename,
                   const std::string &datapath){

  namespace fs = std::experimental::filesystem;
  using namespace HighFive;

  if(isGroup(filename,datapath)){
    return(Rcpp::StringVector::create("list"));
  }
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto arg = file.getDataSet(datapath);
  SEXPTYPE h2t = h2r_T(arg.getDataType().getId());
  Rcpp::StringVector ret;
  if (h2t == REALSXP){
    ret = Rcpp::StringVector::create("double");
  }else{
    if (h2t == INTSXP){
      ret = Rcpp::StringVector::create("integer");
    }else{
      if(h2t == STRSXP){
	ret = Rcpp::StringVector::create("character");
      }else{
	ret = Rcpp::StringVector::create("NULL");
      }
    }
  }

  return(ret);
}




//[[Rcpp::export]]
Rcpp::DataFrame file_acc_ct(const std::string filename){
  using namespace Rcpp;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto ret = Rcpp::DataFrame::create(
      _["count"]=Rcpp::IntegerVector::create(
        file.getObjCount(H5F_OBJ_FILE),
        file.getObjCount(H5F_OBJ_DATASET),
        file.getObjCount(H5F_OBJ_GROUP),
        file.getObjCount(H5F_OBJ_DATATYPE),
        file.getObjCount(H5F_OBJ_ATTR)),
        _["type"]=Rcpp::StringVector::create(
          "Files",
          "DataSets",
          "Groups",
          "DataTypes",
          "Attributes"
        ));

  return(ret);

}



//[[Rcpp::export]]
Rcpp::IntegerVector dim_h5(const std::string &filename,const std::string datapath){

  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto ret =Rcpp::wrap(file.getDataSet(datapath).getDataDimensions());
  return(ret);
}
