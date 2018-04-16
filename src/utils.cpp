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

    File file(filename,File::ReadOnly);
    if(!file.exist(groupname)){
      return(false);
    }
    if(dataname==""){
      return(true);
    }
    return(file.getGroup(groupname).exist(dataname));
}

//[[Rcpp::export]]
bool isObject(const std::string filename, std::string dataname){
  namespace fs = std::experimental::filesystem;
  fs::path d_path = dataname;
  if(auto file = file_r(filename)){
    if(auto grp = file->openGroup(d_path.parent_path())){
      if(auto obj = grp->openObject(d_path.filename())){
	return(true);
      }
    }
  }
  return(false);
}


//[[Rcpp::export]]
bool isDataSet(const std::string filename, std::string dataname){

  // if(groupname=="."){
  //   groupname="/";
  // }
  namespace fs = std::experimental::filesystem;
  fs::path d_path = dataname;


  if(auto file = file_r(filename)){
    auto grp = file->getGroup(d_path.parent_path());
    try{
      HighFive::SilenceHDF5 silence;
      grp.getDataSet(d_path.filename());
      return(true);
    }catch (HighFive::Exception& err) {
      // Rcpp::StringVector retvec(1);
      return(false);
    }
  }else{
    Rcpp::stop("unable to open file for reading!");
  }
}

//[[Rcpp::export]]
Rcpp::StringVector ls_h5(const std::string h5filepath,Rcpp::CharacterVector groupname = Rcpp::CharacterVector::create("/"),
                         bool full_names=false){

  HighFive::File file(h5filepath,HighFive::File::ReadOnly);
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
  fs::path d_path = datapath;
  auto arg = file.getGroup(d_path.parent_path()).getDataSet(d_path.filename());
  SEXPTYPE h2t = h2r_T(arg.getDataType().getId());
  if (h2t == REALSXP){
    return(Rcpp::StringVector::create("double"));
  }
  if (h2t == INTSXP){
    return(Rcpp::StringVector::create("integer"));
  }
  if(h2t == STRSXP){
    return(Rcpp::StringVector::create("character"));
  }
  return(Rcpp::StringVector::create("NULL"));
}


//[[Rcpp::export]]
bool isGroup(const std::string filename, std::string groupname){
  if(groupname=="."){
    groupname="/";
  }
  HighFive::File file(filename,HighFive::File::ReadOnly);
  try{
    HighFive::SilenceHDF5 silence;
    file.getGroup(groupname);
    return(true);
  }catch (HighFive::Exception& err) {
    // Rcpp::StringVector retvec(1);
    return(false);
  }
}





//[[Rcpp::export]]
Rcpp::IntegerVector dim_h5(const std::string &filename,const std::string datapath){

  namespace fs = std::experimental::filesystem;
  fs::path d_path = datapath;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.getGroup(d_path.parent_path());
  try{
    HighFive::SilenceHDF5 silence;
    return(Rcpp::wrap(grp.getDataSet(d_path.filename()).getDataDimensions()));
  }catch (HighFive::Exception& err) {
    // Rcpp::StringVector retvec(1);
    return(Rcpp::IntegerVector::create());
  }
  // return(Rcpp::wrap(HighFive::File(filename,HighFive::File::ReadOnly).getGroup(groupname).getDataSet(dataname).getDataDimensions()));
}
