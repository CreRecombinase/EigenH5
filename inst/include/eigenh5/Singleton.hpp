#pragma once

#include <Rcpp.h>
#include "highfive/highfive.hpp"
#include "EigenH5.h"


// inline HighFive::File file_r(const std::string filename){
//   auto gr = EigenH5::get_singleton(true);
//   return(gr->file_r(filename));
// }

// }
// inline Rcpp::XPtr<HighFive::File> file_w(const std::string filename){
//   auto gr = EigenH5::get_singleton();
//   return(gr->file_w(filename));
// }
// inline Rcpp::XPtr<HighFive::File> file_k(const std::string filename){
//   auto gr = EigenH5::get_singleton();
//   return(gr->file_w(filename));
// }

template<class T> struct always_false : std::false_type {};

template<bool isReadOnly>
class FileManager{
  std::unordered_map<std::string,HighFive::File >  file_map;
  std::unordered_map<std::string,int>  flag_map;
public:
  FileManager(){};
  FileManager(const Rcpp::StringVector filenames);
  HighFive::File get_file(const std::string &f);
  void file_k(const std::string filename);
  std::unordered_map<std::string,HighFive::File >::const_iterator  get_files() const;
  void print()const;
};


template<bool isReadOnly>
inline std::unordered_map<std::string,HighFive::File >::const_iterator  FileManager<isReadOnly>::get_files() const{
  return(file_map.begin());
}

template<bool isReadOnly>
inline FileManager<isReadOnly>::FileManager(const Rcpp::StringVector filenames){
  using namespace HighFive;
  const size_t num_files = filenames.size();
  file_map.reserve(num_files);
  flag_map.reserve(num_files);
  for(int i=0; i<num_files; i++){
    const std::string tfn = Rcpp::as<std::string>(filenames(i));
    auto mtf = file_map.find(tfn);
    auto ff = flag_map.find(tfn);
    if(mtf == file_map.end()){
      int flags = isReadOnly ? HighFive::File::ReadOnly : HighFive::File::ReadWrite | HighFive::File::Create;
      flag_map.insert(std::make_pair(tfn,flags));
      mtf = file_map.emplace_hint(mtf,tfn,File(tfn,flags));
    }else{
      //Rcpp::stop("Duplicate file names not allowed when constructing FileManager");
    }
  }
}


template<bool isReadOnly>
inline HighFive::File FileManager<isReadOnly>::get_file(const std::string &fn){
  using namespace HighFive;
  int flags = isReadOnly ? HighFive::File::ReadOnly : HighFive::File::ReadWrite | HighFive::File::Create;
  auto mtf = file_map.find(fn);
  auto ff = flag_map.find(fn);
  if(ff == flag_map.end()){
    auto ff = flag_map.insert(std::make_pair(fn,flags));
    mtf = file_map.emplace_hint(mtf,fn,File(fn,flags));
  }else{
    if(mtf ==file_map.end()){
      Rcpp::stop("Cannot find file flags for file"+fn);
    }
  }
  return(mtf->second);
}

template<bool isReadOnly>
inline void FileManager<isReadOnly>::print() const{
  using namespace Rcpp;
  const size_t num_files = file_map.size();

  Rcpp::Rcout<<"Filename\tReadOnly\tReadWrite\tTruncate\tCreate\tSWMR_Write"<<std::endl;
  for(auto &fm :	file_map){

    std::string fname = fm.first;
    Rcpp::Rcout<<fname<<"\t";
    int tflag =  flag_map.find(fname)->second;
    bool ret = tflag & HighFive::File::ReadOnly;
    Rcpp::Rcout<<ret<<"\t";
    ret = tflag & HighFive::File::ReadWrite;
    Rcpp::Rcout<<ret<<"\t";
    ret  = tflag & HighFive::File::Truncate;
    Rcpp::Rcout<<ret<<"\t";
    ret = tflag & HighFive::File::Create;
    Rcpp::Rcout<<ret<<"\t";
    ret = tflag & HighFive::File::SWMR_Write;
    Rcpp::Rcout<<ret<<std::endl;
  }
}


// inline HighFive::File FileManager<isReadOnly>::file_r(const std::string filename){
//   return(get_file(filename,HighFive::File::ReadOnly,true));
// }
// inline Rcpp::XPtr<HighFive::File> FileManager<isReadOnly>::file_cw(const std::string filename){
//   return(get_file(filename,HighFive::File::ReadWrite | HighFive::File::Create,false));
// }
// inline Rcpp::XPtr<HighFive::File> FileManager<isReadOnly>::file_w(const std::string filename){
//   return(get_file(filename,HighFive::File::ReadWrite ,false));
// }

template<bool isReadOnly>
inline void FileManager<isReadOnly>::file_k(const std::string filename){
  auto mtf = file_map.find(filename);
  auto ff = flag_map.find(filename);
  if(mtf == file_map.end()){
    Rcpp::stop("unable to find file:"+filename+" in FileManager");
  }else{
    file_map.erase(mtf);
    flag_map.erase(ff);
  }
}
