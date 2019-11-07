#pragma once

#include <string_view>

#define ZSTD_STATIC_LINKING_ONLY

#include <cmath>
#include <complex>
#include <iostream>
#include <iostream>
#include <numeric>

#define STRICT_R_HEADERS
#include <RcppEigen.h>




// #if __has_include(<span>)
// #include <span>
// #else
// #define TCB_SPAN_NAMESPACE_NAME std
// #include "span.hpp"
// #endif


#include "highfive/highfive.hpp"


#include <H5Tpublic.h>







// using Path = std::string;
// using PathNode = std::string;


















std::string get_datapath(const Rcpp::List &list);
std::string get_filepath(const Rcpp::List &list);

// template<bool isReadOnly>
// inline HighFive::DataSet getDataSet (const Rcpp::List file_l, std::unique_ptr<FileManager<isReadOnly> > &fm){
//   auto fn = get_list_scalar<std::string>(file_l,"filename");

//   if(!fn){
//     Rcpp::stop("Cannot find \"filename\" in	input file_l");
//   }

//   if(!fm){
//     fm = std::make_unique<FileManager<isReadOnly> >(Rcpp::wrap(*fn));
//   }
//   auto file =fm->get_file(*fn);
//   auto dsn = get_datapath(file_l);
//   return(file.getDataSet(dsn));
// }

inline constexpr size_t num_chunks(const size_t size, const size_t chunksize) noexcept{
    return 1 + ((size - 1) / chunksize);
  }


#include <eigenh5/Singleton.hpp>
#include <eigenh5/Selection.hpp>
#include <eigenh5/MatSlices.hpp>
//#include <eigenh5/ChunkSelector.hpp>


std::variant<std::pair<int,std::optional<int>>,Rcpp::IntegerVector> dispatch_subset(SEXP x);














//Rcpp::XPtr<HighFive::DataSet> data_cw(const std::string filename, const std::string datapath,const Rcpp::RObject &data,Rcpp::List options);


  
//   constexpr cpp2r():data_t(std::is_same_v<T,double> ? REALSXP : (std::is_same_v<T,int> ? INTSXP : (std::is_same_v<T,std::string> ? STRSXP : NILSXP) ) ){
// };
