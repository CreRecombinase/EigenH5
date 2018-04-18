#pragma once
#include <cmath>
#include <complex>
#include <iostream>
#include <range/v3/core.hpp>
#include <range/v3/view.hpp>
#include <range/v3/action.hpp>

#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <highfive/H5DataSet.hpp>
#include <highfive/H5Filter.hpp>
#include <highfive/H5DataSpace.hpp>
#include <highfive/H5File.hpp>
#include <highfive/H5Attribute.hpp>
#include <highfive/H5Utility.hpp>
#include <highfive/H5DataType.hpp>
#include <highfive/H5Group.hpp>
#include <highfive/H5PropertyList.hpp>
#include <highfive/H5FileDriver.hpp>
#include <highfive/H5Object.hpp>
#include <highfive/H5Selection.hpp>
#include <EigenH5_RcppExports.h>
#include <blosc_filter.h>
#include <lzf/lzf_filter.h>
#include<H5Tpublic.h>
#include <eigenh5/MatSlices.hpp>



std::vector<std::optional<int> > parse_option(const Rcpp::List &list, std::vector<size_t> datadims,std::string prefix);
std::vector<std::optional<Rcpp::IntegerVector> > parse_subset_list(const Rcpp::List &list,std::vector<size_t> datadims);
#include <eigenh5/Selection.hpp>
#include <RcppEigen.h>

#if __has_include(<filesystem>)

#   include <filesystem >
namespace stdx {
  using namespace ::std;
}
#elif __has_include(<experimental/filesystem>)
#   include <experimental/filesystem>
namespace stdx {
  using namespace ::std;
  using namespace ::std::experimental;
}

#else
#   error <experimental/filesystem> and <filesystem> not found
#endif



SEXPTYPE h2r_T(hid_t htype);
bool isGroup(const std::string filename, std::string groupname);
template <typename T>
struct cpp2r{
 static const SEXPTYPE data_t = NILSXP;
};

template<>
struct cpp2r<int>{
   static const SEXPTYPE data_t =INTSXP;
};

template<>
struct cpp2r<double>{
   static const SEXPTYPE data_t =REALSXP;
};

template<>
struct cpp2r<std::string>{
   static const SEXPTYPE data_t =STRSXP;
};

std::shared_ptr<HighFive::File> file_r(const std::string filename);
std::shared_ptr<HighFive::File> file_cw(const std::string filename);
std::shared_ptr<HighFive::File> file_w(const std::string filename);

std::shared_ptr<HighFive::Group> group_r(const std::string filename, const std::string groupname);
std::shared_ptr<HighFive::Group> group_rw(const std::string filename, const std::string datapath);
std::shared_ptr<HighFive::DataSet> data_r(const std::string filename, const std::string datapath);
std::shared_ptr<HighFive::DataSet> data_w(const std::string filename, const std::string datapath);


//std::shared_ptr<HighFive::DataSet> data_cw(const std::string filename, const std::string datapath,const Rcpp::RObject &data,Rcpp::List options);


  
//   constexpr cpp2r():data_t(std::is_same_v<T,double> ? REALSXP : (std::is_same_v<T,int> ? INTSXP : (std::is_same_v<T,std::string> ? STRSXP : NILSXP) ) ){
// };
