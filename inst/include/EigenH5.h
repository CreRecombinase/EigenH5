#ifndef RCPP_EigenH5_H_GEN_
#define RCPP_EigenH5_H_GEN_
#include <cmath>
#include <complex>
#include <iostream>
#include <range/v3/core.hpp>
#include <range/v3/numeric/adjacent_difference.hpp>
#include <range/v3/view/group_by.hpp>
#include <range/v3/view/transform.hpp>
#include <range/v3/action/transform.hpp>
#include <range/v3/action/join.hpp>
#include <range/v3/view/for_each.hpp>
#include <range/v3/view/zip.hpp>
#include <range/v3/view/zip_with.hpp>
#include <range/v3/to_container.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/map.hpp>

#include <range/v3/view/indices.hpp>
#include <range/v3/view/chunk.hpp>
#include <range/v3/algorithm/for_each.hpp>
#include <range/v3/span.hpp>
#include <range/v3/view/join.hpp>
#include <range/v3/view/all.hpp>
#include <range/v3/view/c_str.hpp>

#include <range/v3/algorithm/mismatch.hpp>
#include <highfive/HighFive.hpp>



#include <blosc_filter.h>
#include <lzf/lzf_filter.h>
#include<H5Tpublic.h>
#include <EigenH5_RcppExports.h> 
#include <eigenh5/MatSlices.hpp>
#include <RcppEigen.h>


template<int RTYPE> struct r2cpp_t{
  typedef std::false_type type;
};
template<> struct r2cpp_t<INTSXP>{
  typedef int type;
};
template<> struct r2cpp_t<REALSXP>{
  typedef double type;
};
template<> struct r2cpp_t<LGLSXP>{
  typedef bool type;
};
template<> struct r2cpp_t<STRSXP>{
  typedef std::string type;
};



SEXPTYPE h2r_T(hid_t htype);


#include <eigenh5/Selection.hpp>
#include <eigenh5/H5classes.h>
#include <eigenh5/Matrix.hpp>

#endif
