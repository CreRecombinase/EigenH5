#pragma once
#include "highfive/highfive.hpp"
#include "rcpp_helpers.hpp"


SEXPTYPE h2r_T(const HighFive::DataType htype);
Rcpp::StringVector h2s_T(const HighFive::DataType htype);

SEXP read_attribute(const HighFive::Group &der,const std::string attribute_name);
SEXP read_attribute(const HighFive::DataSet &der,const std::string attribute_name);
SEXPTYPE typeof_h5_dset(const HighFive::DataSet &dset);
