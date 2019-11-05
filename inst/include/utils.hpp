#pragma once
#include "highfive/highfive.hpp"


std::vector<std::string> list_R_attr(const HighFive::DataSet &dset);

inline bool has_R_attr(const HighFive::DataSet &dset){
    return !list_R_attr(dset).empty();
}
