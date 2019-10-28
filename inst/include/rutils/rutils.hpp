#pragma once
#include <Rcpp.h>
#include <optional>

template<typename A,typename B>
inline Rcpp::List wrap_pair(std::pair<A,B> p,std::optional<std::string> name_a=std::nullopt,std::optional<std::string> name_b=std::nullopt){
  using namespace Rcpp;
  return List::create(_[name_a.value_or("")]=wrap(p.first),_[name_b.value_or("")]=wrap(p.second));
}
