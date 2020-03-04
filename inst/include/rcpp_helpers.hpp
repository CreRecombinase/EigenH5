#pragma once
#include <optional>
#include <variant>
#include <Rcpp.h>



std::variant<std::pair<int,std::optional<int>> ,Rcpp::IntegerVector> dispatch_subset(SEXP x);
template <typename T>
struct cpp2r{
 static const SEXPTYPE data_t = NILSXP;
};

template <>
struct cpp2r<bool>{
 static const SEXPTYPE data_t = LGLSXP;
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



template <SEXPTYPE RTYPE>
std::optional<Rcpp::Vector<RTYPE>>
get_list_element(const Rcpp::List &list, const std::string name,
                 const bool empty_is_false = true);

template<SEXPTYPE RTYPE>
std::optional<Rcpp::Vector<RTYPE> > get_any_list_element(const Rcpp::List &list, const std::vector<std::string> name,const bool empty_is_false=true);


template<typename T,SEXPTYPE RTYPE = cpp2r<T>::data_t > std::optional<T> get_list_scalar(const Rcpp::List &list, const std::string name);
std::vector<std::optional<Rcpp::IntegerVector> > parse_subset_list(const Rcpp::List &list,std::vector<size_t> datadims);

template<SEXPTYPE RTYPE>
inline std::optional<Rcpp::Vector<RTYPE> > get_list_element(const Rcpp::List &list, const std::string name,const bool empty_is_false){
  Rcpp::RObject rnames =	list.names();
  if(rnames.isNULL()){
    return(std::nullopt);
  }
  Rcpp::CharacterVector colnames(rnames);
  for(auto tc:colnames){
    if(Rcpp::as<std::string>(tc)==name){
      if(empty_is_false){
	auto mvec =Rcpp::as<Rcpp::Vector<RTYPE> >(list[name]);
	if(mvec.size()>0){
	  return(mvec);
	}else{
	  return(std::nullopt);
	}
      }else{
	return(Rcpp::as<Rcpp::Vector<RTYPE> >(list[name]));
      }
    }
  }
  return(std::nullopt);
}


template<SEXPTYPE RTYPE>
inline std::optional<Rcpp::Vector<RTYPE> > get_any_list_element(const Rcpp::List &list, const std::vector<std::string> name,const bool empty_is_false){
  for(auto el : name){
    if(auto ret = get_list_element<RTYPE>(list,el,empty_is_false))
      return ret;
  }
  return std::nullopt;
}


inline std::vector<std::optional<Rcpp::IntegerVector> > parse_subset_list(const Rcpp::List &list,std::vector<size_t> datadims){
  using rvec_o = std::optional<Rcpp::IntegerVector>;
  const size_t num_dims= datadims.size();


  if(num_dims>2){
    Rcpp::stop("Datasets with Dims>2 currently not supported");
  }
  std::vector<rvec_o> retvec(num_dims,std::nullopt);
  if(num_dims==1){
    if(auto subset_rv =get_list_element<INTSXP>(list,"subset")){
      retvec[0] = *subset_rv;
    }else{
      if(auto subsetf_rv =get_list_element<INTSXP>(list,"filtervec")){
        retvec[0] = *subsetf_rv;
      }
    }
    return(retvec);
  }

  auto subset_ro =get_list_element<INTSXP>(list,"subset_rows",false);
  auto subset_co =get_list_element<INTSXP>(list,"subset_cols",false);
  if(subset_ro && subset_co){
    if(num_dims==1){
      Rcpp::stop("Cannot specify subset_rows and subset_cols when data is 1 dimensional");
    }
  }
  if(subset_ro){
    retvec[0] = *subset_ro;
  }
  if(subset_co){
    retvec[num_dims==1 ? 0 : 1] = *subset_co;
  }
  return(retvec);
}



template<typename T,SEXPTYPE RTYPE>
inline std::optional<T> get_list_scalar(const Rcpp::List &list, const std::string name){
  if(auto rel = get_list_element<RTYPE>(list,name)){
    T rel_b = *(Rcpp::as< std::vector<T> >(*rel).begin());
    return(rel_b);
  }else{
    return(std::nullopt);
  }
}

template<typename T,SEXPTYPE RTYPE>
  inline std::optional<T> get_list_any(const Rcpp::List &list, std::vector<std::string> name_opts){
  for( auto &name : name_opts){
    auto trel =	get_list_scalar<T>(list,name);
    if(trel){
      return(trel);
    }
  }
  return(std::nullopt);
}


inline std::vector<std::optional<int> > parse_option(const Rcpp::List &list, std::vector<size_t> datadims,std::string prefix){

  using int_o = std::optional<int>;
  const size_t num_dims= datadims.size();
  std::vector<int_o> offset_v(num_dims,std::nullopt);
  if(auto offset_o =get_list_element<INTSXP>(list,prefix)){
    if(offset_o->size()!=num_dims){
      Rcpp::stop("subset argument "+prefix+" must satisfy length(offset)==length(dim(data))");
    }
    for(int i=0; i<num_dims;i++){
      int tint = *(offset_o->begin()+i);
      offset_v[i]=tint;
    }
  }
  auto offset_ro =get_list_element<INTSXP>(list,prefix+"_row");
  auto offset_co =get_list_element<INTSXP>(list,prefix+"_col");
  if(offset_ro || offset_co){
    if(offset_ro && offset_co &&(num_dims==1) ){
      Rcpp::stop("subset arguments "+prefix+"_row and "+prefix+"_col cannot both be specified when length(dim(data))==1");
    }
    if(offset_ro){
      offset_v[0]=*(offset_ro->begin());
    }
    if(offset_co){
      offset_v[num_dims==1 ? 0 :1]=*(offset_co->begin());
    }
  }
  return(offset_v);
}


template<typename A,typename B>
inline Rcpp::List wrap_pair(std::pair<A,B> p,std::optional<std::string> name_a=std::nullopt,std::optional<std::string> name_b=std::nullopt){
  using namespace Rcpp;
  return List::create(_[name_a.value_or("")]=wrap(p.first),_[name_b.value_or("")]=wrap(p.second));
}
