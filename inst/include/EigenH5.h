#pragma once
#define ZSTD_STATIC_LINKING_ONLY
#include <cmath>
#include <complex>
#include <iostream>
#include <iostream>
#include <numeric>


#define STRICT_R_HEADERS

#include <boost/optional.hpp>
#include <boost/optional/optional_io.hpp>
#include <boost/variant.hpp>

#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <RcppEigen.h>
//#include "path/path.hpp"






class Path{
public:
  std::string nodes;
  Path(const std::string name):nodes(name){
  }
  std::string parent_path() {
    if(nodes.size()==0){
      Rcpp::stop("path is empty!");
    }
    auto nodes_rend = nodes.crend();
    auto nodes_rbegin = nodes.crbegin();

    if (nodes.size() > 1 && nodes.back() == '/') {
      nodes_rbegin++;
    }
    nodes_rbegin=	std::find(nodes_rbegin,nodes_rend,'/');
    if (nodes_rbegin == nodes_rend) {
      Rcpp::stop("can't find parent path in:" + nodes);
    }
    std::string ret(nodes_rbegin,nodes_rend);
    std::reverse(ret.begin(),ret.end());
    return (ret);
  }
  const char *c_str() const {
    return(nodes.c_str());
  }
  std::string filename() {
    if(nodes.size()==0){
      Rcpp::stop("path is empty!");
    }
    auto nodes_rend = nodes.crend();
    auto nodes_rbegin = nodes.crbegin();
    auto nodes_rrbegin = nodes_rbegin;

    if (nodes.size() > 1 && nodes.back() == '/') {
      nodes_rbegin++;
    }
    nodes_rbegin=	std::find(nodes_rbegin,nodes_rend,'/');
    if (nodes_rbegin == nodes_rend) {
      Rcpp::stop("can't find parent path in:" + nodes);
    }

    std::string ret(nodes_rrbegin,nodes_rbegin);
    std::reverse(ret.begin(),ret.end());

    return (ret);
  }
  operator const std::string&() const { return(nodes); }

};

inline std::string operator+(const std::string a, const Path &b){
  return(a+b.nodes);
}

inline std::ostream &operator<<(std::ostream &os, const Path &dt) {
  os << dt.nodes;
  return os;
}

#include "highfive/highfive.hpp"


#include <H5Tpublic.h>

#if __cplusplus < 201402L

#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>


namespace std {

template<class T> struct _Unique_if {
  typedef unique_ptr<T> _Single_object;
};

template<class T> struct _Unique_if<T[]> {
  typedef unique_ptr<T[]> _Unknown_bound;
};

template<class T, size_t N> struct _Unique_if<T[N]> {
  typedef void _Known_bound;
};

template<class T, class... Args>
typename _Unique_if<T>::_Single_object
  make_unique(Args&&... args) {
    return unique_ptr<T>(new T(std::forward<Args>(args)...));
  }

template<class T>
typename _Unique_if<T>::_Unknown_bound
  make_unique(size_t n) {
    typedef typename remove_extent<T>::type U;
    return unique_ptr<T>(new U[n]());
  }

template<class T, class... Args>
typename _Unique_if<T>::_Known_bound
  make_unique(Args&&...) = delete;
}

#endif

// using Path = std::string;
// using PathNode = std::string;

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

#include "EigenH5_RcppExports.h"

inline Path root_path(const std::string &input) {
  return (input.front() == '/' ? input : "/" + input);
}

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


HighFive::Filter create_filter(std::vector<size_t> data_dimensions,
			       Rcpp::List &options);

template<SEXPTYPE RTYPE> boost::optional<Rcpp::Vector<RTYPE> > get_list_element(const Rcpp::List &list, const std::string name,const bool empty_is_false = true);
template<typename T,SEXPTYPE RTYPE = cpp2r<T>::data_t > boost::optional<T> get_list_scalar(const Rcpp::List &list, const std::string name);
std::vector<boost::optional<Rcpp::IntegerVector> > parse_subset_list(const Rcpp::List &list,std::vector<size_t> datadims);


SEXPTYPE typeof_h5_dset(const HighFive::DataSet &dset);


inline std::vector<boost::optional<int> > parse_option(const Rcpp::List &list, std::vector<size_t> datadims,std::string prefix){

  using int_o = boost::optional<int>;
  const size_t num_dims= datadims.size();
  std::vector<int_o> offset_v(num_dims,boost::none);
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



SEXPTYPE h2r_T(hid_t htype);
bool isGroup(const std::string filename, std::string groupname);


template<SEXPTYPE RTYPE>
inline boost::optional<Rcpp::Vector<RTYPE> > get_list_element(const Rcpp::List &list, const std::string name,const bool empty_is_false){
  Rcpp::RObject rnames =	list.names();
  if(rnames.isNULL()){
    return(boost::none);
  }
  Rcpp::CharacterVector colnames(rnames);
  for(auto tc:colnames){
    if(Rcpp::as<std::string>(tc)==name){
      if(empty_is_false){
	auto mvec =Rcpp::as<Rcpp::Vector<RTYPE> >(list[name]);
	if(mvec.size()>0){
	  return(mvec);
	}else{
	  return(boost::none);
	}
      }else{
	return(Rcpp::as<Rcpp::Vector<RTYPE> >(list[name]));
      }
    }
  }
  return(boost::none);
}

inline std::vector<boost::optional<Rcpp::IntegerVector> > parse_subset_list(const Rcpp::List &list,std::vector<size_t> datadims){
  using rvec_o = boost::optional<Rcpp::IntegerVector>;
  const size_t num_dims= datadims.size();


  if(num_dims>2){
    Rcpp::stop("Datasets with Dims>2 currently not supported");
  }
  std::vector<rvec_o> retvec(num_dims,boost::none);
  if(num_dims==1){
    if(auto subset_rv =get_list_element<INTSXP>(list,"subset")){
      retvec[0] = subset_rv.value();
    }else{
      if(auto subsetf_rv =get_list_element<INTSXP>(list,"filtervec")){
        retvec[0] = subsetf_rv.value();
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
    retvec[0] = subset_ro.value();
  }
  if(subset_co){
    retvec[num_dims==1 ? 0 : 1] = subset_co.value();
  }
  return(retvec);
}



template<typename T,SEXPTYPE RTYPE>
inline boost::optional<T> get_list_scalar(const Rcpp::List &list, const std::string name){
  if(auto rel = get_list_element<RTYPE>(list,name)){
    T rel_b = *(Rcpp::as< std::vector<T> >(*rel).begin());
    return(rel_b);
  }else{
    return(boost::none);
  }
}

template<typename T,SEXPTYPE RTYPE>
  inline boost::optional<T> list_get_any(const Rcpp::List &list, const std::vector<std::string> name_opts){
  for( auto &name : name_opts){
    auto trel =	get_list_scalar<T>(list,name);
    if(trel){
      return(trel);
    }
  }
  return(boost::none);
}








std::string get_datapath(const Rcpp::List &list);
std::string get_filepath(const Rcpp::List &list);

template<bool isReadOnly>
inline HighFive::DataSet getDataSet (const Rcpp::List file_l, std::unique_ptr<FileManager<isReadOnly> > &fm){
  auto fn = get_list_scalar<std::string>(file_l,"filename");

  if(!fn){
    Rcpp::stop("Cannot find \"filename\" in	input file_l");
  }

  if(!fm){
    fm = std::make_unique<FileManager<isReadOnly> >(Rcpp::wrap(*fn));
  }
  auto file =fm->get_file(*fn);
  auto dsn = get_datapath(file_l);
  return(file.getDataSet(dsn));
}

#include <eigenh5/Singleton.hpp>
#include <eigenh5/Selection.hpp>
#include <eigenh5/MatSlices.hpp>
//#include <eigenh5/ChunkSelector.hpp>


std::variant<std::pair<int,int>,Rcpp::IntegerVector> dispatch_subset(SEXP x);














//Rcpp::XPtr<HighFive::DataSet> data_cw(const std::string filename, const std::string datapath,const Rcpp::RObject &data,Rcpp::List options);


  
//   constexpr cpp2r():data_t(std::is_same_v<T,double> ? REALSXP : (std::is_same_v<T,int> ? INTSXP : (std::is_same_v<T,std::string> ? STRSXP : NILSXP) ) ){
// };
