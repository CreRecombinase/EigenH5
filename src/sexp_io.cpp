#include <EigenH5.h>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
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
#include <blosc_filter.h>
#include<H5Tpublic.h>



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

SEXPTYPE h2r_T(hid_t htype){
  auto ht = H5Tget_class(htype);
  if(ht==H5T_INTEGER){
    //Rcpp::Rcout<<"int"<<std::endl;
    return(INTSXP);
  }
  if(ht==H5T_FLOAT){
    //Rcpp::Rcout<<"double"<<std::endl;
    
    return(REALSXP);
  }
  if(ht==H5T_NATIVE_HBOOL){
    //Rcpp::Rcout<<"BOOL"<<std::endl;
    return(LGLSXP);
  }
  if(ht==H5T_STRING){
    //Rcpp::Rcout<<"string"<<std::endl;
    return(STRSXP);
  }
  Rcpp::Rcout<<"NIL"<<std::endl;
  
  return(NILSXP);
}

using namespace Rcpp;
namespace impl{

  template <typename T> void write_v_h5(std::vector<T> &data,
					    const std::string &filename,
					    const std::string &groupname, 
					    const std::string &dataname){

    using namespace HighFive;
    HighFive::File file(filename, HighFive::File::ReadWrite | HighFive::File::Create);
   
    
    Group group = file.createOrGetGroup(groupname);
    
    std::vector<size_t> vec_dims{data.size()};
    int r = 0;
    r = register_blosc(nullptr, nullptr);
    
    // Create a new file using the default property lists.
    Filter filter({1000}, vec_dims, FILTER_BLOSC, r);
    // Create a dataset with double precision floating points


    DataSpace ds = DataSpace(vec_dims);
    
    DataSet dataset = group.createDataSet(dataname, ds, AtomicType<T>(), filter.getId());
    dataset.write(data);
  }
					    

  
template <SEXPTYPE RTYPE> Vector<RTYPE> read_v_h5(
    const std::string &filename,
    const std::string &groupname, 
    const std::string &dataname,
    const size_t offset=0,
    const size_t chunksize=0){
  
  using T = typename r2cpp_t<RTYPE>::type;
  std::vector<T> retvec;
  //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;
  
  using namespace HighFive;
  File file(filename,File::ReadOnly);
  auto grp = file.getGroup(groupname);
  if(chunksize==0){
    grp.getDataSet(dataname).read(retvec);
  }else{
    std::vector<size_t> off_v={offset};
    std::vector<size_t> ret_v={chunksize};
    grp.getDataSet(dataname).select(off_v,ret_v,{}).read(retvec);
  }
  return(Rcpp::wrap(retvec));
}

}

//[[Rcpp::export]]
Rcpp::StringVector read_s_vec_h5(const std::string &filename,
              const std::string &groupname, 
              const std::string &dataname,
              const int offset=0,
              const int chunksize=0){
  
  static_assert(std::is_same<r2cpp_t<STRSXP>::type,std::string>::value);
  return(impl::read_v_h5<STRSXP>(filename,
                                 groupname,
                                 dataname,
                                offset,
                                 chunksize));
}

//[[Rcpp::export]]
Rcpp::IntegerVector read_i_vec_h5(const std::string &filename,
                                 const std::string &groupname, 
                                 const std::string &dataname,
                                 const int offset=0,
                                 const int chunksize=-1){
  
  static_assert(std::is_same<r2cpp_t<INTSXP>::type,int >::value);
  return(impl::read_v_h5<INTSXP>(filename,
                                 groupname,
                                 dataname,
                                 offset,
                                 chunksize));
}

//[[Rcpp::export]]
Rcpp::NumericVector read_d_vec_h5(const std::string &filename,
                                  const std::string &groupname, 
                                  const std::string &dataname,
                                  const int offset=0,
                                  const int chunksize=-1){
  
  static_assert(std::is_same<r2cpp_t<REALSXP>::type,double >::value);
  return(impl::read_v_h5<REALSXP>(filename,
                                  groupname,
                                  dataname,
                                  offset,
                                  chunksize));
}


//[[Rcpp::export]]
SEXPTYPE check_dtype(const std::string &filename,
                 const std::string &groupname, 
                 const std::string &dataname){
  
  using namespace HighFive;
  
  File file(filename,File::ReadOnly);
  return(h2r_T(file.getGroup(groupname).getDataSet(dataname).getDataType().getId()));
}

//[[Rcpp::export]]
SEXP read_vec_h5(const std::string &filename,
                    const std::string &groupname, 
                    const std::string &dataname,
                    const int offset=0,
                    const int chunksize=-1){
  using namespace Rcpp;
  static_assert(std::is_same<r2cpp_t<STRSXP>::type,std::string >::value);
  auto my_t = check_dtype(filename,groupname,dataname);
  switch (my_t){
  case INTSXP: {
    return(read_i_vec_h5(filename,groupname,dataname,offset,chunksize));
  }
  case REALSXP: {
    return(read_d_vec_h5(filename,groupname,dataname,offset,chunksize));
  }
  case STRSXP: {
    return(read_s_vec_h5(filename,groupname,dataname,offset,chunksize));
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d.\n",
      my_t
    );
    return R_NilValue;
  }
  }
}


//[[Rcpp::export]]
void write_vector_h5(const std::string &filename,
                    const std::string &groupname, 
                    const std::string &dataname,
                    SEXP data){
  using namespace Rcpp;

  auto my_t = TYPEOF(data);
  switch (my_t){
  case INTSXP: {
    auto d=Rcpp::as<std::vector<int> >(data);
    impl::write_v_h5<int>(d,filename,groupname,dataname);
    break;
  }
  case REALSXP: {
    auto d=Rcpp::as<std::vector<double> >(data);
    impl::write_v_h5<double>(d,filename,groupname,dataname);
    break;
  }
  case STRSXP: {
    auto d=Rcpp::as<std::vector<std::string> >(data);
    impl::write_v_h5<std::string>(d,filename,groupname,dataname);
    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
  }
  }
}



