// #include "EigenH5.h"
// -------------- Stage 1: Forward Declarations with `RcppCommon.h`

// #define RCPP_DEBUG_LEVEL 1
#include <RcppCommon.h>


// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp17)]]                                        



// Third party library includes that provide the template class of ublas
// Before ublas #include, enable boost::numeric::ublas::shallow_array_adaptor<T>
//#define BOOST_UBLAS_SHALLOW_ARRAY_ADAPTOR 1


#include <boost/numeric/ublas/matrix_sparse.hpp>
#include <boost/numeric/ublas/matrix.hpp>

// Provide Forward Declarations
namespace Rcpp {

  namespace traits{

    // Setup non-intrusive extension via template specialization for
    // 'ublas' class boost::numeric::ublas

    // Support for wrap
    template <typename T> SEXP wrap(const boost::numeric::ublas::vector<T> & obj);
    template <typename T,typename S> SEXP wrap(const boost::numeric::ublas::matrix<T,S> & obj);



    // Support for as<T>
    template <typename T> class Exporter< boost::numeric::ublas::vector<T> >;
    template <typename T,typename S> class Exporter< boost::numeric::ublas::matrix<T,S> >;
  

  }
}

// -------------- Stage 2: Including Rcpp.h

// ------ Place <Rcpp.h> AFTER the Forward Declaration!!!!

#include <Rcpp.h>

// ------ Place Implementations of Forward Declarations AFTER <Rcpp.h>!


// -------------- Stage 3: Implementation the Declarations

// Define template specializations for as<> and wrap
namespace Rcpp {

  namespace traits{

    template <typename T> SEXP wrap(const boost::numeric::ublas::vector<T> & obj){
      RCPP_DEBUG_1("wrap_boost_numeric_ublas_thing<%s>(., false )", DEMANGLE(T))
      const int RTYPE = Rcpp::traits::r_sexptype_traits<T>::rtype ;
  
      return Rcpp::Vector< RTYPE >(obj.begin(), obj.end());
    };
    

    template <typename T,typename S> SEXP wrap(const boost::numeric::ublas::matrix<T,S> & obj){
      RCPP_DEBUG_1("wrap_boost_numeric_ublas_matrix_thing<%s>(., false )", DEMANGLE(T))
      static_assert(std::is_arithmetic<T>::value, "T is not an arithmetic type!");
      const int RTYPE = Rcpp::traits::r_sexptype_traits<T>::rtype ;
      Rcpp::Matrix<RTYPE> retmat(obj.size1(),obj.size2());
      for(auto it=obj.begin1();it!=obj.end1();it++){
        for(auto jt=it.begin();jt!=it.end();jt++){
          retmat(jt.index1(),jt.index2())=*jt;
        }
      }
      return Rcpp::Matrix<RTYPE>(retmat);
    };
    


 


    // Defined as< > case
    template<typename T,typename S> class Exporter< boost::numeric::ublas::matrix<T,S> > {
      typedef typename boost::numeric::ublas::matrix<T,S> OUT ;
      // Convert the type to a valid rtype. 
      const static int RTYPE = Rcpp::traits::r_sexptype_traits<T>::rtype ;
      Rcpp::Matrix<RTYPE> retmat;
  
    public:
      Exporter(SEXP x) : retmat(x)  {
	static_assert(std::is_arithmetic<T>::value, "T is not an arithmetic type!");
	if (TYPEOF(x) != RTYPE)
	  throw std::invalid_argument("Wrong R type for mapped 2D array");
      }
      OUT get() {
	// Need to figure out a way to perhaps do a pointer pass?
	OUT x(retmat.rows(),retmat.cols());
	for(auto it=x.begin1();it!=x.end1();it++){
	  for(auto jt=it.begin();jt!=it.end();jt++){
	    x(jt.index1(),jt.index2())=retmat(jt.index1(),jt.index2());
	  }
	}
    
	return x;
      }
    };

  

    // Defined as< > case
    template<typename T> class Exporter< boost::numeric::ublas::vector<T> > {
      typedef typename boost::numeric::ublas::vector<T> OUT ;
  
      // Convert the type to a valid rtype. 
      const static int RTYPE = Rcpp::traits::r_sexptype_traits< T >::rtype ;
      Rcpp::Vector<RTYPE> vec;
  
    public:
      Exporter(SEXP x) : vec(x) {
        if (TYPEOF(x) != RTYPE)
          throw std::invalid_argument("Wrong R type for mapped 1D array");
      }
      OUT get() {
        
        // Need to figure out a way to perhaps do a pointer pass?
        OUT x(vec.size());
        
        std::copy(vec.begin(), vec.end(), x.begin()); // have to copy data
        
        return x;
      }
    };
    
    
    
  }
}

// -------------- Stage 4: Testing

// Here we define a shortcut to the Boost ublas class to enable multiple ublas
// types via a template.
// ublas::vector<T> => ublas::vector<double>, ... , ublas::vector<int>
namespace ublas = ::boost::numeric::ublas;


// [[Rcpp::export]]
void containment_test(Rcpp::NumericMatrix m1) {  
  
  
  Rcpp::Rcout << "Converting from Rcpp::NumericMatrix to ublas::matrix<double,row_major>" << std::endl;
  
  // initialize the vector to all zero
  ublas::matrix<double,ublas::row_major> xrm = Rcpp::as< ublas::matrix<double,ublas::row_major> >(m1); 
  
  Rcpp::Rcout << "Running output test with ublas::matrix<double,row_major>" << std::endl;
  
  for (auto i = xrm.begin1(); i!=xrm.end1(); i++){
    for(auto j= i.begin();j!=i.end();j++){
      Rcpp::Rcout  << *j << " ";
    }
    Rcpp::Rcout<<std::endl;
  }
  
  Rcpp::Rcout << "Converting from ublas::matrix<double,row_major> to Rcpp::NumericMatrix" << std::endl;

  
  Rcpp::NumericMatrix testm = Rcpp::traits::wrap(xrm);

  Rcpp::Rcout << "Running output test with Rcpp::NumericMatrix" << std::endl;

  for (unsigned i = 0; i < testm.rows(); ++ i){
    for(unsigned j=0; j<testm.cols(); ++j){
      Rcpp::Rcout  << testm(i,j) <<" ";
    }
    Rcpp::Rcout<< std::endl;
  }




  
  Rcpp::Rcout << "Converting from Rcpp::NumericMatrix to ublas::matrix<double,col_major>" << std::endl;
  
  // initialize the vector to all zero
  ublas::matrix<double,ublas::column_major> xcm = Rcpp::as< ublas::matrix<double,ublas::column_major> >(m1); 
  
  Rcpp::Rcout << "Running output test with ublas::matrix<double,col_major>" << std::endl;
  
  for (auto i = xcm.begin1(); i!=xcm.end1(); i++){
    for(auto j= i.begin();j!=i.end();j++){
      Rcpp::Rcout  << *j << " ";
    }
    Rcpp::Rcout<<std::endl;
  }
  
  Rcpp::Rcout << "Converting from ublas::matrix<double,col_major> to Rcpp::NumericMatrix" << std::endl;
  
  Rcpp::NumericMatrix ctestm = Rcpp::traits::wrap(xcm);

  Rcpp::Rcout << "Running output test with Rcpp::NumericVector" << std::endl;

  for (unsigned i = 0; i < ctestm.rows(); ++ i){
    for(unsigned j=0; j<ctestm.cols(); ++j){
      Rcpp::Rcout  << ctestm(i,j) <<" ";
    }
    Rcpp::Rcout<< std::endl;
  }



  
  
}





