#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <array>

// [[Rcpp::interfaces(r, cpp)]]

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
template<> struct r2cpp_t<CHARSXP>{
  typedef std::string type;
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
  // Rcpp::Rcout<<"NIL"<<std::endl;

  return(NILSXP);
}

using namespace Rcpp;


namespace impl {

template <int RTYPE>
int len(const Vector<RTYPE>& x)
{
    return static_cast<int>(x.size());
}

}



// std::variant<Rcpp::IntegerVector,
// 	     Rcpp::NumericVector,
// 	     Rcpp::CharacterVector,
// 	     Rcpp::Vector<NILSXP>> get_obj_v(RObject x){
//   const auto my_t = x.sexp_type();
//   const bool isMatrix = x.hasAttribute("dim");
//   switch (my_t){
//   case INTSXP: {
//     return(Rcpp::Vector<INTSXP>(x));
//   }
//   case REALSXP: {
//     return(Rcpp::Vector<REALSXP>(x));
//   }
//   case STRSXP: {
//     return(Rcpp::Vector<STRSXP>(x));
//   }
//   default: {
//     warning(
// 	    "Invalid SEXPTYPE %d.\n",
// 	    my_t
// 	    );
//     return(Rcpp::Vector<NILSXP>(x));
//   }
//   }

// }




std::vector<size_t> obj_dim(RObject x){
  if(x.hasAttribute("dim")){
    return(Rcpp::as<std::vector<size_t> >(x.attr("dim")));
  }else{
    std::vector<size_t> ret={static_cast<size_t>(XLENGTH(SEXP(x)))};
    return(ret);
  }
    // auto vec=get_obj_v(x);
    // return(std::visit([](auto&& arg) -> std::vector<size_t> {
    // 	  std::vector<size_t> retvec = {static_cast<size_t>(arg.size())};
    // 	  return(retvec);
    // 	},vec));
}

// [[Rcpp::export]]
int len(RObject x)
{
    RCPP_RETURN_VECTOR(impl::len, x);
}




template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
Matrix<RTYPE> read_elem_m_h5(HighFive::Selection &file_sel,
			     DatasetSelection<2> &mem_sel){

  auto r_size =	file_sel.getDataDimensions();
  if(r_size.size()!=2){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot read matrix dataset unless it is rank 2");
  }
  Rcpp::Matrix<RTYPE> retmat(r_size[0],r_size[1]);
  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&retmat(0,0),r_size[0],r_size[1]);
  mem_sel.readEigen(file_sel,mretmat);
  return(retmat);
}


template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
void write_elem_m_h5(HighFive::Selection &file_sel,
		     DatasetSelection<2> &mem_sel,
		    Rcpp::Matrix<RTYPE> wmat){

  auto r_size =	file_sel.getDataDimensions();
  if(r_size.size()!=2){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot write matrix dataset unless it is rank 2");
  }

  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&wmat(0,0),wmat.nrow(),wmat.ncol());
  mem_sel.writeEigen(file_sel,mretmat);
}




template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
Rcpp::Vector<RTYPE> read_elem_v_h5(HighFive::Selection &file_sel,
				   DatasetSelection<1> &mem_sel){

  auto r_size =	file_sel.getDataDimensions();
  if(r_size.size()!=1){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot read vector dataset unless it is rank 1");
  }
  std::vector<T> retv(r_size[0]);
  //  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&retmat(0,0),r_size[0],r_size[1]);

  mem_sel.readVector<T>(file_sel,retv);
  return(Rcpp::wrap(retv));
}


template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type>
void write_elem_v_h5(HighFive::Selection &file_sel,
		     DatasetSelection<1> &mem_sel,
		    Rcpp::Vector<RTYPE> wvec){

  auto r_size =	file_sel.getDataDimensions();
  if(r_size.size()!=1){
    Rcpp::Rcerr<<"Dataset is of rank: "<<r_size.size()<<std::endl;
    Rcpp::stop("Cannot write vector dataset unless it is rank 1");
  }
  mem_sel.writeVector(file_sel,std::move(Rcpp::as<std::vector<T>>(wvec)));
}




HighFive::DataSet create_dataset(HighFive::Group &group,
				 const std::string &dataname,
				 const Rcpp::RObject &data,
				 std::vector<size_t> data_dimensions,
				 std::vector<size_t> chunk_dimensions){
  using namespace HighFive;
  const bool use_chunksize = chunk_dimensions.size()>0;
  if(use_chunksize){
    chunk_dimensions = Filter::guess_chunk(chunk_dimensions);
  }
  DataSpace space = DataSpace(data_dimensions);
  Filter filter	= use_chunksize ? Filter(chunk_dimensions,FILTER_BLOSC,1) : Filter::From(space,FILTER_BLOSC);
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<int>(), filter));
  }
  case REALSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<double>(), filter));
  }
  case STRSXP: {
    return(group.createDataSet(dataname, space, HighFive::AtomicType<std::string>(), filter));
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::stop("Can't create type");
  }
  }
}





SEXPTYPE typeof_h5_dset(HighFive::DataSet &dset){

  using namespace HighFive;
  return(h2r_T(dset.getDataType().getId()));
}











std::vector<size_t> dataset_dims(std::string filename,
				 std::string datapath){

  namespace fs = stdx::filesystem;
  fs::path dp=datapath;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  if(auto grp =file.openGroup(dp.parent_path())){
    if(auto dset = grp->openDataSet(dp.filename())){
      return(dset->getDataDimensions());
    }
  }
  std::vector<size_t> retvec;
  return(retvec);
}



//[[Rcpp::export]]
SEXP read_vector(std::string filename,
		 std::string datapath,
		 Rcpp::List subset){
  using namespace Rcpp;
  namespace fs = stdx::filesystem;
  fs::path dp=datapath;

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto grp = file.getGroup(groupname);
  auto dset = file.getGroup(groupname).getDataSet(dataname);
  auto dims = dset.getDataDimensions();

  auto datasel = DatasetSelection<1>::ProcessList(subset,dims);

  auto file_sel=datasel.makeSelection(dset);
  auto my_t = typeof_h5_dset(dset);
  //  auto my_t = check_dtype(filename,groupname,dataname);


  switch (my_t){
  case INTSXP: {
    auto ret = read_elem_v_h5<INTSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  case REALSXP: {
    auto ret = read_elem_v_h5<REALSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  case STRSXP: {
    auto ret = read_elem_v_h5<STRSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}





// SEXP read_object(std::string filename,
//                     std::string groupname,
//                     std::string dataname,
//                     const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(0,0),
//                     const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create(-1,-1),
//                     const Rcpp::IntegerVector subset_rows = Rcpp::IntegerVector::create(),
//                     const Rcpp::IntegerVector subset_cols = Rcpp::IntegerVector::create()){
  
//   using namespace Rcpp;
//   if(groupname=="."){
//     groupname="/";
//   }
//   std::vector<int> tchunksizes(chunksizes.size());
//   for(int i=0; i<tchunksizes.size();i++){
//     tchunksizes[i]=chunksizes(i)==(-1) ? NA_INTEGER  : chunksizes(i);
//   }
//   HighFive::File file(filename,HighFive::File::ReadOnly);
//   auto grp = file.getGroup(groupname);
//   auto dset = file.getGroup(groupname).getDataSet(dataname);
//   auto dims = dset.getDataDimensions();
//   const size_t num_dims=dims.size();
//   if(num_dims==1){
//     DatasetSelection<1> datasel({construct_dimrange(offsets(0),chunksizes(0),subset_rows,dims[0])},{dims[0]});
//     auto file_sel=datasel.makeSelection(dset);
//     auto my_t = typeof_h5_dset(dset);
//     switch (my_t){
//     case INTSXP: {
//       return(read_elem_v_h5<INTSXP>(file_sel,datasel));
//       break;
//     }
//     case REALSXP: {
//       return(read_elem_v_h5<REALSXP>(file_sel,datasel));
//       break;
//     }
//     case STRSXP: {
//       return(read_elem_v_h5<STRSXP>(file_sel,datasel));
//       break;
//     }
//     default: {
//       warning(
//         "Invalid SEXPTYPE %d.\n",
//         my_t
//       );
//       Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;
//       Rcpp::stop("Can't read type");
//       return R_NilValue;
//     }
//     }
//   }else{
    
//     std::array<size_t,2>tdims={dims[0],dims[1]};
//     DatasetSelection<2> datasel({
//       construct_dimrange(offsets(0),tchunksizes[0],subset_rows,dims[0]),
//       construct_dimrange(offsets(1),tchunksizes[1],subset_cols,dims[1])},tdims);
    
//     auto file_sel=datasel.makeSelection(dset);
//     auto my_t = typeof_h5_dset(dset);
    
    
//     switch (my_t){
//     case INTSXP: {
//       return(read_elem_m_h5<INTSXP>(file_sel,datasel));
//       break;
//     }
//     case REALSXP: {
//       return(read_elem_m_h5<REALSXP>(file_sel,datasel));
//       break;
//     }
//     default: {
//       warning(
//         "Invalid SEXPTYPE %d.\n",
//         my_t
//       );
//       Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;
//       Rcpp::stop("Can't read type");
//       return R_NilValue;
//     }
//     }
    
//   }
  
// }






//[[Rcpp::export]]
SEXP read_matrix(std::string filename,
		 std::string datapath,
		 const Rcpp::List subset){

  using namespace Rcpp;
  namespace fs = stdx::filesystem;

  HighFive::File file(filename,HighFive::File::ReadOnly);
  fs::path dp=datapath;
  auto dset = file.getDataSet(datapath);
  auto dims = dset.getDataDimensions();
  auto datasel = DatasetSelection<2>::ProcessList(subset,dims);


  auto file_sel=datasel.makeSelection(dset);
  auto my_t = typeof_h5_dset(dset);

  switch (my_t){
  case INTSXP: {
    auto ret = read_elem_m_h5<INTSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  case REALSXP: {
    auto ret = read_elem_m_h5<REALSXP>(file_sel,datasel);

    return(ret);
    break;
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d.\n",
      my_t
    );
    Rcpp::Rcerr<<datapath<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}





//[[Rcpp::export]]
bool update_matrix(RObject data,
                   const std::string filename,
                   const std::string datapath,
                   const Rcpp::List &options){
  using namespace Rcpp;
  namespace fs = stdx::filesystem;
  fs::path dp=datapath;
  bool write_success=false;
  HighFive::File file(filename,HighFive::File::ReadWrite);
  if(auto dset = file.openDataSet(datapath)){
    auto dims = dset->getDataDimensions();
    auto datasel = DatasetSelection<2>::ProcessList(options,dims);
    auto file_sel=datasel.makeSelection(*dset);
    auto my_t = typeof_h5_dset(*dset);
    switch (my_t){
    case INTSXP: {
      Rcpp::Matrix<INTSXP> wmat(data);
      write_elem_m_h5<INTSXP>(file_sel,datasel,wmat);
      write_success=true;
      break;
    }
    case REALSXP: {
      Rcpp::Matrix<REALSXP> wmat(data);
      write_elem_m_h5<REALSXP>(file_sel,datasel,wmat);
      write_success=true;
      break;
    }
    default: {
      warning(
	      "Invalid SEXPTYPE %d.\n",
	      my_t
	      );
      Rcpp::Rcerr<<dp.filename()<<" has type that can't be written"<<std::endl;

      Rcpp::stop("Can't read type");
    }
    }
  }
  file.flush();
  return(write_success);
}



//[[Rcpp::export]]
bool update_vector(RObject data,
		   std::string filename,
		   std::string datapath,
		   Rcpp::List options){
  using namespace Rcpp;

  bool write_success=false;
  HighFive::File file(filename,HighFive::File::ReadWrite);

  if( auto dset = file.openDataSet(datapath)){
    auto dims = dset->getDataDimensions();

    auto datasel = DatasetSelection<1>::ProcessList(options,dims);
    auto file_sel=datasel.makeSelection(*dset);
    auto my_t = typeof_h5_dset(*dset);
    switch (my_t){
    case INTSXP: {
      Rcpp::Vector<INTSXP> wvec(data);
      write_elem_v_h5<INTSXP>(file_sel,datasel,wvec);
      write_success=true;
      break;
    }
    case REALSXP: {
      Rcpp::Vector<REALSXP> wvec(data);
      write_elem_v_h5<REALSXP>(file_sel,datasel,wvec);
      write_success=true;
      break;
    }
    case STRSXP: {
      Rcpp::Vector<STRSXP> wvec(data);
      write_elem_v_h5<STRSXP>(file_sel,datasel,wvec);
      write_success=true;
      break;
    }
    default: {
      warning(
	      "Invalid SEXPTYPE %d.\n",
	      my_t
	      );
      Rcpp::Rcerr<<datapath<<" has type that can't be written"<<std::endl;

      Rcpp::stop("Can't read type");
    }

    }
    file.flush();

  }

  if(!write_success){
    Rcpp::stop("write failed!");
  }
  return(write_success);
}






//[[Rcpp::export]]
 bool create_dataset_h5(const std::string &filename,
		    const std::string datapath,
                     const RObject &data,
		    Rcpp::List options){
   using namespace HighFive;
   namespace fs = stdx::filesystem;
   fs::path dp=datapath;
   bool create_success=false;
   HighFive::File file(filename,HighFive::File::Create | HighFive::File::ReadWrite);
   auto group = file.openGroup(dp.parent_path()).value_or(file.createGroup(dp.parent_path()));

   auto data_d = get_list_element<INTSXP>(options,"dim");
   if(!data_d){
     data_d =get_list_element<INTSXP>(options,"dims");
   }
   auto chunksize_d = get_list_element<INTSXP>(options,"chunksizes");
   if(!chunksize_d){
     chunksize_d = get_list_element<INTSXP>(options,"chunksize");
   }
   std::vector<size_t>	dimvec = Rcpp::as<std::vector<size_t> >(data_d.value_or(Rcpp::wrap(obj_dim(data))));
   Rcpp::IntegerVector tvec;
   std::vector<size_t>	chunkvec = Rcpp::as<std::vector<size_t> >(chunksize_d.value_or(tvec));
   create_dataset(group,dp.filename(),data,dimvec,chunkvec);
   create_success=true;
   file.flush();
  return(create_success);
 }


 // void create_vector(const std::string &filename,
 // 		    const std::string datapath,
 // 		    RObject data,
 // 		    Rcpp::List options){
 //  using namespace HighFive;
 //  using namespace HighFive;
 //  
 //  fs::path dp=datapath;
 //  HighFive::File file(filename,);
 //  if(dim.size()!=1){
 //    Rcpp::stop("length(dim) must be equal to 1");
 //  }
 //  if(chunksize.size()>1){
 //    Rcpp::stop("length(chunksize) cannot be greater than one (must be length 1 or empty)");
 //  }
 //  auto grp = file.createOrGetGroups(dp.parent_path());
 //  create_dataset(grp,dp.filename(),data,Rcpp::as<std::vector<size_t> >(dim),Rcpp::as<std::vector<size_t> >(chunksize));
 // }






// bool write_df_h5(Rcpp::DataFrame &df,const std::string groupname,const std::string outfile,Rcpp::IntegerVector deflate_level=Rcpp::IntegerVector::create(4)){
//   HighFive::File file(outfile, HighFive::File::ReadWrite | HighFive::File::Create);
//   HighFive::Group group = file.createOrGetGroups(groupname);

//   const size_t df_cols=df.ncol();
//   std::vector<std::string> df_colnames = Rcpp::as<std::vector<std::string> >(df.names());

//   for(int i=0; i<df_cols;i++){
//     auto t_colname= df_colnames[i];
//     auto t_col = df[t_colname];
//     auto my_t = TYPEOF(t_col);
//     switch (my_t){
//     case INTSXP: {
//       auto d=Rcpp::as<std::vector<int> >(t_col);
//       impl::write_v_h5<int>(d,file,group,t_colname);
//       break;
//     }
//     case REALSXP: {
//       auto d=Rcpp::as<std::vector<double> >(t_col);
//       impl::write_v_h5<double>(d,file,group,t_colname);
//       break;
//     }
//     case STRSXP: {
//       auto d=Rcpp::as<std::vector<std::string> >(t_col);
//       impl::write_v_h5<std::string>(d,file,group,t_colname);
//       break;
//     }
//     default: {
//       warning(
//         "Invalid SEXPTYPE %d.\n",
//         my_t
//       );
//       return(false);
//     }
//     }
//   }
//   return(true);
// }




// std::vector<int>  get_dims_h5(const std::string filename, std::string datapath){
//   
// }




// Rcpp::List read_l_h5(const std::string h5filepath,
//                            const std::string groupname,
//                            Rcpp::CharacterVector subcols = Rcpp::CharacterVector::create(),
//                            Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(),
//                            Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(),
//                            Rcpp::IntegerVector filtervec = Rcpp::IntegerVector::create()
//                            ){
//   using namespace Rcpp;

//   HighFive::File file(h5filepath,HighFive::File::ReadOnly);
//   auto grp = file.getGroup(groupname);
//   std::vector<std::string> df_cols;
//   std::vector<size_t> col_sizes;
//   std::vector<size_t> elem(filtervec.size());
//   std::transform(filtervec.begin(),filtervec.end(),elem.begin(),[](int f) -> size_t{return f-1;});

//   if((offset.size()!=0) ^ (chunksize.size()!=0)){
//     Rcpp::Rcerr<<"with offset size: "<<offset.size()<<" and chunksize size: "<<chunksize.size()<<std::endl;
//     Rcpp::stop("offset and chunksize must both be specified or neither can be specified ");
//   }
//   const bool read_subset = !elem.empty();
//   const bool read_chunk = (offset.size()!=0) && (chunksize.size()!=0);
//   const size_t offset_r= read_chunk ? offset[0] :  0;
//   const size_t chunksize_r= read_chunk ? chunksize[0] :  0;



//   if(read_subset && read_chunk){
//     Rcpp::stop("filtervec and offset/chunksize cannot both be specified");
//   }
//   size_t num_cols = (subcols.size()==0) ? grp.getNumberObjects() : subcols.size();
//   if(subcols.size()!=0){
//     df_cols=Rcpp::as<std::vector<std::string> >(subcols);
//   }else{
//     df_cols.resize(num_cols);
//     for(int i=0; i<num_cols;i++){
//       df_cols[i]=grp.getObjectName(i);
//     }
//   }
//   col_sizes.resize(num_cols);
//   Rcpp::List retdf;
//   for(int i=0; i<num_cols;i++){
//     auto cname = df_cols[i];
//     auto dset = grp.getDataSet(cname);
//     auto dims = dset.getDataDimensions();
//     if(dims.size()!=1){
//       Rcpp::Rcerr<<"Can't read non-vector df_col: "<<cname<<std::endl;
//       Rcpp::stop("Currently unable to read non-vector columns");
//     }
//     col_sizes[i]=dims[0];
//     if(i>0){
//       if(col_sizes[i]!=col_sizes[i-1]){
//         Rcpp::Rcerr<<"df_col: "<<cname<<" has"<<col_sizes[i];
//         Rcpp::Rcerr<<"df_col: "<<df_cols[i-1]<<" has"<<col_sizes[i-1];
//         Rcpp::stop("All columns must have the same length");
//       }
//     }

//     auto my_t = h2r_T(dset.getDataType().getId());
//     switch (my_t){
//     case INTSXP: {
//       if(!read_subset){
//         retdf[cname]=read_v_h5<INTSXP>(file,
//                                              grp,
//                                              cname,
//                                              offset_r,chunksize_r);


//     }else{
//         retdf[cname]=read_elem_v_h5<INTSXP>(file,
//                                                   grp,
//                                                   cname,
//                                                   elem);
//       }
//       break;
//     }
//     case REALSXP: {
//       if(!read_subset){
//       retdf[cname]=read_v_h5<REALSXP>(file,
//                                             grp,
//                                             cname,
//                                             offset_r,chunksize_r);
//     }else{
//         retdf[cname]=read_elem_v_h5<REALSXP>(file,
//                                                    grp,
//                                                    cname,
//                                                    elem);
//       }
//       break;

//     }
//     case STRSXP: {
//       if(!read_subset){
//       retdf[cname]=read_v_h5<STRSXP>(file,
//                                            grp,
//                                            cname,
//                                            offset_r,chunksize_r);
//     }else{
//         retdf[cname]=read_elem_v_h5<STRSXP>(file,
//                                                   grp,
//                                                   cname,
//                                                   elem);
//       }
//       break;

//     }
//     default: {
//       warning(
//         "Invalid SEXPTYPE %d.\n",
//         my_t
//       );
//       Rcpp::Rcerr<<cname<<" has type that can't be read"<<std::endl;
//       Rcpp::stop("Can't read type");
//       return R_NilValue;
//     }
//     }
//   }
//   retdf.attr("names")=Rcpp::wrap(df_cols);
//   return retdf;
// }
