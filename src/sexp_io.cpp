#include <EigenH5.h>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::depends(BH)]]

#include <progress.hpp>
#include <array>

// [[Rcpp::interfaces(r, cpp)]]



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




SEXPTYPE check_dtype(HighFive::DataSet& dset){
  return(h2r_T(dset.getDataType().getId()));
}




SEXPTYPE check_dtype(const std::string &filename,
                     const std::string &groupname,
                     const std::string &dataname){

  using namespace HighFive;

  File file(filename,File::ReadOnly);
  return(h2r_T(file.getGroup(groupname).getDataSet(dataname).getDataType().getId()));
}


//SEXP read_vector_h5(const std::string &filename,
//                  const std::string &groupname,
//                  const std::string &dataname,
//                  Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(),
//                  Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(),
//                  Rcpp::IntegerVector filtervec = Rcpp::IntegerVector::create()){
//   using namespace Rcpp;

//   HighFive::File file(filename,HighFive::File::ReadOnly);
//   auto grp = file.getGroup(groupname);

//   std::vector<size_t> elem(filtervec.size());
//   std::transform(filtervec.begin(),filtervec.end(),elem.begin(),[](int f) -> size_t{return f-1;});

//   const bool read_subset = !elem.empty();
//   const bool read_chunk = (offset.size()!=0) && (chunksize.size()!=0);
//   const size_t offset_r= read_chunk ? offset[0] :  0;
//   const size_t chunksize_r= read_chunk ? chunksize[0] :  0;


//   if((offset.size()!=0) ^ (chunksize.size()!=0)){
//     Rcpp::stop("offset and chunksize must both be specified or neither can be specified ");
//   }
//   if(read_subset && read_chunk){
//     Rcpp::stop("filtervec and offset/chunksize cannot both be specified");
//   }


//   auto my_t = check_dtype(filename,groupname,dataname);
//   switch (my_t){
//   case INTSXP: {
//     if(!read_subset){
//     return read_v_h5<INTSXP>(file,
//                                          grp,
//                                          dataname,
//                                          offset_r,chunksize_r);


//   }else{
//     return read_elem_v_h5<INTSXP>(file,
//                                               grp,
//                                               dataname,
//                                               elem);
//   }
//   break;
//   }
//   case REALSXP: {
//     if(!read_subset){
//     return read_v_h5<REALSXP>(file,
//                                           grp,
//                                           dataname,
//                                           offset_r,chunksize_r);
//   }else{
//     return read_elem_v_h5<REALSXP>(file,
//                                                grp,
//                                                dataname,
//                                                elem);
//   }
//   break;

//   }
//   case STRSXP: {
//     if(!read_subset){
//     return read_v_h5<STRSXP>(file,
//                                          grp,
//                                          dataname,
//                                          offset_r,chunksize_r);
//   }else{
//     return read_elem_v_h5<STRSXP>(file,
//                                               grp,
//                                               dataname,
//                                               elem);
//   }
//   break;

//   }
//   default: {
//     warning(
//       "Invalid SEXPTYPE %d.\n",
//       my_t
//     );
//     Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;
//     Rcpp::stop("Can't read type");
//     return R_NilValue;
//   }
//   }
// }

// //[[Rcpp::export]]
Rcpp::IntegerVector get_dims_h5(const std::string &filename,
                                const std::string &groupname,
                                const std::string &dataname){
  return(Rcpp::wrap(HighFive::File(filename,HighFive::File::ReadOnly).getGroup(groupname).getDataSet(dataname).getDataDimensions()));
}


// Rcpp::DataFrame gen_MatSlice(const std::string &filename,
//                                 const std::string &groupname,
// 			     const std::string &dataname,
// 			     Rcpp::List subset_indices=Rcpp::ListOf<Rcpp::IntegerVector>::create(Rcpp::IntegerVector::create())){
//     HighFive::File file(filename,HighFive::File::ReadOnly);
//   auto grp = file.getGroup(groupname);
//   auto dset = file.getGroup(groupname).getDataSet(dataname);
//   auto dims = dset.getDataDimensions();
//   const int num_dims=dims.size();
//   int n_subsets = subset_indices.size();
//   if(n_subsets>num_dims){
//     Rcpp::Rcerr<<"Rank of "<<groupname<<"/"<<dataname<<" is"<<num_dims<<std::endl;
//     Rcpp::Rcerr<<"Rank of selection is is"<<n_subsets<<std::endl;
//   }
//   if(num_dims>4){
//     Rcpp::stop("EigenH5 currently can't handle data with dimensions greater than 4");
//   }
//   std::vector<std::vector<dim_sel> > chunk_vec(num_dims);
//   for(int i=0;i<num_dims;i++){
//     std::vector<int> local_subset_vec;
//     if(i<n_subsets){
//       local_subset_vec=as<std::vector<int> >(as<IntegerVector>(subset_indices[i]));
//     }
//     chunk_vec[i] = find_cont(local_subset_vec.begin(),local_subset_vec.end(),dims[i]);
//   }
// }









DimRange gen_DimRange(int offset,
		      int chunksize,
		      const Rcpp::IntegerVector subset_index,
		      int dset_dim){
  if(!Rcpp::IntegerVector::is_na(offset)){
    if(subset_index.size()>0){
      Rcpp::stop("Can't specify	subset_index and offset");
    }
    if(Rcpp::IntegerVector::is_na(chunksize)){
       chunksize=dset_dim-offset;
    }
    return(DimRange(dim_sel(offset,chunksize,dset_dim)));
  }
  if(subset_index.size()>0){
    return(DimRange(subset_index.begin(),subset_index.end()));
  }
  return(DimRange(dim_sel(0,dset_dim,dset_dim)));
}





DatasetSelection gen_vector_selection(const int offset,
				      const int chunksize,
				      const Rcpp::IntegerVector subset
				      const int dset_dim){
  return(DatasetSelection(gen_DimRange(offset,chunksize,subset,dset_dim),
			  gen_DimRange(0,1,Rcpp::IntegerVector::create(),1),{dset_dim,1}));

}




DatasetSelection gen_matrix_selection(const Rcpp::IntegerVector offsets,
				      const Rcpp::IntegerVector chunksizes,
				      const Rcpp::IntegerVector subset_rows,
				      const Rcpp::IntegerVector subset_cols,
				      std::vector<int> dset_dims){
  return(DatasetSelection(gen_DimRange(offsets(0),chunksizes(0),subset_rows,dset_dims[0]),
			  gen_DimRange(offsets(1),chunksizes(1),subset_cols,dset_dims[1]),dset_dims));

}


template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type> Rcpp::Matrix<RTYPE> read_elem_m_h5(DatasetSelection mat_sel,HighFive::DataSet &dset){

  if constexpr (!std::is_arithmetic<T>::value){
      //      Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> ttrm();
      std::vector<T>ttrm (mat_sel.n_elem[0]*mat_sel.n_elem[1]);
      Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > tretmat(ttrm.data(),mat_sel.n_elem[0],mat_sel.n_elem[1]);
      mat_sel.readEigen(dset,tretmat);
      Rcpp::Vector<RTYPE> retv(ttrm.begin(),ttrm.end());
      Rcpp::Matrix<RTYPE> retmat(mat_sel.n_elem[0],mat_sel.n_elem[1],retv.begin());
      // for(int i=0;i<mat_sel.n_elem[1]; i++){
      // 	for(int j=0; j<mat_sel.n_elem[0]; j++){
      // 	  t_el=ttrm(0,0);
      // 	  retmat(j,i)=Rcpp::String(t_el);
      // 	}
      // }
      return(retmat);
    }
  else{
    Rcpp::Matrix<RTYPE> retmat(mat_sel.n_elem[0],mat_sel.n_elem[1]);
    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > tretmat(&retmat(0,0),mat_sel.n_elem[0],mat_sel.n_elem[1]);
    mat_sel.readEigen(dset,tretmat);
    return(retmat);
  }
}

template <SEXPTYPE RTYPE,typename T= typename r2cpp_t<RTYPE>::type> void write_elem_m_h5(DatasetSelection mat_sel,HighFive::DataSet &dset,Rcpp::Matrix<RTYPE> writemat){
  if constexpr (!std::is_arithmetic<T>::value){
      Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> ttrm(mat_sel.n_elem[0],mat_sel.n_elem[1]);
      Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > tretmat(ttrm.data(),mat_sel.n_elem[0],mat_sel.n_elem[1]);
      for(int i=0;i<mat_sel.n_elem[1]; i++){
	for(int j=0; j<mat_sel.n_elem[0]; j++){
	  ttrm(j,i)=Rcpp::as<T>(writemat(j,i));
	}
      }
      mat_sel.writeEigen(dset,tretmat);
    }
  else{
    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > tretmat(&writemat(0,0),mat_sel.n_elem[0],mat_sel.n_elem[1]);
    mat_sel.writeEigen(dset,tretmat);
  }


}









//[[Rcpp::export]]
SEXP read_matrix_h5(const std::string &filename,
                    const std::string &groupname,
                    const std::string &dataname,
                    Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(),
                    Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create(),
                    const Rcpp::IntegerVector subset_rows = Rcpp::IntegerVector::create(),
                    const Rcpp::IntegerVector subset_cols = Rcpp::IntegerVector::create()){


  if(offsets.size()==0){
    offsets = Rcpp::IntegerVector::create(NA_INTEGER,NA_INTEGER);
  }

  using namespace Rcpp;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.getGroup(groupname);
  auto dset = file.getGroup(groupname).getDataSet(dataname);
  auto dims = dset.getDataDimensions();
  std::vector<int> ddims(dims.begin(),dims.end());
  if(chunksizes.size()==0){
    chunksizes = Rcpp::IntegerVector::create(NA_INTEGER,NA_INTEGER);
  }

  auto mat_sel =gen_matrix_selection(offsets,chunksizes,subset_rows,subset_cols,ddims);

  auto my_t = check_dtype(dset);

  switch (my_t){
  case INTSXP: {
    return(read_elem_m_h5<INTSXP>(mat_sel,dset));
    break;
  }
  case REALSXP: {
    return(read_elem_m_h5<REALSXP>(mat_sel,dset));
    break;
  }
  case STRSXP: {
    return(read_elem_m_h5<STRSXP>(mat_sel,dset));
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










// void write_vector_h5(const std::string &filename,
//                      const std::string &groupname,
//                      const std::string &dataname,
//                      SEXP data){
//   using namespace Rcpp;
//   HighFive::File file(filename, HighFive::File::ReadWrite | HighFive::File::Create);
//   HighFive::Group group = file.createOrGetGroups(groupname);
//   auto my_t = TYPEOF(data);
//   switch (my_t){
//   case INTSXP: {
//     auto d=Rcpp::as<std::vector<int> >(data);
//     write_v_h5<int>(d,file,group,dataname);
//     break;
//   }
//   case REALSXP: {
//     auto d=Rcpp::as<std::vector<double> >(data);
//     write_v_h5<double>(d,file,group,dataname);
//     break;
//   }
//   case STRSXP: {
//     auto d=Rcpp::as<std::vector<std::string> >(data);

//     write_v_h5<std::string>(d,file,group,dataname);
//     break;
//   }
//   default: {
//     warning(
//       "Invalid SEXPTYPE %d.\n",
//       my_t
//     );
//   }
//   }
// }



//[[Rcpp::export]]
void create_matrix_h5(const std::string &filename,
                     const std::string &groupname,
                     const std::string &dataname,
                     SEXP data,
                     const bool doTranspose=false,
		      Rcpp::IntegerVector dims=Rcpp::IntegerVector::create(),
                     const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create() ){

 using namespace Rcpp;
  std::vector<size_t> local_chunksizes;

  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));

  HighFive::File file(filename,HighFive::File::ReadWrite|HighFive::File::Create);

  auto grp = file.createOrGetGroups(groupname);
  const bool create_ds=!grp.exist(dataname);
  std::vector<int> data_dims=Rcpp::as<std::vector<int> >(dims);
  if(dims.size()==1){
    dims.push_back(1);
  }
  if(dims.size()!=2){
    Rcpp::stop("dims should be of length 2");
  }
  auto dsel= gen_matrix_selection(Rcpp::IntegerVector::create(0,0),
				  dims,
				  Rcpp::IntegerVector::create(),
				  Rcpp::IntegerVector::create(),data_dims);

  auto my_t = TYPEOF(data);
  switch (my_t){
  case INTSXP: {
    //    Rcpp::Matrix<INTSXP> rmat(data);
    dsel.createDataset<int>(grp,dataname,local_chunksizes);
    break;
  }
  case REALSXP: {
    //    Rcpp::Matrix<REALSXP> rmat(data);
    dsel.createDataset<double>(grp,dataname,local_chunksizes);
    break;
  }
  case STRSXP: {
    //    Rcpp::Matrix<STRSXP> rmat(data);
    dsel.createDataset<std::string>(grp,dataname,local_chunksizes);
    break;
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d.\n",
      my_t
    );
    Rcpp::stop("Unable to write data of this type");
  }
  }
}





//#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

template <int RTYPE>
Vector<INTSXP> dims(const Matrix<RTYPE>& x)
{
    return Vector<INTSXP>::create(x.nrow(), x.ncol());
}

template <int RTYPE>
int len(const Vector<RTYPE>& x)
{
    return static_cast<int>(x.size());
}


} // impl


IntegerVector dims(RObject x)
{
    RCPP_RETURN_MATRIX(impl::dims, x);
}







int len(RObject x)
{
    RCPP_RETURN_VECTOR(impl::len, x);
}



//[[Rcpp::export]]
void write_vector_h5(const std::string &filename,
                     const std::string &groupname,
                     const std::string &dataname,
                     SEXP data,
                     const Rcpp::IntegerVector offset=Rcpp::IntegerVector::create(0),
                     const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create() ){
  using namespace Rcpp;
  std::vector<size_t> local_chunksizes;

  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));

  HighFive::File file(filename,HighFive::File::ReadWrite|HighFive::File::Create);

  auto grp = file.createOrGetGroups(groupname);
  const bool create_ds=!grp.exist(dataname);

  int data_dim=len(data);
  int dset_dim=data_dims;
  if(!create_ds){
    data_dim=static_cast<int>(grp.getDataSet(dataname).getDataDimensions()[0]);
  }else{
    dset_dim=data_dim;
  }
  //: grp.getDataSet(dataname).getDataDimensions();
  auto dsel= gen_vector_selection(offset[0],
				  *(data_dims.begin()),
				  Rcpp::IntegerVector::create(),
				  Rcpp::IntegerVector::create(),dset_dim);

  auto my_t = TYPEOF(data);
  switch (my_t){
  case INTSXP: {
    Rcpp::Matrix<INTSXP> rmat(data);
    auto dset = create_ds? dsel.createDataset<int>(grp,dataname,local_chunksizes) : grp.getDataSet(dataname);
    write_elem_m_h5<INTSXP,int>(dsel,dset,rmat);
    break;
  }
  case REALSXP: {
    Rcpp::Matrix<REALSXP> rmat(data);
    auto dset = create_ds? dsel.createDataset<double>(grp,dataname,local_chunksizes) : grp.getDataSet(dataname);
    write_elem_m_h5<REALSXP,double>(dsel,dset,rmat);
    break;
  }
  case STRSXP: {
    Rcpp::Matrix<STRSXP> rmat(data);
    auto dset = create_ds? dsel.createDataset<std::string>(grp,dataname,local_chunksizes) : grp.getDataSet(dataname);
    write_elem_m_h5<STRSXP,std::string>(dsel,dset,rmat);
    break;
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d.\n",
      my_t
    );
    Rcpp::stop("Unable to write data of this type");
  }
  }
}



// template<typename T>
// std::vector<int> get_mat_dims(const Rcpp::Matrix<T> m){
//   return({m.rows(),m.cols});
// }





//[[Rcpp::export]]
void write_matrix_h5(const std::string &filename,
                     const std::string &groupname,
                     const std::string &dataname,
                     SEXP data,
                     const bool doTranspose=false,
                     const Rcpp::IntegerVector offsets=Rcpp::IntegerVector::create(0,0),
                     const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create() ){
  using namespace Rcpp;
  std::vector<size_t> local_chunksizes;

  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));

  HighFive::File file(filename,HighFive::File::ReadWrite|HighFive::File::Create);

  auto grp = file.createOrGetGroups(groupname);
  const bool create_ds=!grp.exist(dataname);

  std::vector<int> data_dims=Rcpp::as<std::vector<int> >(dims(data));
  std::vector<int> dset_dims(data_dims.size());
  if(!create_ds){
    auto dsd=grp.getDataSet(dataname).getDataDimensions();
    std::copy(dsd.begin(),dsd.end(),dset_dims.begin());
  }else{
    std::copy(data_dims.begin(),data_dims.end(),dset_dims.begin());
  }
  //: grp.getDataSet(dataname).getDataDimensions();
  auto dsel= gen_matrix_selection(offsets,
				  Rcpp::IntegerVector(data_dims.begin(),data_dims.end()),
				  Rcpp::IntegerVector::create(),
				  Rcpp::IntegerVector::create(),dset_dims);

  auto my_t = TYPEOF(data);
  switch (my_t){
  case INTSXP: {
    Rcpp::Matrix<INTSXP> rmat(data);
    auto dset = create_ds? dsel.createDataset<int>(grp,dataname,local_chunksizes) : grp.getDataSet(dataname);
    write_elem_m_h5<INTSXP,int>(dsel,dset,rmat);
    break;
  }
  case REALSXP: {
    Rcpp::Matrix<REALSXP> rmat(data);
    auto dset = create_ds? dsel.createDataset<double>(grp,dataname,local_chunksizes) : grp.getDataSet(dataname);
    write_elem_m_h5<REALSXP,double>(dsel,dset,rmat);
    break;
  }
  case STRSXP: {
    Rcpp::Matrix<STRSXP> rmat(data);
    auto dset = create_ds? dsel.createDataset<std::string>(grp,dataname,local_chunksizes) : grp.getDataSet(dataname);
    write_elem_m_h5<STRSXP,std::string>(dsel,dset,rmat);
    break;
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d.\n",
      my_t
    );
    Rcpp::stop("Unable to write data of this type");
  }
  }
}


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
//       write_v_h5<int>(d,file,group,t_colname);
//       break;
//     }
//     case REALSXP: {
//       auto d=Rcpp::as<std::vector<double> >(t_col);
//       write_v_h5<double>(d,file,group,t_colname);
//       break;
//     }
//     case STRSXP: {
//       auto d=Rcpp::as<std::vector<std::string> >(t_col);
      
//       write_v_h5<std::string>(d,file,group,t_colname);
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





//[[Rcpp::export]]
Rcpp::StringVector get_objs_h5(Rcpp::CharacterVector h5filepath,Rcpp::CharacterVector groupname=Rcpp::CharacterVector::create("/")){

  HighFive::File file(Rcpp::as<std::string>(h5filepath(0)),HighFive::File::ReadOnly);
  std::string gname=Rcpp::as<std::string>(groupname(0));
  // if(file.is)
  auto grp = file.getGroup(gname);
  const size_t num_cols = grp.getNumberObjects();
  Rcpp::StringVector retvec(num_cols);
  for(int i=0; i<num_cols;i++){
    retvec[i]=grp.getObjectName(i);
  }
  return(retvec);
}



// Rcpp::List read_l_h5(const std::string h5filepath,
// 		     const std::string groupname,
// 		     Rcpp::CharacterVector subcols = Rcpp::CharacterVector::create(),
// 		     Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(),
// 		     Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(),
// 		     Rcpp::IntegerVector filtervec = Rcpp::IntegerVector::create()
// 		     ){
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






// Rcpp::ListOf<Rcpp::IntegerVector> intersect_snpinfo_h5(std::vector<std::string> h5files){

//    using namespace HighFive;
//   const size_t num_files=h5files.size();

//   std::vector<int> pos_map;
//   std::vector<int> chr_map;
//   std::vector<int> idx_map;
//   using namespace ranges;
//   size_t num_elem_min=std::numeric_limits<size_t>::max();
//   std::map<std::pair<int,int>,std::vector<std::pair<int,int> > > cur_pos_map;
//   std::map<int,std::vector<int> > ret_idx_map;
//   for(int i=0; i < num_files; i++){
//     auto tfile=h5files[i];
//     Rcpp::Rcout<<"Reading file: "<<tfile<<std::endl;
//     HighFive::File fv(tfile,File::ReadOnly);
//     fv.getGroup("SNPinfo").getDataSet("pos").read(pos_map);
//     fv.getGroup("SNPinfo").getDataSet("chr").read(chr_map);
//     fv.getGroup("SNPinfo").getDataSet("snp_id").read(idx_map);
//     const size_t num_elem_i = idx_map.size();
//     if(num_elem_i<num_elem_min){
//       num_elem_min=num_elem_i;
//     }
//     for(int j=0; j<num_elem_i;j++){
//       cur_pos_map[{chr_map[j],pos_map[j]}].push_back({idx_map[j],i});
//     }

//     //Rcpp::Rcout<<"size of: "<<idx_map[i].size()<<std::endl;
//   }
//   for(int i=0; i<num_files;i++){
//     ret_idx_map[i].reserve(num_elem_min);
//   }

//   for(const auto& k_v : cur_pos_map){
//     if((k_v.second.size())==num_files){
//       for(const auto& e : k_v.second){
//         ret_idx_map[e.second].push_back(e.first);
//       }
//     }
//   }

//   List resl(num_files);
//   for(int i=0;i<num_files;i++){
//     resl[i]=wrap(ret_idx_map[i]);
//   }
//   resl.names()=Rcpp::wrap(h5files);

//   return(resl);

// }
