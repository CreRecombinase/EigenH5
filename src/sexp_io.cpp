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


//  template <typename T> void write_v_h5(std::vector<T> &data,
// 					HighFive::File &file,
// 					HighFive::Group & group,
// 					const std::string &dataname){

//     using namespace HighFive;


//     //

//     std::vector<size_t> vec_dims{data.size()};
//     // int r = 0;

//     // Create a new file using the default property lists.
//     Filter filter({1000}, vec_dims, FILTER_BLOSC, 1);
//     // Create a dataset with double precision floating points


//     DataSpace ds = DataSpace(vec_dims);

//     DataSet dataset = group.createDataSet(dataname, ds, AtomicType<T>(), filter.getId());
//     //  if(std::is_same_v<T,std::string>){
//     //    Rcpp::Rcerr<<"Using boost"<<std::endl;
//     using Marray = boost::multi_array_ref<T,1>;
//     std::array<typename Marray::size_type,1> data_dims= {{data.size()}};
//     boost::multi_array_ref<T,1> tw(data.data(),data_dims);
//     dataset.write(tw);
//     // }else{
//     //   dataset.write(data);
//     // }
//   }




//   template <SEXPTYPE RTYPE> Vector<RTYPE> read_v_h5(
// 						    HighFive::File &file,
// 						    HighFive::Group & grp,
// 						    const std::string &dataname,
// 						    const size_t offset=0,
// 						    const size_t chunksize=0){

//     using T = typename r2cpp_t<RTYPE>::type;
//     std::vector<T> retvec;
//     using namespace HighFive;

//     if(chunksize==0){
//       grp.getDataSet(dataname).read(retvec);
//     }else{
//       std::vector<size_t> off_v={offset};
//       std::vector<size_t> ret_v={chunksize};
//       grp.getDataSet(dataname).select(off_v,ret_v,{}).read(retvec);
//     }
//     return(Rcpp::wrap(retvec));
//   }


//   template <SEXPTYPE RTYPE> Vector<RTYPE> read_elem_v_h5(
// 							 HighFive::File &file,
// 							 HighFive::Group & grp,
// 							 const std::string &dataname,
// 							 std::vector<size_t> elem){

//     using T = typename r2cpp_t<RTYPE>::type;
//     std::vector<T> retvec;
//     //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;

//     using namespace HighFive;

//     grp.getDataSet(dataname).select(HighFive::ElementSet(elem)).read(retvec);
//     return(Rcpp::wrap(retvec));
//   }







// bool data_exists(const std::string &filename,
//                      const std::string &groupname,
//                      const std::string &dataname){
//   using namespace HighFive;
//     File file(filename,File::ReadOnly);
//     if(!file.exist(groupname)){
//       return(false);
//     }
//     return(file.getGroup(groupname).exist(dataname));
// }



// SEXPTYPE check_dtype(const std::string &filename,
//                      const std::string &groupname,
//                      const std::string &dataname){

//   using namespace HighFive;

//   File file(filename,File::ReadOnly);
//   return(h2r_T(file.getGroup(groupname).getDataSet(dataname).getDataType().getId()));
// }


// SEXP read_vector_h5(const std::string &filename,
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

//[[Rcpp::export]]
Rcpp::DataFrame gen_MatSlice(const std::string &filename,
                                const std::string &groupname,
			     const std::string &dataname,
			     Rcpp::List subset_indices=Rcpp::ListOf<Rcpp::IntegerVector>::create(Rcpp::IntegerVector::create())){
    HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.getGroup(groupname);
  auto dset = file.getGroup(groupname).getDataSet(dataname);
  auto dims = dset.getDataDimensions();
  const int num_dims=dims.size();
  int n_subsets = subset_indices.size();
  if(n_subsets>num_dims){
    Rcpp::Rcerr<<"Rank of "<<groupname<<"/"<<dataname<<" is"<<num_dims<<std::endl;
    Rcpp::Rcerr<<"Rank of selection is is"<<n_subsets<<std::endl;
  }
  if(num_dims>4){
    Rcpp::stop("EigenH5 currently can't handle data with dimensions greater than 4");
  }
  std::vector<std::vector<dim_sel> > chunk_vec(num_dims);
  for(int i=0;i<num_dims;i++){
    std::vector<int> local_subset_vec;
    if(i<n_subsets){
      local_subset_vec=as<std::vector<int> >(as<IntegerVector>(subset_indices[i]));
    }
    chunk_vec[i] = find_cont(local_subset_vec.begin(),local_subset_vec.end(),dims[i]);
  }
}



// //[[Rcpp::export]]
Rcpp::DataFrame cont_diff(Rcpp::IntegerVector inp,int chunksize=0){
  using namespace ranges;
  const int out_size=inp.size();
  if(chunksize==0){
    chunksize = out_size;
  }
  using namespace std::placeholders;
  
  auto ir = make_iterator_range(inp.begin(), inp.end());
  // auto dr = make_iterator_range(diff_v.begin(), diff_v.end());
  auto b_chunk = std::bind(view::chunk,_1,chunksize);
  std::vector<std::tuple<int,int,int> > ar= view::zip_with([](int i,int j){
    return(std::make_tuple(i-1,j));
  },ir,view::ints(0)) | view::group_by([&](std::tuple<int,int> i, std::tuple<int,int> j){
    return((std::get<0>(i)-std::get<0>(j))==(std::get<1>(i)-std::get<1>(j)));
  }) | view::transform(b_chunk) | view::join | view::transform([](auto el){
    auto elr = el.front();
    int csize= distance(el);
    return(std::make_tuple(std::get<0>(elr),std::get<1>(elr),csize));
  });
  
  int tkk=0;
  const int n_groups = ar.size();
  Rcpp::IntegerVector chunk_i(n_groups);
  Rcpp::IntegerVector in_beg(n_groups);
  Rcpp::IntegerVector out_beg(n_groups);
  Rcpp::IntegerVector csize(n_groups);
  for(int i=0; i<n_groups;i++){
    auto te=ar[i];
    chunk_i[i]=i;
    in_beg[i]=std::get<0>(te);
    out_beg[i]=std::get<1>(te);
    csize[i]  = std::get<2>(te);
  }
 
 
  using namespace Rcpp;
  
  return(DataFrame::create( _["chunk_id"]=chunk_i,
                            _["in_offset"]=in_beg,
                            _["out_offset"]=out_beg,
                            _["chunksize"]=csize));
}


											  
  

// template<typename It>
// std::vector<dim_sel> find_cont(It itb, It ite,const int total_size, int chunksize=0){
//   using namespace Rcpp;
//   using namespace ranges;
//   using iarray = std::array<int,2>;
//   using piarray = std::pair<iarray,iarray>;
  

//   const int n_elem = ite-itb;

//   std::transform(itb,ite,itb,[](int f){return f-1;});

//   auto itbb=itb;
//   auto it = itb;
//   int tot_dist=0;

//   std::vector<dim_sel> sub_ranges;
//     sub_ranges.reserve(n_elem/2);
//   if(it==ite && chunksize==0){
//     sub_ranges.push_back(dim_sel(0,total_size-1,0,total_size-1));
//     return(sub_ranges);
//   }

//   if(chunksize==0){
//     chunksize = n_elem;
//   }

//   while(it!=ite){
//     int sf=0;

//     it = std::adjacent_find(itb,ite,[&](int i,int j){
//       sf++;
//       return(((j-i)!=1) && (sf>=chunksize));
//     });
//     int iti = it==ite ? *(it-1) : *(it);
//     int itb_pos = itb-itbb;
//     int reg_size = it==ite ? it-itb : (it-itb+1);
//     sub_ranges.push_back(dim_sel(*itb,iti,tot_dist,tot_dist+reg_size-1));
//     if(it!=ite){
//       it++;
//     }
//     tot_dist=tot_dist+reg_size;
//     itb=it;
//   }
//   return(sub_ranges);
// }



// // Rcpp::DataFrame cont_reg(Rcpp::IntegerVector input,int chunksize=0,int total_size=0){
 
// //   auto ret=find_cont(input.begin(),input.end(),total_size,chunksize);
// //   const size_t nret=ret.size();
// //   using namespace Rcpp;
// //   IntegerVector begv(nret);
// //   IntegerVector endv(nret);
// //   IntegerVector sv1(nret);
// //   IntegerVector sv2(nret);
// //   std::array<int,2> tra,trb;
// //   for(int i=0;i<nret;i++){
// //     std::tie(tra,trb) = ret[i];
// //     begv[i]=tra[0];
// //     endv[i]=tra[1];
// //     sv1[i]=trb[0];
// //     sv2[i]=trb[1];
// //   }
// //   return(DataFrame::create( _["in_start"] = begv,
// //                             _["in_stop"] = endv,
// //                             _["out_start"] = sv1,
// //                             _["out_stop"] = sv2));
// // }



// // template<class Rng,int RTYPE,typename T=typename r2cpp_t<RTYPE>::type>
// // class Vector_view
// //   : public ranges::view_adaptor<Vector_view<Rng,RTYPE,T>, Rng>
// // {
// //     friend ranges::range_access;
// //     class adaptor : public ranges::adaptor_base
// //     {
// //     public:
// //         adaptor() = default;
// //       //        adaptor(ranges::semiregular_t<Fun> const &fun) : fun_(fun) {}
// //         // Here is where we apply Fun to the elements:
// //         auto read(ranges::iterator_t<Rng> it) const -> T
// //         {
// //             return fun_(*it);
// //         }
// //     };
// //     adaptor begin_adaptor() const { return {fun_}; }
// //     adaptor end_adaptor() const { return {fun_}; }
// // public:
// //     transform_view() = default;
// //     transform_view(Rng && rng, Fun fun)
// //       : transform_view::view_adaptor{std::forward<Rng>(rng)}
// //       , fun_(std::move(fun))
// //     {}
// // };







// // typedef boost::multi_array_ref<int,1> i_1;
// // typedef boost::multi_array_ref<int,2> i_2;
// // typedef boost::multi_array_ref<int,3> i_3;
// // typedef boost::multi_array_ref<int,4> i_4;

// // typedef boost::multi_array_ref<double,1> d_1;
// // typedef boost::multi_array_ref<double,2> d_2;
// // typedef boost::multi_array_ref<double,3> d_3;
// // typedef boost::multi_array_ref<double,4> d_4;

// // typedef boost::multi_array_ref<Rcpp::String,1> s_1;
// // typedef boost::multi_array_ref<Rcpp::String,2> s_2;
// // typedef boost::multi_array_ref<Rcpp::String,3> s_3;
// // typedef boost::multi_array_ref<Rcpp::String,4> s_4;





// // std::variant<i_1,i_2,i_3,i_4,d_1,d_2,d_3,d_4,s_1,s_2,s_3,s_4> boost_wrap(Rcpp::RObject data){


// //
// //     Rcpp::Rcout<<"Checking Type"<<std::endl;
// //



// //   switch (my_t){
// //   case INTSXP: {
// //     Rcpp::Rcout<<"INT"<<std::endl;
// //     int* tdata=INTEGER(SEXP(data));
// //     if(chunksize.size()==1){
// //       Rcpp::Rcout<<"i_1"<<std::endl;
// //       i_1 tref(tdata,chunksize,boost::c_storage_order());
// //       return(tref);
// //     }
// //     if(chunksize.size()==2){
// //       Rcpp::Rcout<<"i_2"<<std::endl;
// //       i_2 tref(tdata,chunksize,boost::fortran_storage_order());
// //       return(tref);
// //     }
// //     if(chunksize.size()==3){
// //       Rcpp::Rcout<<"i_3"<<std::endl;
// //       i_3 tref(tdata,chunksize,boost::fortran_storage_order());
// //       return(tref);
// //     }
// //     if(chunksize.size()==4){
// //       Rcpp::Rcout<<"i_4"<<std::endl;
// //       i_4 tref(tdata,chunksize,boost::fortran_storage_order());
// //       return(tref);
// //     }
// //     break;
// //   }
// //   case REALSXP:{
// //     double* tdata=REAL(SEXP(data));
// //     if(chunksize.size()==1){
// //        Rcpp::Rcout<<"d_1"<<std::endl;
// //       d_1 tref(tdata,chunksize,boost::fortran_storage_order());
// //       return(tref);
// //     }
// //     if(chunksize.size()==2){
// //        Rcpp::Rcout<<"d_2"<<std::endl;
// //       d_2 tref(tdata,chunksize,boost::fortran_storage_order());
// //       return(tref);
// //     }
// //     if(chunksize.size()==3){
// //        Rcpp::Rcout<<"d_3"<<std::endl;
// //       d_3 tref(tdata,chunksize,boost::fortran_storage_order());
// //       return(tref);
// //     }
// //     if(chunksize.size()==4){
// //       Rcpp::Rcout<<"d_3"<<std::endl;
// //       d_4 tref(tdata,chunksize,boost::fortran_storage_order());
// //       return(tref);
// //     }
// //     break;
// //   }
// //   }
// // }
   

  

// // void multi_array_variant(SEXP input_mat){
// //   Rcpp::Rcout<<"Calling boost_wrap"<<std::endl;
// //   auto myv = boost_wrap(input_mat);
// // }

  
  




// SEXP read_matrix_h5(const std::string &filename,
//                     const std::string &groupname,
//                     const std::string &dataname,
//                     const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(),
//                     const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create(),
//                     const Rcpp::IntegerVector subset_rows = Rcpp::IntegerVector::create(),
//                     const Rcpp::IntegerVector subset_cols = Rcpp::IntegerVector::create()){
//   using namespace Rcpp;
//   HighFive::File file(filename,HighFive::File::ReadOnly);
//   auto grp = file.getGroup(groupname);
//   auto dset = file.getGroup(groupname).getDataSet(dataname);
//   auto dims = dset.getDataDimensions();

//   std::vector<int> local_offsets=Rcpp::as< std::vector<int> >(offsets);
//   std::vector<int> local_chunksizes=Rcpp::as< std::vector<int> >(chunksizes);
//   std::vector<int> local_subset_rows=Rcpp::as< std::vector<int> >(subset_rows);
//   std::vector<int> local_subset_cols=Rcpp::as< std::vector<int> >(subset_cols);

//   const bool read_subset_rows = (local_subset_rows.size()!=0);
//   const bool read_subset_cols = (local_subset_cols.size()!=0);
//   auto row_chunks= find_cont(local_subset_rows.begin(),local_subset_rows.end(),dims[0]);
//   auto col_chunks= find_cont(local_subset_cols.begin(),local_subset_cols.end(),dims[1]);

//   bool read_chunk = (local_offsets.size()!=0) && (local_chunksizes.size()!=0);
//   if(read_subset_rows && read_chunk){
//     Rcpp::stop("subset_rows and chunking can't both be specified");
//   }
//   if(read_subset_cols && read_chunk){
//     Rcpp::stop("subset_rows and chunking can't both be specified");
//   }
//   if(read_chunk && ((local_offsets.size()!=2) && (local_chunksizes.size()!=2))){
//     Rcpp::stop("offset and chunksize must both be empty or must both be length two vectors");
//   }
//   if(!read_chunk){
//   }


//   if((local_offsets.size()!=0) ^ (local_chunksizes.size()!=0)){
//     Rcpp::stop("offset and chunksize must both be specified or neither can be specified ");
//   }

//   if(!read_chunk){
//     local_offsets = std::vector<int>{0,0};
//     local_chunksizes = std::vector<int>{static_cast<int>(dims[0]),static_cast<int>(dims[1])};
//   }
//   std::array<int,2> start_r = {local_offsets[0],local_offsets[1]};
//   std::array<int,2> stop_r  = {local_offsets[0]+local_chunksizes[0]-1,local_offsets[1]+local_chunksizes[1]-1};


//   const bool read_subset = read_subset_rows || read_subset_cols;

//   auto my_t = check_dtype(filename,groupname,dataname);


//   switch (my_t){
//   case INTSXP: {
//     return(read_elem_m_h5<INTSXP>(dset,row_chunks,col_chunks));
//     break;
//   }
//   case REALSXP: {
//     return(read_elem_m_h5<REALSXP>(dset,row_chunks,col_chunks));
//     break;
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


// void create_matrix_h5(const std::string &filename,
//                      const std::string &groupname,
//                      const std::string &dataname,
//                      SEXP data,
//                      const bool doTranspose=false,
//                      const Rcpp::IntegerVector dims=Rcpp::IntegerVector::create(),
//                      const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create() ){
//   using namespace Rcpp;
//   std::vector<size_t> local_chunksizes;
//   std::vector<size_t> local_dims;
//   std::copy(dims.begin(),dims.end(),std::back_inserter(local_dims));
//   std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));

//   HighFive::File file(filename,HighFive::File::ReadWrite|HighFive::File::Create);
//   auto grp = file.createOrGetGroups(groupname);


//   auto my_t = TYPEOF(data);
//   switch (my_t){
//   case INTSXP: {
//     create_m_h5<int>(local_dims,grp,dataname,doTranspose,local_chunksizes);
//     break;
//   }
//   case REALSXP: {
//     create_m_h5<double>(local_dims,grp,dataname,doTranspose,local_chunksizes);
//     break;
//   }
//   case STRSXP: {
//     create_m_h5<std::string>(local_dims,grp,dataname,doTranspose,local_chunksizes);
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







// void write_matrix_h5(const std::string &filename,
//                      const std::string &groupname,
//                      const std::string &dataname,
//                      SEXP data,
//                      const bool doTranspose=false,
//                      const Rcpp::IntegerVector offsets=Rcpp::IntegerVector::create(0,0),
//                      const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create() ){
//   using namespace Rcpp;
//   std::vector<size_t> local_chunksizes;

//   std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));

//   HighFive::File file(filename,HighFive::File::ReadWrite|HighFive::File::Create);
//   auto grp = file.createOrGetGroups(groupname);
//   const bool create_ds=!grp.exist(dataname);
//   auto my_t = TYPEOF(data);
//   switch (my_t){
//   case INTSXP: {
//     Rcpp::Matrix<INTSXP> rmat(data);
//     const int rows=rmat.rows();
//     const int cols=rmat.cols();
//     Eigen::Map<Eigen::MatrixXi> d(&rmat(0,0),rows,cols);
//     if(create_ds){
//       create_m_h5<int>({{static_cast<size_t>(rows),static_cast<size_t>(cols)}},grp,dataname,doTranspose,local_chunksizes);
//     }
//     auto dset = grp.getDataSet(dataname);

//     write_m_h5<int>(d,dset,{offsets[0],offsets[1]},{offsets[0]+rows-1,offsets[1]+cols-1});
//     // Rcpp::Rcout<<"Row_out: "<<offsets[0]<<" : "<<offsets[0]+rows-1<<std::endl;
//     // Rcpp::Rcout<<"Col out: "<<offsets[1]<<" : "<<offsets[1]+cols-1<<std::endl;
//     break;
//   }
//   case REALSXP: {
//     Rcpp::Matrix<REALSXP> rmat(data);
//     const int rows=rmat.rows();
//     const int cols=rmat.cols();
//     Eigen::Map<Eigen::MatrixXd> d(&rmat(0,0),rows,cols);
//     if(create_ds){
//       create_m_h5<double>({{static_cast<size_t>(rows),static_cast<size_t>(cols)}},grp,dataname,doTranspose,local_chunksizes);
//     }
//     auto dset = grp.getDataSet(dataname);
//     // Rcpp::Rcout<<"Row_out: "<<offsets[0]<<" : "<<offsets[0]+rows-1<<std::endl;
//     // Rcpp::Rcout<<"Col out: "<<offsets[1]<<" : "<<offsets[1]+cols-1<<std::endl;
//     write_m_h5<double>(d,dset,{offsets[0],offsets[1]},{offsets[0]+rows-1,offsets[1]+cols-1});
//     break;
//   }
//   // case STRSXP: {
//   //   Rcpp::Matrix<STRSXP> rmat(data);
//   //   const int rows=rmat.rows();
//   //   const int cols=rmat.cols();
//   //   std::vector<std::string> tv(rows*cols);
//   //
//   //   for(int i=0; i<rows;i++){
//   //     for(int j=0;j<cols;j++){
	
//   //   Eigen::Map<Eigen::MatrixXd> d(&rmat(0,0),rows,cols);
//   //   if(create_ds){
//   //     create_m_h5<std::string>(d,grp,dataname,doTranspose,local_chunksizes);
//   //   }
//   //   auto dset = grp.getDataSet(dataname);
//   //   // Rcpp::Rcout<<"Row_out: "<<offsets[0]<<" : "<<offsets[0]+rows-1<<std::endl;
//   //   // Rcpp::Rcout<<"Col out: "<<offsets[1]<<" : "<<offsets[1]+cols-1<<std::endl;
//   //   write_m_h5<double>(d,dset,{offsets[0],offsets[1]},{offsets[0]+rows-1,offsets[1]+cols-1});
//   //   break;
//   // }
//   default: {
//     warning(
//       "Invalid SEXPTYPE %d.\n",
//       my_t
//     );
//     Rcpp::stop("Unable to write data of this type");
//   }
//   }
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


//[[Rcpp::export]]
Rcpp::List read_l_h5(const std::string h5filepath,
		     const std::string groupname,
		     Rcpp::CharacterVector subcols = Rcpp::CharacterVector::create(),
		     Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(),
		     Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(),
		     Rcpp::IntegerVector filtervec = Rcpp::IntegerVector::create()
		     ){
  using namespace Rcpp;

  HighFive::File file(h5filepath,HighFive::File::ReadOnly);
  auto grp = file.getGroup(groupname);
  std::vector<std::string> df_cols;
  std::vector<size_t> col_sizes;
  std::vector<size_t> elem(filtervec.size());
  std::transform(filtervec.begin(),filtervec.end(),elem.begin(),[](int f) -> size_t{return f-1;});

  if((offset.size()!=0) ^ (chunksize.size()!=0)){
    Rcpp::Rcerr<<"with offset size: "<<offset.size()<<" and chunksize size: "<<chunksize.size()<<std::endl;
    Rcpp::stop("offset and chunksize must both be specified or neither can be specified ");
  }
  const bool read_subset = !elem.empty();
  const bool read_chunk = (offset.size()!=0) && (chunksize.size()!=0);
  const size_t offset_r= read_chunk ? offset[0] :  0;
  const size_t chunksize_r= read_chunk ? chunksize[0] :  0;



  if(read_subset && read_chunk){
    Rcpp::stop("filtervec and offset/chunksize cannot both be specified");
  }
  size_t num_cols = (subcols.size()==0) ? grp.getNumberObjects() : subcols.size();
  if(subcols.size()!=0){
    df_cols=Rcpp::as<std::vector<std::string> >(subcols);
  }else{
    df_cols.resize(num_cols);
    for(int i=0; i<num_cols;i++){
      df_cols[i]=grp.getObjectName(i);
    }
  }
  col_sizes.resize(num_cols);
  Rcpp::List retdf;
  for(int i=0; i<num_cols;i++){
    auto cname = df_cols[i];
    auto dset = grp.getDataSet(cname);
    auto dims = dset.getDataDimensions();
    if(dims.size()!=1){
      Rcpp::Rcerr<<"Can't read non-vector df_col: "<<cname<<std::endl;
      Rcpp::stop("Currently unable to read non-vector columns");
    }
    col_sizes[i]=dims[0];
    if(i>0){
      if(col_sizes[i]!=col_sizes[i-1]){
        Rcpp::Rcerr<<"df_col: "<<cname<<" has"<<col_sizes[i];
        Rcpp::Rcerr<<"df_col: "<<df_cols[i-1]<<" has"<<col_sizes[i-1];
        Rcpp::stop("All columns must have the same length");
      }
    }

    auto my_t = h2r_T(dset.getDataType().getId());
    switch (my_t){
    case INTSXP: {
      if(!read_subset){
        retdf[cname]=read_v_h5<INTSXP>(file,
                                             grp,
                                             cname,
                                             offset_r,chunksize_r);


    }else{
        retdf[cname]=read_elem_v_h5<INTSXP>(file,
                                                  grp,
                                                  cname,
                                                  elem);
      }
      break;
    }
    case REALSXP: {
      if(!read_subset){
      retdf[cname]=read_v_h5<REALSXP>(file,
                                            grp,
                                            cname,
                                            offset_r,chunksize_r);
    }else{
        retdf[cname]=read_elem_v_h5<REALSXP>(file,
                                                   grp,
                                                   cname,
                                                   elem);
      }
      break;

    }
    case STRSXP: {
      if(!read_subset){
      retdf[cname]=read_v_h5<STRSXP>(file,
                                           grp,
                                           cname,
                                           offset_r,chunksize_r);
    }else{
        retdf[cname]=read_elem_v_h5<STRSXP>(file,
                                                  grp,
                                                  cname,
                                                  elem);
      }
      break;

    }
    default: {
      warning(
        "Invalid SEXPTYPE %d.\n",
        my_t
      );
      Rcpp::Rcerr<<cname<<" has type that can't be read"<<std::endl;
      Rcpp::stop("Can't read type");
      return R_NilValue;
    }
    }
  }
  retdf.attr("names")=Rcpp::wrap(df_cols);
  return retdf;
}





//[[Rcpp::export]]
Rcpp::ListOf<Rcpp::IntegerVector> intersect_snpinfo_h5(std::vector<std::string> h5files){

   using namespace HighFive;
  const size_t num_files=h5files.size();

  std::vector<int> pos_map;
  std::vector<int> chr_map;
  std::vector<int> idx_map;
  using namespace ranges;
  size_t num_elem_min=std::numeric_limits<size_t>::max();
  std::map<std::pair<int,int>,std::vector<std::pair<int,int> > > cur_pos_map;
  std::map<int,std::vector<int> > ret_idx_map;
  for(int i=0; i < num_files; i++){
    auto tfile=h5files[i];
    Rcpp::Rcout<<"Reading file: "<<tfile<<std::endl;
    HighFive::File fv(tfile,File::ReadOnly);
    fv.getGroup("SNPinfo").getDataSet("pos").read(pos_map);
    fv.getGroup("SNPinfo").getDataSet("chr").read(chr_map);
    fv.getGroup("SNPinfo").getDataSet("snp_id").read(idx_map);
    const size_t num_elem_i = idx_map.size();
    if(num_elem_i<num_elem_min){
      num_elem_min=num_elem_i;
    }
    for(int j=0; j<num_elem_i;j++){
      cur_pos_map[{chr_map[j],pos_map[j]}].push_back({idx_map[j],i});
    }

    //Rcpp::Rcout<<"size of: "<<idx_map[i].size()<<std::endl;
  }
  for(int i=0; i<num_files;i++){
    ret_idx_map[i].reserve(num_elem_min);
  }

  for(const auto& k_v : cur_pos_map){
    if((k_v.second.size())==num_files){
      for(const auto& e : k_v.second){
        ret_idx_map[e.second].push_back(e.first);
      }
    }
  }

  List resl(num_files);
  for(int i=0;i<num_files;i++){
    resl[i]=wrap(ret_idx_map[i]);
  }
  resl.names()=Rcpp::wrap(h5files);

  return(resl);

}



