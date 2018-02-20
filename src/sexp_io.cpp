
#include <EigenH5.h>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(RcppProgress)]]
//#include <Eigen/CXX11/Tensor>
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
                                      HighFive::File &file,
                                      HighFive::Group & group,
                                      const std::string &dataname){

  using namespace HighFive;


  //

  std::vector<size_t> vec_dims{data.size()};
  // int r = 0;

  // Create a new file using the default property lists.
  Filter filter({1000}, vec_dims, FILTER_BLOSC, 1);
  // Create a dataset with double precision floating points


  DataSpace ds = DataSpace(vec_dims);

  DataSet dataset = group.createDataSet(dataname, ds, AtomicType<T>(), filter.getId());
  dataset.write(data);
}


template <typename T> void create_m_h5(typename Eigen::Map<typename Eigen::Matrix<enable_if_t<std::is_arithmetic<T>::value,T>,Eigen::Dynamic,Eigen::Dynamic> > &data,
                                      HighFive::Group &grp,
                                      const std::string &dataname,
                                      const bool doTranspose=false,
                                      std::vector<size_t> chunk_dims={}){
  using namespace HighFive;
  std::vector<size_t> mat_dims={static_cast<size_t>(data.rows()),static_cast<size_t>(data.cols())};
  if (chunk_dims.empty()) {
    const size_t MAX_CHUNK = 1024*1024;
    const size_t chunk_rows = static_cast<size_t>(std::min(static_cast<double>(data.rows()),std::ceil(static_cast<double>(MAX_CHUNK)/static_cast<double>(data.cols()))));
    chunk_dims = {chunk_rows, static_cast<size_t>(data.cols())};
    //Rcpp::Rcout<<"chunk_dims: "<<chunk_dims[0]<<" , "<<chunk_dims[1]<<std::endl;

  }
  // Create a new file using the default property lists.
  // if(doTranspose){
  //   Rcpp::Rcout<<"transpose!"<<std::endl;
  // }
  Filter filter(chunk_dims, FILTER_BLOSC, 0,doTranspose);
  // Create a dataset with double precision floating points


  DataSpace ds = DataSpace(mat_dims,doTranspose);
  DataSet dataset = grp.createDataSet(dataname, ds, AtomicType<T>(), filter.getId(), doTranspose);

}






void fix_set_ss(std::vector<int> &starts,
                std::vector<int> &stops,
                std::vector<bool>sorted,
                const std::vector<size_t> dims,
                std::vector<size_t> &chunksize){
  const size_t n_dimsf=dims.size();
  for(int i=0;i<n_dimsf;i++){
    if(stops[i]<0){
      stops[i]=dims[i]-stops[0];
    }
    if(starts[i]<0){
      starts[i]=dims[i]-starts[0];
    }
    if(starts[i]>stops[i]){
      std::swap(starts[i],stops[i]);
      sorted[i]=false;
    }else{
      sorted[i]=true;
    }
    chunksize[i]=static_cast<size_t>(stops[i]-starts[i]+1);
  }
}

template <SEXPTYPE RTYPE,int RM =Eigen::ColMajor,typename T= enable_if_t<std::is_arithmetic<typename r2cpp_t<RTYPE>::type >::value,typename r2cpp_t<RTYPE>::type> >
void read_m_h5(
    HighFive::DataSet &dset,
    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,RM> > &retmat,
    std::vector<int> starts={{0,0}},
    std::vector<int> stops={{0,0}}){

  using namespace HighFive;
  std::vector<bool> sorted(starts.size());

  //using T = typename ;
  // Rcpp::Rcout<<"Reading from :"<<starts[0]<<","<<starts[1]<<std::endl;
  // Rcpp::Rcout<<"to :"<<stops[0]<<","<<stops[1]<<std::endl;

  //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;


  auto d_dims = dset.getDataDimensions();
  std::vector<size_t> chunksize(d_dims);
  std::vector<size_t> offsets(starts.begin(),starts.end());
  fix_set_ss(starts,stops,sorted, d_dims,chunksize);
  //using T = typename ;

  dset.selectEigen(offsets,chunksize,{}).read(retmat);
  if(!sorted[1]){
    retmat.rowwise().reverse();
  }
  if(!sorted[0]){
    retmat.colwise().reverse();
  }
}




template <SEXPTYPE RTYPE,typename T= enable_if_t<std::is_arithmetic<typename r2cpp_t<RTYPE>::type >::value,typename r2cpp_t<RTYPE>::type> >
void read_a_h5(
    HighFive::DataSet &dset,
    T *temp_p,
    std::vector<int> starts,
    std::vector<int> stops){

  using namespace HighFive;
  std::vector<bool> sorted(starts.size());

  //using T = typename ;
  // Rcpp::Rcout<<"Reading from :"<<starts[0]<<","<<starts[1]<<std::endl;
  // Rcpp::Rcout<<"to :"<<stops[0]<<","<<stops[1]<<std::endl;

  //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;


  auto d_dims = dset.getDataDimensions();
  std::vector<size_t> chunksize(d_dims);
  fix_set_ss(starts,stops,sorted, d_dims,chunksize);
  // T* temp_p = data_ref;
  //using T = typename ;
  std::vector<size_t> offsets(starts.begin(),starts.end());
  dset.selectEigen(offsets,chunksize,{}).read(temp_p);
  // if(!sorted_cols){
  //   retmat.rowwise().reverse();
  // }
  // if(!sorted_rows){
  //   retmat.colwise().reverse();
  // }
}



template <typename T> void write_m_h5(typename Eigen::Map<typename Eigen::Matrix<enable_if_t<std::is_arithmetic<T>::value,T>,Eigen::Dynamic,Eigen::Dynamic> > &data,
                                      HighFive::DataSet &dset,
                                      std::vector<int> starts={{0,0}},
                                      std::vector<int> stops={{0,0}}){

  using namespace HighFive;
  bool sorted_cols,sorted_rows;

  //using T = typename ;

  //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;
  
  std::vector<bool> sorted(2);
  std::vector<size_t> chunksize(starts.size());
  auto d_dims = dset.getDataDimensions();
  fix_set_ss(starts,stops,sorted,d_dims,chunksize);
  std::vector<size_t> offsets(starts.begin(),starts.end());
  if(!sorted[1]){
    data.rowwise().reverse();
  }
  if(!sorted[0]){
    data.colwise().reverse();
  }
  dset.selectEigen(offsets,{chunksize},{}).write(data);
}




template <SEXPTYPE RTYPE> Vector<RTYPE> read_v_h5(
    HighFive::File &file,
    HighFive::Group & grp,
    const std::string &dataname,
    const size_t offset=0,
    const size_t chunksize=0){

  using T = typename r2cpp_t<RTYPE>::type;
  std::vector<T> retvec;
  //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;

  using namespace HighFive;

  if(chunksize==0){
    grp.getDataSet(dataname).read(retvec);
  }else{
    std::vector<size_t> off_v={offset};
    std::vector<size_t> ret_v={chunksize};
    grp.getDataSet(dataname).select(off_v,ret_v,{}).read(retvec);
  }
  return(Rcpp::wrap(retvec));
}


template <SEXPTYPE RTYPE> Vector<RTYPE> read_elem_v_h5(
    HighFive::File &file,
    HighFive::Group & grp,
    const std::string &dataname,
    std::vector<size_t> elem){

  using T = typename r2cpp_t<RTYPE>::type;
  std::vector<T> retvec;
  //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;

  using namespace HighFive;

  grp.getDataSet(dataname).select(HighFive::ElementSet(elem)).read(retvec);
  return(Rcpp::wrap(retvec));
}




template <SEXPTYPE RTYPE,typename T= enable_if_t<std::is_arithmetic<typename r2cpp_t<RTYPE>::type >::value,typename r2cpp_t<RTYPE>::type>,
          typename RR,typename CR>
Matrix<RTYPE> read_elem_m_h5(
    HighFive::DataSet &dset,
    RR elem_rows,
    CR elem_cols){

  //using T = typename r2cpp_t<RTYPE>::type;

  //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;
 using namespace ranges;



  const int n_rows = elem_rows.back().second.back()+1;

  const int n_cols = elem_cols.back().second.back()+1;
  // const size_t n_rows = distance(elem_rows);
  // const size_t n_cols = distance(elem_cols);
  Rcpp::Matrix<RTYPE> rretmat(n_rows,n_cols);
  // using namespace ranges;
  //
  // auto chunk_view= view::zip(view::ints(0),elem) | view::group_by([](auto a, auto b) {
  //   return std::get<1>(a)+1 == std::get<1>(b);});

  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&rretmat(0,0),n_rows,n_cols);
  for(auto row_it = elem_rows.begin(); row_it!=elem_rows.end();row_it++){
    for(auto col_it = elem_cols.begin(); col_it!=elem_cols.end(); col_it++){
      auto row_in_arr = row_it->first;
      auto row_out_arr = row_it->second;


      auto col_in_arr = col_it->first;
      auto col_out_arr = col_it->second;

      // Rcpp::Rcout<<"Row_in_arr:"<<row_in_arr.front()<<","<<row_in_arr.back()<<std::endl;
      // Rcpp::Rcout<<"Row_out_arr:"<<row_out_arr.front()<<","<<row_out_arr.back()<<std::endl;
      //
      // Rcpp::Rcout<<"Col_in_arr:"<<col_in_arr.front()<<","<<col_in_arr.back()<<std::endl;
      // Rcpp::Rcout<<"Col_out_arr:"<<col_out_arr.front()<<","<<col_out_arr.back()<<std::endl;

      const int  tcolsize = col_out_arr.back()-col_out_arr.front()+1;
      const int  trowsize = row_out_arr.back()-row_out_arr.front()+1;

      Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> treadmat(trowsize,tcolsize);
      Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> > ttreadmat(treadmat.data(),trowsize,tcolsize);
      read_m_h5<RTYPE>(dset,ttreadmat,{row_in_arr.front(),col_in_arr.front()},{row_in_arr.back(),col_in_arr.back()});
      // Rcpp::Rcout<<"Assigning treadmat of size"<<treadmat.rows()<<" x "<<treadmat.cols()<<std::endl;
      // Rcpp::Rcout<<"To a block of size"<<trowsize<<" x "<<tcolsize<<std::endl;
      // Rcpp::Rcout<<"Starting at"<<row_out_arr[0]<<" , "<<col_out_arr[0]<<std::endl;
      // Rcpp::Rcout<<"Total size is "<<n_rows<<" , "<<n_cols<<std::endl;

      mretmat.block(row_out_arr[0],col_out_arr[0],trowsize,tcolsize)=treadmat;
    }
  }
  return(rretmat);
}




// template <SEXPTYPE RTYPE,typename T= enable_if_t<std::is_arithmetic<typename r2cpp_t<RTYPE>::type >::value,typename r2cpp_t<RTYPE>::type>,
//           typename RRR,>
// Vector<RTYPE> read_elem_a_h5(
//     HighFive::DataSet &dset,
//     RRR elem_r){

//   using namespace ranges;


//   const int n_dims= dist(elem_r);
//   std::vector<int> dim_vec(n_dims) = view::transform(elem_r,[](auto r){
//       return(r.back().second.back()+1);
//     });
//   int total_dim = accumulate(dim_vec,meta::multiplies);


//   Rcpp::Vector<RTYPE> rretmat(total_dim);


//   //  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&rretmat(0,0),n_rows,n_cols);
//   for(int i=0;i<n_dims;i++){
//     const int c_tot=dim_vec[i];
//     for(int j=0;
//   for(auto row_it = elem_rows.begin(); row_it!=elem_rows.end();row_it++){
//     for(auto col_it = elem_cols.begin(); col_it!=elem_cols.end(); col_it++){
//       auto row_in_arr = row_it->first;
//       auto row_out_arr = row_it->second;


//       auto col_in_arr = col_it->first;
//       auto col_out_arr = col_it->second;

//       // Rcpp::Rcout<<"Row_in_arr:"<<row_in_arr.front()<<","<<row_in_arr.back()<<std::endl;
//       // Rcpp::Rcout<<"Row_out_arr:"<<row_out_arr.front()<<","<<row_out_arr.back()<<std::endl;
//       //
//       // Rcpp::Rcout<<"Col_in_arr:"<<col_in_arr.front()<<","<<col_in_arr.back()<<std::endl;
//       // Rcpp::Rcout<<"Col_out_arr:"<<col_out_arr.front()<<","<<col_out_arr.back()<<std::endl;

//       const int  tcolsize = col_out_arr.back()-col_out_arr.front()+1;
//       const int  trowsize = row_out_arr.back()-row_out_arr.front()+1;

//       Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> treadmat(trowsize,tcolsize);
//       Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> > ttreadmat(treadmat.data(),trowsize,tcolsize);
//       read_a_h5<RTYPE>(dset,ttreadmat,{row_in_arr.front(),col_in_arr.front()},{row_in_arr.back(),col_in_arr.back()});
//       // Rcpp::Rcout<<"Assigning treadmat of size"<<treadmat.rows()<<" x "<<treadmat.cols()<<std::endl;
//       // Rcpp::Rcout<<"To a block of size"<<trowsize<<" x "<<tcolsize<<std::endl;
//       // Rcpp::Rcout<<"Starting at"<<row_out_arr[0]<<" , "<<col_out_arr[0]<<std::endl;
//       // Rcpp::Rcout<<"Total size is "<<n_rows<<" , "<<n_cols<<std::endl;

//       mretmat.block(row_out_arr[0],col_out_arr[0],trowsize,tcolsize)=treadmat;
//     }
//   }
//   return(rretmat);
// }

// }
}


//[[Rcpp::export]]
bool data_exists(const std::string &filename,
                     const std::string &groupname,
                     const std::string &dataname){
  using namespace HighFive;
    File file(filename,File::ReadOnly);
    if(!file.exist(groupname)){
      return(false);
    }
    return(file.getGroup(groupname).exist(dataname));
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
SEXP read_vector_h5(const std::string &filename,
                 const std::string &groupname,
                 const std::string &dataname,
                 Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(),
                 Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(),
                 Rcpp::IntegerVector filtervec = Rcpp::IntegerVector::create()){
  using namespace Rcpp;

  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.getGroup(groupname);

  std::vector<size_t> elem(filtervec.size());
  std::transform(filtervec.begin(),filtervec.end(),elem.begin(),[](int f) -> size_t{return f-1;});

  const bool read_subset = !elem.empty();
  const bool read_chunk = (offset.size()!=0) && (chunksize.size()!=0);
  const size_t offset_r= read_chunk ? offset[0] :  0;
  const size_t chunksize_r= read_chunk ? chunksize[0] :  0;


  if((offset.size()!=0) ^ (chunksize.size()!=0)){
    Rcpp::stop("offset and chunksize must both be specified or neither can be specified ");
  }
  if(read_subset && read_chunk){
    Rcpp::stop("filtervec and offset/chunksize cannot both be specified");
  }


  auto my_t = check_dtype(filename,groupname,dataname);
  switch (my_t){
  case INTSXP: {
    if(!read_subset){
    return impl::read_v_h5<INTSXP>(file,
                                         grp,
                                         dataname,
                                         offset_r,chunksize_r);


  }else{
    return impl::read_elem_v_h5<INTSXP>(file,
                                              grp,
                                              dataname,
                                              elem);
  }
  break;
  }
  case REALSXP: {
    if(!read_subset){
    return impl::read_v_h5<REALSXP>(file,
                                          grp,
                                          dataname,
                                          offset_r,chunksize_r);
  }else{
    return impl::read_elem_v_h5<REALSXP>(file,
                                               grp,
                                               dataname,
                                               elem);
  }
  break;

  }
  case STRSXP: {
    if(!read_subset){
    return impl::read_v_h5<STRSXP>(file,
                                         grp,
                                         dataname,
                                         offset_r,chunksize_r);
  }else{
    return impl::read_elem_v_h5<STRSXP>(file,
                                              grp,
                                              dataname,
                                              elem);
  }
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

//[[Rcpp::export]]
Rcpp::IntegerVector get_dims_h5(const std::string &filename,
                                const std::string &groupname,
                                const std::string &dataname){
  return(Rcpp::wrap(HighFive::File(filename,HighFive::File::ReadOnly).getGroup(groupname).getDataSet(dataname).getDataDimensions()));

}



template<typename It>
std::vector<std::pair<std::array<int,2>,std::array<int,2> > > find_cont(It itb, It ite){
  using namespace Rcpp;
  using namespace ranges;
  using iarray = std::array<int,2>;
  using piarray = std::pair<iarray,iarray>;

  std::transform(itb,ite,itb,[](int f){return f-1;});

  std::vector<std::pair<std::array<int,2>,std::array<int,2> > > sub_ranges;
  const int n_elem = ite-itb;
  sub_ranges.reserve(n_elem/2);
  auto itbb=itb;
  auto it = itb;
  int tot_dist=0;
  while(it!=ite){
    it = std::adjacent_find(itb,ite,[](int i,int j){
      // Rcpp::Rcout<<"i is : "<<i<<std::endl;
      // Rcpp::Rcout<<"j is : "<<j<<std::endl;
      return((j-i)!=1);
    });
    int iti = it==ite ? *(it-1) : *(it);
    int itb_pos = itb-itbb;
    int reg_size = it==ite ? it-itb : (it-itb+1);
    sub_ranges.push_back(piarray{{{*itb,iti}},{{tot_dist,tot_dist+reg_size-1}}});
    if(it!=ite){
      it++;
    }
    tot_dist=tot_dist+reg_size;
    itb=it;
  }
  return(sub_ranges);
}



//[[Rcpp::export]]
SEXP read_matrix_h5(const std::string &filename,
                    const std::string &groupname,
                    const std::string &dataname,
                    const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(),
                    const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create(),
                    const Rcpp::IntegerVector subset_rows = Rcpp::IntegerVector::create(),
                    const Rcpp::IntegerVector subset_cols = Rcpp::IntegerVector::create()){
  using namespace Rcpp;
  using iarray = std::array<int,2>;
  using piarray = std::pair<iarray,iarray>;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.getGroup(groupname);

  std::vector<int> local_offsets=Rcpp::as<std::vector<int> >(offsets);
  std::vector<int> local_chunksizes=Rcpp::as<std::vector<int> >(chunksizes);
  std::vector<int> local_subset_rows=Rcpp::as<std::vector<int> >(subset_rows);
  std::vector<int> local_subset_cols=Rcpp::as<std::vector<int> >(subset_cols);

  const bool read_subset_rows = (local_subset_rows.size()!=0);
  const bool read_subset_cols = (local_subset_cols.size()!=0);
  std::vector<piarray> row_chunks= find_cont(local_subset_rows.begin(),local_subset_rows.end());
  std::vector<piarray> col_chunks= find_cont(local_subset_cols.begin(),local_subset_cols.end());

  bool read_chunk = (local_offsets.size()!=0) && (local_chunksizes.size()!=0);
  if(read_subset_rows && read_chunk){
    Rcpp::stop("subset_rows and chunking can't both be specified");
  }
  if(read_subset_cols && read_chunk){
    Rcpp::stop("subset_rows and chunking can't both be specified");
  }
  if(read_chunk && ((local_offsets.size()!=2) && (local_chunksizes.size()!=2))){
    Rcpp::stop("offset and chunksize must both be empty or must both be length two vectors");
  }
  if(!read_chunk){
  }
  auto dset = file.getGroup(groupname).getDataSet(dataname);
  auto dims = dset.getDataDimensions();

  if((local_offsets.size()!=0) ^ (local_chunksizes.size()!=0)){
    Rcpp::stop("offset and chunksize must both be specified or neither can be specified ");
  }

  if(!read_chunk){
    local_offsets = std::vector<int>{0,0};
    local_chunksizes = std::vector<int>{static_cast<int>(dims[0]),static_cast<int>(dims[1])};
  }
  std::array<int,2> start_r = {local_offsets[0],local_offsets[1]};
  std::array<int,2> stop_r  = {local_offsets[0]+local_chunksizes[0]-1,local_offsets[1]+local_chunksizes[1]-1};
  if(row_chunks.empty()){
    row_chunks.push_back(piarray{{{local_offsets[0],local_offsets[0]+local_chunksizes[0]-1}},{{0,local_chunksizes[0]-1}}});
  }
  if(col_chunks.empty()){
    col_chunks.push_back(piarray{{{local_offsets[1],local_offsets[1]+local_chunksizes[1]-1}},{{0,local_chunksizes[1]-1}}});
  }





  const bool read_subset = read_subset_rows || read_subset_cols;

  auto my_t = check_dtype(filename,groupname,dataname);


  switch (my_t){
  case INTSXP: {
    return(impl::read_elem_m_h5<INTSXP>(dset,row_chunks,col_chunks));
    break;
  }
  case REALSXP: {
    return(impl::read_elem_m_h5<REALSXP>(dset,row_chunks,col_chunks));
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



//[[Rcpp::export]]
SEXP read_array_h5(const std::string &filename,
                   const std::string &groupname,
                   const std::string &dataname,
                   const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(),
                   const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()){

  using namespace Rcpp;
  bool read_chunk = (offsets.size()!=0) && (chunksizes.size()!=0);
  using iarray = std::array<int,2>;
  using piarray = std::pair<iarray,iarray>;
  HighFive::File file(filename,HighFive::File::ReadOnly);
  auto grp = file.getGroup(groupname);

  auto dset = file.getGroup(groupname).getDataSet(dataname);
  auto dims = dset.getDataDimensions();
  const size_t num_dims=dims.size();

  std::vector<int> local_offsets(num_dims);

  std::vector<int> local_chunksizes(num_dims);
  std::copy_n(offsets.begin(),offsets.size(),local_offsets.begin());
  std::copy_n(chunksizes.begin(),chunksizes.size(),local_chunksizes.begin());

  int tot_size=1;
  std::vector<int> stop_r(num_dims);
  for(int i=0;i<num_dims;i++){
    if(i<=offsets.size()){
      local_offsets[i]=offsets(i);
    }
    if(i<=chunksizes.size()){
      local_chunksizes[i]=chunksizes(i);
    }else{
      local_chunksizes[i]=dims[i]-local_offsets[i];
    }
    tot_size=tot_size*local_chunksizes[i];
    stop_r[i]=local_offsets[i]+local_chunksizes[i]-1;
  }




  std::vector<int> start_r = local_offsets;


  auto my_t = check_dtype(filename,groupname,dataname);

  std::reverse(local_chunksizes.begin(),local_chunksizes.end());
  switch (my_t){
  case INTSXP: {
    Rcpp::IntegerVector retvec(tot_size);
    retvec.attr("dim") = Rcpp::wrap(local_chunksizes);
    impl::read_a_h5<INTSXP>(dset,&retvec[0],start_r,stop_r);
    return(retvec);
    break;
  }
  case REALSXP: {
    Rcpp::NumericVector retvec(tot_size);
    retvec.attr("dim") = Rcpp::wrap(local_chunksizes);
    impl::read_a_h5<REALSXP>(dset,&retvec[0],start_r,stop_r);
    return(retvec);
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

//
// void copy_matrix_h5(const std::string &infilename,
//                     const std::string &outilename,
//                     const std::string &groupname,
//                     const std::string &dataname,
//                     const bool doTranspose=false,
//                     std::vector<size_t> chunk_dims={},
//                     Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(),
//                     Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(),
//                     Rcpp::IntegerVector subset_rows = Rcpp::IntegerVector::create(),
//                     Rcpp::IntegerVector subset_cols = Rcpp::IntegerVector::create()){
//   using namespace Rcpp;
//
//
//   using namespace HighFive;
//   File infile(infilename,File::ReadOnly);
//   File outfile(infilename,File::ReadWrite|File::Create);
//
//   auto grp = infile.getGroup(groupname);
//   auto ogrp = outfile.getGroup(groupname);
//
//
//
//
//
//   const bool read_subset_rows = (subset_rows.size()!=0);
//   const bool read_subset_cols = (subset_cols.size()!=0);
//
//   std::vector<size_t> elem(read_subset_rows ? subset_rows.size() : subset_cols.size());
//   auto fv_begin  = read_subset_rows ? subset_rows.begin() : subset_cols.begin();
//   auto fv_end  = read_subset_rows ? subset_rows.end() : subset_cols.end();
//
//   std::transform(fv_begin,fv_end,elem.begin(),[](auto f) -> size_t{return f-1;});
//
//   if(read_subset_rows && read_subset_cols){
//     Rcpp::stop("can only subset rows or cols");
//   }
//
//   const bool read_chunk = (offset.size()!=0) && (chunksize.size()!=0);
//   if(read_subset_rows && read_chunk){
//     Rcpp::stop("subset_rows and chunking can't both be specified");
//
//   }
//   if(read_subset_cols && read_chunk){
//     Rcpp::stop("subset_rows and chunking can't both be specified");
//
//   }
//
//
//   if(read_chunk && ((offset.size()!=2) && (chunksize.size()!=2))){
//     Rcpp::stop("offset and chunksize must both be empty or must both be length two vectors");
//   }
//   std::array<size_t,2> offset_r ={read_chunk ? static_cast<size_t>(offset[0]) :  0,read_chunk ? static_cast<size_t>(offset[1]) :  0} ;
//   std::array<size_t,2> chunksize_r= {read_chunk ? static_cast<size_t>(chunksize[0]) :  0,read_chunk ? static_cast<size_t>(chunksize[1]) :  0} ;
//
//
//   if((offset.size()!=0) ^ (chunksize.size()!=0)){
//     Rcpp::stop("offset and chunksize must both be specified or neither can be specified ");
//   }
//   const bool read_subset = read_subset_rows || read_subset_cols;
//
//
//   auto my_t = check_dtype(infilename,groupname,dataname);
//   switch (my_t){
//   case INTSXP: {
//
//   break;
//   }
//   case REALSXP: {
//
//   break;
//     }
//
//
//   }
//   break;
//
//   }
//     // case STRSXP: {
//     //   if(!read_subset){
//     //   return impl::read_m_h5<STRSXP>(file,
//     //                                  grp,
//     //                                  dataname,
//     //                                  offset_r,chunksize_r);
//     // }else{
//     //   if(read_subset_rows){
//     //     return impl::read_elem_m_h5<STRSXP,true>(file,
//     //                                              grp,
//     //                                              dataname,
//     //                                              elem);
//     //
//     //   }else{
//     //     return impl::read_elem_m_h5<STRSXP,false>(file,
//     //                                               grp,
//     //                                               dataname,
//     //                                               elem);
//     //
//     //   }
//     //
//     //
//     // }
//     // break;
//   default: {
//     warning(
//       "Invalid SEXPTYPE %d.\n",
//       my_t
//     );
//     Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;
//     Rcpp::stop("Can't read type");
//   }
//   }
// }




//[[Rcpp::export]]
void write_vector_h5(const std::string &filename,
                     const std::string &groupname,
                     const std::string &dataname,
                     SEXP data){
  using namespace Rcpp;
  HighFive::File file(filename, HighFive::File::ReadWrite | HighFive::File::Create);
  HighFive::Group group = file.createOrGetGroups(groupname);
  auto my_t = TYPEOF(data);
  switch (my_t){
  case INTSXP: {
    auto d=Rcpp::as<std::vector<int> >(data);
    impl::write_v_h5<int>(d,file,group,dataname);
    break;
  }
  case REALSXP: {
    auto d=Rcpp::as<std::vector<double> >(data);
    impl::write_v_h5<double>(d,file,group,dataname);
    break;
  }
  case STRSXP: {
    auto d=Rcpp::as<std::vector<std::string> >(data);
    impl::write_v_h5<std::string>(d,file,group,dataname);
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

//[[Rcpp::export]]
void create_matrix_h5(const std::string &filename,
                     const std::string &groupname,
                     const std::string &dataname,
                     SEXP data,
                     const bool doTranspose=false,
                     const Rcpp::IntegerVector dims=Rcpp::IntegerVector::create(),
                     const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create() ){
  using namespace Rcpp;
  std::vector<size_t> local_chunksizes;
  std::vector<size_t> local_dims;
  std::copy(dims.begin(),dims.end(),std::back_inserter(local_dims));
  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));

  HighFive::File file(filename,HighFive::File::ReadWrite|HighFive::File::Create);
  auto grp = file.createOrGetGroups(groupname);


  auto my_t = TYPEOF(data);
  switch (my_t){
  case INTSXP: {
    Rcpp::Matrix<INTSXP> rmat;
    const size_t rows=dims[0];
    const size_t cols=dims[1];
    Eigen::Map<Eigen::MatrixXi> d(nullptr,rows,cols);
    impl::create_m_h5<int>(d,grp,dataname,doTranspose,local_chunksizes);
    break;
  }
  case REALSXP: {
    Rcpp::Matrix<REALSXP> rmat;
    const size_t rows=dims[0];
    const size_t cols=dims[1];
    Eigen::Map<Eigen::MatrixXd> d(nullptr,rows,cols);
    impl::create_m_h5<double>(d,grp,dataname,doTranspose,local_chunksizes);
    break;
  }
  case STRSXP: {
    Rcpp::stop("Writing string matrices not yet implemented");
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
  auto my_t = TYPEOF(data);
  switch (my_t){
  case INTSXP: {
    Rcpp::Matrix<INTSXP> rmat(data);
    const int rows=rmat.rows();
    const int cols=rmat.cols();
    Eigen::Map<Eigen::MatrixXi> d(&rmat(0,0),rows,cols);
    if(create_ds){
      impl::create_m_h5<int>(d,grp,dataname,doTranspose,local_chunksizes);
    }
    auto dset = grp.getDataSet(dataname);

    impl::write_m_h5<int>(d,dset,{offsets[0],offsets[1]},{offsets[0]+rows-1,offsets[1]+cols-1});
    // Rcpp::Rcout<<"Row_out: "<<offsets[0]<<" : "<<offsets[0]+rows-1<<std::endl;
    // Rcpp::Rcout<<"Col out: "<<offsets[1]<<" : "<<offsets[1]+cols-1<<std::endl;
    break;
  }
  case REALSXP: {
    Rcpp::Matrix<REALSXP> rmat(data);
    const int rows=rmat.rows();
    const int cols=rmat.cols();
    Eigen::Map<Eigen::MatrixXd> d(&rmat(0,0),rows,cols);
    if(create_ds){
      impl::create_m_h5<double>(d,grp,dataname,doTranspose,local_chunksizes);
    }
    auto dset = grp.getDataSet(dataname);
    // Rcpp::Rcout<<"Row_out: "<<offsets[0]<<" : "<<offsets[0]+rows-1<<std::endl;
    // Rcpp::Rcout<<"Col out: "<<offsets[1]<<" : "<<offsets[1]+cols-1<<std::endl;
    impl::write_m_h5<double>(d,dset,{offsets[0],offsets[1]},{offsets[0]+rows-1,offsets[1]+cols-1});
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

//[[Rcpp::export]]
bool write_df_h5(Rcpp::DataFrame &df,const std::string groupname,const std::string outfile,Rcpp::IntegerVector deflate_level=Rcpp::IntegerVector::create(4)){
  HighFive::File file(outfile, HighFive::File::ReadWrite | HighFive::File::Create);
  HighFive::Group group = file.createOrGetGroups(groupname);

  const size_t df_cols=df.ncol();
  std::vector<std::string> df_colnames = Rcpp::as<std::vector<std::string> >(df.names());

  for(int i=0; i<df_cols;i++){
    auto t_colname= df_colnames[i];
    auto t_col = df[t_colname];
    auto my_t = TYPEOF(t_col);
    switch (my_t){
    case INTSXP: {
      auto d=Rcpp::as<std::vector<int> >(t_col);
      impl::write_v_h5<int>(d,file,group,t_colname);
      break;
    }
    case REALSXP: {
      auto d=Rcpp::as<std::vector<double> >(t_col);
      impl::write_v_h5<double>(d,file,group,t_colname);
      break;
    }
    case STRSXP: {
      auto d=Rcpp::as<std::vector<std::string> >(t_col);
      impl::write_v_h5<std::string>(d,file,group,t_colname);
      break;
    }
    default: {
      warning(
        "Invalid SEXPTYPE %d.\n",
        my_t
      );
      return(false);
    }
    }
  }
  return(true);
}


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
        retdf[cname]=impl::read_v_h5<INTSXP>(file,
                                             grp,
                                             cname,
                                             offset_r,chunksize_r);


    }else{
        retdf[cname]=impl::read_elem_v_h5<INTSXP>(file,
                                                  grp,
                                                  cname,
                                                  elem);
      }
      break;
    }
    case REALSXP: {
      if(!read_subset){
      retdf[cname]=impl::read_v_h5<REALSXP>(file,
                                            grp,
                                            cname,
                                            offset_r,chunksize_r);
    }else{
        retdf[cname]=impl::read_elem_v_h5<REALSXP>(file,
                                                   grp,
                                                   cname,
                                                   elem);
      }
      break;

    }
    case STRSXP: {
      if(!read_subset){
      retdf[cname]=impl::read_v_h5<STRSXP>(file,
                                           grp,
                                           cname,
                                           offset_r,chunksize_r);
    }else{
        retdf[cname]=impl::read_elem_v_h5<STRSXP>(file,
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



