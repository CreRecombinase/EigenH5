#ifndef EIGENH5_MATRIX_HPP
#define EIGENH5_MATRIX_HPP


template <typename T> void create_m_h5(const std::vector<size_t> mat_dims,
					 HighFive::Group &grp,
					 const std::string &dataname,
					 const bool doTranspose=false,
					 std::vector<size_t> chunk_dims={}){
    using namespace HighFive;

    //Make initial chunking guess
    if (chunk_dims.empty()) {
      chunk_dims=mat_dims;
      //Rcpp::Rcout<<"chunk_dims: "<<chunk_dims[0]<<" , "<<chunk_dims[1]<<std::endl;
    }
    chunk_dims=Filter::reset_chunks_vec(chunk_dims,mat_dims);
    Filter filter(chunk_dims, FILTER_BLOSC, 0,doTranspose);
    // Create a dataset with double precision floating points


    DataSpace ds = DataSpace(mat_dims,doTranspose);
    DataSet dataset = grp.createDataSet(dataname, ds, AtomicType<T>(), filter.getId(), doTranspose);

}


template <typename T,int RM =Eigen::ColMajor>
void read_m_h5(Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,RM> > &tref,
	       HighFive::DataSet &dset,
	       const CompactSelection<2> &comp_sel){

  const int Dims=2;
  using namespace HighFive;

  auto is_sorted=comp_sel.get_sorted();
  for(int i=0;i<Dims;i++){
    if(!is_sorted[i]){
      Rcpp::Rcerr<<"In dimension: "<<i<<" of array"<<std::endl;
      Rcpp::stop("indices must be sorted for arrays");
    }
  }
  dset.selectEigen(comp_sel.get_offsets(),comp_sel.get_chunksizes(),{}).read(tref);

  if(!is_sorted[0]){
    tref.rowwise().reverse();
  }
  if(!is_sorted[1]){
    tref.colwise().reverse();
  }
}


template <typename T,int RM =Eigen::ColMajor>
void write_m_h5(Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,RM> > &tref,
		HighFive::DataSet &dset,const CompactSelection<2> &comp_sel){

  const int Dims=2;
  using namespace HighFive;
  auto is_sorted=comp_sel.get_sorted();



  if(!is_sorted[0]){
    tref.rowwise().reverse();
  }
  if(!is_sorted[1]){
    tref.colwise().reverse();
  }
    dset.selectEigen(comp_sel.get_output_offsets(),comp_sel.get_chunksizes(),{}).write(tref);
}




template <SEXPTYPE RTYPE,typename T= enable_if_t<std::is_arithmetic<typename r2cpp_t<RTYPE>::type >::value,typename r2cpp_t<RTYPE>::type> >
Rcpp::Matrix<RTYPE> read_elem_m_h5(HighFive::DataSet &dset,const dataset_selection<2> dsel){

    //using T = typename r2cpp_t<RTYPE>::type;

    //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;
    using namespace ranges;
    const int Dims=2;




    const size_t n_rows=dsel.n_elem[0];
    const size_t n_cols=dsel.n_elem[1];

    Rcpp::Matrix<RTYPE> rretmat(n_rows,n_cols);
    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&rretmat(0,0),n_rows,n_cols);
    const long long N = dsel.num_sel;
    std::vector<T> tvec;
    for( long long n=0 ; n<N ; ++n ) {
      CompactSelection<2> temp_sel(dsel.cartesian_index(n));
      tvec.resize(temp_sel.get_total_size());
      //      Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> treadmat(trowsize,tcolsize);
      auto tdim_sizes=temp_sel.get_chunksizes();
      Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> > ttreadmat(tvec.data(),tdim_sizes[0],tdim_sizes[1]);

      auto out_starts=temp_sel.get_output_offsets();
      read_m_h5<T>(dset,ttreadmat,temp_sel);
      mretmat.block(out_starts[0],out_starts[1],tdim_sizes[0],tdim_sizes[1])=ttreadmat;
    }
  return(rretmat);
  }


  template <SEXPTYPE RTYPE,typename T= enable_if_t<std::is_arithmetic<typename r2cpp_t<RTYPE>::type >::value,typename r2cpp_t<RTYPE>::type>,
	    int RM =Eigen::ColMajor>
  void write_elem_m_h5(Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,RM> > &retmat,
		       HighFive::DataSet &dset,
		       const dataset_selection<2> dsel){

    //using T = typename r2cpp_t<RTYPE>::type;

    //Rcpp::Vector<r2cpp_t<RTYPE>::type> retvec;
    using namespace ranges;
    const int Dims=2;
    const size_t n_rows=dsel.n_elem[0];
    const size_t n_cols=dsel.n_elem[1];

    //    Rcpp::Matrix<RTYPE> rretmat(n_rows,n_cols);
    //    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > mretmat(&rretmat(0,0),n_rows,n_cols);
    const long long N = dsel.num_sel;

    for( long long n=0 ; n<N ; ++n ) {
      CompactSelection<2> temp_sel(dsel.cartesian_index(n));
      //      tvec.resize(temp_sel.get_total_size);
      //
      auto tdim_sizes=temp_sel.get_chunksizes();
      auto out_starts=temp_sel.get_output_offsets();
      auto in_starts=temp_sel.get_offsets();
      Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> treadmat(tdim_sizes[0],tdim_sizes[1]);
      Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> > ttreadmat(treadmat.data(),tdim_sizes[0],tdim_sizes[1]);
      ttreadmat=retmat.block(in_starts[0],in_starts[1],tdim_sizes[0],tdim_sizes[1])=ttreadmat;
      write_m_h5<T>(dset,ttreadmat,temp_sel);
    }

  }


#endif
