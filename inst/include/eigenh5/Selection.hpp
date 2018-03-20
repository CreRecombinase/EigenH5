#ifndef EIGENH5_SELECTION_HPP
#define EIGENH5_SELECTION_HPP




template<typename T,size_t Dims> void block_assign_read(boost::multi_array_ref<T,Dims> &retref,
						     boost::multi_array_ref<T,Dims> &tarr,
						     std::array<boost::multi_array_types::index_range,Dims> ranges){
  if constexpr(Dims==1){
      retref[ boost::indices[ ranges[0] ] ] = tarr;
    }else{
    if constexpr( Dims==2){
	retref[ boost::indices[ ranges[0] ][ ranges[1] ] ] = tarr;
      }else{
      if constexpr( Dims==3){
	  retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ] ] = tarr;
	}else{
	if constexpr( Dims==4){
	    retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ][ ranges[3] ] ] = tarr;
	  }else{
	  static_assert(Dims<=4, "Arrays of dimension > 4 not supported");
	}
      }
    }
  }
}

template<typename T,size_t Dims> void block_assign_write(boost::multi_array_ref<T,Dims> &retref,
							 boost::multi_array_ref<T,Dims> &tarr,
							 std::array<boost::multi_array_types::index_range,Dims> ranges){
  if constexpr(Dims==1){
      tarr =retref[ boost::indices[ ranges[0] ] ] = ;
    }else{
    if constexpr( Dims==2){
        tarr = retref[ boost::indices[ ranges[0] ][ ranges[1] ] ] ;
      }else{
      if constexpr( Dims==3){
	  tarr =  retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ] ];
	}else{
	if constexpr( Dims==4){
	    tarr = retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ][ ranges[3] ] ] ;
	  }else{
	  static_assert(Dims<=4, "Arrays of dimension > 4 not supported");
	}
      }
    }
  }
}






struct dim_sel{
public:
  int in_start;
  int in_stop;
  int out_start;
  int out_stop;
  bool sorted;
  int chunksize;

  dim_sel(const int in_start_,int in_stop_,const int out_start_,const int out_stop_,const int dimsize){
    if(in_stop_<0){
      in_stop=dimsize-in_stop;
    }
    if(in_start_<0){
      in_start=dimsize-in_start;
    }
    if(in_start>in_stop){
      std::swap(in_start,in_stop);
      sorted=false;
    }else{
      sorted=true;
    }
    chunksize =	static_cast<size_t>(in_stop-in_start+1);
    if(chunksize!=(out_stop-out_start+1)){
      Rcpp::stop("chunksize mismatch");
    }
  }
  dim_sel(const int offset,const int dimsize){
    in_stop=dimsize-1;
    if(offset<0){
      in_start=dimsize-offset;
    }else{
      in_start=offset;
    }
    chunksize=in_stop-in_start+1;
    out_start=0;
    out_stop=chunksize-1;
    sorted=true;
  }

  dim_sel(const int offset, const int chunksize_,const int dimsize):chunksize(chunksize_){
  if(offset<0){
    in_start=dimsize-offset;
  }
  out_start=0;
  out_stop=chunksize-1;
  if(in_start+chunksize>=dimsize){
    Rcpp::Rcerr<<"offset: "<<offset<<" chunksize: "<<chunksize<<" dimsize: "<<dimsize<<std::endl;
    Rcpp::stop("Illegal selection, offset+chunksize > dimsize");
  }
  sorted=true;
}
};




template<size_t Dims>
class DatasetSelection{
public:
  const std::vector<std::vector<dim_sel> >  &dim_sels;
  const int num_sel;
  std::array<int,Dims> n_elem;
  std::array<int,Dims> dataset_dimensions;
  DatasetSelection(std::vector<std::vector<dim_sel> > &dim_sels_,std::vector<int> dataset_dimensions_):
    dim_sels(dim_sels_),
    dataset_dimensions(dataset_dimensions_.begin(),dataset_dimensions.end()),
    num_sel(accumulate( dim_sels.begin(), dim_sels.end(), 1, []( auto aa, auto b ) { return aa*b.size(); } )){
    for(int i=0;i<Dims;i++){
      n_elem[i]=dim_sels[i].back().out_stop+1;
    }
  }
  std::array<dim_sel,Dims> cartesian_index(const int i)const {
    if(i>num_sel){
      Rcpp::Rcerr<<"i: "<<i<<", N: "<<num_sel<<std::endl;
      Rcpp::stop("index is larger than size of cartesian product in data_selection");
    }
    std::array<dim_sel,Dims> sel_array;
    lldiv_t q { i, 0 };
    for( int j=Dims-1 ; 0<=j ; --j ) {
      q = std::div( q.quot, dim_sels[j].size() );
      sel_array[j]=dim_sels[j][q.rem];
    }
    return(sel_array);
  }
  template<typename T,int Options> void readEigen(HighFive::DataSet &dset,
						  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Options> >& retmat)const {
    if(Dims!=2){
      Rcpp::stop("readEigen cannot be used when dimensions are different than 2");
    }
    const int elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<int>());
    if(elem_total!=retmat.size()){
      Rcpp::Rcerr<<"retmat is "<<retmat.size()<<" and elem_total is "<<elem_total<<std::endl;
      Rcpp::stop("retmat must have same number of elements as dataset selection");
    }
    auto so =  boost::fortran_storage_order();
    boost::multi_array_ref<T,Dims> retref(&rretmat[0],n_elem,so);
    std::vector<T> tvec;
    for( long long n=0 ; n<num_sel ; ++n ) {
      CompactSelection<Dims> temp_sel(cartesian_index(n));
      temp_sel.readEigenBlock(dset,retmat);
    }
  }
};

template<size_t Dims>
class CompactSelection{
  const std::array<dim_sel,Dims> &selections;
public:
  CompactSelection(const std::array<dim_sel,Dims> dim_sels_):selections(dim_sels_){
  }
  std::vector<size_t> get_chunksizes()const {
    std::vector<size_t> retvec(Dims);
    for(int i=0; i<Dims;i++){
      retvec[i]=selections[i].chunksize;
    }
    return(retvec);
  }
  std::array<boost::multi_array_types::index_range,Dims> get_ranges()const {
    std::array<boost::multi_array_types::index_range,Dims> tranges;
    for(int i=0; i<Dims;i++){
      tranges[i]=boost::multi_array_types::index_range().start(selections[i].out_start).finish(selections[i].out_stop);
    }
    return(tranges);
  }
  std::vector<size_t> get_offsets()const {
    std::vector<size_t> retvec(Dims);
    for(int i=0; i<Dims;i++){
      retvec[i]=selections[i].in_start;
    }
    return(retvec);
  }
  std::vector<size_t> get_output_offsets()const {
    std::vector<size_t> retvec(Dims);
    for(int i=0; i<Dims;i++){
      retvec[i]=selections[i].out_start;
    }
    return(retvec);
  }
  std::vector<bool> get_sorted()const {
    std::vector<bool> retvec(Dims);
    for(int i=0; i<Dims;i++){
      retvec[i]=selections[i].sorted;
    }
    return(retvec);
  }
  int get_total_size()const {
    int ts=1;
    for(int i=0; i<Dims;i++){
      ts*=selections[i].chunksizes;
    }
    return(ts);
  }

  template<typename T, RM =Eigen::ColMajor> Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,RM> readEigen(HighFive::DataSet &dset){
    using namespace HighFive;
    auto chunksizes=get_chunksizes();
    Eigen::MatrixXd tref(chunksizes[0],chunksizes[1]);
    dset.selectEigen(get_offsets(),chunksizes,{}).read(tref);
    auto is_sorted=get_sorted();
  if(!is_sorted[0]){
    tref.rowwise().reverse();
  }
  if(!is_sorted[1]){
    tref.colwise().reverse();
  }
  return(tref);
  }

  template<typename T, RM =Eigen::ColMajor> void readEigenBlock(HighFive::DataSet &dset,Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,RM> > &retmat){
    auto tdim_sizes=get_chunksizes();
    auto out_starts=temp_sel.get_output_offsets();
    mretmat.block(out_starts[0],out_starts[1],tdim_sizes[0],tdim_sizes[1])=readEigen(dset);
  }


  template<typename T> boost::multi_array<T,Dims> readArray(HighFive::DataSet &dset){
    auto so =  boost::fortran_storage_order();
    auto chunksizes=get_chunksizes();
    boost::multi_array<T,Dims> reta(chunksizes,so);
    boost::multi_array_ref<T,Dims> rreta(reta.data(),chunksizes,so);
    using namespace HighFive;
    auto is_sorted=comp_sel.get_sorted();
    for(int i=0;i<Dims;i++){
      if(!is_sorted[i]){
	Rcpp::Rcerr<<"In dimension: "<<i<<" of array"<<std::endl;
	Rcpp::stop("indices must be sorted for arrays");
      }
    }
    dset.selectEigen(get_offsets(),chunksizes(),{}).read(rreta);
    return(reta);
  }

  template<typename T> void readArrayBlock(HighFive::DataSet &dset,
					   boost::multi_array_ref<T,Dims> &retref){
    boost::multi_array<T,Dims> reta=readArray(dset);
    auto chunksizes=get_chunksizes();
    boost::multi_array_ref<T,Dims> rreta(reta.data(),chunksizes,so);
    block_assign_read(retref,rreta,get_range());
  }




// template<SEXPTYPE RTYPE,typename T,size_t Dims>
// class RH5IO{
//   DatasetSelection<Dims> dsel;
//   const std::string filename;
//   const std::string dataname;
//   const std::string groupname;


// public:
//   RH5IO( const DatasetSelection<Dims> &dsel,
// 	 const std::string filename_,
// 	 const std::string groupname)


// };


#endif
