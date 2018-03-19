#ifndef EIGENH5_SELECTION_HPP
#define EIGENH5_SELECTION_HPP

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
class dataset_selection{
public:
  const std::vector<std::vector<dim_sel> >  &dim_sels;
  const int num_sel;
  std::array<int,Dims> n_elem;
  std::array<int,Dims> dataset_dimensions;
  dataset_selection(std::vector<std::vector<dim_sel> > &dim_sels_,std::vector<int> dataset_dimensions_):
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

template<SEXPTYPE RTYPE,typename T,size_t Dims>
class RH5IO{
  dataset_selection<Dims> dsel;
  const std::string filename;
  const std::string dataname;
  const std::string groupname;


public:
  RH5IO( const dataset_selection<Dims> &dsel,
	 const std::string filename_,
	 const std::string groupname)


};


#endif
