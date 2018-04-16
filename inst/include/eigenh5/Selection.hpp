#pragma once
#include <boost/icl/interval_set.hpp>
//#include "Interval.hpp"

struct IntegerVector_range
  : ranges::view_facade<IntegerVector_range>
{
private:
  friend struct ranges::range_access;
  Rcpp::IntegerVector vec_;
  struct cursor
  {
  private:
    Rcpp::IntegerVector::const_iterator iter;
  public:
    cursor() = default;
    cursor(Rcpp::IntegerVector::const_iterator it): iter(it){}
    int const & read() const{
      return *iter;
    }
    bool equal(cursor const &that) const
    {
      return iter == that.iter;
    }
    void next()
    {
      ++iter;
    }
    void prev()
    {
      --iter;
    }
    std::ptrdiff_t distance_to(cursor const &that) const
    {
      return that.iter - iter;
    }
    void advance(std::ptrdiff_t n)
    {
      iter += n;
    }
  };
  cursor begin_cursor() const
  {
    return {vec_.begin()};
  }
  cursor end_cursor() const
  {
    return {vec_.end()};
  }
public:
  IntegerVector_range()
    : vec_()
  {}
  IntegerVector_range(Rcpp::IntegerVector vec):vec_(vec){};

};


std::vector<int> get_dims(const Rcpp::RObject m);


// template<typename T>
// class


// class counted_interval{
// public:
//   counted_interval():



struct dim_sel{

public:
  size_t in_start;
  size_t in_stop;
  size_t out_start;
  size_t out_stop;
  bool sorted;
  size_t chunksize;


  dim_sel(const int in_start_,const int in_stop_,const int out_start_,const int out_stop_){
    in_stop=in_stop_;
    in_start=in_start_;
    if(in_start>in_stop){
      std::swap(in_start,in_stop);
      sorted=false;
    }else{
      sorted=true;
    }
    out_start=out_start_;
    out_stop=out_stop_;
    chunksize =	static_cast<size_t>(in_stop-in_start+1);
    if(chunksize!=(out_stop-out_start+1)){
      Rcpp::Rcerr<<"chunksize: "<<chunksize<<" in_stop: "<<in_stop<<" in_start: "<<in_start<<std::endl;
      Rcpp::Rcerr<<" out_stop: "<<out_stop<<" out_start: "<<out_start<<std::endl;
      Rcpp::stop("chunksize mismatch");
    }
  }


  dim_sel(const int in_start_,const int in_stop_,const int out_start_,const int out_stop_,const size_t dimsize){
    if(in_stop_<0){
      in_stop=dimsize-in_stop;
    }else{
      in_stop=in_stop_;
    }
    if(in_start_<0){
      in_start=dimsize-in_start;
    }else{
      in_start=in_start_;
    }
    if(in_start>in_stop){
      std::swap(in_start,in_stop);
      sorted=false;
    }else{
      sorted=true;
    }
    out_start=out_start_;
    out_stop=out_stop_;
    chunksize =	static_cast<size_t>(in_stop-in_start+1);
    if(chunksize!=(out_stop-out_start+1)){
      Rcpp::Rcerr<<"chunksize: "<<chunksize<<" in_stop: "<<in_stop<<" in_start: "<<in_start<<std::endl;
      Rcpp::Rcerr<<"dimsize: "<<dimsize<<" out_stop: "<<out_stop<<" out_start: "<<out_start<<std::endl;
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
  dim_sel(const int offset,int chunksize_,const size_t dimsize):chunksize(chunksize_){
    if(offset<0){
      in_start=dimsize-offset;
    }else{
      in_start=offset;
    }
    if(Rcpp::IntegerVector::is_na(chunksize_)){
      chunksize_ = dimsize;
    }
    in_stop=in_start+chunksize_-1;
    out_start=0;
    out_stop=chunksize-1;
    if(in_stop>dimsize || in_stop>dimsize){
      Rcpp::Rcerr<<"offset: "<<offset<<" chunksize: "<<chunksize<<" dimsize: "<<dimsize<<std::endl;
      Rcpp::Rcerr<<"offset: "<<offset<<" chunksize: "<<chunksize<<" dimsize: "<<dimsize<<std::endl;
      Rcpp::stop("Illegal selection, offset+chunksize > dimsize");
    }
    sorted=true;
  }
  dim_sel():
    in_start(0),
    in_stop(0),
    out_start(0),
    out_stop(0),
    chunksize(1),
    sorted(true){}
};

class DimRange{

public:
  //  IntervalTree<dim_sels> tree_

  const std::vector<dim_sel> dim_sels;

  DimRange(std::vector<dim_sel> &dim_sels_):
    dim_sels(std::move(dim_sels_))
  {
    using namespace boost;
    using namespace boost::icl;
    boost::icl::interval_set<int> in_sels;
    const size_t num_sels=dim_sels.size();
    out_size=0;
    size_t t_in_start=0;
    all_sorted=true;
    isCompact_=dim_sels.size()==1;
    for(auto &te : dim_sels){
      if((t_in_start> te.in_start) || (!te.sorted)){
	all_sorted=false;
      }
      t_in_start=te.in_start;
      in_sels.insert(construct<discrete_interval<int> >(te.in_start,te.in_stop,interval_bounds::closed()));
      out_size+=te.out_stop-te.out_start;
    }
    in_size = length(in_sels);
    isRepeated_=in_size<out_size;
  }
  DimRange(Rcpp::IntegerVector::const_iterator itb, Rcpp::IntegerVector::const_iterator ite):
    dim_sels(find_cont(itb,ite))
  {
    using namespace boost;
    using namespace boost::icl;
    boost::icl::interval_set<int> in_sels;
    const size_t num_sels=dim_sels.size();
    out_size=0;
    size_t t_in_start=0;
    all_sorted=true;
    isCompact_=dim_sels.size()==1;
    for(auto &te : dim_sels){
      if((t_in_start> te.in_start) || (!te.sorted)){
	all_sorted=false;
      }
      t_in_start=te.in_start;
      in_sels.insert(construct<discrete_interval<int> >(te.in_start,te.in_stop,interval_bounds::closed()));
      out_size+=te.out_stop-te.out_start+1;
    }
    in_size = length(in_sels);
    isRepeated_=in_size<out_size;
    if(in_size>out_size){
      Rcpp::Rcerr<<"size of interval to be read is :"<<in_size<<std::endl;
      Rcpp::Rcerr<<"size of space for it is :"<<out_size<<std::endl;
      Rcpp::stop("out size must be greater than equal to in_size");
    }

  }
  DimRange(const dim_sel dim_sel_):
    dim_sels({dim_sel_}),
    out_size(dim_sel_.chunksize),
    isCompact_(dim_sels.size()==1),
    all_sorted(dim_sels[0].sorted),
    isRepeated_(false)
  {}
  DimRange(const size_t dimsize):
    dim_sels({dim_sel(0,dimsize)}),
    out_size(dimsize),
    isCompact_(true),
    all_sorted(true){}
  DimRange():
    dim_sels({dim_sel()}),
    out_size(dim_sels.back().out_stop+1),
    isCompact_(true),
    all_sorted(dim_sels[0].sorted),
    isRepeated_(false){}




  size_t get_n_elem() const{
    return(out_size);
  }
  size_t get_num_selections() const{
    return(dim_sels.size());
  }
  bool isCompact()const{
    return(isCompact_);
  }
  bool isSorted()const {
    return(all_sorted);
  }
  bool isRepeated() const{
    return(isRepeated_);
  }
private:
  size_t in_size;
  size_t out_size;
  bool isCompact_;
  bool all_sorted;
  bool isRepeated_;
  static std::vector<dim_sel> find_cont(Rcpp::IntegerVector::const_iterator itb,Rcpp::IntegerVector::const_iterator ite);

};



template<size_t Dims>
class DatasetSelection{
public:
  std::array<size_t,Dims> dataset_dimensions;
  std::array<DimRange,Dims> sels;
  std::array<size_t,Dims> n_elem;
  std::array<size_t,Dims> n_selections;
  std::vector<std::array<size_t,Dims> > offsets_in;
  std::vector<std::array<size_t,Dims> > offsets_out;
  std::vector<std::array<size_t,Dims> > chunksizes;
  std::vector<std::array<bool,Dims> > reverses;
  std::array<bool,Dims> all_sorted;
  bool all_dim_sorted;


  DatasetSelection (std::array<size_t,Dims> dataset_dimensions_):dataset_dimensions(dataset_dimensions_){
    for(int i=0;i<Dims;i++){
      sels[i]=DimRange(dataset_dimensions[i]);
      n_elem[i]=dataset_dimensions[i];
      n_selections[i]=1;
    }
  }



  // dataset_dimensions(dataset_dimensions_){
  DatasetSelection (std::array<DimRange,Dims> sels_, std::array<size_t,Dims> dataset_dimensions_):sels(sels_),
											       dataset_dimensions(dataset_dimensions_){

    if constexpr(Dims > 2){
      Rcpp::stop("Datasets with Dims>2 currently not supported");
    }
    size_t tot_selections=1;
    for(int i=0; i<Dims;i++){
      n_elem[i]=sels[i].get_n_elem();
      n_selections[i]=sels[i].get_num_selections();
      tot_selections=tot_selections*n_selections[i];
      all_sorted[i]=sels[i].isSorted();
    }
    all_dim_sorted=true;
    for(auto it= all_sorted.begin();it!=all_sorted.end();it++){
      if(!(*it)){
	all_dim_sorted=false;
      }
    }

    offsets_in.resize(tot_selections);
    chunksizes.resize(tot_selections);
    if(!all_dim_sorted){
      reverses.resize(tot_selections);
      offsets_out.resize(tot_selections);
    }

    if constexpr (Dims==1){
	for(int i=0; i<tot_selections;i++){
	  offsets_in[i]={sels[0].dim_sels[i].in_start};
	  chunksizes[i]={sels[0].dim_sels[i].chunksize};
	  if(!all_dim_sorted){
	    reverses[i] = {!sels[0].dim_sels[i].sorted};
	    offsets_out[i] = {sels[0].dim_sels[i].out_start};
	  }
	}
      }else{
      int tj=0;
      for(int i=0; i<n_selections[0];i++){
	for(int	j=0;j<n_selections[1];j++){
	  offsets_in[tj]={sels[0].dim_sels[i].in_start,sels[1].dim_sels[j].in_start};
	  chunksizes[tj]={sels[0].dim_sels[i].chunksize,sels[1].dim_sels[j].chunksize};
	  if(!all_dim_sorted){
	    reverses[tj] = {!sels[0].dim_sels[i].sorted,sels[1].dim_sels[j].sorted};
	    offsets_out[i] = {sels[0].dim_sels[i].out_start,sels[1].dim_sels[j].sorted};
	  }
	  tj++;
	}
      }
    }
  }

  template<typename T,int Options,int RN,int CN>
  void flipRows(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> > retmat,
					       const std::array<size_t,Dims> &offset,
					       const std::array<size_t,Dims> &chunksize)const ;

  template<typename T,int Options,int RN,int CN>
  void flipCols(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> > retmat,
					       const std::array<size_t,Dims> &offset,
					       const std::array<size_t,Dims> &chunksize)const ;

  HighFive::Selection makeSelection(const HighFive::DataSet &dset)const;
  template<typename T,int Options,int RN,int CN>
  void readEigen(HighFive::Selection &selection,
		 Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const;

  template<typename T,int Options,int RN,int CN>
  void writeEigen(HighFive::Selection &selection,
		  Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const;
  template<typename T>
  void readVector(HighFive::Selection &selection,
		  std::vector<T> &rvec)const;

  template<typename T>
  void writeVector(HighFive::Selection &selection,
		   std::vector<T> wvec)const;

private:
  template<typename T>
  void flipVec(std::vector<T> &mvec) const;

  template<typename T,int Options,int RN,int CN>
  void flipMat(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const;



};

template<size_t Dims>
HighFive::Selection DatasetSelection<Dims>::makeSelection(const HighFive::DataSet &dset) const{
  if(offsets_in.empty()){
    std::vector<size_t>	t_offsets(Dims,0);
    std::vector<size_t>	t_chunksizes = dset.getDataDimensions();
    return(dset.selectEigen(t_offsets,t_chunksizes,{}));
  }
  return(dset.selectRanges(offsets_in,chunksizes,n_elem));

  //  return();
}

template<size_t Dims>
template<typename T>
inline void DatasetSelection<Dims>::flipVec(std::vector<T> &rvec) const{
  static_assert(Dims==1,"flipVec cannot be used when dimensions are greater than 1");
  std::array<bool,1> search_el={{true}};
  if(!all_dim_sorted){
    auto it=reverses.begin();
    auto itb=it;
    auto ite=reverses.end();
    auto rvb= rvec.begin();
    while(it!=ite){
      it=std::find(it,ite,search_el);
      if(it!=ite){
	const size_t t_ind=it-itb;
	auto tv_b = rvb+offsets_out[t_ind][0];
	auto tv_e = tv_b+chunksizes[t_ind][0];
	std::reverse(tv_b,tv_e);
	it++;
      }
    }
  }
}


template<size_t Dims>
template<typename T,int Options,int RN,int CN>
inline void DatasetSelection<Dims>::flipMat(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const {
  static_assert(Dims<=2,"readEigen cannot be used when dimensions are greater than 2");
  if(!all_dim_sorted){
    auto it=reverses.begin();
    auto itb=it;
    auto ite=reverses.end();
    while(it!=ite){
      it=std::find_if(it,ite,[](auto tel){
	  return(std::find(tel.begin(),tel.end(),true)!=tel.end());
	});
      if(it!=ite){
	const size_t t_ind=it-itb;
	size_t t_dim=1;
	for(auto tel=it->begin(); tel!=it->end();tel++){
	  if(*tel){
	    if(t_dim==1){
	      flipRows(retmat,offsets_out[t_ind],chunksizes[t_ind]);
	    }else{
	      flipCols(retmat,offsets_out[t_ind],chunksizes[t_ind]);
	    }
	  }
	  t_dim++;
	}
	it++;
      }
    }
  }
}


template<size_t Dims>
template<typename T,int Options,int RN,int CN>
inline void DatasetSelection<Dims>::readEigen(HighFive::Selection &selection,
					Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const {
  if constexpr(Dims>2){
      Rcpp::stop("readEigen cannot be used when dimensions are greater than 2");
    }
  const size_t elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<size_t>());
  if(elem_total!=retmat.size()){
    Rcpp::Rcerr<<"retmat is "<<retmat.size()<<" and elem_total is "<<elem_total<<std::endl;
    Rcpp::stop("retmat must have same number of elements as dataset selection");
  }
  selection.read(retmat);
  flipMat(retmat);
}


template<size_t Dims>
template<typename T>
inline void DatasetSelection<Dims>::readVector(HighFive::Selection &selection,
					       std::vector<T> &rvec)const {

  static_assert(Dims==1,"readVector cannot be used when dimensions are greater than 1");
  using iter_t= typename std::vector<T>::iterator;
  const size_t elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<size_t>());
  if(elem_total!=rvec.size()){
    Rcpp::Rcerr<<"rvec is "<<rvec.size()<<" and elem_total is "<<elem_total<<std::endl;
    Rcpp::stop("retmat must have same number of elements as dataset selection");
  }
  selection.read(rvec);
  flipVec(rvec);
}

template<size_t Dims>
template<typename T>
inline void DatasetSelection<Dims>::writeVector(HighFive::Selection &selection,
					       std::vector<T> wvec)const {

  static_assert(Dims==1,"readVector cannot be used when dimensions are greater than 1");

  const size_t elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<size_t>());
  if(elem_total!=wvec.size()){
    Rcpp::Rcerr<<"wvec is "<<wvec.size()<<" and elem_total is "<<elem_total<<std::endl;
    Rcpp::stop("wvec must have same number of elements as dataset selection");
  }
  flipVec(wvec);
  selection.write(wvec);
}


template<size_t Dims>
template<typename T,int Options,int RN,int CN>
inline void DatasetSelection<Dims>::writeEigen(HighFive::Selection &selection,
					Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const {
  static_assert(Dims<=2,"writeEigen cannot be used when dimensions are greater than 2");
  const size_t elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<size_t>());
  if(elem_total!=retmat.size()){
    Rcpp::Rcerr<<"retmat is "<<retmat.size()<<" and elem_total is "<<elem_total<<std::endl;
    Rcpp::stop("retmat must have same number of elements as dataset selection");
  }
  flipMat(retmat);
  selection.write(retmat);
}




template<size_t Dims>
template<typename T,int Options,int RN,int CN>
inline void DatasetSelection<Dims>::flipRows(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> > retmat,
					      const std::array<size_t,Dims> &offset,
					      const std::array<size_t,Dims> &chunksize) const{

  if constexpr(Dims>2){
      Rcpp::stop("flipBlock cannot be used when dimensions are greater than 2");
    }
  if constexpr(Dims==1){
      retmat.block(offset[0],0,chunksize[0],1).colwise().reverse();
    }else{
    retmat.block(offset[0],offset[1],chunksize[0],chunksize[1]).colwise().reverse();
  }
}



template<size_t Dims>
template<typename T,int Options,int RN,int CN>
inline void DatasetSelection<Dims>::flipCols(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> > retmat,
					      const std::array<size_t,Dims> &offset,
					      const std::array<size_t,Dims> &chunksize) const{

  if constexpr(Dims>2){
      Rcpp::stop("flipBlock cannot be used when dimensions are greater than 2");
    }
  if constexpr(Dims==1){
      retmat.block(0,offset[0],1,chunksize[0]).rowwise().reverse();
    }else{
    retmat.block(offset[0],offset[1],chunksize[0],chunksize[1]).rowwise().reverse();
  }
}



// template<typename T,int Options> void DatasetSelection::readArray(HighFive::DataSet &dset,
// 						  Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Options> >& retmat)const {
//     if(Dims!=2){
//       Rcpp::stop("readEigen cannot be used when dimensions are different than 2");
//     }
//     const int elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<int>());
//     if(elem_total!=retmat.size()){
//       Rcpp::Rcerr<<"retmat is "<<retmat.size()<<" and elem_total is "<<elem_total<<std::endl;
//       Rcpp::stop("retmat must have same number of elements as dataset selection");
//     }
//     for( long long n=0 ; n<num_sel ; ++n ) {
//       CompactSelection<Dims> temp_sel(cartesian_index(n));
//       temp_sel.readEigenBlock(dset,retmat);
//     }
// }

// template<typename T,int Options,int RN,int CN>
// inline void DatasetSelection::writeEigen(HighFive::DataSet &dset,
// 					 Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const {
//   if(Dims!=2){
//     Rcpp::stop("readEigen cannot be used when dimensions are different than 2");
//   }
//   const size_t elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<size_t>());
//   if(elem_total!=retmat.size()){
//     Rcpp::Rcerr<<"retmat is "<<retmat.size()<<" and elem_total is "<<elem_total<<std::endl;
//     Rcpp::stop("retmat must have same number of elements as dataset selection");
//   }
//   const std::array<size_t,2> n_sel ({row_sels.get_num_selections(),col_sels.get_num_selections()});
//   for(int i=0; i<n_sel[0]; i++){
//     for(int j=0; j<n_sel[1];j++){
//       CompactSelection<2> temp_sel({row_sels.dim_sels[i],col_sels.dim_sels[j]});
//       temp_sel.writeEigenBlock(dset,retmat);
//     }
//   }


// }

// template<size_t Dims>
// template<typename T>
// inline HighFive::DataSet DatasetSelection<Dims>::createDataset(HighFive::Group &grp,const std::string dataname,std::array<size_t,Dims> chunk_dims)const{


// using namespace HighFive;

//  std::vector<size_t> mat_dims={static_cast<size_t>(n_elem[0]),static_cast<size_t>(n_elem[1])};
//   if (chunk_dims.empty()) {
//     const size_t MAX_CHUNK = 1024*1024;
//     const size_t chunk_rows = static_cast<size_t>(std::min(static_cast<double>(mat_dims[0]),std::ceil(static_cast<double>(MAX_CHUNK)/static_cast<double>(mat_dims[1]))));
//     //Rcpp::Rcout<<"chunk_dims: "<<chunk_dims[0]<<" , "<<chunk_dims[1]<<std::endl;
//     chunk_dims={chunk_rows, static_cast<size_t>(mat_dims[1])};
//   }
//   // Create a new file using the default property lists.
//   // if(doTranspose){
//   //   Rcpp::Rcout<<"transpose!"<<std::endl;
//   // }
//   Filter filter(chunk_dims, FILTER_BLOSC, 0,false);
//   // Create a dataset with double precision floating points


//   DataSpace ds = DataSpace(mat_dims,false);
//   return(grp.createDataSet(dataname, ds, AtomicType<T>(), filter.getId(), false));
// }



// template<size_t Dims>
// template<typename T> boost::multi_array<T,Dims> CompactSelection<Dims>::readArray(HighFive::DataSet &dset)const {
//     auto so =  boost::fortran_storage_order();
//     auto chunksizes=get_chunksizes();
//     boost::multi_array<T,Dims> reta(chunksizes,so);
//     boost::multi_array_ref<T,Dims> rreta(reta.data(),chunksizes,so);
//     using namespace HighFive;
//     auto is_sorted=comp_sel.get_sorted();
//     for(int i=0;i<Dims;i++){
//       if(!is_sorted[i]){
// 	Rcpp::Rcerr<<"In dimension: "<<i<<" of array"<<std::endl;
// 	Rcpp::stop("indices must be sorted for arrays");
//       }
//     }
//     dset.selectEigen(get_offsets(),chunksizes(),{}).read(rreta);
//     return(reta);
//   }
// template<size_t Dims>
//   template<typename T> void CompactSelection<Dims>::block_assign_write(boost::multi_array_ref<T,Dims> &retref,
// 							   boost::multi_array_ref<T,Dims> &tarr,
// 							   std::array<boost::multi_array_types::index_range,Dims> ranges)const {
//     if constexpr(Dims==1){
// 	tarr =retref[ boost::indices[ ranges[0] ] ] = ;
//       }else{
//       if constexpr( Dims==2){
// 	  tarr = retref[ boost::indices[ ranges[0] ][ ranges[1] ] ] ;
// 	}else{
// 	if constexpr( Dims==3){
// 	    tarr =  retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ] ];
// 	  }else{
// 	  if constexpr( Dims==4){
// 	      tarr = retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ][ ranges[3] ] ] ;
// 	    }else{
// 	    static_assert(Dims<=4, "Arrays of dimension > 4 not supported");
// 	  }
// 	}
//       }
//     }
//   }

// template<size_t Dims>
//   template<typename T> void CompactSelection<Dims>::block_assign_read(boost::multi_array_ref<T,Dims> &retref,
// 							  boost::multi_array_ref<T,Dims> &tarr,
// 							  std::array<boost::multi_array_types::index_range,Dims> ranges)const {
//     if constexpr(Dims==1){
// 	retref[ boost::indices[ ranges[0] ] ] = tarr;
//       }else{
//       if constexpr( Dims==2){
// 	  retref[ boost::indices[ ranges[0] ][ ranges[1] ] ] = tarr;
// 	}else{
// 	if constexpr( Dims==3){
// 	    retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ] ] = tarr;
// 	  }else{
// 	  if constexpr( Dims==4){
// 	      retref[ boost::indices[ ranges[0] ][ ranges[1] ][ ranges[2] ][ ranges[3] ] ] = tarr;
// 	    }else{
// 	    static_assert(Dims<=4, "Arrays of dimension > 4 not supported");
// 	  }
// 	}
//       }
//     }
//   }
// template<size_t Dims>
//   template<typename T> void CompactSelection<Dims>::readArrayBlock(HighFive::DataSet &dset,
// 					   boost::multi_array_ref<T,Dims> &retref){
//     boost::multi_array<T,Dims> reta=readArray(dset);
//     auto chunksizes=get_chunksizes();
//     boost::multi_array_ref<T,Dims> rreta(reta.data(),chunksizes,so);
//     block_assign_read(retref,rreta,get_range());
//   }
