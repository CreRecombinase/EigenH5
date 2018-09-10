#pragma once
#include <boost/icl/interval_set.hpp>
#include <range/v3/core.hpp>
#include <range/v3/view.hpp>
#include <range/v3/action.hpp>
#include "highfive/highfive.hpp"
#include <RcppEigen.h>
#include <Rcpp.h>




std::vector<int> get_dims(const Rcpp::RObject m);


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

  static std::vector<dim_sel> parse_chunk_list(const Rcpp::List &list,std::vector<size_t> datadims);
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
      chunksize = dimsize;
    }
    in_stop=in_start+chunksize-1;
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
  std::pair<size_t,size_t> unsorted_in() const{
    if(sorted){
      return(std::pair(in_start,in_stop));
    }else{
      return(std::pair(in_stop,in_start));
    }
  }
  std::pair<size_t,size_t> unsorted_out() const{
    return(std::pair(out_start,out_stop));
  }

  bool operator<(const dim_sel &other)const{
    return(this->unsorted_in()<other.unsorted_in());
  }
  bool operator>(const dim_sel &other)const{
    return(this->unsorted_in()>other.unsorted_in());
  }
  bool operator<=(const dim_sel &other)const{
    return(this->unsorted_in()<=other.unsorted_in());
  }
  bool operator>=(const dim_sel &other)const{
    return(this->unsorted_in()>=other.unsorted_in());
  }
  bool operator==(const dim_sel &other)const{
    return(this->unsorted_in()==other.unsorted_in());
  }
};

inline std::vector<dim_sel> dim_sel::parse_chunk_list(const Rcpp::List &list,std::vector<size_t> datadims){
    using int_o = std::optional<int>;

  const size_t num_dims= datadims.size();
  if(num_dims>2){
    Rcpp::stop("Datasets with Dims>2 currently not supported");
  }
  std::vector<dim_sel> retvec(num_dims);

  std::vector<int_o> offset_v =	parse_option(list,datadims,"offset");
  std::vector<int_o> offsets_v = parse_option(list,datadims,"offsets");
  
  std::vector<int_o> chunksize_v =parse_option(list,datadims,"datasize");
  std::vector<int_o> chunksizes_v =parse_option(list,datadims,"datasizes");
  
  for(int i=0; i<num_dims;i++){
    int o_o = offset_v[i].value_or(offsets_v[i].value_or(0));
    int c_o = chunksize_v[i].value_or(chunksizes_v[i].value_or(datadims[i]-o_o));
    retvec[i]=dim_sel(o_o,c_o,datadims[i]);
  }
  return(retvec);
}









class DimRange{

public:

  std::vector<dim_sel> dim_sels;
  DimRange(Rcpp::IntegerVector::const_iterator itb, Rcpp::IntegerVector::const_iterator ite);

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

  static DimRange construct_dimrange(dim_sel chunk,
				   std::optional<Rcpp::IntegerVector> subset);
  Eigen::ArrayXi permutation_order() const;



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
  //  static std::vector<dim_sel> find_cont(Rcpp::IntegerVector::const_iterator itb,Rcpp::IntegerVector::const_iterator ite);

};


inline Eigen::ArrayXi DimRange::permutation_order() const {

  if(isRepeated_){
    Rcpp::stop("Cannot represent selection as a permutation if the selection has repeated indices!");
  }
  if(all_sorted){
    return(Eigen::ArrayXi::LinSpaced(out_size,dim_sels.front().out_start,dim_sels.back().out_stop));
  }else{
    if(isCompact_){
      return(Eigen::ArrayXi::LinSpaced(in_size,dim_sels[0].out_stop,dim_sels[0].out_start));
    }else{
      Eigen::ArrayXi retvec(in_size);
      auto rit= retvec.data();
      for(auto &t_sel : dim_sels){
	const size_t t_size = t_sel.out_stop - t_sel.out_start + 1;
	auto nrit = std::generate_n(rit, t_size, [n = static_cast<int>(t_sel.out_start)] () mutable { return n++; });
	if(!t_sel.sorted){
	  std::reverse(rit,nrit);
	}
	rit=nrit;
      }
      return(retvec);
    }
  }
}



inline DimRange DimRange::construct_dimrange(dim_sel chunk,
			    std::optional<Rcpp::IntegerVector> subset){
  if(subset){
    return(DimRange(subset->begin(),subset->end()));
  }
  return(DimRange(chunk));
}


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
  const bool doTranspose;

  bool all_dim_compact;
  bool all_dim_sorted;


  DatasetSelection (std::array<size_t,Dims> dataset_dimensions_,bool doTranspose_=false):dataset_dimensions(dataset_dimensions_),doTranspose(doTranspose_){

    for(int i=0;i<Dims;i++){
      sels[i]=DimRange(dataset_dimensions[i]);
      n_elem[i]=dataset_dimensions[i];
      n_selections[i]=1;
    }
    all_dim_compact=true;
    all_dim_sorted=true;
  }



  DatasetSelection (std::array<DimRange,Dims> sels_,
		    std::array<size_t,Dims> dataset_dimensions_,const bool doTranspose_=false):sels(sels_),doTranspose(doTranspose_),
											       dataset_dimensions(dataset_dimensions_){

    if constexpr(Dims > 2){
      Rcpp::stop("Datasets with Dims>2 currently not supported");
    }

    all_dim_compact=true;
    all_dim_sorted=true;
    size_t tot_selections=1;
    for(int i=0; i<Dims;i++){
      n_elem[i]=sels[i].get_n_elem();
      n_selections[i]=sels[i].get_num_selections();
      tot_selections=tot_selections*n_selections[i];
      if(!sels[i].isCompact()){
	all_dim_compact=false;
      }
      if(!sels[i].isSorted()){
	all_dim_sorted=false;
      }
    }

    offsets_in.resize(tot_selections);
    chunksizes.resize(tot_selections);

    if constexpr (Dims==1){
	for(int i=0; i<tot_selections;i++){
	  offsets_in[i]={sels[0].dim_sels[i].in_start};
	  chunksizes[i]={sels[0].dim_sels[i].chunksize};
	}
      }else{
      int tj=0;
      for(int i=0; i<n_selections[0];i++){
	for(int	j=0;j<n_selections[1];j++){
	  offsets_in[tj]={sels[0].dim_sels[i].in_start,sels[1].dim_sels[j].in_start};
	  chunksizes[tj]={sels[0].dim_sels[i].chunksize,sels[1].dim_sels[j].chunksize};
	  tj++;
	}
      }
    }
  }

  static DatasetSelection<Dims>	ProcessList(const Rcpp::List options, std::vector<size_t> dims);


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
  std::array<size_t,Dims> get_selection_dim()const{
    return(n_elem);
  }
private:
  template<typename T>
  void flipVec(std::vector<T> &mvec) const;

  template<typename T,int Options,int RN,int CN>
  void flipMat(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const;



  };

template<size_t Dims>
inline DatasetSelection<Dims> DatasetSelection<Dims>::ProcessList(const Rcpp::List options, std::vector<size_t> dims){

  if(dims.size()!=Dims){
    Rcpp::stop("dims must be either equal in size to Dims, or must be empty and accompany  valid filename+datapath arguments ");
  }

  auto dchunk_v = dim_sel::parse_chunk_list(options,dims);
  bool doTranspose_ =	get_list_scalar<bool>(options,"doTranspose").value_or(false);
  auto dslice_v = parse_subset_list(options,dims);
  if constexpr(Dims==2){
      std::array<size_t,2>tdims={dims[0],dims[1]};
      return(DatasetSelection<Dims>({
	    DimRange::construct_dimrange(dchunk_v[0],dslice_v[0]),
	    DimRange::construct_dimrange(dchunk_v[1],dslice_v[1])},tdims,doTranspose_));
    }
  if constexpr(Dims==1){
      std::array<size_t,1>tdims={dims[0]};
      return(DatasetSelection<1>({
	    DimRange::construct_dimrange(dchunk_v[0],dslice_v[0])},tdims));
    }
}

inline DimRange::DimRange(Rcpp::IntegerVector::const_iterator itb,Rcpp::IntegerVector::const_iterator ite){
  using namespace Rcpp;
  using namespace ranges;
  using namespace boost;
  using namespace boost::icl;

  all_sorted=true;
  out_size= ite-itb;
  dim_sels.reserve(out_size/2);

  boost::icl::interval_set<int> in_sels;

  int tot_dist=0;
  int reg_size=0;
  auto t_it = itb;
  auto itbb=itb;

  if(out_size==1){
    if(*itb<=0){
      Rcpp::stop("indexes must be greather than 0!");
    }
    dim_sel tds((*itb)-1,(*itb)-1,0,0);
    in_sels.insert(construct<discrete_interval<int> >(tds.in_start,tds.in_stop,interval_bounds::closed()));
    dim_sels.push_back(tds);
    tot_dist=1;
  }else{
    for(auto it=itb; it!=ite; it++){
      bool ins=true;
      if(*it<=0){
	Rcpp::stop("indexes must be greather than 0!");
      }
      auto nit= it+1;
      if(nit!=ite){
	int diff = (*nit)-(*it);
	if(diff<0){
	  all_sorted=false;
	}
	if(abs(diff)==1){
	  t_it=nit;
	  reg_size++;
	  ins=false;
	}
      }
      if(ins){
	if((t_it-itbb)!=abs((*t_it-*itbb))){
	  Rcpp::stop("dim_sel is not compact!");
	}
	dim_sel tds((*itbb)-1,*(t_it)-1,tot_dist,tot_dist+(t_it-itbb));
	in_sels.insert(construct<discrete_interval<int> >(tds.in_start,tds.in_stop,interval_bounds::closed()));
	dim_sels.push_back(tds);
	tot_dist=tot_dist+tds.chunksize;
	itbb=nit;
	t_it=nit;
      }
    }
  }
  isCompact_ = dim_sels.size()==1;
  if(tot_dist!=out_size){
    Rcpp::stop("error in constructing DimRange, tot_dist != out_size");
  }
  in_size = length(in_sels);
  isRepeated_=in_size<out_size;
  if(isRepeated_){
    Rcpp::stop("repeated indicies in dataset subsetting is currently unsupported!");
  }
  if(in_size>out_size){
    Rcpp::Rcerr<<"size of interval to be read is :"<<in_size<<std::endl;
    Rcpp::Rcerr<<"size of space for it is :"<<out_size<<std::endl;
    Rcpp::stop("out size must be greater than equal to in_size");
  }
  if(!all_sorted){
    std::sort(dim_sels.begin(),dim_sels.end());
  }
}



  // while(it!=ite){
  //   it = std::adjacent_find(itb,ite,[](int i,int j){
  //     // Rcpp::Rcout<<"i is : "<<i<<std::endl;
  //     // Rcpp::Rcout<<"j is : "<<j<<std::endl;
  //     return(std::abs(j-i)!=1);
  //   });
  //   int iti = (it==ite ? *(it-1) : *(it))-1;
  //   //    int itb_pos = itb-itbb;
  //   int reg_size = it==ite ? it-itb : (it-itb+1);
  //   dim_sel tds
  //   dim_se.push_back(dim_sel((*itb)-1,iti,tot_dist,tot_dist+reg_size-1));
  //   //			 piarray{{{*itb,iti}},{{tot_dist,tot_dist+reg_size-1}}});
  //   if(it!=ite){
  //     it++;
  //   }
  //   tot_dist=tot_dist+reg_size;
  //   itb=it;
  // }

  // for(auto &te : ds){
  //   in_sels.insert(
  //   tout_size+=te.out_stop-te.out_start+1;
  // }









template<size_t Dims>
inline HighFive::Selection DatasetSelection<Dims>::makeSelection(const HighFive::DataSet &dset) const{
  if(offsets_in.empty()){
    std::vector<size_t>	t_offsets(Dims,0);
    std::vector<size_t>	t_chunksizes = dset.getDataDimensions();
    return(dset.selectEigen(t_offsets,t_chunksizes,{}));
  }
  return(dset.selectRanges(offsets_in,chunksizes,n_elem));
}

template<size_t Dims>
template<typename T>
inline void DatasetSelection<Dims>::flipVec(std::vector<T> &rvec) const{
  static_assert(Dims==1,"flipVec cannot be used when dimensions are greater than 1");
  if(!all_dim_sorted){
    Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,1> > retmat(rvec.data(),rvec.size());
    if(!sels[0].isSorted()){
      if(sels[0].isCompact()){
	retmat.colwise().reverseInPlace();
      }else{
	retmat=Eigen::PermutationMatrix<Eigen::Dynamic>(sels[0].permutation_order().matrix())*retmat;
      }
    }
  }
}


template<size_t Dims>
template<typename T,int Options,int RN,int CN>
inline void DatasetSelection<Dims>::flipMat(Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const {
  static_assert(Dims<=2,"readEigen cannot be used when dimensions are greater than 2");
  if(!all_dim_sorted){
    if(!sels[0].isSorted()){
      if(sels[0].isCompact()){
	retmat.colwise().reverseInPlace();
      }else{
	retmat=Eigen::PermutationMatrix<Eigen::Dynamic>(sels[0].permutation_order().matrix())*retmat;
      }
    }
    if(!sels[1].isSorted()){
      if(sels[1].isCompact()){
	retmat.rowwise().reverseInPlace();
      }else{
	retmat=retmat*(Eigen::PermutationMatrix<Eigen::Dynamic>(sels[1].permutation_order().matrix()).transpose());
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
    Rcpp::Rcerr<<"retmat is "<<retmat.rows()<<" x "<<retmat.cols()<<std::endl;
    Rcpp::Rcerr<<"selection is  is "<<n_elem[0]<<" x "<<n_elem[1]<<std::endl;
    Rcpp::stop("retmat must have same number of elements as dataset selection");
  }
  if(doTranspose){
    if constexpr(Options==Eigen::RowMajor){
      Eigen::Map<Eigen::Matrix<T,RN,CN,Eigen::ColMajor> > tretmat(retmat.data(),retmat.cols(),retmat.rows());
      selection.read(tretmat);
      flipMat(tretmat);
      }else{
      if constexpr(Options==Eigen::ColMajor && CN!=1){
	  Eigen::Map<Eigen::Matrix<T,RN,CN,Eigen::RowMajor> > tretmat(retmat.data(),retmat.cols(),retmat.rows());
	  selection.read(tretmat);
	  flipMat(tretmat);
	}
    }
  }else{
    selection.read(retmat);
    flipMat(retmat);
  }
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
    Rcpp::Rcerr<<"retmat is "<<retmat.rows()<<" x "<<retmat.cols()<<std::endl;
    Rcpp::Rcerr<<"selection is  is "<<n_elem[0]<<" x "<<n_elem[1]<<std::endl;
    Rcpp::stop("retmat must have same number of elements as dataset selection in WriteEigen");
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
      retmat.block(0,offset[0],1,chunksize[0]).rowwise().reverseInPlace();
    }else{
    retmat.block(offset[0],offset[1],chunksize[0],chunksize[1]).rowwise().reverseInPlace();
  }
}

