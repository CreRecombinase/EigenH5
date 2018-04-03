#pragma once

#include <boost/range/adaptor/indexed.hpp>


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


// template<Rng>
// struct cont_range
//   : ranges::view_facade<cont_range<Rng> >
// {
// private:
//   friend struct ranges::range_access;
//   boost::indexed_range<decltype(Rng)> Rng_;
//   struct cursor
//   {
//   private:
//     Rcpp::IntegerVector::const_iterator iter;
//   public:
//     cursor() = default;
//     cursor(Rcpp::IntegerVector::const_iterator it): iter(it){}
//     int const & read() const{
//       return *iter;
//     }
//     bool equal(cursor const &that) const
//     {
//       return iter == that.iter;
//     }
//     void next()
//     {
//       ++iter;
//     }
//     void prev()
//     {
//       --iter;
//     }
//     std::ptrdiff_t distance_to(cursor const &that) const
//     {
//       return that.iter - iter;
//     }
//     void advance(std::ptrdiff_t n)
//     {
//       iter += n;
//     }
//   };
//   cursor begin_cursor() const
//   {
//     return {vec_.begin()};
//   }
//   cursor end_cursor() const
//   {
//     return {vec_.end()};
//   }
// public:
//   IntegerVector_range()
//     : vec_()
//   {}
//   IntegerVector_range(Rcpp::IntegerVector vec):vec_(vec){};

// };


// namespace ranges
// {
// inline namespace v3
// {
// template<class Rng>
// class cont_view
//   :public ranges::view_facade<cont_view<Rng>,is_finite<Rng>::value ? finite : range_cardinality<Rng>::value >

// }
// }



// namespace ranges
// {
//     inline namespace v3
//     {
//         // TODO group_by could support Input ranges by keeping mutable state in
//         // the range itself. The group_by view would then be mutable-only and
//         // Input.

//         /// \addtogroup group-views
//         /// @{
//         template<typename Rng>
//         struct group_cont_view
//           : view_facade<
// 	  group_cont_view<Rng>,
// 	  is_finite<Rng>::value ? finite : range_cardinality<Rng>::value>
//         {
//         private:
//             friend range_access;
//             Rng rng_;
//             template<bool IsConst>
//             struct cursor
//             {
//             private:
//                 friend range_access;
// 	      friend group_by_view;
// 	      iterator_t<Rng> cur_;
// 	      sentinel_t<Rng> last_;

//                 struct pred
//                 {
//                     iterator_t<Rng> first_;
//                     semiregular_ref_or_val_t<Fun, IsConst> fun_;
//                     bool operator()(range_reference_t<Rng> ref) const
//                     {
//                         return invoke(fun_, *first_, ref);
//                     }
//                 };
//                 take_while_view<iterator_range<iterator_t<Rng>, sentinel_t<Rng>>, pred> read() const
//                 {
//                     return {{cur_, last_}, {cur_, fun_}};
//                 }
//                 void next()
//                 {
//                     cur_ = find_if_not(cur_, last_, pred{cur_, fun_});
//                 }
//                 bool equal(default_sentinel) const
//                 {
//                     return cur_ == last_;
//                 }
//                 bool equal(cursor const &that) const
//                 {
//                     return cur_ == that.cur_;
//                 }
//                 cursor(semiregular_ref_or_val_t<Fun, IsConst> fun, iterator_t<Rng> first,
//                     sentinel_t<Rng> last)
//                   : cur_(first), last_(last), fun_(fun)
//                 {}
//             public:
//                 cursor() = default;
//             };
//             cursor<false> begin_cursor()
//             {
//                 return {fun_, ranges::begin(rng_), ranges::end(rng_)};
//             }
//             CONCEPT_REQUIRES(Invocable<Fun const&, range_common_reference_t<Rng>,
//                 range_common_reference_t<Rng>>() && Range<Rng const>())
//             cursor<true> begin_cursor() const
//             {
//                 return {fun_, ranges::begin(rng_), ranges::end(rng_)};
//             }
//         public:
//             group_by_view() = default;
//             group_by_view(Rng rng, Fun fun)
//               : rng_(std::move(rng))
//               , fun_(std::move(fun))
//             {}
//         };

//         namespace view
//         {
//             struct group_by_fn
//             {
//             private:
//                 friend view_access;
//                 template<typename Fun>
//                 static auto bind(group_by_fn group_by, Fun fun)
//                 RANGES_DECLTYPE_AUTO_RETURN
//                 (
//                     make_pipeable(std::bind(group_by, std::placeholders::_1, std::move(fun)))
//                 )
//             public:
//                 template<typename Rng, typename Fun>
//                 using Concept = meta::and_<
//                     ForwardRange<Rng>,
//                     IndirectRelation<Fun, iterator_t<Rng>>>;

//                 template<typename Rng, typename Fun,
//                     CONCEPT_REQUIRES_(Concept<Rng, Fun>())>
//                 group_by_view<all_t<Rng>, Fun> operator()(Rng && rng, Fun fun) const
//                 {
//                     return {all(static_cast<Rng&&>(rng)), std::move(fun)};
//                 }

//             #ifndef RANGES_DOXYGEN_INVOKED
//                 template<typename Rng, typename Fun,
//                     CONCEPT_REQUIRES_(!Concept<Rng, Fun>())>
//                 void operator()(Rng &&, Fun) const
//                 {
//                     CONCEPT_ASSERT_MSG(ForwardRange<Rng>(),
//                         "The object on which view::group_by operates must be a model of the "
//                         "ForwardRange concept.");
//                     CONCEPT_ASSERT_MSG(IndirectRelation<Fun, iterator_t<Rng>>(),
//                         "The function passed to view::group_by must be callable with two arguments "
//                         "of the range's common reference type, and its return type must be "
//                         "convertible to bool.");
//                 }
//             #endif
//             };

//             /// \relates group_by_fn
//             /// \ingroup group-views
//             RANGES_INLINE_VARIABLE(view<group_by_fn>, group_by)
//         }
//         /// @}
//     }
// }





std::vector<int> get_dims(const Rcpp::RObject m);







struct dim_sel{

public:
  int in_start;
  int in_stop;
  int out_start;
  int out_stop;
  bool sorted;
  int chunksize;


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


  dim_sel(const int in_start_,const int in_stop_,const int out_start_,const int out_stop_,const int dimsize){
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
  dim_sel(const int offset, const int chunksize_,const int dimsize):chunksize(chunksize_){
    if(offset<0){
      in_start=dimsize-offset;
    }else{
      in_start=offset;
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
    in_start=0,
    in_stop=0,
    out_start=0,
    out_stop=0,
    chunksize=1,
    sorted=true{}
};

class DimRange{

public:
  const std::vector<dim_sel> dim_sels;
  DimRange(std::vector<dim_sel> &dim_sels_):
    dim_sels(std::move(dim_sels_)),
    n_elem(dim_sels.back().out_stop+1),
    isCompact_(dim_sels.size()==1)
  {}
  // static DimRange createDimRange(const Rcpp::IntegerVector subset_dim,const size_t tot_dimsize,const int offset,const int chunksize){
  //   if(
  DimRange(Rcpp::IntegerVector::const_iterator itb, Rcpp::IntegerVector::const_iterator ite):
    dim_sels(find_cont(itb,ite)),
    n_elem(dim_sels.back().out_stop+1),
    isCompact_(dim_sels.size()==1)
  {}
  DimRange(const dim_sel dim_sel_):
    dim_sels({dim_sel_}),
    n_elem(dim_sels.back().out_stop+1),
    isCompact_(dim_sels.size()==1)
  {}
  DimRange():
    dim_sels({dim_sel()}),
    n_elem(dim_sels.back().out_stop+1),
    isCompact_(true){}

  size_t get_n_elem() const{
    return(n_elem);
  }
  size_t get_num_selections() const{
    return(dim_sels.size());
  }
  bool isCompact()const{
    return(isCompact_);
  }
private:
  const size_t n_elem;
  const bool isCompact_;
  static std::vector<dim_sel> find_cont(Rcpp::IntegerVector::const_iterator itb,Rcpp::IntegerVector::const_iterator ite);
};


template<size_t Dims>
class CompactSelection{
  const std::array<dim_sel,Dims> selections;
public:
  CompactSelection(const std::array<dim_sel,Dims> &dim_sels_):selections(dim_sels_){
  }
  std::vector<size_t> get_chunksizes()const;
  std::array<boost::multi_array_types::index_range,Dims> get_ranges()const;
  std::vector<size_t> get_offsets()const;
  std::vector<size_t> get_output_offsets()const;
  std::vector<bool> get_sorted()const;
  int get_total_size()const;

  template<typename T, int RM,int RN,int CN>
  Eigen::Matrix<T,RN,CN,RM> readEigen(HighFive::DataSet &dset) const;

  template<typename T, int RM,int RN,int CN >
  void writeEigen(HighFive::DataSet &dset,Eigen::Map<Eigen::Matrix<T,RN,CN,RM> > tref) const;

  template<typename T, int RM,int RN,int CN>
  void readEigenBlock(HighFive::DataSet &dset,Eigen::Map<Eigen::Matrix<T,RN,CN,RM> > &retmat)const ;

  template<typename T, int RM,int RN,int CN>
  void writeEigenBlock(HighFive::DataSet &dset,Eigen::Map<Eigen::Matrix<T,RN,CN,RM> > &retmat)const;
};



class DatasetSelection{
public:

  std::array<int,2> dataset_dimensions;
  DimRange row_sels;
  DimRange col_sels;
  //  const std::array<std::vector<dim_sel>,2>  dim_sels;

  const size_t num_sel;

  std::array<size_t,2> n_elem;
  const size_t Dims;
  const bool isVector;
  DatasetSelection (DimRange row_range, DimRange col_range, std::vector<int> dataset_dimensions_);
  DatasetSelection (DimRange vec_range, const int dataset_dimensions_);

  template<typename T,int Options,int RN,int CN> void readEigen(HighFive::DataSet &dset,
						  Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const;
  template<typename T,int Options,int RN,int CN> void writeEigen(HighFive::DataSet &dset,
						   Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const;

  template<typename T> HighFive::DataSet createDataset(HighFive::Group &grp,const std::string dataname,std::vector<size_t> chunk_dims)const;

};

template<typename T,int Options,int RN,int CN>
inline void DatasetSelection::readEigen(HighFive::DataSet &dset,
					Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const {
  if(Dims>2){
      Rcpp::stop("readEigen cannot be used when dimensions are greater than 2");
    }
    const size_t elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<size_t>());
    if(elem_total!=retmat.size()){
      Rcpp::Rcerr<<"retmat is "<<retmat.size()<<" and elem_total is "<<elem_total<<std::endl;
      Rcpp::stop("retmat must have same number of elements as dataset selection");
    }
    const std::array<size_t,2> n_sel ({row_sels.get_num_selections(),col_sels.get_num_selections()});
    for(size_t i=0; i<n_sel[0]; i++){
      for(size_t j=0; j<n_sel[1];j++){
	CompactSelection<2> temp_sel({row_sels.dim_sels[i],col_sels.dim_sels[j]});
	temp_sel.readEigenBlock(dset,retmat);
      }
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

template<typename T,int Options,int RN,int CN>
inline void DatasetSelection::writeEigen(HighFive::DataSet &dset,
					 Eigen::Map<Eigen::Matrix<T,RN,CN,Options> >& retmat)const {
  if(Dims!=2){
    Rcpp::stop("readEigen cannot be used when dimensions are different than 2");
  }
  const size_t elem_total= std::accumulate(n_elem.begin(),n_elem.end(),1,std::multiplies<size_t>());
  if(elem_total!=retmat.size()){
    Rcpp::Rcerr<<"retmat is "<<retmat.size()<<" and elem_total is "<<elem_total<<std::endl;
    Rcpp::stop("retmat must have same number of elements as dataset selection");
  }
  const std::array<size_t,2> n_sel ({row_sels.get_num_selections(),col_sels.get_num_selections()});
  for(int i=0; i<n_sel[0]; i++){
    for(int j=0; j<n_sel[1];j++){
      CompactSelection<2> temp_sel({row_sels.dim_sels[i],col_sels.dim_sels[j]});
      temp_sel.writeEigenBlock(dset,retmat);
    }
  }


}

template<typename T>
inline HighFive::DataSet DatasetSelection::createDataset(HighFive::Group &grp,const std::string dataname,std::vector<size_t> chunk_dims)const{


using namespace HighFive;
 std::vector<size_t> mat_dims={static_cast<size_t>(n_elem[0]),static_cast<size_t>(n_elem[1])};
  if (chunk_dims.empty()) {
    const size_t MAX_CHUNK = 1024*1024;
    const size_t chunk_rows = static_cast<size_t>(std::min(static_cast<double>(mat_dims[0]),std::ceil(static_cast<double>(MAX_CHUNK)/static_cast<double>(mat_dims[1]))));
    //Rcpp::Rcout<<"chunk_dims: "<<chunk_dims[0]<<" , "<<chunk_dims[1]<<std::endl;
    chunk_dims={chunk_rows, static_cast<size_t>(mat_dims[1])};
  }
  // Create a new file using the default property lists.
  // if(doTranspose){
  //   Rcpp::Rcout<<"transpose!"<<std::endl;
  // }
  Filter filter(chunk_dims, FILTER_BLOSC, 0,false);
  // Create a dataset with double precision floating points


  DataSpace ds = DataSpace(mat_dims,false);
  return(grp.createDataSet(dataname, ds, AtomicType<T>(), filter.getId(), false));


}


template<size_t Dims>
inline std::vector<size_t> CompactSelection<Dims>::get_chunksizes()const {
  std::vector<size_t> retvec(Dims);
  for(int i=0; i<Dims;i++){
    retvec[i]=selections[i].chunksize;
  }
  return(retvec);
}

template<size_t Dims>
inline std::array<boost::multi_array_types::index_range,Dims> CompactSelection<Dims>::get_ranges()const {
  std::array<boost::multi_array_types::index_range,Dims> tranges;
  for(int i=0; i<Dims;i++){
    tranges[i]=boost::multi_array_types::index_range().start(selections[i].out_start).finish(selections[i].out_stop);
  }
  return(tranges);
}

template<size_t Dims>
inline std::vector<size_t> CompactSelection<Dims>::get_offsets()const {
  std::vector<size_t> retvec(Dims);
  for(int i=0; i<Dims;i++){
    retvec[i]=selections[i].in_start;
  }
  return(retvec);
}

template<size_t Dims>
inline std::vector<size_t> CompactSelection<Dims>::get_output_offsets()const {
  std::vector<size_t> retvec(Dims);
  for(int i=0; i<Dims;i++){
    retvec[i]=selections[i].out_start;
  }
  return(retvec);
}

template<size_t Dims>
inline std::vector<bool> CompactSelection<Dims>::get_sorted()const {
  std::vector<bool> retvec(Dims);
  for(int i=0; i<Dims;i++){
    retvec[i]=selections[i].sorted;
  }
  return(retvec);
}


template<size_t Dims>
inline int CompactSelection<Dims>::get_total_size()const{
  int ts=1;
  for(int i=0; i<Dims;i++){
    ts*=selections[i].chunksizes;
  }
  return(ts);
}


template<size_t Dims>
template<typename T, int RM,int RN,int CN>
inline Eigen::Matrix<T,RN,CN,RM> CompactSelection<Dims>::readEigen(HighFive::DataSet &dset)const {
    using namespace HighFive;
    auto chunksizes=get_chunksizes();
    Eigen::Matrix<T,RN,CN,RM> tref(chunksizes[0],chunksizes[1]);
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




template<size_t Dims>
template<typename T, int RM,int RN,int CN>
  inline void CompactSelection<Dims>::writeEigen(HighFive::DataSet &dset,Eigen::Map<Eigen::Matrix<T,RN,CN,RM> > tref) const{
    using namespace HighFive;
    auto chunksizes=get_chunksizes();
    //  Eigen::MatrixXd tref(chunksizes[0],chunksizes[1]);
    auto is_sorted=get_sorted();
    if(!is_sorted[0]){
      tref.rowwise().reverse();
    }
    if(!is_sorted[1]){
      tref.colwise().reverse();
    }
    dset.selectEigen(get_offsets(),chunksizes,{}).write(tref);
  }


template<size_t Dims>
template<typename T, int RM,int RN,int CN >
inline void CompactSelection<Dims>::readEigenBlock(HighFive::DataSet &dset,Eigen::Map<Eigen::Matrix<T,RN,CN,RM> > &retmat)const {
    auto tdim_sizes=get_chunksizes();
    auto out_starts=get_output_offsets();
    retmat.block(out_starts[0],out_starts[1],tdim_sizes[0],tdim_sizes[1])=readEigen<T,RM>(dset);
  }


template<size_t Dims>
template<typename T, int RM,int RN,int CN>
inline void CompactSelection<Dims>::writeEigenBlock(HighFive::DataSet &dset,Eigen::Map<Eigen::Matrix<T,RN,CN,RM> > &retmat)const {
    auto tdim_sizes=get_chunksizes();
    auto out_starts=get_output_offsets();
    Eigen::Matrix<T,RN,CN,RM>  tempm=retmat.block(out_starts[0],out_starts[1],tdim_sizes[0],tdim_sizes[1]);
    Eigen::Map<Eigen::Matrix<T,RN,CN,RM> > tref(tempm.data(),tdim_sizes[0],tdim_sizes[1]);
    writeEigen(dset,tref);
  }


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
