#include <array>
#include <Rcpp.h>
#include <Rinternals.h>
// [[Rcpp::interfaces(r, cpp)]]
//[[Rcpp::plugins(cpp17)]]
//#include <boost/variant/multivisitors.hpp>
#include "highfive/H5DataSet.hpp"
#include "xtensor-r/rarray.hpp"
#include "xtensor-r/rtensor.hpp"
#include "xtensor-r/rvectorize.hpp"
#include "xtensor-r/rcontainer.hpp"
#include <cstddef>
#include <iterator>
#include <xtensor/xarray.hpp>
#include <xtensor/xdynamic_view.hpp>
#include <xtensor/xview.hpp>
#include <range/v3/all.hpp>
#include <optional>

class Interval{
  size_t offset;
  size_t size;
public:
  Interval(int offset_,int size_):offset(offset_),size(size_){}
  const size_t get_offset() const{return offset;}
  const size_t get_size() const{return size;}
};


class SubInterval{
  size_t offset;
  size_t size;
public:
  constexpr SubInterval(size_t offset_,int chunksize_):
    offset(offset_),
    size(std::max(chunksize_,0)){
    if(chunksize_<0){
      Rcpp::Rcerr<<offset<<"-"<<chunksize_<<std::endl;
      Rcpp::stop("Cannot specify negative offset in SubInterval constructor without specifying total_size_");
    }
  }
  constexpr SubInterval(std::pair<int,int> offset_chunksize):
    offset(offset_chunksize.first),
    size(std::max(offset_chunksize.second,0)){
    if(offset_chunksize.second<0){
      Rcpp::Rcerr<<offset<<"-"<<offset_chunksize.second<<std::endl;
      Rcpp::stop("Cannot specify negative offset in SubInterval constructor without specifying total_size_");
    }
  }

  constexpr SubInterval(std::pair<int,std::optional<int> > offset_chunksize,size_t total_size_):
    offset(offset_chunksize.first),
    size(size=offset_chunksize.second.value_or(total_size_-offset))
  {
    if(offset_chunksize.second.has_value()  && offset_chunksize.second<0){
      Rcpp::stop("negative chunksize has been deprecated in SubInterval");
    }
    if(offset+size > total_size_){
      Rcpp::Rcerr<<"offset_chunksize is: "<<offset_chunksize.first<<","<<*offset_chunksize.second<<std::endl;
      Rcpp::Rcerr<<"total_size_ is: "<<total_size_<<std::endl;
      Rcpp::Rcerr<<"SubInterval is: "<<offset<<"-"<<offset+size<<std::endl;
      Rcpp::Rcerr<<"Interval is: (0)-"<<total_size_<<std::endl;
      Rcpp::stop("SubInterval is out of bounds");
    }
  }
  constexpr SubInterval(int offset_,int chunksize_,size_t total_size_):
    offset(offset_),
    size(chunksize_){
    if(chunksize_<0){
      Rcpp::stop("negative chunksize has been deprecated in SubInterval");
    }
    if(offset+size >= total_size_){
      Rcpp::Rcerr<<"SubInterval is: "<<offset<<"-"<<offset+size<<std::endl;
      Rcpp::Rcerr<<"Interval is: (0)-"<<total_size_<<std::endl;
      Rcpp::stop("SubInterval is out of bounds");
    }

  }
  constexpr SubInterval chunk_selection(const SubInterval selection,const size_t chunksize) const noexcept {
    const auto chunk_sel_start = selection.offset-selection.offset%chunksize;
    const auto new_sel_size = std::min(SubInterval(chunk_sel_start,selection.size+(selection.offset-chunk_sel_start)).num_chunks(chunksize)*chunksize,size-chunk_sel_start);
    return SubInterval(chunk_sel_start,new_sel_size);
  }
  constexpr SubInterval chunk_i(const size_t i, const size_t chunksize) const noexcept{
    return(SubInterval(i*chunksize,std::min(size-(i*chunksize),chunksize)));
  }
  constexpr bool operator==(const SubInterval &y) const noexcept {
    return(y.size==size && y.offset==offset);
  }
  constexpr bool operator!=(const SubInterval &y) const noexcept {
    return(y.size!=size or y.offset !=offset);
  }


  // x.sub_chunk(y) returns the SubInterval of x that corresponds to it's overlap with y
  constexpr SubInterval sub_chunk(const SubInterval &y) const noexcept{

    // const auto sub_offset = y.offset;
    // const auto chunk_offset = size;

    const auto y_end = y.offset+y.size-1;
    const auto end = offset+size-1;
    if(y.offset<offset){
      return SubInterval(0,y_end>=end ? size : y_end-offset+1);
    }
    if(y_end>end){
      return SubInterval(y.offset-offset,end-(y.offset-offset)+1);
    }
    return SubInterval(y.offset-offset, y.size);
  }

  constexpr size_t get_offset() const noexcept{
    return offset;
  }
  constexpr size_t get_size() const noexcept{
    return size;
  }
  constexpr size_t get_back() const noexcept{
    return offset+size-1;
  }
  constexpr size_t num_chunks(const size_t chunksize)const noexcept{
    return 1 + ((size - 1) / chunksize);
  }
  friend class chunk_chunker;


  // static std::vector<SubInterval> make_chunked_selection( const SubInterval parent, const SubInterval selection,const size_t chunksize){





  //   //    const size_t


  // }

};




class rep_chunk_indexer{
  std::array<size_t,2> offset_size; //Logical offset/size of chunk on disk
  std::vector<int> chunk_indexes; //indexes within chunk
  std::vector<int> out_indexes; //indexes in source/dest memory
  using	i_it= std::vector<int>::const_iterator;
public:
  rep_chunk_indexer(const std::array<size_t,2> offset_size_):
    offset_size(offset_size_),
    chunk_indexes([](const size_t res_num){
		    std::vector<int> retv;
		    retv.reserve(res_num);
		    return(retv);
		  }(offset_size[1])),
    out_indexes([](const size_t res_num){
		  std::vector<int> retv;
		  retv.reserve(res_num);
		  return(retv);
		}(offset_size[1])){
  }
  int push_back(const int index,const int i) noexcept
  {
    chunk_indexes.push_back(index-offset_size[0]);
    out_indexes.push_back(i);
    return out_indexes.size();
  }
  size_t disk_offset()const noexcept{
    return offset_size[0];
  }
  size_t disk_size()const noexcept{
    return offset_size[1];
  }
  size_t chunk_size()const noexcept{
    return chunk_indexes.size();
  }
  auto chunk_slice() const{
    return xt::keep(chunk_indexes);
  }

  auto mem_slice() const{
    return xt::keep(out_indexes);
  }

  int chunk_i(size_t i)const noexcept{
    return chunk_indexes[i];
  }
  int mem_i(size_t i)const noexcept{
    return out_indexes[i];
  }
};




class IndexParser{
public:
private:
  int p;
  int dimsize;
  int chunksize;
  int num_chunks;
  std::vector<rep_chunk_indexer> needed_chunks;
  std::vector<std::unique_ptr<rep_chunk_indexer>> tneeded_chunks;

  using	chunk_it =std::vector<rep_chunk_indexer>::const_iterator;
  int chunks_used;
  bool is_unsorted;
  using Iv=Rcpp::IntegerVector;
  //  const
  size_t max_chunksize;

public:
  using chunk_type=rep_chunk_indexer;
  IndexParser(const size_t dimsize_,const size_t chunksize_,const Iv input,bool whole_only=false): p(input.size()),
												   dimsize(dimsize_),
												   chunksize(chunksize_),
												   num_chunks(ceilf(static_cast<float>(dimsize)/static_cast<float>(chunksize))),
												   tneeded_chunks(num_chunks),
												   chunks_used(0),
												   is_unsorted(false),
												   max_chunksize(0)
  {

    if(whole_only){
      Rcpp::stop("IndexParser not available when whole_only set to true");
    }

    // super easy to calculate which chunks we need
    int i=0;
    int oi=-1;
    int ix=0;
    for(auto inp: input){
      i=inp-1;
      if(i<=oi){
	is_unsorted=true;
      }

      int chunk_no = i/chunksize;
      if(chunk_no>=tneeded_chunks.size()){
	//	  Rcpp::Rcerr<<"in index_dimension"<<index_dimension_<<std::endl;
	Rcpp::Rcerr<<"indexing beyond the end of the dataset! ("<<inp<<">="<<dimsize<<")"<<std::endl;
	Rcpp::stop("indexing error!");
      }
      auto &it_el = tneeded_chunks[chunk_no];
      if(it_el==nullptr){

	it_el=std::unique_ptr<rep_chunk_indexer>(
              new rep_chunk_indexer({static_cast<size_t>(chunk_no * chunksize),
				     static_cast<size_t>(chunksize)}));
	chunks_used++;
		 // shared_ptr<Employee> TempEmp(new Employee());
      }
      int tc = it_el->push_back(i,ix);
      if(tc>max_chunksize){
	max_chunksize=tc;
      }
      oi=i;
      ix++;
    }


    tneeded_chunks.erase(std::remove_if(tneeded_chunks.begin(),tneeded_chunks.end(),[](const auto&  data_el){
										      return(data_el==nullptr);
										 }),tneeded_chunks.end());
    if(tneeded_chunks.size()!=chunks_used){
      Rcpp::Rcerr<<"counted chunks is: "<<chunks_used<<", but there are: "<<needed_chunks.size()<<" chunk indexers!"<<std::endl;
      Rcpp::stop("the last chunk is bad!");
    }
    needed_chunks.reserve(chunks_used);
    for(auto &a:tneeded_chunks){
      needed_chunks.emplace_back(std::move(*a));
    }
  }

  chunk_it begin() const noexcept{
    return(needed_chunks.cbegin());
  }
  chunk_it end() const noexcept{
    return(needed_chunks.cend());
  }

  int total_size()const noexcept{
    return p;
  }
  int total_chunksize() const noexcept{ return chunksize; }

  size_t max_read_chunksize() const noexcept{
    return max_chunksize;
  }

  size_t n_chunks() const noexcept{
    return chunks_used;
  }
  bool sorted() const noexcept{
    return !is_unsorted;
  }
  auto n_c() const{
    return ranges::view::all(needed_chunks);
  }
  IndexParser ( IndexParser && ) = default;
  IndexParser &  operator= ( IndexParser && ) = default;
  IndexParser ( const IndexParser & ) = delete;
  IndexParser & operator= ( const IndexParser & ) = delete;
};



class chunk_chunker{
  SubInterval global_interval;
  SubInterval relative_interval;
  //offset in source/dest memory
  //  const std::array<size_t,2> sub_offset_size; // offset/size within chunk
  size_t  dest_d_offset;
public:
  chunk_chunker(const SubInterval interval_,
                const SubInterval &chunk_,
		const size_t dest_offset_)
    : global_interval(interval_),
      relative_interval(global_interval.sub_chunk(chunk_)),
      dest_d_offset(dest_offset_){
  }

  size_t chunk_size() const noexcept{
    return(relative_interval.size);
  }
  size_t chunk_offset() const noexcept{
    return(relative_interval.offset);
  }
  auto chunk_slice() const{
    return(xt::range(chunk_offset(),chunk_offset()+chunk_size()));
  }
  auto mem_slice() const{
    return(xt::range(dest_offset(),dest_offset()+dest_size()));
  }

  size_t disk_offset() const noexcept{
    return global_interval.offset;
  }

  size_t disk_size() const noexcept{
    return global_interval.size;
  }

  size_t dest_offset() const noexcept{
    return dest_d_offset;
  }

  size_t dest_size() const noexcept{
    return chunk_size();
  }

  int chunk_i(size_t i)const noexcept{
    return relative_interval.offset+i;
  }

  int mem_i(size_t i)const noexcept{
    return dest_d_offset+i;
  }
};



class ChunkParser{
private:
  int dimsize;
  SubInterval selection;
  int chunksize;
  int p;
  int chunks_used;

  std::vector<chunk_chunker> needed_chunks;
public:
  using	chunk_it =std::vector<chunk_chunker>::const_iterator;
  using	chunk_type=chunk_chunker;
  ChunkParser(const size_t  dimsize_,const size_t chunksize_,const std::pair<int,std::optional<int>> selection_p,const bool whole_only=false):
    dimsize(dimsize_),
    selection(selection_p,dimsize),
    chunksize(chunksize_),
    p(selection.get_size())
  {
    const auto chunk_selection = SubInterval(0,dimsize).chunk_selection(selection,chunksize);
    if(whole_only){
      if(chunk_selection.get_offset()!=selection.get_offset()){
        Rcpp::stop("chunk offset must be on a chunk boundary when whole_only set to true");
      }
      if(selection.get_size()%chunksize!=0){
	Rcpp::stop("size of selection must be a multiple of chunksize when whole_only set to true");
      }
    }

    chunks_used = chunk_selection.num_chunks(chunksize);
    needed_chunks.reserve(chunks_used);
    int	out_o=0;
    for(int i=0; i<chunks_used; i++){
      auto &new_chunker = needed_chunks.emplace_back(chunk_selection.chunk_i(i,chunksize),
                                                    selection,
                                                    out_o);
      out_o+=new_chunker.chunk_size();
    }

    if(needed_chunks.size()!=chunks_used){
      // back_chunk.
      Rcpp::Rcerr<<"needed_chunks.size() is: "<<needed_chunks.size()<<" while chunks_used is :"<<chunks_used<<std::endl;
      Rcpp::Rcerr<<"needed_chunks is of size: "<<needed_chunks.size();
      Rcpp::stop("something has gone wrong with the chunk iteration");
    }
  }
  //  std::vector<

  chunk_it begin() const{
    return(needed_chunks.cbegin());
  }
  chunk_it end() const{
    return(needed_chunks.cend());
  }
  constexpr bool sorted() const{
    return true;
  }
  size_t total_size() const{
    return p;
  }
  size_t total_chunksize()const{
    return chunksize;
  }
  size_t n_chunks()const{
    return chunks_used;
  }
  size_t max_read_chunksize() const{
    return chunksize;
  }
  auto n_c() const{
    return(ranges::view::all(needed_chunks));
  }
};


template<typename T>
struct t2chunk_t;

template<> struct t2chunk_t<std::pair<int,std::optional<int>> >{
  typedef ChunkParser c_type;
};


template<> struct t2chunk_t<Rcpp::IntegerVector >{
  typedef IndexParser c_type;
};


template<typename D>
struct xtm_t;

template<>
struct xtm_t<int>{
  typedef	xt::rtensor<int,1> retvec_type;
  typedef	xt::rtensor<int,2> retmat_type;
  typedef	xt::rtensor<int,3> reta_type;
  typedef	xt::xtensor<int,1,xt::layout_type::row_major> buffvec_type;
  typedef	xt::xtensor<int,2,xt::layout_type::row_major> buffmat_type;
  typedef	xt::xtensor<int,3,xt::layout_type::row_major> buffa_type;
  typedef       Rcpp::IntegerVector rrvec_type;
  typedef       Rcpp::IntegerMatrix rrmat_type;
};

template<>
struct xtm_t<double>{
  typedef	xt::rtensor<double,1> retvec_type;
  typedef	xt::rtensor<double,2> retmat_type;
  typedef	xt::rtensor<double,3> reta_type;
  typedef	xt::xtensor<double,1,xt::layout_type::row_major> buffvec_type;
  typedef	xt::xtensor<double,2,xt::layout_type::row_major> buffmat_type;
  typedef	xt::xtensor<double,3,xt::layout_type::row_major> buffa_type;
  typedef       Rcpp::NumericVector rrvec_type;
  typedef       Rcpp::NumericMatrix rrmat_type;
};


template<>
struct xtm_t<std::string>{
  typedef	Rcpp::StringVector retvec_type;
  typedef	Rcpp::StringMatrix retmat_type;
  typedef	std::false_type reta_type;
  typedef	xt::xtensor<char,2,xt::layout_type::row_major> buffvec_type;
  typedef       xt::xtensor<char,3,xt::layout_type::row_major> buffmat_type;

  typedef	std::false_type buffa_type;
  typedef       Rcpp::StringVector rrvec_type;
  typedef       Rcpp::StringMatrix rrmat_type;
};



template<>
struct xtm_t<unsigned char>{
  typedef	xt::rtensor<unsigned char,1> retvec_type;
  typedef	xt::rtensor<unsigned char,2> retmat_type;
  typedef	xt::rtensor<unsigned char,3> reta_type;
  typedef	xt::xtensor<unsigned char,1,xt::layout_type::row_major> buffvec_type;
  typedef	xt::xtensor<unsigned char,2,xt::layout_type::row_major> buffmat_type;
  typedef	xt::xtensor<unsigned char,3,xt::layout_type::row_major> buffa_type;
  typedef       Rcpp::RawVector rrvec_type;
  typedef       Rcpp::RawMatrix rrmat_type;
};





template<typename D>
class DataSet_Context{

public:
  const HighFive::DataSet& d;
private:

  using tensor_type=typename xtm_t<D>::buffmat_type;
  const size_t elem_size;
  const std::vector<size_t> dataset_chunksizes;
  std::vector<std::byte> raw_chunk_buffer;
public:
  DataSet_Context(const HighFive::DataSet& d_):d(d_),elem_size(d.getDataType().n_elem()),dataset_chunksizes(d.getFilter().get_chunksizes()){
    auto raw_size=std::accumulate(dataset_chunksizes.begin(),dataset_chunksizes.end(),sizeof(typename tensor_type::value_type)*elem_size,std::multiplies<size_t>());
    raw_chunk_buffer.resize(raw_size);
  }

  template<class TA,class LC>
  void write_dataset(const TA& tchunk_r, LC& lambda){
    auto res = lambda(tchunk_r,raw_chunk_buffer.data(),raw_chunk_buffer.size(),elem_size);
    d.write_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset()},res);
  }

  template<class TA, class TB,class LC>
  void write_dataset(const TA& tchunk_r, const TB& tchunk_c, LC& lambda){
    auto res = lambda(tchunk_r,tchunk_c,raw_chunk_buffer.data(),raw_chunk_buffer.size(),elem_size);
    d.write_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset(),tchunk_c.disk_offset()},res);
  }

  template<class TA, class TB,class TC,class LC>
  void write_dataset(const TA& tchunk_r, const TB& tchunk_c,const TC& tchunk_a,  LC& lambda){
    auto res = lambda(tchunk_r,tchunk_c,tchunk_a,raw_chunk_buffer.data(),raw_chunk_buffer.size(),elem_size);
    d.write_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset(),tchunk_c.disk_offset(),tchunk_a.disk_offset()},res);
  }


  template<class TA,class LC>
  void read_dataset(const TA& tchunk_r, LC& lambda){
    auto res = d.read_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset()});
    lambda(tchunk_r,raw_chunk_buffer.data(),res,elem_size);
  }

  template<class TA, class TB,class LC>
  void read_dataset(const TA& tchunk_r, const TB& tchunk_c, LC& lambda){
    auto res = d.read_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset(),tchunk_c.disk_offset()});
    lambda(tchunk_r,tchunk_c,raw_chunk_buffer.data(),res,elem_size);

  }

  template<class TA, class TB,class TC,class LC>
  void read_dataset(const TA& tchunk_r, const TB& tchunk_c,const TC& tchunk_a, LC& lambda){
    auto res = d.read_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset(),tchunk_c.disk_offset(),tchunk_a.disk_offset()});
    lambda(tchunk_r,tchunk_c,tchunk_a,raw_chunk_buffer.data(),res,elem_size);
  }


  template<class RngA,class LC>
  void iter_dataset_read(const RngA& RowChunksBegin,const RngA& RowChunksEnd,LC& lambda){
    for(auto &tchunk_r = RowChunksBegin; tchunk_r!=RowChunksEnd; ++tchunk_r){
      read_dataset(tchunk_r,lambda);
    }
  }

  template<class RngA, class RngB,class LC>
    void iter_dataset_read(const RngA& RowChunksBegin,const RngA& RowChunksEnd, const RngB& ColChunksBegin,const RngB& ColChunksEnd,LC& lambda){
    for(auto &tchunk_r = RowChunksBegin; tchunk_r!=RowChunksEnd; ++tchunk_r){
      for(auto &tchunk_c = ColChunksBegin; tchunk_c!=ColChunksEnd; ++tchunk_c){
	read_dataset(tchunk_r,tchunk_c,lambda);
      }
    }
  }



  template<class RngA, class RngB,class RngC,class LC>
  void iter_dataset_read(const RngA& RowChunksBegin,const RngA& RowChunksEnd, const RngB& ColChunksBegin,const RngB& ColChunksEnd,const RngC& AChunksBegin,const RngC& AChunksEnd,LC& lambda){
    for(auto &tchunk_r = RowChunksBegin; tchunk_r!=RowChunksEnd; ++tchunk_r){
      for(auto &tchunk_c = ColChunksBegin; tchunk_c!=ColChunksEnd; ++tchunk_c){
	for(auto &tchunk_a = AChunksBegin; tchunk_a!=AChunksEnd; ++tchunk_a){
	  read_dataset(tchunk_r,tchunk_c,tchunk_a,lambda);
	}
      }
    }
  }

  template<class RngA,class LC>
  void iter_dataset_write(const RngA& RowChunksBegin,const RngA& RowChunksEnd,LC& lambda){
    for(auto &tchunk_r = RowChunksBegin; tchunk_r!=RowChunksEnd; ++tchunk_r){
      write_dataset(tchunk_r,lambda);
    }
  }


  template<class RngA, class RngB,class LC>
  void iter_dataset_write(const RngA& RowChunksBegin,const RngA& RowChunksEnd, const RngB& ColChunksBegin,const RngB& ColChunksEnd,LC& lambda){
    for(auto &tchunk_r = RowChunksBegin; tchunk_r!=RowChunksEnd; ++tchunk_r){
      for(auto &tchunk_c = ColChunksBegin; tchunk_c!=ColChunksEnd; ++tchunk_c){
	write_dataset(tchunk_r,tchunk_c,lambda);
      }
    }
  }

  template<class RngA, class RngB,class RngC,class LC>
  void iter_dataset_write(const RngA& RowChunksBegin,const RngA& RowChunksEnd, const RngB& ColChunksBegin,const RngB& ColChunksEnd,const RngC& AChunksBegin,const RngC& AChunksEnd,LC& lambda){
    for(auto &tchunk_r = RowChunksBegin; tchunk_r!=RowChunksEnd; ++tchunk_r){
      for(auto &tchunk_c = ColChunksBegin; tchunk_c!=ColChunksEnd; ++tchunk_c){
	for(auto &tchunk_a = AChunksBegin; tchunk_a!=AChunksEnd; ++tchunk_a){
	  write_dataset(tchunk_r,tchunk_c,tchunk_a,lambda);
	}
      }
    }
  }

};
