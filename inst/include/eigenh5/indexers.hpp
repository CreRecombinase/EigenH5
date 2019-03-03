#include <array>
#include <Rcpp.h>
#include <Rinternals.h>
// [[Rcpp::interfaces(r, cpp)]]
//#include <boost/variant/multivisitors.hpp>
#include "xtensor-r/rarray.hpp"
#include "xtensor-r/rtensor.hpp"
#include "xtensor-r/rvectorize.hpp"
#include "xtensor-r/rcontainer.hpp"
#include <xtensor/xarray.hpp>
#include <xtensor/xdynamic_view.hpp>
#include <xtensor/xview.hpp>
#include <range/v3/all.hpp>




class rep_chunk_indexer{
  const std::array<size_t,2> offset_size; //Logical offset/size of chunk on disk
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


inline constexpr size_t find_sub_offset(const size_t chunk_offset,const size_t sub_offset){
  // auto sub_end =sub_offset+sub_size-1;
  // const auto chunk_end=chunk_offset+chunk_size-1;

  if(sub_offset<=chunk_offset){ //If the overall selection starts before (or at the start of ) this chunk, it has to start at 0 for this chunk( we assume that this chunk overlaps with the selection)
    return 0;
  }

  return(sub_offset-chunk_offset); //If the overall selection starts after the start of this chunk, the start is simply the difference
}

inline constexpr size_t find_sub_size(const size_t chunk_offset,const size_t chunk_size, const size_t sub_offset, const size_t sub_size){
  auto sub_end =sub_offset+sub_size-1;
  const auto chunk_end=chunk_offset+chunk_size-1;

  if(sub_offset<=chunk_offset){ //If the overall selection starts before the beginning of this chunk
    if(sub_end>=chunk_end){//and ends after the end
      return chunk_size; //, then we return the whole chunk
    }
    //If the selection starts before the beginning but ends before the end, then the chunksize is simply the difference between the start of this chunk and the end of the selection
    return sub_end-chunk_offset+1;
  }

  if(sub_end>= chunk_end){//If the selection starts after the beginning of the selection and ends after the end
    return(chunk_end-sub_offset+1);
  }
  //If the selection starts after the beginning and ends before the end, then the sub_size is correct
  return sub_size;
}



class IndexParser{
public:
private:
  const int p;

  //  const int index_dimension;

  const int dimsize;
  const int chunksize;
  const int num_chunks;
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
  const std::array<size_t,2> offset_size;// Logical offset/size of chunk on disk
  const size_t sub_offset;
  const size_t sub_size;

  //  const std::array<size_t,2> sub_offset_size; // offset/size within chunk
  const size_t  dest_d_offset; //offset in source/dest memory
public:
  chunk_chunker(const std::array<size_t, 2> offset_size_,
                const std::array<int, 2> sub_offset_size_,
		const size_t dest_offset_)
    : offset_size(offset_size_),
      sub_offset(find_sub_offset(offset_size[0],static_cast<size_t>(sub_offset_size_[0]))),
      sub_size(find_sub_size(offset_size_[0],offset_size_[1],static_cast<size_t>(sub_offset_size_[0]),static_cast<size_t>(sub_offset_size_[1]))),
      dest_d_offset(dest_offset_){
  }

  size_t chunk_size() const noexcept{
    return(sub_size);
  }
  size_t chunk_offset() const noexcept{
    return(sub_offset);
  }
  auto chunk_slice() const{
    return(xt::range(chunk_offset(),chunk_offset()+chunk_size()));
  }
  auto mem_slice() const{
    return(xt::range(dest_offset(),dest_offset()+dest_size()));
  }

  size_t disk_offset() const noexcept{
    return offset_size[0];
  }

  size_t disk_size() const noexcept{
    return offset_size[1];
  }

  size_t dest_offset() const noexcept{
    return dest_d_offset;
  }

  size_t dest_size() const noexcept{
    return chunk_size();
  }

  int chunk_i(size_t i)const noexcept{
    return sub_offset+i;
  }

  int mem_i(size_t i)const noexcept{
    return dest_d_offset+i;
  }

};

class ChunkParser{
private:

  const int dimsize;
  const int chunksize;
  const int p;
  int chunks_used;
  std::vector<chunk_chunker> needed_chunks;

public:
  using	chunk_it =std::vector<chunk_chunker>::const_iterator;
  using	chunk_type=chunk_chunker;
  //  ChunkParser(
  ChunkParser(const size_t  dimsize_,const size_t chunksize_,const std::pair<int,int> offset_chunksize,bool whole_only=false):
    dimsize(dimsize_),
    chunksize(chunksize_),
    p((offset_chunksize.second<0) ? (dimsize-offset_chunksize.first) : offset_chunksize.second),
    chunks_used(ceilf(static_cast<float>(p)/static_cast<float>(chunksize)))

  {
    needed_chunks.reserve(chunks_used);
    int chunk_chunksize=offset_chunksize.second;
    int chunk_offset=offset_chunksize.first;

    if(chunk_chunksize<0){
      chunk_chunksize=dimsize-chunk_offset;
    }
    if(chunk_offset+chunk_chunksize>dimsize){
      //      Rcpp::Rcerr<<"in index_dimension"<<index_dimension_<<std::endl;
      Rcpp::Rcerr<<"indexing beyond the end of the dataset! ("<<chunk_offset<<"+"<<chunk_chunksize<<">"<<dimsize<<")"<<std::endl;
      Rcpp::stop("indexing error!");
    }
    int i=0;
    int	out_o=0;
    std::array<int, 2> sub_oa = {
				 chunk_offset,
				 chunk_chunksize
    };

    int chunk_start = chunk_offset-chunk_offset%chunksize;
    if(whole_only){
      if(chunk_offset%chunksize!=0){
	Rcpp::stop("chunk offset must be on a chunk boundary when whole_only set to true");
      }
      if(chunk_chunksize%chunksize!=0){
	Rcpp::stop("chunk_chunksize must be a multiple of chunksize when whole_only set to true");
      }
    }

    for(int tpo=chunk_start; tpo<(chunk_offset+chunk_chunksize); tpo+=chunksize){
      int chunk_no = tpo/chunksize;
      const size_t t_chunk_offset = static_cast<size_t>(chunk_no * chunksize);
      const size_t t_chunk_end = static_cast<size_t>(std::min<size_t>(static_cast<size_t>(dimsize),static_cast<size_t>(t_chunk_offset+chunksize-1)));
      needed_chunks.emplace_back(chunk_chunker({t_chunk_offset,t_chunk_end-t_chunk_offset+1},
									  sub_oa,
					       out_o));
      out_o+=needed_chunks[i].chunk_size();
#ifdef DEBUG
      if(whole_only){
	if(needed_chunks[i]->chunk_size()!=needed_chunks[i]->disk_size()){
	  Rcpp::Rcerr<<"chunk_size: "<<needed_chunks[i]->chunk_size()<<"\ndisk_size: "<<needed_chunks[i]->disk_size()<<"\n";
	  Rcpp::stop("chunksize must be equal to disk size");
	}
      }
#endif
      i++;
    }
    if(i!=chunks_used){
      Rcpp::stop("something has gone wrong with the chunk iteration");
    }
  }
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
  ChunkParser ( ChunkParser && ) = default;
  ChunkParser &  operator= ( ChunkParser && ) = default;
  ChunkParser ( const ChunkParser & ) = delete;
  ChunkParser & operator= ( const ChunkParser & ) = delete;
};


template<typename T>
struct t2chunk_t;

template<> struct t2chunk_t<std::pair<int,int> >{
  typedef ChunkParser c_type;
};

template<> struct t2chunk_t<Rcpp::IntegerVector >{
  typedef IndexParser c_type;
};


template<typename D>
struct xtm_t;// {
//   typedef	std::false_type retvec_type;
//   typedef	std::false_type retmat_type;
//   typedef	std::false_type reta_type;
//   typedef	std::false_type buffvec_type;
//   typedef	std::false_type buffmat_type;
//   typedef	std::false_type buffa_type;
// };

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
