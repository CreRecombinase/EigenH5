#include <algorithm>
#include <array>
#include "xtensor-r/rarray.hpp"
// [[Rcpp::interfaces(r, cpp)]]
//[[Rcpp::plugins(cpp17)]]
//#include <boost/variant/multivisitors.hpp>
#include <cstddef>
#include <iterator>
#include <xtensor/xarray.hpp>
#include <xtensor/xdynamic_view.hpp>
#include <xtensor/xview.hpp>
#include <range/v3/view/all.hpp>
#include <optional>

class Interval{
  size_t offset;
  size_t size;
public:
  Interval(int offset_,int size_):offset(offset_),size(size_){}
  const size_t get_offset() const{return offset;}
  const size_t get_size() const{return size;}
};

#ifdef DEBUGNE
#define DNE
#else
#define DNE noexcept
#endif


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
  constexpr SubInterval chunk_selection(const SubInterval selection,const size_t chunksize) const DNE {
    const auto chunk_sel_start = selection.closest_boundary_below(chunksize);
    const auto chunk_sel_stop = selection.closest_boundary_above(chunksize);
    return truncate_back(front_back(chunk_sel_start,chunk_sel_stop));
  }
  constexpr static SubInterval front_back(const size_t front,const size_t back) DNE{
    return(SubInterval(front,back-front+1));
  }
  constexpr size_t closest_boundary_below(const size_t chunksize) const DNE{
    return(offset-(offset%chunksize));
  }
  constexpr size_t closest_boundary_above(const size_t chunksize) const DNE{
    return(chunksize*num_chunks_total(chunksize)-1);
  }

  constexpr SubInterval new_offset(const size_t offset_) const DNE{
    const auto new_size = offset_ > offset ? size -(offset_-offset) : size + (offset-offset_);
    return(SubInterval(offset_,new_size));
  }
  constexpr SubInterval new_back(const size_t back) const DNE{
    const auto new_size = back-offset+1;
    return(SubInterval(offset,new_size));
  }
  constexpr SubInterval truncate_back(const SubInterval &other) const DNE{
    if(other.get_back()>get_back()){
      return other.new_back(get_back());
    }
    return other;
  }
  constexpr SubInterval chunk_i(const size_t i, const size_t chunksize) const DNE{
    return(SubInterval(offset+i*chunksize,std::min(size-(i*chunksize),chunksize)));
  }
  constexpr bool operator==(const SubInterval &y) const DNE {
    return(y.size==size && y.offset==offset);
  }
  constexpr bool operator!=(const SubInterval &y) const DNE {
    return(y.size!=size or y.offset !=offset);
  }


  //! Returns the SubInterval of x that corresponds to overlap with y
  constexpr SubInterval sub_chunk(const SubInterval &y) const DNE{

    // const auto sub_offset = y.offset;
    // const auto chunk_offset = size;

    const auto y_end = y.offset+y.size-1;
    const auto end = offset+size-1;
    if(y.offset<offset){
      return SubInterval(0,y_end>=end ? size : y_end-offset+1);
    }
    if(y_end>end){
      return SubInterval(y.offset-offset,size-(y.offset-offset));
    }
    return SubInterval(y.offset-offset, y.size);
  }

  constexpr size_t get_offset() const DNE{
    return offset;
  }
  constexpr size_t get_size() const DNE{
    return size;
  }
  constexpr size_t get_back() const DNE{
    return offset+size-1;
  }
  constexpr size_t num_chunks_total(const size_t chunksize)const DNE{
    return 1 + ((size+offset - 1) / chunksize);
  }
  constexpr size_t num_chunks(const size_t chunksize)const DNE{
    return 1 + ((size - 1) / chunksize);
  }
  friend class chunk_chunker;
  // friend std::ostream& operator<<(std::ostream& os, const SubInterval& dt);
  friend std::ostream& operator<<(std::ostream& os, const SubInterval& dt){
  os<<dt.get_offset()<<"-"<<dt.get_back();
  return os;
}


};


template<typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& dt){
  if(dt.size()<=5){
    std::for_each(dt.begin(),dt.end(),[&](int i){
                                        os<<i<<",";
                                      });
    return os;
  }

  std::for_each(dt.begin(),dt.begin()+5,[&](int i){
                                          os<<i<<",";
                                        });
  os<<"...";
  return os;
}


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
  int push_back(const int index,const int i) DNE
  {
    chunk_indexes.push_back(index-offset_size[0]);
    out_indexes.push_back(i);
    return out_indexes.size();
  }
  size_t chunk_offset() const DNE {
    return(static_cast<size_t>(*std::min_element(chunk_indexes.begin(),chunk_indexes.end())));
  }
  size_t disk_offset()const DNE{
    return offset_size[0];
  }
  size_t disk_size()const DNE{
    return offset_size[1];
  }
  size_t chunk_size()const DNE{
    return chunk_indexes.size();
  }
  auto chunk_slice() const{
    return xt::keep(chunk_indexes);
  }

  auto mem_slice() const{
    return xt::keep(out_indexes);
  }

  int chunk_i(size_t i)const DNE{
    return chunk_indexes[i];
  }
  int mem_i(size_t i)const DNE{
    return out_indexes[i];
  }
  // friend std::ostream& operator<<(std::ostream& os, const rep_chunk_indexer& dt);
  friend std::ostream& operator<<(std::ostream& os, const rep_chunk_indexer& dt){
    auto chunk_p =std::minmax_element(dt.chunk_indexes.begin(),dt.chunk_indexes.end());
    auto out_p =std::minmax_element(dt.out_indexes.begin(),dt.out_indexes.end());

    os<<"rep_chunk_indexer: chunk_slice"<<*chunk_p.first<<"-"<<*chunk_p.second<<" mem_slice: "<<*out_p.first<<"-"<<*out_p.second;
  return os;
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

  chunk_it begin() const DNE{
    return(needed_chunks.cbegin());
  }
  chunk_it end() const DNE{
    return(needed_chunks.cend());
  }

  int total_size()const DNE{
    return p;
  }
  int total_chunksize() const DNE{ return chunksize; }

  size_t max_read_chunksize() const DNE{
    return max_chunksize;
  }

  size_t n_chunks() const DNE{
    return chunks_used;
  }
  bool sorted() const DNE{
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
  SubInterval disk_interval;
  SubInterval chunk_interval;
  //offset in source/dest memory
  //  const std::array<size_t,2> sub_offset_size; // offset/size within chunk
  size_t  dest_d_offset;
public:
  constexpr chunk_chunker(const SubInterval interval_,
                const SubInterval &chunk_,
		const size_t dest_offset_)
    : disk_interval(interval_),
      chunk_interval(disk_interval.sub_chunk(chunk_)),
      dest_d_offset(dest_offset_){
  }

  size_t chunk_size() const DNE{
    return(chunk_interval.size);
  }
  size_t chunk_offset() const DNE{
    return(chunk_interval.offset);
  }
  auto chunk_slice() const{
    return(xt::range(chunk_offset(),chunk_offset()+chunk_size()));
  }
  auto mem_slice() const{
    return(xt::range(dest_offset(),dest_offset()+dest_size()));
  }

  size_t disk_offset() const DNE{
    return disk_interval.offset;
  }

  size_t disk_size() const DNE{
    return disk_interval.size;
  }

  size_t dest_offset() const DNE{
    return dest_d_offset;
  }

  size_t dest_size() const DNE{
    return chunk_size();
  }

  int chunk_i(size_t i)const DNE{
    return chunk_interval.offset+i;
  }

  int mem_i(size_t i)const DNE{
    return dest_d_offset+i;
  }
  friend std::ostream& operator<<(std::ostream& os, const chunk_chunker& dt)
{
  os << "chunk_chunker: "<<"chunk_slice: "<<dt.chunk_interval<<" mem_slice: "<<SubInterval(dt.dest_offset(),dt.dest_size())<<" dest_d_offset: "<<dt.dest_d_offset;
  return os;
}
  // friend std::ostream& operator<<(std::ostream& os, const chunk_chunker& dt);
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
