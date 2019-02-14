#pragma once
#include "highfive/highfive.hpp"
#include "boost/optional.hpp"
#include <RcppEigen.h>
#include <cmath>
#include <Rcpp/Vector.h>
#include <Rcpp.h>
#include <zstd.h>


//typedef std::set<int> Set;
//Two types of chunk reader
//1 vector of indices
//Pros
//  * Allows for more or less arbitrary subsetting of the data
//  * Can easily express arbitrary indexing (out of order, repeats, etc.)
//  * Only requires one traversal of the _entire_ index vector (and one more per chunk)
//Cons
//  * Eigen does not (currently) support arbitrary indexing
//  * There is _possibly_ some performance overhead when the indexes are actually chunked
//  * There is a relatively high storage overhead when indexes actually are chunked
// class chunk_indexer{
//   const std::array<size_t,2> offset_size;
//   std::set<int> indexes;
//   using set_it=std::set<int>::const_iterator;
//   set_it last_v;
//   int elem_ct;
// public:
//   chunk_indexer(const std::array<size_t,2> offset_size_):
//     offset_size(offset_size_),
//     indexes(),
//     last_v(indexes.begin()),
//     elem_ct(0){
//   }
//    int push_back(const int index){
//     indexes.insert(index-offset_size[0],last_v);
//     elem_ct++;
//     return(index-offset_size[0]);
//   }
//    size_t chunk_size()const {
//     return indexes.size();
//    }
//   size_t read_size() const{
//     return elem_ct;
//   }
//    size_t offset() const{
//     return offset_size[0];
//   }
//    size_t raw_size() const{
//     return offset_size[1];
//   }
// };




class rep_chunk_indexer{
  const std::array<size_t,2> offset_size;
  std::vector<int> chunk_indexes;
  std::vector<int> out_indexes;
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
  int push_back(const int index,const int i){
    chunk_indexes.push_back(index-offset_size[0]);
    out_indexes.push_back(i);
    return out_indexes.size();
  }
  size_t disk_offset()const{
    return offset_size[0];
  }
  size_t disk_size()const{
    return offset_size[1];
  }
  size_t chunk_size()const{
    return chunk_indexes.size();
  }
  const	std::vector<int>& chunk_i()const{
    return chunk_indexes;
  }
  const	std::vector<int>& dest_i()const{
    return out_indexes;
  }


  
};


// };
//Chunk reader only handles figuring out which parts of each chunk to read
// In the most general case I _also_ need to keep track of where to put the thing

class IndexParser{
public:
    const HighFive::DataSet& d;
  const HighFive::Filter f;
private:
  const int p;

  const int index_dimension;

  const int dimsize;
  const int chunksize;
  const int num_chunks;
  std::vector<std::unique_ptr<rep_chunk_indexer>> needed_chunks;

  using	chunk_it =std::vector<std::unique_ptr<rep_chunk_indexer>>::const_iterator;

  bool is_unsorted;
  int chunks_used;
  size_t max_chunksize;
  using Iv=Rcpp::IntegerVector;
public:

  IndexParser(const HighFive::DataSet& d_,const Iv input,const int index_dimension_=0): p(input.size()),
											d(d_),
											f(d.getFilter()),
											  index_dimension(index_dimension_),
											  dimsize(d_.getDataDimensions()[index_dimension]),
											  chunksize(f.get_chunksizes()[index_dimension]),
											  num_chunks(ceilf(static_cast<float>(dimsize)/static_cast<float>(chunksize))),
											  needed_chunks(num_chunks),
											  chunks_used(0),
											  is_unsorted(false),
											  max_chunksize(0)


    {


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
        auto &it_el = needed_chunks[chunk_no];
        if(it_el == nullptr){
          it_el = std::unique_ptr<rep_chunk_indexer>(
              new rep_chunk_indexer({static_cast<size_t>(chunk_no * chunksize),
                                 static_cast<size_t>(chunksize)}));
	  chunks_used++;
          // unique_ptr<Employee> TempEmp(new Employee());
        }
	  int tc = it_el->push_back(i,ix);
	  if(tc>max_chunksize){
	    max_chunksize=tc;
	  }
	  oi=i;
	  ix++;
      }


      std::unique_ptr<rep_chunk_indexer> bad_el;
      std::remove_if(needed_chunks.begin(),needed_chunks.end(),[&bad_el](const std::unique_ptr<rep_chunk_indexer>& data_el){
								 return(data_el==bad_el);
							       });
    }

  chunk_it begin() const{
    return(needed_chunks.cbegin());
  }
  chunk_it end() const{
    return(needed_chunks.cend());
  }
  
  

  int total_size()const {
    return p;
  }
  int total_chunksize() const { return chunksize; }

  size_t max_read_chunksize() const{
    return max_chunksize;
  }
  
  size_t n_chunks() const{
    return chunks_used;
  }
  bool sorted() const{
    return !is_unsorted;
  }
  IndexParser ( IndexParser && ) = default;
  IndexParser &  operator= ( IndexParser && ) = default;
  IndexParser ( const IndexParser & ) = delete;
  IndexParser & operator= ( const IndexParser & ) = delete;


};



class chunk_chunker{
  const std::array<size_t,2> offset_size;
  const std::array<size_t,2> sub_offset_size;
  const size_t  dest_d_offset;
public:
  chunk_chunker(const std::array<size_t, 2> offset_size_,
                const std::array<int, 2> sub_offset_size_,
		const size_t dest_offset_)
      : offset_size(offset_size_),
        sub_offset_size([](const std::array<size_t, 2> offset_size_,
			   const std::array<int, 2> sub_offset_size_){
			  const int chunk_offset = offset_size_[0];
			  const int chunk_end = chunk_offset+offset_size_[1];
			  const auto sel_offset = sub_offset_size_[0];
			  const auto sel_end = sel_offset+sub_offset_size_[1];
			  const int sel_diff = sel_offset - static_cast<int>(chunk_offset);
			  const auto sub_offset=static_cast<size_t>(std::max(sel_diff,0));
			  const auto sub_size = static_cast<size_t>(std::min(sel_end-sub_offset,chunk_end-sub_offset));
			  return(std::array<size_t,2>{sub_offset,sub_size});
			}(offset_size_,sub_offset_size_)),
	dest_d_offset(dest_offset_){
  }
				
			   
  
  size_t chunk_size() const{
    return(sub_offset_size[1]);
  }
  size_t chunk_offset() const{
    return(sub_offset_size[0]);
  }
  
  size_t disk_offset() const{
    return offset_size[0];
  }
  size_t disk_size() const{
    return offset_size[1];
  }
  size_t dest_offset() const{
    return dest_d_offset;
  }
  size_t dest_size() const{
    return chunk_size();
  }
};




class ChunkParser{
public:
  const HighFive::DataSet& d;
  const HighFive::Filter f;
private:
  const int index_dimension;
  const int dimsize;
  const int chunksize;
  const int p;
  int chunks_used;
  std::vector<std::unique_ptr<chunk_chunker>> needed_chunks;
  using	chunk_it =std::vector<std::unique_ptr<chunk_chunker>>::const_iterator;
public:
  ChunkParser(const HighFive::DataSet& d_,const std::pair<int,int> offset_chunksize,const int index_dimension_=0):
    d(d_),
    f(d.getFilter()),
    index_dimension(index_dimension_),
    dimsize(d.getDataDimensions()[index_dimension]),
    chunksize(f.get_chunksizes()[index_dimension]),
    p((offset_chunksize.second<0) ? (dimsize-offset_chunksize.first) : offset_chunksize.second),
    chunks_used(ceilf(static_cast<float>(p)/static_cast<float>(chunksize))),
    needed_chunks(chunks_used)
  {
    int chunk_chunksize=offset_chunksize.second;
    int chunk_offset=offset_chunksize.first;

    if(chunk_chunksize<0){
      chunk_chunksize=dimsize-chunk_offset;
    }
    int i=0;
    int	out_o=0;
    std::array<int, 2> sub_oa = {
      chunk_offset,
      chunk_chunksize
    };

    int chunk_start = chunk_offset-chunk_offset%chunksize;

    for(int tpo=chunk_start; tpo<(chunk_offset+chunk_chunksize); tpo+=chunksize){
      int chunk_no = tpo/chunksize;
      const size_t t_chunk_offset = static_cast<size_t>(chunk_no * chunksize);
      const size_t t_chunk_end = static_cast<size_t>(std::min<size_t>(static_cast<size_t>(dimsize),static_cast<size_t>(t_chunk_offset+chunksize)));
      needed_chunks[i] = std::unique_ptr<chunk_chunker>(
          new chunk_chunker({t_chunk_offset,t_chunk_end-t_chunk_offset},
			    sub_oa,
			    out_o));
      out_o+=needed_chunks[i]->chunk_size();
      i++;
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
  ChunkParser ( ChunkParser && ) = default;
  ChunkParser &  operator= ( ChunkParser && ) = default;
  ChunkParser ( const ChunkParser & ) = delete;
  ChunkParser & operator= ( const ChunkParser & ) = delete;
};


// class

// typedef struct ZSTD_DCtx_s ZSTD_DCtx;
// ZSTD_DCtx* ZSTD_createDCtx(void);

//inline void close_zstd(std::FILE* fp) { std::fclose(fp); }

// class Zstd_d{
//   typedef struct ZSTD_DCtx_s ZSTD_DCtx;
//   ZSTD_DCtx* context;
// public:
//   Zstd_d():context(ZSTD_createDCtx()){

//   }
//   ~Zstd_d(){
//     ZSTD_freeDCtx(context);
//   }

// };


class ZSTD_d{
  //  std::atomic<bool>

  std::unique_ptr<ZSTD_DCtx,decltype(&ZSTD_freeDCtx)> ctxt;
  std::vector<unsigned char> raw_chunk_buffer;
  std::vector<unsigned char> chunk_chunk_buffer;

  // Eigen::Matrix<char, Eigen::Dynamic, 1> raw_chunk_buffer;
  // Eigen::Matrix<D, Eigen::Dynamic, 1> chunk_chunk_buffer;
public:
  ZSTD_d(const size_t max_size):ctxt(ZSTD_createDCtx(),&ZSTD_freeDCtx),
				raw_chunk_buffer(max_size),
				chunk_chunk_buffer(max_size){}
  void*	input_buffer()noexcept{
    return(raw_chunk_buffer.data());
  }

  void* output_buffer()noexcept{
    return(chunk_chunk_buffer.data());
  }

  void decompress(const size_t cchunk_size){
    auto rc = ZSTD_decompressDCtx(ctxt.get(),chunk_chunk_buffer.data(), chunk_chunk_buffer.size(), raw_chunk_buffer.data(),cchunk_size);
    if(ZSTD_isError(rc)){
      Rcpp::Rcerr<<ZSTD_getErrorName(rc)<<std::endl;
      for(int ik =0; ik<raw_chunk_buffer.size();ik++){
	Rcpp::Rcerr<<static_cast<int>(raw_chunk_buffer[ik])<<std::endl;
      }
      if(ZSTD_isFrame((void*) raw_chunk_buffer.data(), cchunk_size)){
	Rcpp::Rcerr<<"Frame is valid"<<std::endl;
      }
      else{
	Rcpp::Rcerr<<"Frame is not valid"<<std::endl;
      }
      auto ret2 = ZSTD_getFrameContentSize((void*) raw_chunk_buffer.data(),cchunk_size);

      if(ret2==0){
	Rcpp::Rcerr<<"Frame valid but empty"<<std::endl;
	Rcpp::stop("Error decompressing zstd");
      }
      if(ret2==ZSTD_CONTENTSIZE_ERROR){
	Rcpp::Rcerr<<"invalid magic number, or srcSize is too small"<<std::endl;
	Rcpp::stop("Error decompressing zstd");
      }
      if(ret2==ZSTD_CONTENTSIZE_UNKNOWN){
	Rcpp::Rcerr<<"Frame size cannot be determined"<<std::endl;
	Rcpp::stop("Error decompressing zstd");
      }else{
	Rcpp::Rcerr<<"Frame size is :"<<ret2<<" and you said it was :"<<cchunk_size<<std::endl;
	Rcpp::stop("Error decompressing zstd");
      }
      Rcpp::stop("Error decompressing zstd");
    }
  }
};


class no_filter_d{
  //  std::atomic<bool>
  // typedef struct ZSTD_DCtx_s ZSTD_DCtx;
  // std::unique_ptr<ZSTD_DCtx,decltype(&ZSTD_freeDCtx)> context;
  std::vector<unsigned char> raw_chunk_buffer;
  std::vector<unsigned char> chunk_chunk_buffer;

  // Eigen::Matrix<char, Eigen::Dynamic, 1> raw_chunk_buffer;
  // Eigen::Matrix<D, Eigen::Dynamic, 1> chunk_chunk_buffer;
public:
  no_filter_d(const size_t max_size):
    raw_chunk_buffer(max_size),
    chunk_chunk_buffer(max_size){}
  void*	input_buffer(){
    return(raw_chunk_buffer.data());
  }

  void* output_buffer(){
    return(chunk_chunk_buffer.data());
  }

  void decompress(const size_t){
    std::copy(raw_chunk_buffer.begin(),raw_chunk_buffer.end(),chunk_chunk_buffer.begin());
  }
};


template <typename T, typename D>
class VecShuttle {
  const T &Chunks;
  const size_t total_data_size;
  const size_t chunksize;
public:
  VecShuttle(const T& Chunks_,const size_t elem_size=1):
    Chunks(Chunks_),
    total_data_size(Chunks.total_size()),
    chunksize(Chunks.total_chunksize()){}
  Rcpp::Vector<cpp2r<D>::data_t> read_vector(){
    using RVT =   Rcpp::Vector<cpp2r<D>::data_t>;
    RVT retvec(total_data_size);
    Eigen::Map<Eigen::Matrix<D,Eigen::Dynamic,1>>tretvec(&retvec[0],total_data_size);
    if(Chunks.f.get_filter_info().first=="no_filter"){
      no_filter_d decomp(chunksize*sizeof(D));
      Eigen::Map<Eigen::Matrix<D,Eigen::Dynamic,1>>chunk_buff(static_cast<D*>(decomp.output_buffer()),chunksize);
      for(auto &tchunk:Chunks){
	auto res = Chunks.d.read_raw_chunk(decomp.input_buffer(),{tchunk->disk_offset()});
	decomp.decompress(res);
	copy_rows(chunk_buff,tretvec,*tchunk);
      }
    }else{
      ZSTD_d decomp(chunksize*sizeof(D));
      Eigen::Map<Eigen::Matrix<D,Eigen::Dynamic,1>>chunk_buff(static_cast<D*>(decomp.output_buffer()),chunksize);
      for(auto &tchunk:Chunks){
	auto res = Chunks.d.read_raw_chunk(decomp.input_buffer(),{tchunk->disk_offset()});
	decomp.decompress(res);
	copy_rows(chunk_buff,tretvec,*tchunk);
      }
    }
    return retvec;
  }

};





template <typename TA, typename TB,typename D>
class MatrixShuttle {
  const TA& RowChunks;
  const TB& ColChunks;

  const size_t total_data_rowsize;
  const size_t total_data_colsize;
  const size_t chunksize_rows;
  const size_t chunksize_cols;
public:
  MatrixShuttle(const  TA& RowChunks_,const TB& ColChunks_, const size_t elem_size=1):
    RowChunks(RowChunks_),
    ColChunks(ColChunks_),
    total_data_rowsize(RowChunks.total_size()),
    total_data_colsize(ColChunks.total_size()),
    chunksize_rows(RowChunks.total_chunksize()),
    chunksize_cols(ColChunks.total_chunksize()){}

  Rcpp::Matrix<cpp2r<D>::data_t> read_matrix(){
    using RVT =   Rcpp::Matrix<cpp2r<D>::data_t>;
    RVT retmat(total_data_rowsize,total_data_colsize);
    Eigen::Map<Eigen::Matrix<D,Eigen::Dynamic,Eigen::Dynamic>>tretvec(&retmat[0],total_data_rowsize,total_data_colsize);
    const auto& d=RowChunks.d;
    const auto& f=RowChunks.f;
    if(f.get_filter_info().first=="no_filter"){
      no_filter_d decomp(chunksize_rows*chunksize_cols*sizeof(D));
      Eigen::Map<Eigen::Matrix<D,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor>>chunk_buff(static_cast<D*>(decomp.output_buffer()),chunksize_rows,chunksize_cols);
      for(auto &tchunk_r:RowChunks){
	for(auto &tchunk_c:ColChunks){
	  auto res = d.read_raw_chunk(decomp.input_buffer(),{tchunk_r->disk_offset(),tchunk_c->disk_offset()});
	  decomp.decompress(res);
	  copy_rows_cols(chunk_buff,tretvec,*tchunk_r,*tchunk_c);
	}
      }
    }else{
      ZSTD_d decomp(chunksize_rows*chunksize_cols*sizeof(D));
       Eigen::Map<Eigen::Matrix<D,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor>>chunk_buff(static_cast<D*>(decomp.output_buffer()),chunksize_rows,chunksize_cols);
      for(auto &tchunk_r:RowChunks){
	for(auto &tchunk_c:ColChunks){
	  auto res = d.read_raw_chunk(decomp.input_buffer(),{tchunk_r->disk_offset(),tchunk_c->disk_offset()});
	  decomp.decompress(res);
	  copy_rows_cols(chunk_buff,tretvec,*tchunk_r,*tchunk_c);
	}
      }
    }
    return retmat;
  }

};

template<typename T,int CN, int O>
inline void copy_rows( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,CN,O>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,CN,O>> &dest, const rep_chunk_indexer& rowset ) {
  const auto chunk_iv = rowset.chunk_i();
  const auto dest_iv = rowset.dest_i();
  const size_t p=dest_iv.size();
  for(int i=0; i<p; i++){
    dest.row(dest_iv[i])=input.row(chunk_iv[i]);
  }
}

template<typename T,int RN, int O>
inline void copy_cols( const Eigen::Map<Eigen::Matrix<T,RN,Eigen::Dynamic,O>> &input, Eigen::Map<Eigen::Matrix<T,RN,Eigen::Dynamic,O>> &dest, const rep_chunk_indexer& colset) {
  const auto chunk_iv = colset.chunk_i();
  const auto dest_iv = colset.dest_i();
  const size_t p=dest_iv.size();
  for(int i=0; i<p; i++){
    dest.col(dest_iv[i])=input.col(chunk_iv[i]);
  }

}



template<typename T>
inline void copy_rows_cols( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor>> &dest, const rep_chunk_indexer& rowset,const rep_chunk_indexer& colset) {


  const auto row_chunk_iv = rowset.chunk_i();
  const auto row_dest_iv = rowset.dest_i();
  const size_t row_p=row_dest_iv.size();


  const auto col_chunk_iv = colset.chunk_i();
  const auto col_dest_iv = colset.dest_i();
  const size_t col_p=col_dest_iv.size();
  for(int j=0; j<row_p; j++){
    for(int i=0; i<col_p; i++){
      dest(row_dest_iv[j],col_dest_iv[i])=input(row_chunk_iv[j],col_chunk_iv[i]);
    }
  }

}





template<typename T>
inline void copy_rows_cols( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::ColMajor>> &dest, const rep_chunk_indexer& rowset,const rep_chunk_indexer& colset) {


  const auto row_chunk_iv = rowset.chunk_i();
  const auto row_dest_iv = rowset.dest_i();
  const size_t row_p=row_dest_iv.size();


  const auto col_chunk_iv = colset.chunk_i();
  const auto col_dest_iv = colset.dest_i();
  const size_t col_p=col_dest_iv.size();
  for(int i=0; i<col_p; i++){
    for(int j=0; j<row_p; j++){
      dest(row_dest_iv[j],col_dest_iv[i])=input(row_chunk_iv[j],col_chunk_iv[i]);
    }
  }

}



template<typename T>
inline void copy_rows_cols( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::ColMajor>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,Eigen::ColMajor>> &dest, const rep_chunk_indexer& rowset,const rep_chunk_indexer& colset) {


  const auto row_chunk_iv = rowset.chunk_i();
  const auto row_dest_iv = rowset.dest_i();
  const size_t row_p=row_dest_iv.size();


  const auto col_chunk_iv = colset.chunk_i();
  const auto col_dest_iv = colset.dest_i();
  const size_t col_p=col_dest_iv.size();
  for(int i=0; i<col_p; i++){
    for(int j=0; j<row_p; j++){
      dest(row_dest_iv[j],col_dest_iv[i])=input(row_chunk_iv[j],col_chunk_iv[i]);
    }
  }

}


template<typename T,int OA,int OB>
inline void copy_rows_cols( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,OA>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,OB>> &dest, const chunk_chunker& rowset,const chunk_chunker& colset) {
  dest.block(rowset.dest_offset(),colset.dest_offset(),rowset.dest_size(),colset.dest_size())=input.block(rowset.chunk_offset(),colset.chunk_offset(),rowset.chunk_size(),colset.chunk_size());
}


template<typename T,int OA,int OB>
inline void copy_rows_cols( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,OA>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,OB>> &dest, const chunk_chunker& rowset,const rep_chunk_indexer& colset) {

  const auto col_chunk_iv = colset.chunk_i();
  const auto col_dest_iv = colset.dest_i();
  const size_t col_p=col_dest_iv.size();
  for(int i=0; i<col_p; i++){
    dest.block(rowset.dest_offset(),col_dest_iv[i],rowset.dest_size(),1)=input.block(rowset.chunk_offset(),col_chunk_iv[i],rowset.chunk_size(),1);
  }
}


template<typename T,int OA,int OB>
inline void copy_rows_cols( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,OA>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic,OB>> &dest, const rep_chunk_indexer& rowset,const chunk_chunker& colset) {

  const auto row_chunk_iv = rowset.chunk_i();
  const auto row_dest_iv = rowset.dest_i();
  const size_t row_p=row_dest_iv.size();
  for(int i=0; i<row_p; i++){
    dest.block(row_dest_iv[i],colset.dest_offset(),1,colset.dest_size())=input.block(row_chunk_iv[i],colset.chunk_offset(),1,colset.chunk_size());
  }
}



template<typename T,int CN, int OA,int OB>
inline void copy_rows( const Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,CN,OA>> &input, Eigen::Map<Eigen::Matrix<T,Eigen::Dynamic,CN,OB>> &dest, const chunk_chunker& rowset ) {
  dest.block(rowset.dest_offset(),0,rowset.dest_size(),input.cols())=input.block(rowset.chunk_offset(),0,rowset.chunk_size(),input.cols());
}

template<typename T,int RN, int OA,int OB>
inline void copy_cols( const Eigen::Map<Eigen::Matrix<T,RN,Eigen::Dynamic,OA>> &input, Eigen::Map<Eigen::Matrix<T,RN,Eigen::Dynamic,OB>> &dest, const chunk_chunker& colset) {
  dest.block(0,colset.dest_offset(),input.rows(),colset.dest_size())=input;
}
