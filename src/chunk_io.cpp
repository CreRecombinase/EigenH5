#include "EigenH5.h"
#include <variant>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::depends(BH)]]
#include <progress.hpp>
#include <array>
#include <Rcpp.h>
#include <Rinternals.h>
// [[Rcpp::interfaces(r, cpp)]]
//#include <boost/variant/multivisitors.hpp>







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
private:
  const int p;

  //  const int index_dimension;

  const int dimsize;
  const int chunksize;
  const int num_chunks;
  std::vector<std::unique_ptr<rep_chunk_indexer>> needed_chunks;

  using	chunk_it =std::vector<std::unique_ptr<rep_chunk_indexer>>::const_iterator;
  int chunks_used;
  bool is_unsorted;

  size_t max_chunksize;
  using Iv=Rcpp::IntegerVector;
public:

  IndexParser(const size_t dimsize_,const size_t chunksize_,const Iv input,bool whole_only=false): p(input.size()),
									     dimsize(dimsize_),
									     chunksize(chunksize_),
									     num_chunks(ceilf(static_cast<float>(dimsize)/static_cast<float>(chunksize))),
									     needed_chunks(num_chunks),
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
	if(chunk_no>=needed_chunks.size()){
	  //	  Rcpp::Rcerr<<"in index_dimension"<<index_dimension_<<std::endl;
	  Rcpp::Rcerr<<"indexing beyond the end of the dataset! ("<<inp<<">="<<dimsize<<")"<<std::endl;
	  Rcpp::stop("indexing error!");
	}
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
      needed_chunks.erase(std::remove_if(needed_chunks.begin(),needed_chunks.end(),[&bad_el](const std::unique_ptr<rep_chunk_indexer>& data_el){
								 return(data_el==bad_el);
										   }),needed_chunks.end());
      if(needed_chunks.size()!=chunks_used){
	Rcpp::Rcerr<<"counted chunks is: "<<chunks_used<<", but there are: "<<needed_chunks.size()<<" chunk indexers!"<<std::endl;
	Rcpp::stop("the last chunk is bad!");
      }
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
			  const int chunk_end = chunk_offset+offset_size_[1]-1;
			  size_t sel_offset = static_cast<size_t>(sub_offset_size_[0]);
			  auto sel_end = sel_offset+sub_offset_size_[1]-1;
			  if(sel_offset<=chunk_offset){
			    if(sel_end>chunk_end){
			      return(std::array<size_t,2>{0,offset_size_[1]});
			    }
			    return(std::array<size_t,2>{0,sel_end-chunk_offset+1});
			  }
			  if(sel_end>chunk_end){
			    return(std::array<size_t,2>{sel_offset-chunk_offset,chunk_end-sel_offset+1});
			  }
			  return(std::array<size_t,2>{sel_offset-chunk_offset,static_cast<size_t>(sub_offset_size_[1])});
			  //   sel_end=chunk_end;
			  // }
			  // if(sel_offset<chunk_offset){
			  //   sel_offset=0;
			  // }else{
			  //   sel_offset=sel_offset-chunk_offset;
			  // }
			  // const auto sub_size = static_cast<size_t>(sel_end-sel_offset+1);
			  // return(std::array<size_t,2>{sel_offset,sub_size});
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
private:

  const int dimsize;
  const int chunksize;
  const int p;
  int chunks_used;
  std::vector<std::unique_ptr<chunk_chunker>> needed_chunks;
  using	chunk_it =std::vector<std::unique_ptr<chunk_chunker>>::const_iterator;
public:
  ChunkParser(const size_t  dimsize_,const size_t chunksize_,const std::pair<int,int> offset_chunksize,bool whole_only=false):
    dimsize(dimsize_),
    chunksize(chunksize_),
    p((offset_chunksize.second<0) ? (dimsize-offset_chunksize.first) : offset_chunksize.second),
    chunks_used(ceilf(static_cast<float>(p)/static_cast<float>(chunksize))),
    needed_chunks(chunks_used)
  {
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
      const size_t t_chunk_end = static_cast<size_t>(std::min<size_t>(static_cast<size_t>(dimsize),static_cast<size_t>(t_chunk_offset+chunksize)));
      needed_chunks[i] = std::unique_ptr<chunk_chunker>(
          new chunk_chunker({t_chunk_offset,t_chunk_end-t_chunk_offset},
			    sub_oa,
			    out_o));
      out_o+=needed_chunks[i]->chunk_size();
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

template<class E>
auto chunk_view(E&& e,const rep_chunk_indexer& rowset){
  return(xt::dynamic_view(std::forward<E>(e),{xt::keep(rowset.chunk_i())}));
}

template<class E>
auto chunk_view(E&& e,const chunk_chunker& rowset){
  return(xt::view(std::forward<E>(e),xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size())));
}

template<class E>
auto mem_view(E&& e,const rep_chunk_indexer& rowset){
  return(xt::dynamic_view(std::forward<E>(e),{xt::keep(rowset.dest_i())}));
}

template<class E>
auto mem_view(E&& e,const chunk_chunker& rowset){
  return(xt::view(std::forward<E>(e),xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size())));
}

template<class E>
auto chunk_view(E&& e,const rep_chunk_indexer& rowset,const rep_chunk_indexer& colset){
  return(xt::dynamic_view(std::forward<E>(e),{xt::keep(rowset.chunk_i()),xt::keep(colset.chunk_i())}));
}

template<class E>
auto chunk_view(E&& e,const chunk_chunker& rowset,const rep_chunk_indexer& colset){
  return(xt::dynamic_view(std::forward<E>(e),{xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::keep(colset.chunk_i())}));
}

template<class E>
auto chunk_view(E&& e,const rep_chunk_indexer& rowset,const chunk_chunker& colset){
    return(xt::dynamic_view(std::forward<E>(e), {xt::keep(rowset.chunk_i()),xt::range(colset.chunk_offset(),colset.chunk_offset()+colset.chunk_size())}));
}

template<class E>
auto chunk_view(E&& e,const chunk_chunker& rowset,const chunk_chunker& colset){
    return(xt::view(std::forward<E>(e),xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::range(colset.chunk_offset(),colset.chunk_offset()+colset.chunk_size())));
}



template<class E>
auto mem_view(E&& e,const rep_chunk_indexer& rowset,const rep_chunk_indexer& colset){
  return(xt::dynamic_view(std::forward<E>(e),{xt::keep(rowset.dest_i()),xt::keep(colset.dest_i())}));
}

template<class E>
auto mem_view(E&& e,const chunk_chunker& rowset,const rep_chunk_indexer& colset){
  return(xt::dynamic_view(std::forward<E>(e),{xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::keep(colset.dest_i())}));
}

template<class E>
auto mem_view(E&& e,const rep_chunk_indexer& rowset,const chunk_chunker& colset){
    return(xt::dynamic_view(std::forward<E>(e), {xt::keep(rowset.dest_i()),xt::range(colset.dest_offset(),colset.dest_offset()+colset.dest_size())}));
}

template<class E>
auto mem_view(E&& e,const chunk_chunker& rowset,const chunk_chunker& colset){
    return(xt::view(std::forward<E>(e),xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::range(colset.dest_offset(),colset.dest_offset()+colset.dest_size())));
}







template <typename D,typename T>
class VecShuttle {
  const T &Chunks;
  const size_t total_data_size;
  const size_t chunksize;
  using tensor_type=xt::xtensor<D,1,xt::layout_type::row_major>;
  using rtensor_type=xt::rtensor<D,1>;
  using rshape_type = typename rtensor_type::shape_type;
  tensor_type chunk_buffer;
  std::vector<std::byte> raw_chunk_buffer;
  using shape_type = typename tensor_type::shape_type;
public:

  VecShuttle(const T& Chunks_,const size_t elem_size=1):
    Chunks(Chunks_),
    total_data_size(Chunks.total_size()),
    chunksize(Chunks.total_chunksize()),
    chunk_buffer(shape_type{chunksize}),
    raw_chunk_buffer(chunksize*sizeof(D))
  {}
  template<typename Z>
  xt::rtensor<D,1> read_vector(Z &decomp,const HighFive::DataSet& d){
    //    auto a1 = xt::adapt(v, shape);

    xt::rtensor<D,1> retvec(rshape_type{static_cast<int>(total_data_size)});
    for(auto &tchunk:Chunks){
      raw_chunk_buffer.resize(tchunk->disk_size());
      chunk_buffer.resize({tchunk->disk_size()});
      auto res = d.read_raw_chunk(raw_chunk_buffer,{tchunk->disk_offset()});
      if(res){
	decomp.decompress(chunk_buffer.data(),chunk_buffer.size()*sizeof(D),raw_chunk_buffer.data(),res.value());
      }else{
	decomp.memcpy(chunk_buffer.data(),chunk_buffer.size()*sizeof(D),raw_chunk_buffer.data(),raw_chunk_buffer.size());
      }
      mem_view(retvec,*tchunk)=chunk_view(chunk_buffer,*tchunk);

    }
    return retvec;
  }

  template<typename Z>
  void write_vector(const xt::rtensor<D,1>& retmat,Z &decomp,const HighFive::DataSet& d){

    for(auto &tchunk_r:Chunks){
      chunk_view(chunk_buffer,*tchunk_r)=mem_view(retmat,*tchunk_r);
      if(auto res = decomp.compress(raw_chunk_buffer.data(),raw_chunk_buffer.size(),chunk_buffer.data(),chunk_buffer.size()*sizeof(D))){
	d.write_raw_chunk(raw_chunk_buffer,{tchunk_r->disk_offset()},res);
      }else{
	decomp.memcpy(raw_chunk_buffer.data(),raw_chunk_buffer.size(),chunk_buffer.data(),chunk_buffer.size()*sizeof(D));
	d.write_raw_chunk(raw_chunk_buffer,{tchunk_r->disk_offset()},raw_chunk_buffer.size());
      }

    }
  }


};








template <typename D,typename TA, typename TB>
class MatrixShuttle {
  const TA& RowChunks;
  const TB& ColChunks;
  const size_t total_data_rowsize;
  const size_t total_data_colsize;
  const size_t chunksize_rows;
  const size_t chunksize_cols;
  xt::xtensor<D,2,xt::layout_type::row_major> chunk_buffer;
  std::vector<std::byte> raw_chunk_buffer;
public:
  MatrixShuttle(const  TA& RowChunks_,const TB& ColChunks_, const size_t elem_size=1):
    RowChunks(RowChunks_),
    ColChunks(ColChunks_),
    total_data_rowsize(RowChunks.total_size()),
    total_data_colsize(ColChunks.total_size()),
    chunksize_rows(RowChunks.total_chunksize()),
    chunksize_cols(ColChunks.total_chunksize()),
    chunk_buffer({chunksize_rows,chunksize_cols}),
    raw_chunk_buffer(chunksize_rows*sizeof(D)*chunksize_cols*sizeof(D))
  {}
  template<typename Z>
  xt::rtensor<D,2> read_matrix(Z &decomp,const HighFive::DataSet& d){
    xt::rtensor<D,2> retmat({static_cast<int>(total_data_rowsize),static_cast<int>(total_data_colsize)});
    //    int r_ct=0;
    for(auto &tchunk_r:RowChunks){
      //      int c_ct=0;
      for(auto &tchunk_c:ColChunks){
	auto res = d.read_raw_chunk(raw_chunk_buffer,{tchunk_r->disk_offset(),tchunk_c->disk_offset()});
	if(res){
	  decomp.decompress(chunk_buffer.data(),chunk_buffer.size()*sizeof(D),raw_chunk_buffer.data(),res.value());
	}else{
	  decomp.memcpy(chunk_buffer.data(),chunk_buffer.size()*sizeof(D),raw_chunk_buffer.data(),raw_chunk_buffer.size());
	}
	// decomp.decompress(raw_chunk_buffer.data(),chunk_buffer.data(),res,{tchunk_r->disk_size()*tchunk_c->disk_size()});
	mem_view(retmat,*tchunk_r,*tchunk_c)=chunk_view(chunk_buffer,*tchunk_r,*tchunk_c);

	//	copy_rows_cols(chunk_buffer,retmat,*tchunk_r,*tchunk_c);
      }

    }
    return retmat;
  }
  template<typename Z>
  void write_matrix(const xt::rtensor<D,2>& retmat,Z &decomp,const HighFive::DataSet& d){

    for(auto &tchunk_r:RowChunks){
      for(auto &tchunk_c:ColChunks){

	chunk_view(chunk_buffer,*tchunk_r,*tchunk_c)=mem_view(retmat,*tchunk_r,*tchunk_c);
	if(auto res = decomp.compress(raw_chunk_buffer.data(),raw_chunk_buffer.size(),
                               chunk_buffer.data(),chunk_buffer.size()*sizeof(D))){
	  d.write_raw_chunk(raw_chunk_buffer,{tchunk_r->disk_offset(),tchunk_c->disk_offset()},res);
	}else{
	  decomp.memcpy(raw_chunk_buffer.data(),raw_chunk_buffer.size(),chunk_buffer.data(),chunk_buffer.size()*sizeof(D));
	  d.write_raw_chunk(raw_chunk_buffer,{tchunk_r->disk_offset(),tchunk_c->disk_offset()},raw_chunk_buffer.size());
	}

      }
    }
  }



};




// template<typename T,xt::layout_type LA>
// inline void copy_rows_cols( const xt::xtensor<T,2,LA> &input, xt::rtensor<T,2> &dest, const rep_chunk_indexer& rowset,const rep_chunk_indexer& colset) {
//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();

//   const auto col_chunk_iv = colset.chunk_i();
//   const auto col_dest_iv = colset.dest_i();
//   xt::dynamic_view(dest,{xt::keep(row_dest_iv),xt::keep(col_dest_iv)})=  xt::dynamic_view(input,{xt::keep(row_chunk_iv),xt::keep(col_chunk_iv)});
// }


// template<typename T,xt::layout_type LA>
// inline void copy_rows_cols(const xt::xtensor<T,2,LA> &input, xt::rtensor<T,2> &dest, const chunk_chunker& rowset,const chunk_chunker& colset) {
//   xt::view(dest,xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::range(colset.dest_offset(),colset.dest_offset()+colset.dest_size()))=
//     xt::view(input,xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::range(colset.chunk_offset(),colset.chunk_offset()+colset.chunk_size()));
// }


// template<typename T,xt::layout_type LA>
// inline void copy_rows_cols(const xt::xtensor<T,2,LA> &input, xt::rtensor<T,2> &dest,  const chunk_chunker& rowset,const rep_chunk_indexer& colset) {

//   const auto col_chunk_iv = colset.chunk_i();
//   const auto col_dest_iv = colset.dest_i();
//   xt::dynamic_view(dest,{xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::keep(col_dest_iv)})=
//     xt::dynamic_view(input,{xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::keep(col_chunk_iv)});
// }


// template<typename T,xt::layout_type LA>
// inline void copy_rows_cols(const xt::xtensor<T,2,LA> &input, xt::rtensor<T,2> &dest,const rep_chunk_indexer& rowset,const chunk_chunker& colset) {

//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();
//   xt::dynamic_view(dest,{xt::keep(row_dest_iv),xt::range(colset.dest_offset(),colset.dest_offset()+colset.dest_size())})=
//     xt::dynamic_view(input,{xt::keep(row_chunk_iv),xt::range(colset.chunk_offset(),colset.chunk_offset()+colset.chunk_size())});
// }


// template<typename T,xt::layout_type LA>
// inline void copy_rows(const xt::xtensor<T,2,LA> &input, xt::rtensor<T,2> &dest,  const chunk_chunker& rowset){
//   xt::view(dest,xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::all())=
//     xt::view(input,xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::all());

// }

// template<typename T,xt::layout_type LA>
// inline void copy_rows(const xt::xtensor<T,1,LA> &input, xt::rtensor<T,1> &dest,  const chunk_chunker& rowset){
//   xt::view(dest,xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()))=
//     xt::view(input,xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()));

// }


// template<typename T,xt::layout_type LA>
// inline void copy_rows(const xt::xtensor<T,1,LA> &input, xt::rtensor<T,1> &dest,  const rep_chunk_indexer& rowset){
//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();
//   xt::dynamic_view(dest,{xt::keep(row_dest_iv)})=
//     xt::dynamic_view(input,{xt::keep(row_chunk_iv)});

// }



// template<typename T,xt::layout_type LA>
// inline void copy_rows(const xt::xtensor<T,2,LA> &input, xt::rtensor<T,2> &dest,  const rep_chunk_indexer& rowset){
//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();
//   xt::dynamic_view(dest,{xt::keep(row_dest_iv),xt::all()})=
//     xt::dynamic_view(input,{xt::keep(row_chunk_iv),xt::all()});
// }







// //####-------------------Writers------------------------------------------

// template<typename T,xt::layout_type LA>
// inline void rows_cols_copy( xt::xtensor<T,2,LA> &input, const xt::rtensor<T,2> &dest, const rep_chunk_indexer& rowset,const rep_chunk_indexer& colset) {
//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();

//   const auto col_chunk_iv = colset.chunk_i();
//   const auto col_dest_iv = colset.dest_i();
//   xt::dynamic_view(input,{xt::keep(row_chunk_iv),xt::keep(col_chunk_iv)})=  xt::dynamic_view(dest,{xt::keep(row_dest_iv),xt::keep(col_dest_iv)});
// }


// template<typename T,xt::layout_type LA>
// inline void rows_cols_copy(xt::xtensor<T,2,LA> &input, const xt::rtensor<T,2> &dest, const chunk_chunker& rowset,const chunk_chunker& colset) {
//   xt::view(input,xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::range(colset.chunk_offset(),colset.chunk_offset()+colset.chunk_size()))=  xt::view(dest,xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::range(colset.dest_offset(),colset.dest_offset()+colset.dest_size()));
// }


// template<typename T,xt::layout_type LA>
// inline void rows_cols_copy(xt::xtensor<T,2,LA> &input, const xt::rtensor<T,2> &dest,  const chunk_chunker& rowset,const rep_chunk_indexer& colset) {

//   const auto col_chunk_iv = colset.chunk_i();
//   const auto col_dest_iv = colset.dest_i();
//   xt::dynamic_view(input,{xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::keep(col_chunk_iv)})=  xt::dynamic_view(dest,{xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::keep(col_dest_iv)});
// }


// template<typename T,xt::layout_type LA>
// inline void rows_cols_copy(xt::xtensor<T,2,LA> &input,const  xt::rtensor<T,2> &dest,const rep_chunk_indexer& rowset,const chunk_chunker& colset) {

//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();
//   xt::dynamic_view(input,{xt::keep(row_chunk_iv),xt::range(colset.chunk_offset(),colset.chunk_offset()+colset.chunk_size())})=  xt::dynamic_view(dest,{xt::keep(row_dest_iv),xt::range(colset.dest_offset(),colset.dest_offset()+colset.dest_size())});
// }


// template<typename T,xt::layout_type LA>
// inline void rows_copy(xt::xtensor<T,2,LA> &input,const xt::rtensor<T,2> &dest,  const chunk_chunker& rowset){

//     xt::view(input,xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()),xt::all())=
//       xt::view(dest,xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()),xt::all());
// }

// template<typename T,xt::layout_type LA>
// inline void rows_copy(xt::xtensor<T,1,LA> &input,const xt::rtensor<T,1> &dest,  const chunk_chunker& rowset){
//     xt::view(input,xt::range(rowset.chunk_offset(),rowset.chunk_offset()+rowset.chunk_size()))=
//         xt::view(dest,xt::range(rowset.dest_offset(),rowset.dest_offset()+rowset.dest_size()));
// }


// template<typename T,xt::layout_type LA>
// inline void rows_copy(xt::xtensor<T,1,LA> &input,const  xt::rtensor<T,1> &dest,  const rep_chunk_indexer& rowset){
//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();
//     xt::dynamic_view(input,{xt::keep(row_chunk_iv)})=  xt::dynamic_view(dest,{xt::keep(row_dest_iv)});

// }



// template<typename T,xt::layout_type LA>
// inline void rows_copy(xt::xtensor<T,2,LA> &input,const xt::rtensor<T,2> &dest,  const rep_chunk_indexer& rowset){
//   const auto row_chunk_iv = rowset.chunk_i();
//   const auto row_dest_iv = rowset.dest_i();

//   xt::dynamic_view(input,{xt::keep(row_chunk_iv),xt::all()})=  xt::dynamic_view(dest,{xt::keep(row_dest_iv),xt::all()});



// }











template<typename T>
struct t2chunk_t{
  typedef std::false_type type;
};

template<> struct t2chunk_t<std::pair<int,int> >{
  typedef ChunkParser type;
}; 

template<> struct t2chunk_t<Rcpp::IntegerVector >{
  typedef IndexParser type;
};


class VecDim{
public:
  const std::array<size_t,1> dimsize;
  const std::array<size_t,1> chunksize;
  VecDim(std::vector<size_t> dim_d,std::vector<size_t> dim_c):dimsize(std::array<size_t,1>{dim_d[0]}),
							      chunksize(std::array<size_t,1>{dim_c[0]}){}
  // VecDim(std::array<size_t,1> dimsize_,std::array<size_t,1> chunksize_):dimsize(dimsize_),
  // 									chunksize(chunksize_){}
};

class MatDim{
public:
  const std::array<size_t,2> dimsize;
  const std::array<size_t,2> chunksize;
  //  MatDim(std::array<size_t,2> dimsize_,std::array<size_t,2> chunksize_): dimsize(dimsize_),chunksize(chunksize_){}
  MatDim(std::vector<size_t> dim_d,std::vector<size_t> dim_c):dimsize(std::array<size_t,2>{dim_d[0],dim_d[1]}),
							      chunksize(std::array<size_t,2>{dim_c[0],dim_c[1]}){}
};


template<typename T>
class DataSet_V{

public:
  const T Dim;
  std::variant<int,double> data_type;
  std::variant<NoCompressor,LzfCompressor,BloscCompressor,GzipCompressor,ZstdCompressor> decompressor;


  DataSet_V(const HighFive::DataSet &dset):Dim(dset.getDataDimensions(),dset.getFilter().get_chunksizes()),
					   decompressor(dset.getFilter().getDecompressor()){

    auto ret_t = typeof_h5_dset(dset);
    switch(ret_t){
    case INTSXP: {
      data_type=int();
      break;
    }
    case REALSXP: {
      data_type=double();
      break;
    }
    case STRSXP: {
      Rcpp::stop("no string types...");
      break;
    }
    default:{
      Rcpp::stop("can't create type");
      break;
    }
    }
    //    decompressor = dset.getFilter().getDecompressor();
  }
};




class VisitVectorRead
{
public:
  const HighFive::DataSet &d;
  const VecDim Dim;
  VisitVectorRead(const HighFive::DataSet &d_, const VecDim Dim_):d(d_),Dim(Dim_){}
  template<typename Q,typename Z,typename T>
  SEXP operator()(Q ret,Z& decomp,T rowdat)const{

    using RT=typename t2chunk_t<T>::type;
    RT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat);
    VecShuttle<Q,RT> mover(rows,1);
    return(mover.template read_vector<Z>(decomp,d));
  }
};




class VisitMatrixRead

{
public:
  const HighFive::DataSet &d;
  const MatDim Dim;
  VisitMatrixRead(const HighFive::DataSet &d_,const MatDim Dim_):d(d_),Dim(Dim_){}
  template<typename Q,typename Z,typename RT,typename CT>
  SEXP operator()(Q ret,Z& decomp,RT rowdat,CT coldat)const{
    using RRT=typename t2chunk_t<RT>::type;
    using CCT=typename t2chunk_t<CT>::type;

    const RRT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat);
    const CCT cols(Dim.dimsize[1],Dim.chunksize[1],coldat);

    MatrixShuttle<Q,RRT,CCT> mover(rows,cols,1);
    return(mover.template read_matrix<Z>(decomp,d));
  }
};







//[[Rcpp::export]]
SEXP read_matrix_v(const std::string filename,  const std::string datapath, SEXP rows,SEXP cols){
  auto drow=dispatch_subset(rows);
  auto dcol=dispatch_subset(cols);
  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  DataSet_V<MatDim> dsv(dset);
  auto data_t =dsv.data_type;
  auto &decomp = dsv.decompressor;
  return(std::visit(VisitMatrixRead(dset,dsv.Dim),data_t,decomp,drow,dcol));
}



//[[Rcpp::export]]
SEXP read_vector_v(const std::string filename,  const std::string datapath, SEXP rows){

  auto drow=dispatch_subset(rows);
  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  DataSet_V<VecDim> dsv(dset);
  auto data_t =dsv.data_type;
  auto &decomp = dsv.decompressor;



  return(std::visit(VisitVectorRead(dset,dsv.Dim),data_t,decomp,drow));
}

std::variant<xt::rtensor<int,2>,xt::rtensor<double,2>> xtensor_mat(Rcpp::RObject data){
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    return(Rcpp::as<xt::rtensor<int,2>>(data));
  }
  case REALSXP: {
    return(Rcpp::as<xt::rtensor<double,2>>(data));
  }
  default:{
    Rcpp::stop("Can currently only read and write numeric and integer types");
    return(Rcpp::as<xt::rtensor<double,2>>(data));
  }
  }

}



std::variant<xt::rtensor<int,1>,xt::rtensor<double,1>> xtensor_vec(Rcpp::RObject data){
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    return(Rcpp::as<xt::rtensor<int,1>>(data));
  }
  case REALSXP: {
    return(Rcpp::as<xt::rtensor<double,1>>(data));
  }
  default:{
    Rcpp::stop("Can currently only read and write numeric and integer types");
    return(Rcpp::as<xt::rtensor<double,1> >(data));
  }

  }
}








class VisitMatrixWrite

{
public:
  const HighFive::DataSet &d;
  const MatDim Dim;
  VisitMatrixWrite(const HighFive::DataSet &d_,const MatDim Dim_):d(d_),Dim(Dim_){}
  template<typename Q,typename Z,typename RT,typename CT>
  void operator()(xt::rtensor<Q,2> &write,Z& decomp,RT rowdat,CT coldat)const{
    using RRT=typename t2chunk_t<RT>::type;
    using CCT=typename t2chunk_t<CT>::type;

    const RRT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat,true);
    const CCT cols(Dim.dimsize[1],Dim.chunksize[1],coldat,true);

    MatrixShuttle<Q,RRT,CCT> mover(rows,cols,1);
    mover.template write_matrix<Z>(write,decomp,d);
  // return(read_matrix_tt<Q,Z,RT,CT>(d,Dim.dimsize,Dim.chunksize,decomp,rowdat,coldat));
  }
};



class VisitVectorWrite
{
public:
  const HighFive::DataSet &d;
  const VecDim Dim;
  VisitVectorWrite(const HighFive::DataSet &d_, const VecDim Dim_):d(d_),Dim(Dim_){}
  template<typename Q,typename Z,typename T>
  void operator()(xt::rtensor<Q,1> &write,Z& decomp,T rowdat)const{

    using RT=typename t2chunk_t<T>::type;
    RT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat,true);
    VecShuttle<Q,RT> mover(rows,1);
    return(mover.template write_vector<Z>(write,decomp,d));
  }
};



//[[Rcpp::export]]
 void update_matrix_v(Rcpp::RObject data,const std::string filename,  const std::string datapath, SEXP rows,SEXP cols){
  auto drow=dispatch_subset(rows);
  auto dcol=dispatch_subset(cols);
  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadWrite);

  auto w_t=xtensor_mat(data);
  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  DataSet_V<MatDim> dsv(dset);
  //  auto data_t =dsv.data_type;
  auto &decomp = dsv.decompressor;
  return(std::visit(VisitMatrixWrite(dset,dsv.Dim),w_t,decomp,drow,dcol));
}



//[[Rcpp::export]]
void update_vector_v(Rcpp::RObject data,const std::string filename,  const std::string datapath, SEXP rows){

  auto drow=dispatch_subset(rows);
  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadWrite);
  auto w_t=xtensor_vec(data);
  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  DataSet_V<VecDim> dsv(dset);
  //  auto data_t =dsv.data_type;
  auto &decomp = dsv.decompressor;


  std::visit(VisitVectorWrite(dset,dsv.Dim),w_t,decomp,drow);
}
