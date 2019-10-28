#include "EigenH5.h"
#include <variant>
#include<eigenh5/indexers.hpp>
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH)]]






template <typename D,typename T>
class VecShuttle {
  const T &Chunks;
  const size_t total_data_size;
  const size_t chunksize;
  const size_t elem_size;
  using tensor_type=typename xtm_t<D>::buffvec_type;
  using rtensor_type=typename xtm_t<D>::retvec_type;

  tensor_type chunk_buffer;
  std::vector<std::byte> raw_chunk_buffer;
  using shape_type = typename tensor_type::shape_type;
public:
  VecShuttle(const T& Chunks_,const size_t elem_size_=1):
    Chunks(Chunks_),
    total_data_size(Chunks.total_size()),
    chunksize(Chunks.total_chunksize()),
    elem_size(elem_size_),
    chunk_buffer([](size_t chunksize_,size_t elem_size_){
		   if constexpr(std::is_arithmetic_v<D>){
return tensor_type(shape_type{chunksize_});
 }else{
		     return(tensor_type({chunksize_,elem_size_}));
		   }
		 }(chunksize,elem_size)),
    raw_chunk_buffer(chunksize*sizeof(typename tensor_type::value_type)*elem_size)
  {}
 rtensor_type create_retvec(){
    if constexpr(std::is_arithmetic_v<D>){
   using rshape_type = typename rtensor_type::shape_type;
    return(rtensor_type(rshape_type{static_cast<int>(total_data_size)}));
    }else{
      return(rtensor_type(static_cast<int>(total_data_size)));
    }
 }


  template<typename Z>
  rtensor_type read_vector(Z &decomp,const HighFive::DataSet& d){
    rtensor_type retvec=create_retvec();
    for(auto &tchunk:Chunks){
      auto res = d.read_raw_chunk(raw_chunk_buffer,{tchunk.disk_offset()});
      decomp.decompress(chunk_buffer.data(),chunk_buffer.size()*sizeof(typename tensor_type::value_type),raw_chunk_buffer.data(),res);
      if constexpr(!std::is_arithmetic_v<D>){
	  const auto rsize = tchunk.chunk_size();
	  xt::xtensor<char,1> tbuff(xt::xtensor<char,1>::shape_type{elem_size});
	  std::string ts;
	  for(int i=0;i< rsize; i++){
		tbuff=xt::view(chunk_buffer,tchunk.chunk_i(i),xt::all());
		ts=std::string(reinterpret_cast<char*>(tbuff.data()));
		retvec(tchunk.mem_i(i))=ts;
		std::fill(tbuff.begin(), tbuff.end(), 0);
	      }
	}else{
	if constexpr(std::is_same_v<T,chunk_chunker>){

	      xt::view(retvec,tchunk.mem_slice())=
		xt::view(chunk_buffer,tchunk.chunk_slice());
	    }else{
	  xt::dynamic_view(retvec,{tchunk.mem_slice()})=
	    xt::dynamic_view(chunk_buffer,{tchunk.chunk_slice()});
	}
      }
    }
    return retvec;
  }

  template<typename Z>
  void write_vector(const rtensor_type & retmat,Z &decomp,const HighFive::DataSet& d){
    for(auto &tchunk_r:Chunks){
      if constexpr(!std::is_arithmetic_v<D>){

	  const auto rsize = tchunk_r.chunk_size();
	  xt::xtensor<char,1> tbuff(xt::xtensor<char,1>::shape_type{elem_size});
	  std::string ts;
	  for(int i=0;i< rsize; i++){
	    std::fill(tbuff.begin(), tbuff.end(), 0);
	    ts=Rcpp::as<std::string>(retmat(tchunk_r.mem_i(i)));
	    std::memcpy(tbuff.data(),ts.data(),ts.size());
	    xt::view(chunk_buffer,tchunk_r.chunk_i(i),xt::all())=tbuff;
	  }
	}else{

	if constexpr(std::is_same_v<T,chunk_chunker>){
	    xt::view(chunk_buffer,tchunk_r.chunk_slice())=
	      xt::view(retmat,tchunk_r.mem_slice());
	  }else{
	  xt::dynamic_view(chunk_buffer,{tchunk_r.chunk_slice()})=
	    xt::dynamic_view(retmat,{tchunk_r.mem_slice()});
	}
      }
      auto res = decomp.compress(raw_chunk_buffer.data(),raw_chunk_buffer.size(),chunk_buffer.data(),chunk_buffer.size()*sizeof(typename tensor_type::value_type));
      d.write_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset()},res);
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
  const size_t elem_size;
  using tensor_type=typename xtm_t<D>::buffmat_type;
  using rtensor_type=typename xtm_t<D>::retmat_type;
  tensor_type chunk_buffer;
  std::vector<std::byte> raw_chunk_buffer;

public:
  MatrixShuttle(const  TA& RowChunks_,const TB& ColChunks_, const size_t elem_size_=1):
    RowChunks(RowChunks_),
    ColChunks(ColChunks_),
    total_data_rowsize(RowChunks.total_size()),
    total_data_colsize(ColChunks.total_size()),
    chunksize_rows(RowChunks.total_chunksize()),
    chunksize_cols(ColChunks.total_chunksize()),
    elem_size(elem_size_),
    chunk_buffer([](size_t chunksize_rows_,size_t chunksize_cols_,size_t elem_size_){
		   if constexpr(std::is_arithmetic_v<D>){
return tensor_type({chunksize_rows_,chunksize_cols_});
 }else{
		     return(tensor_type({chunksize_rows_,elem_size_,chunksize_cols_}));
		   }
		 }(chunksize_rows,chunksize_cols,elem_size)),
    raw_chunk_buffer(chunksize_rows*elem_size*chunksize_cols*sizeof(typename tensor_type::value_type))
  {}

  rtensor_type create_retmat(){
    if constexpr(std::is_arithmetic_v<D>){
    return(rtensor_type({static_cast<int>(total_data_rowsize),static_cast<int>(total_data_colsize)}));
    }else{
      return(rtensor_type(static_cast<int>(total_data_rowsize),static_cast<int>(total_data_colsize)));
    }
  }

  template<typename Z>
  rtensor_type read_matrix(Z &decomp,const HighFive::DataSet& d){
    auto retmat=create_retmat();
    for(auto &tchunk_r:RowChunks){
      for(auto &tchunk_c:ColChunks){


	auto res = d.read_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset(),tchunk_c.disk_offset()});
	decomp.decompress(chunk_buffer.data(),chunk_buffer.size()*sizeof(typename tensor_type::value_type),raw_chunk_buffer.data(),res);

	if constexpr(!std::is_arithmetic_v<D>){
	      const size_t rsize = tchunk_r.chunk_size();
	      const size_t csize = tchunk_c.chunk_size();
	      xt::xtensor<char,1> tbuff(xt::xtensor<char,1>::shape_type{elem_size});
	      std::string ts;
	      for(int i=0;i< rsize; i++){
		for(int j=0; j<csize; j++){
		  tbuff=xt::view(chunk_buffer,tchunk_r.chunk_i(i),xt::all(),tchunk_c.chunk_i(j));
		  ts=std::string(reinterpret_cast<char*>(tbuff.data()));
		    retmat(tchunk_r.mem_i(i),tchunk_c.mem_i(j))=ts;
		  std::fill(tbuff.begin(), tbuff.end(), 0);
		}
	      }
	  }else{
	  if constexpr(std::is_same_v<TB,chunk_chunker> && std::is_same_v<TA,chunk_chunker>){

	      xt::view(retmat,tchunk_r.mem_slice(),tchunk_c.mem_slice())=
		xt::view(chunk_buffer,tchunk_r.chunk_slice(),tchunk_c.chunk_slice());
	    }else{
	    xt::dynamic_view(retmat,{tchunk_r.mem_slice(),tchunk_c.mem_slice()})=
	      xt::dynamic_view(chunk_buffer,{tchunk_r.chunk_slice(),tchunk_c.chunk_slice()});
	  }
	}
      }
    }
    return retmat;
  }
  template<typename Z>
  void write_matrix(const rtensor_type& retmat,Z &decomp,const HighFive::DataSet& d){

    int rc=0;
    int cc=0;
    for(auto &tchunk_r:RowChunks){
      cc=0;
      for(auto &tchunk_c:ColChunks){
	if constexpr(!std::is_arithmetic_v<D>){
	    xt::xtensor<char,1> tbuff(xt::xtensor<char,1>::shape_type{elem_size});
	    std::string ts;
	    const size_t rsize = tchunk_r.chunk_size();
	    const size_t csize = tchunk_c.chunk_size();
	    for(int i=0;i< rsize; i++){
	      for(int j=0; j<csize; j++){
		std::fill(tbuff.begin(), tbuff.end(), 0);
		ts=Rcpp::as<std::string>(retmat(tchunk_r.mem_i(i),tchunk_c.mem_i(j)));
		std::memcpy(tbuff.data(),ts.data(),ts.size());

		#ifdef DEBUG
		if(xt::view(chunk_buffer,tchunk_r.chunk_i(i),xt::all(),tchunk_c.chunk_i(j)).size()<tbuff.size()){
		  Rcpp::Rcerr<<"i is: "<<i<<" and j is: "<<j<<std::endl;
		  Rcpp::stop("you're about to do some illegal copying (so instead I stopped)");
		}
		#endif

		xt::view(chunk_buffer,tchunk_r.chunk_i(i),xt::all(),tchunk_c.chunk_i(j))=tbuff;

	      }
	    }
	  }else{
	  if constexpr(std::is_same_v<TB,chunk_chunker> && std::is_same_v<TA,chunk_chunker>){
	      xt::view(chunk_buffer,tchunk_r.chunk_slice(),tchunk_c.chunk_slice())=
		xt::view(retmat,tchunk_r.mem_slice(),tchunk_c.mem_slice());
	    }else{
	    xt::dynamic_view(chunk_buffer,{tchunk_r.chunk_slice(),tchunk_c.chunk_slice()})=
	      xt::dynamic_view(retmat,{tchunk_r.mem_slice(),tchunk_c.mem_slice()});
	  }
	}
	auto res = decomp.compress(raw_chunk_buffer.data(),raw_chunk_buffer.size(),
				   chunk_buffer.data(),chunk_buffer.size()*sizeof(typename tensor_type::value_type));
	d.write_raw_chunk(raw_chunk_buffer,{tchunk_r.disk_offset(),tchunk_c.disk_offset()},res);
	cc++;
      }
      rc++;
    }
  }
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
  std::variant<int,double,std::string> data_type;
  std::variant<NoCompressor,LzfCompressor,BloscCompressor,GzipCompressor,ZstdCompressor> decompressor;
  std::vector<std::string> R_attrs;


  DataSet_V(const HighFive::DataSet &dset):Dim(dset.getDataDimensions(),dset.getFilter().get_chunksizes()),
					   decompressor(dset.getFilter().getDecompressor()),
                                           R_attrs(list_R_attr(dset))
  {

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
      data_type=std::string();
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


template<typename T>
class DataSet_VNS{

public:
  const T Dim;
  std::variant<int,double> data_type;
  std::variant<NoCompressor,LzfCompressor,BloscCompressor,GzipCompressor,ZstdCompressor> decompressor;


  DataSet_VNS(const HighFive::DataSet &dset):Dim(dset.getDataDimensions(),dset.getFilter().get_chunksizes()),
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
  VisitVectorRead(const HighFive::DataSet &d_, const VecDim Dim_,const bool read_attributes=false):d(d_),Dim(Dim_){}
  template<typename Q,typename Z,typename T>
  SEXP operator()(Q ret,Z& decomp,T rowdat)const{
    using RT=typename t2chunk_t<T>::c_type;
    RT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat);
    if constexpr(std::is_same_v<Q,std::string>){
      auto elem_size = H5Tget_size(d.getDataType().getId());
      VecShuttle<Q,RT> mover(rows,elem_size);
      return(Rcpp::as<typename xtm_t<Q>::rrvec_type>(mover.template read_vector<Z>(decomp,d)));
    }else{
      VecShuttle<Q,RT> mover(rows,1);
      auto ret=Rcpp::as<typename xtm_t<Q>::rrvec_type>(mover.template read_vector<Z>(decomp,d));
      ret.attr("dim") =  R_NilValue;
      return(ret);
    }
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
    using RRT=typename t2chunk_t<RT>::c_type;
    using CCT=typename t2chunk_t<CT>::c_type;
    const RRT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat);
    const CCT cols(Dim.dimsize[1],Dim.chunksize[1],coldat);
    if constexpr(std::is_same_v<Q,std::string>){
    auto elem_size = H5Tget_size(d.getDataType().getId());
    MatrixShuttle<Q,RRT,CCT> mover(rows,cols,elem_size);
    return(mover.template read_matrix<Z>(decomp,d));
    }else{
      MatrixShuttle<Q,RRT,CCT> mover(rows,cols,1);
    return(mover.template read_matrix<Z>(decomp,d));
    }

  }
};



class VisitMatrixReadRL
{
public:
  const HighFive::DataSet &d;
  const MatDim Dim;
  VisitMatrixReadRL(const HighFive::DataSet &d_,const MatDim Dim_):d(d_),Dim(Dim_){}
  template<typename Q,typename Z,typename RT,typename CT>
  auto operator()(Q ret,Z& decomp,RT rowdat,CT coldat)const{
    using RRT=typename t2chunk_t<RT>::c_type;
    using CCT=typename t2chunk_t<CT>::c_type;

    using tensor_type=typename xtm_t<Q>::buffmat_type;
    using rtensor_type=typename xtm_t<Q>::retmat_type;
    using shape_type = typename tensor_type::shape_type;
    const RRT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat);
    const CCT cols(Dim.dimsize[1],Dim.chunksize[1],coldat);
    DataSet_Context<Q> dsc(d);
    if constexpr(!std::is_same_v<Q,std::string>){
	auto rl = Rcpp::List::import_transform(rows.begin(),rows.end(),[&decomp,&dsc,&cols](const auto rr){

								  auto ctc=static_cast<unsigned long>(cols.total_chunksize());
								  rtensor_type row_mat({static_cast<int>(rr.chunk_size()),static_cast<int>(cols.total_size())});
								  tensor_type buff_mat(shape_type{rr.disk_size(),ctc});
								  auto read_lambda=[&row_mat,&buff_mat,&decomp](const auto& tchunk_r,const auto& tchunk_c,const void* raw_data,std::optional<size_t> res,size_t elem_size){
										     decomp.decompress(buff_mat.data(),buff_mat.size()*sizeof(typename tensor_type::value_type),raw_data,res);
										     if constexpr(std::is_same_v<typename RRT::chunk_type,chunk_chunker> && std::is_same_v<typename CCT::chunk_type,chunk_chunker>){
											 xt::view(row_mat,xt::all(),tchunk_c.mem_slice())=
											   xt::view(buff_mat,tchunk_r.chunk_slice(),tchunk_c.chunk_slice());
										       }else{
										       xt::dynamic_view(row_mat,{xt::all(),tchunk_c.mem_slice()})=
											 xt::dynamic_view(buff_mat,{tchunk_r.chunk_slice(),tchunk_c.chunk_slice()});
										     }
										   };
								  for(const auto& cb:cols){
								    dsc.read_dataset(rr,cb,read_lambda);
								  }
								  return row_mat;
								       });
	return rl;

    }
  }
};





//[[Rcpp::export]]
Rcpp::List read_matrix_rl(const std::string filename,  const std::string datapath, SEXP rows,SEXP cols){

   auto drow=dispatch_subset(rows);
  auto dcol=dispatch_subset(cols);
  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  DataSet_VNS<MatDim> dsv(dset);
  auto data_t =dsv.data_type;
  auto &decomp = dsv.decompressor;
  return(std::visit(VisitMatrixReadRL(dset,dsv.Dim),data_t,decomp,drow,dcol));

}





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

  Rcpp::RObject ret(std::visit(VisitVectorRead(dset,dsv.Dim),data_t,decomp,drow));
  if(!dsv.R_attrs.empty()){
    for(auto &attr : dsv.R_attrs){
      ret.attr(attr.substr(2))=read_attribute(dset,attr);
    }
  }
  return(ret);
}

std::variant<xt::rtensor<int,2>,xt::rtensor<double,2>,Rcpp::StringMatrix> xtensor_mat(Rcpp::RObject data){
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    return(Rcpp::as<xt::rtensor<int,2>>(data));
  }
  case REALSXP: {
    return(Rcpp::as<xt::rtensor<double,2>>(data));
  }
  case STRSXP:{
    return(Rcpp::as<Rcpp::StringMatrix>(data));
  }
  default:{
    Rcpp::stop("Can currently only read and write numeric, integer and string types");
    return(Rcpp::as<xt::rtensor<double,2>>(data));
  }
  }

}



std::variant<xt::rtensor<int,1>,xt::rtensor<double,1>,Rcpp::StringVector> xtensor_vec(Rcpp::RObject data){
  auto my_t = data.sexp_type();
  switch (my_t){
  case INTSXP: {
    return(Rcpp::as<xt::rtensor<int,1>>(data));
  }
  case REALSXP: {
    return(Rcpp::as<xt::rtensor<double,1>>(data));
  }
  case STRSXP: {
    return Rcpp::as<Rcpp::StringVector>(data);
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
  void operator()(Q &write,Z& decomp,RT rowdat,CT coldat)const{
    using RRT=typename t2chunk_t<RT>::c_type;
    using CCT=typename t2chunk_t<CT>::c_type;

    const RRT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat,true);
    const CCT cols(Dim.dimsize[1],Dim.chunksize[1],coldat,true);
    if constexpr(std::is_same_v<Q,Rcpp::StringMatrix>){
    auto elem_size = H5Tget_size(d.getDataType().getId());
    MatrixShuttle<std::string,RRT,CCT> mover(rows,cols,elem_size);
    mover.template write_matrix<Z>(write,decomp,d);
    }else{
      using QR=typename Q::value_type;
      MatrixShuttle<QR,RRT,CCT> mover(rows,cols,1);
      mover.template write_matrix<Z>(write,decomp,d);
    }
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
  void operator()(Q &write,Z& decomp,T rowdat)const{

    using RT=typename t2chunk_t<T>::c_type;
    RT rows(Dim.dimsize[0],Dim.chunksize[0],rowdat,true);
    if constexpr(std::is_same_v<Q,Rcpp::StringVector>){
    auto elem_size = H5Tget_size(d.getDataType().getId());
    VecShuttle<std::string,RT> mover(rows,elem_size);
    mover.template write_vector<Z>(write,decomp,d);
    }else{
      using QT = typename Q::value_type;
      VecShuttle<QT,RT> mover(rows,1);
      mover.template write_vector<Z>(write,decomp,d);
    }
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
  auto data_a =	Rcpp::as<Rcpp::Nullable<Rcpp::IntegerVector> >(data.attr("dim"));

  HighFive::File file(filename,HighFive::File::ReadWrite);
  auto w_t=xtensor_vec(data);
  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  DataSet_V<VecDim> dsv(dset);
  //  auto data_t =dsv.data_type;
  auto &decomp = dsv.decompressor;


  std::visit(VisitVectorWrite(dset,dsv.Dim),w_t,decomp,drow);
  if(data_a.isNull()){
    data.attr("dim")=R_NilValue;
  }
    //
    //    Rcpp::Rcout<<Rcpp::wrap(data.attr("dim"))<<data_a<<std::endl;
}
