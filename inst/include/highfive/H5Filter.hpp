//
// Created by nwknoblauch on 12/2/17.
//

#pragma once

#include "H5Object.hpp"
#include "H5PropertyList.hpp"
#include "H5Zpublic.h"
#include "H5DataSpace.hpp"
#include <functional>
#include <numeric>
#include <optional>
#include <variant>


#ifdef USE_BLOSC
#include "blosc_filter.h"
#endif

#ifdef USE_LZF
#include "lzf/lzf_filter.h"
#endif


#include "zstd/zstd_h5plugin.h"
#include "zstd/zstd.h"
#include "zlib.h"


class NoCompressor{
public:
  NoCompressor(const std::vector<unsigned int> &cd){}

  void decompress(void* uncompressed,size_t uc_size,const void* compressed,std::optional<size_t> cb_size){
    std::memcpy(uncompressed,compressed,cb_size.value_or(uc_size));
  }

  std::optional<size_t> compress(void* compressed,size_t cb_size,const void* uncompressed,size_t uc_size){
#ifdef DEBUG
    if(uc_size>cb_size){
      Rcpp::stop("compressed buffer is smaller than uncompressed buffer in NoCompressor");
    }
#endif
    std::memcpy(compressed,uncompressed,uc_size);
    return uc_size;
  }
};



class GzipCompressor{
public:
  const int aggression;
  GzipCompressor(const std::vector<unsigned int> &cd_):aggression(static_cast<int>(cd_[0])){}
  void decompress(void* uncompressed,size_t uc_size,const void* compressed,std::optional<size_t> cb_size){
    if(cb_size){
      size_t tcb_size=uc_size;

      auto ret = uncompress(reinterpret_cast<unsigned char*>(uncompressed),
			    &tcb_size,
			    reinterpret_cast<const unsigned char*>(compressed),
			    *cb_size);
      // ret = uncompress((Bytef *)read_dst_buf, (uLongf *)&buf_size, pt_readbuf, (uLong)read_chunk_nbytes);

      if(Z_BUF_ERROR == ret) {
	Rcpp::stop("Error decompressing data");
      }
    }
    else{
      std::memcpy(uncompressed,compressed,uc_size);
    }
  }


  std::optional<size_t> compress(void* compressed,size_t cb_size,const void* uncompressed,size_t uc_size){
    size_t o_cb_size=cb_size;
    auto ret = compress2(reinterpret_cast<unsigned char*>(compressed), &cb_size,reinterpret_cast<const unsigned char*>(uncompressed),uc_size, aggression);
    if(Z_BUF_ERROR == ret) {
      Rcpp::stop("zbuff overflow");
    } else if(Z_MEM_ERROR == ret) {
      Rcpp::stop("deflate memory error");
    } else if(Z_OK != ret) {
      Rcpp::stop("other deflate error");
    }
    if(cb_size<o_cb_size){
      return cb_size;
    }else{
      std::memcpy(compressed,uncompressed,uc_size);
      return std::nullopt;
    }
  }
};


class LzfCompressor{
public:
  LzfCompressor(const std::vector<unsigned int> &cd_){
#ifndef USE_LZF
    Rcpp::stop("please recompile EigenH5 with -DUSE_LZF (and the lzf library)");
#endif

  }
  void memcpy(void* output,size_t osize,const void* input,size_t isize){
    std::memcpy(output,input,osize);
  }
  void decompress(void* uncompressed,size_t uc_size,const void* compressed,std::optional<size_t> cb_size){
#ifndef USE_LZF
    Rcpp::stop("please recompile EigenH5 with -DUSE_LZF (and the lzf library)");
#else

    if(cb_size){
      auto ret = lzf_decompress(compressed,*cb_size,uncompressed,uc_size);
      if(!ret){
	if(errno == E2BIG){
	  Rcpp::stop("lzf too small");
	} else if(errno == EINVAL) {

	  Rcpp::stop("Invalid data for LZF decompression");

	} else {
	  Rcpp::stop("Unknown LZF decompression error");
	}


	if(Z_BUF_ERROR == ret) {
	  Rcpp::stop("Error decompressing data");
	}
      }
    }else{
      std::memcpy(uncompressed,compressed,uc_size);
    }
#endif
  }
  std::optional<size_t> compress(void* compressed,size_t cb_size,const void* uncompressed,const size_t uc_size) noexcept{
#ifndef USE_LZF
    Rcpp::stop("please recompile EigenH5 with -DUSE_LZF (and the lzf library)");
#else
    auto ret = lzf_compress(uncompressed,uc_size,compressed,cb_size);
    if(ret>0){
      return(ret);
    }else{
      std::memcpy(compressed,uncompressed,uc_size);
      return std::nullopt;
    }
#endif
  }

  
};
  
  
  
class BloscCompressor{
#ifdef USE_BLOSC
  int clevel;
  int doshuffle;
  int compcode;
  int code;
  int typesize;
  int outbuf_size;
#endif

public:
  BloscCompressor(const std::vector<unsigned int> cd_){
#ifndef USE_BLOSC
    Rcpp::stop("please recompile EigenH5 with -DUSE_BLOSC (and the blosc library)");
#else

    char tstr [] = "blosclz";
    const char* compname = tstr;     /* The compressor by default */
    auto cds =cd_.size();
    if(cds>4){
      typesize=static_cast<int>(cd_[2]);
      outbuf_size=static_cast<int>(cd_[3]);
    }else{
      Rcpp::stop("Blosc Compressor needs the values from cd_");
    }
    if(cds>=5){
      clevel=static_cast<int>(cd_[4]);
    }else{
      clevel=5;
    }
    if(cds>=6){
      doshuffle=static_cast<int>(cd_[5]);
    }else{
      doshuffle=1;
    }
    if(cds>=7){
      compcode=static_cast<int>(cd_[6]);
      auto complist = blosc_list_compressors();
      code = blosc_compcode_to_compname(compcode, &compname);
      if (code == -1) {
	Rcpp::Rcerr<<"this Blosc library does not have support for the "<<compname<<" compressor, but only for:"<<complist<<std::endl;
	Rcpp::stop("Can't initialize blosc");
      }
    }

    blosc_set_compressor(compname);
#endif
  }
  void decompress(void* uncompressed,size_t uc_size,const void* compressed,std::optional<size_t> cb_size){
#ifdef USE_BLOSC
    if(cb_size){
      size_t cbytes, blocksize,outbuf_size;
      size_t osize=uc_size;
      blosc_cbuffer_sizes(compressed, &outbuf_size, &cbytes, &blocksize);
      if(outbuf_size>osize){
	Rcpp::stop("output buffer size larger than total size!");
      }
      auto  status = blosc_decompress(compressed, uncompressed, uc_size);
      if (status <= 0) {    /* decompression failed */
	Rcpp::stop("Blosc decompression error");
      }
    }else{
      std::memcpy(uncompressed,compressed,uc_size);
    }
#endif
  }

  std::optional<size_t> compress(void* compressed,size_t cb_size,const void* uncompressed,size_t uc_size){
#ifdef USE_BLOSC
    size_t cbytes, blocksize,outbuf_size;
    auto  status = blosc_compress(clevel,doshuffle,typesize,uc_size,uncompressed,compressed,cb_size);
    if (status <= 0) {    /* decompression failed */
      std::memcpy(compressed,uncompressed,uc_size);
      return std::nullopt;
    }
    return(status);
    #else
    Rcpp::stop("Rebuild EigenH5 using BLOSC");
    return(std::nullopt);
#endif
  }
};
  
  
  
  
class ZstdCompressor{
public:
  std::unique_ptr<ZSTD_DCtx,decltype(&ZSTD_freeDCtx)> ctxt_d;
  std::unique_ptr<ZSTD_CCtx,decltype(&ZSTD_freeCCtx)> ctxt_c;
  int aggression;
  ZstdCompressor(const std::vector<unsigned int> cd_):ctxt_d(ZSTD_createDCtx(),&ZSTD_freeDCtx),
						      ctxt_c(ZSTD_createCCtx(),&ZSTD_freeCCtx){
    if(cd_.size()>0){
      aggression=static_cast<int>(cd_[0]);
    }else{
      aggression=22;
    }
  }
  //  std::optional<size_t> compress(void* compressed,size_t cb_size,const void* uncompressed,size_t uc_size){
  void decompress(void* uncompressed,size_t uc_size,const void* compressed,std::optional<size_t> cb_size){
    if(cb_size){
      auto rc = ZSTD_decompressDCtx(ctxt_d.get(),uncompressed, uc_size, compressed,*cb_size);
      if(ZSTD_isError(rc)){
	Rcpp::Rcerr<<ZSTD_getErrorName(rc)<<std::endl;
        auto ret2 = ZSTD_getFrameContentSize((void*) compressed,*cb_size);

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
	  Rcpp::Rcerr<<"Frame size is :"<<ret2<<" and you said it was :"<<uc_size<<std::endl;
	  Rcpp::stop("Error decompressing zstd");
	}
	Rcpp::stop("Error decompressing zstd");
      }
    }else{
      std::memcpy(uncompressed,compressed,uc_size);
    }
  }

  std::optional<size_t> compress(void* compressed,size_t cb_size,const void* uncompressed,size_t uc_size){

    auto rc = ZSTD_compressCCtx(ctxt_c.get(),compressed, cb_size,uncompressed,uc_size,aggression);
    if(ZSTD_isError(rc)){
      Rcpp::Rcerr<<"There was an error with compression!"<<std::endl;
      Rcpp::Rcerr<<ZSTD_getErrorName(rc)<<std::endl;
      return std::nullopt;
    }
    if(rc<0){
      std::memcpy(compressed,uncompressed,uc_size);
      return std::nullopt;
    }
    return rc;
  }
};
  


namespace HighFive {

  class Filter: public Object {

  public:
    static const hid_t gzip;// = 1;
    static const hid_t blosc;// = 32001;
    static const hid_t lzf4;// = 32000;
    static const hid_t zstd;// = 32015;
    static const hid_t no_filter;// = 0;
    static const size_t CHUNK_BASE = 16*1024;
    static const size_t CHUNK_MIN = 8*1024;
    static const size_t CHUNK_MAX = 1024*1024;
    Filter(const std::vector<size_t> &chunk_dims, const hid_t filter_id, std::vector<unsigned int> cd_values);
    using compressors = std::variant<NoCompressor,LzfCompressor,BloscCompressor,GzipCompressor,ZstdCompressor>;
    Filter();
    static std::vector<size_t> guess_chunk(const std::vector<size_t> data_shape);
    static Filter From(const DataSpace &dataspace,const hid_t filter_id,std::vector<unsigned int> cd_values={});
    hid_t getId() const;
    const std::vector<size_t> &get_chunksizes()const;
    compressors getDecompressor() const;

    std::pair<std::string,std::vector<unsigned int> > get_filter_info() const;
  protected:
    std::vector<size_t> chunksizes;
    //    hid_t _hid;
    friend class ::HighFive::DataSet;
  };


} // HighFive





namespace HighFive {

  const hid_t filter_gzip = 1;
  const hid_t filter_blosc = 32001;
  const hid_t filter_lzf4 = 32000;
  const hid_t filter_zstd = 32015;
  const hid_t filter_no_filter = 0;

  inline Filter::Filter() {
    _hid = H5Pcreate(H5P_DATASET_CREATE);
  }
  inline Filter::Filter(const std::vector<size_t> &chunk_dims, const hid_t filter_id, std::vector<unsigned int> cd_values):
    chunksizes(chunk_dims) {
    _hid = H5Pcreate(H5P_DATASET_CREATE);
    if (_hid < 0) {
      HDF5ErrMapper::ToException<FilterException>("Unable to get create PropertyList");
    }
    size_t chunk_size = std::accumulate(chunk_dims.begin(), chunk_dims.end(), 1, std::multiplies<size_t>());
    if(chunk_size>CHUNK_MAX){
      HDF5ErrMapper::ToException<FilterException>("Chunk size:"+std::to_string(chunk_size)+" is larger than max chunksize:  "+std::to_string(CHUNK_MAX));
    }
    const size_t c_size = chunk_dims.size();
    std::vector<hsize_t> nchunk_dims(c_size);
    std::copy(chunk_dims.begin(), chunk_dims.end(), nchunk_dims.begin());
    herr_t rr = 0;
    if(c_size>0){
      rr = H5Pset_chunk(_hid, c_size, nchunk_dims.data());
      if (rr < 0) {
	HDF5ErrMapper::ToException<FilterException>("Unable to set chunk size");
      }
    }else{
      if(filter_id!=filter_no_filter){
	HDF5ErrMapper::ToException<FilterException>("Compression filter cannot be used without chunking");
      }
    }

    switch(filter_id){
    case filter_gzip:{
      unsigned int comp_opt = 1;
      if(!cd_values.empty()){
	comp_opt = cd_values[0];
      }
      rr =H5Pset_deflate(_hid,comp_opt);
      break;
    }
    case filter_lzf4:{
      rr = H5Pset_shuffle(_hid);
      rr = H5Pset_filter(_hid, filter_id, H5Z_FLAG_OPTIONAL, 0, NULL);
      break;
    }
    case filter_no_filter:{
      break;
    }
    default:{
      if(cd_values.empty()){
	rr = H5Pset_filter(_hid, filter_id, H5Z_FLAG_OPTIONAL, 0, nullptr);
      }else{
	rr = H5Pset_filter(_hid, filter_id, H5Z_FLAG_OPTIONAL, cd_values.size(), cd_values.data());
      }
    }
    }
    if (rr < 0) {
      HDF5ErrMapper::ToException<FilterException>("Unable to set filter");
    }
  }
  inline std::vector<size_t> Filter::guess_chunk(std::vector<size_t> data_shape){
    using it_el = std::vector<size_t>::value_type;
    auto bad_el = std::find_if(data_shape.begin(),data_shape.end(),[=](it_el it){
	  return(it>=CHUNK_MAX);
      });
    while(bad_el!=data_shape.end()){
      *bad_el/=2;
      bad_el = std::find_if(data_shape.begin(),data_shape.end(),[=](it_el it){
								  return(it>=CHUNK_MAX);
								});
    }
    auto chunk_size=data_shape;
    size_t prop_chunk_size = std::accumulate(chunk_size.begin(), chunk_size.end(), 1, std::multiplies<size_t>());
    while(prop_chunk_size>=CHUNK_MAX){
      auto it=chunk_size.begin();
      *it/=2;
      prop_chunk_size = std::accumulate(chunk_size.begin(), chunk_size.end(), 1, std::multiplies<size_t>());
    }
    return(chunk_size);
  }
  inline Filter Filter::From(const DataSpace &dataspace,const hid_t filter_id,std::vector<unsigned int> cd_values){
    return(Filter(guess_chunk(dataspace.getDimensions()),filter_id,cd_values));
  }
  inline hid_t Filter::getId() const {
    return _hid;
  }
  inline std::pair<std::string,std::vector<unsigned int> > Filter::get_filter_info() const{
    std::vector<unsigned int> cd(10);
    std::vector<unsigned int> flgs(10);
    unsigned int tflg;
    std::vector<char> name(30);
    size_t ncd=cd.size();
    size_t nname = name.size();
    //We want to get the last filter
    int num_filt=H5Pget_nfilters(_hid);
    if(num_filt>0){
      auto filt_ret=H5Pget_filter(_hid,
				  num_filt-1,
				  flgs.data(),
				  &ncd,
				  cd.data(),
				  nname,
				  name.data(),
				  &tflg);
      if(filt_ret<0){
	HDF5ErrMapper::ToException<FilterException>("Unable to get filter");
      }
      std::string	nameret(name.data());
      std::vector<unsigned int> ret(ncd);
      std::copy(cd.begin(),cd.begin()+ncd,ret.begin());
      return(std::make_pair(nameret,ret));
    }else{
      return(std::make_pair("no_filter",std::vector<unsigned int>()));
    }
  }

  inline Filter::compressors Filter::getDecompressor() const{
    std::vector<unsigned int> cd(10);
    std::array<unsigned int,10> flgs;
    unsigned int tflg;
    std::vector<char> name(30);
    size_t ncd=cd.size();
    size_t nname = name.size();
    //We want to get the last filter
    int num_filt=H5Pget_nfilters(_hid);
    if(num_filt>0){
      auto filt_ret=H5Pget_filter(_hid,
				  num_filt-1,
				  flgs.data(),
				  &ncd,
				  cd.data(),
				  nname,
				  name.data(),
				  &tflg);
      if(filt_ret<0){
	HDF5ErrMapper::ToException<FilterException>("Unable to get filter");
      }
      switch(filt_ret){
      case H5Z_FILTER_DEFLATE:{
	//	GzipCompressor gzd(cd);
	return(GzipCompressor(cd));
      }
      case 32001:{
	//	BloscCompressor blodo(cd);
      	return(BloscCompressor(cd));
      }
      case 32000:{
	//	LzfCompressor lzd(cd);
	return(LzfCompressor(cd));
      }
      case 32015:{
	//        ZstdCompressor zstdd(cd);
	return(ZstdCompressor(cd));
      }
      default: {
	Rcpp::Rcerr<<"Invalid filter:"<< filt_ret<<std::endl;
	return NoCompressor(cd);
      }
      }
    }else{
      return NoCompressor(cd);
    }
  }

  inline const std::vector<size_t> &Filter::get_chunksizes()const{
    return(chunksizes);
  }

} // HighFive




//#include "bits/H5Filter_misc.hpp"
