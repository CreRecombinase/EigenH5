//
// Created by nwknoblauch on 12/2/17.
//

#pragma once
#include "../H5Filter.hpp"
//#include "lzf_filter.h"
//#include "
#include "H5Zpublic.h"
#include <functional>

namespace HighFive {

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
    auto rr = H5Pset_chunk(_hid, c_size, nchunk_dims.data());
    if (rr < 0) {
      HDF5ErrMapper::ToException<FilterException>("Unable to set chunk size");
    }
    switch(filter_id){
    case gzip:{
      unsigned int comp_opt = 1;
      if(!cd_values.empty()){
	comp_opt = cd_values[0];
      }
      rr =H5Pset_deflate(_hid,comp_opt);
      break;
    }
    case lzf:{
      rr = H5Pset_shuffle(_hid);
      rr = H5Pset_filter(_hid, filter_id, H5Z_FLAG_OPTIONAL, 0, NULL);
      break;
    }
    default:{
      if(cd_values.empty()){
	rr = H5Pset_filter(_hid, filter_id, H5Z_FLAG_OPTIONAL, 0, nullptr);
      }else{
	//      unsigned int* tcd = cd_values.data();
	rr = H5Pset_filter(_hid, filter_id, H5Z_FLAG_OPTIONAL, cd_values.size(), cd_values.data());
      }
    }
    }
    if (rr < 0) {
      HDF5ErrMapper::ToException<FilterException>("Unable to set filter");
    }
  }
  inline std::vector<size_t> Filter::guess_chunk(std::vector<size_t> data_shape){
    auto bad_el = std::find_if(data_shape.begin(),data_shape.end(),[=](auto it){
	  return(it>=CHUNK_MAX);
      });
    while(bad_el!=data_shape.end()){
      *bad_el/=2;
      bad_el = std::find_if(data_shape.begin(),data_shape.end(),[=](auto it){
								  return(it>=CHUNK_MAX);
								});
    }
    auto chunk_size=data_shape;
    size_t prop_chunk_size = std::accumulate(chunk_size.begin(), chunk_size.end(), 1, std::multiplies<size_t>());
    size_t iti=0;
    while(prop_chunk_size>=CHUNK_MAX){
      // auto it=data_shape[(iti++ %
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
    auto filt_ret=H5Pget_filter(_hid,
				0,
				flgs.data(),
				&ncd,
				cd.data(),
				nname,
				name.data(),
				&tflg);
    std::string	nameret(name.data());
    std::vector<unsigned int> ret(ncd);
    std::copy(cd.begin(),cd.begin()+ncd,ret.begin());
    return(std::make_pair(nameret,ret));
  }
  
  inline std::vector<size_t> Filter::get_chunksizes()const{
    return(chunksizes);
  }

} // HighFive





