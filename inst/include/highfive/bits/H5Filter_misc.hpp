//
// Created by nwknoblauch on 12/2/17.
//

#pragma once

#include "../H5Filter.hpp"
#include <functional>

namespace HighFive {

  inline Filter::Filter() {
    _hid = H5Pcreate(H5P_DATASET_CREATE);
  }


  inline Filter::Filter(const std::vector<size_t> &chunk_dims, const hid_t filter_id, const int r) {
    if (r < 0) {
      HDF5ErrMapper::ToException<FilterException>(
						  "Filter Improperly registered");
    }
    _hid = H5Pcreate(H5P_DATASET_CREATE);
    if (_hid < 0) {
      HDF5ErrMapper::ToException<FilterException>(
						  "Unable to get create PropertyList");
    }
    size_t chunk_size = std::accumulate(chunk_dims.begin(), chunk_dims.end(), 1, std::multiplies<size_t>());
    if(chunk_size>CHUNK_MAX){
      HDF5ErrMapper::ToException<FilterException>(
						  "Chunk size:"+std::to_string(chunk_size)+" is larger than max chunksize:  "+std::to_string(CHUNK_MAX));
    }



    const size_t c_size = chunk_dims.size();
    std::vector<hsize_t> nchunk_dims(c_size);
    std::copy(chunk_dims.begin(), chunk_dims.end(), nchunk_dims.begin());

    //std::cout<<"Final Chunk Dims: "<<nchunk_dims[0]<<" "<<nchunk_dims[1]<<std::endl;
    auto rr = H5Pset_chunk(_hid, c_size, nchunk_dims.data());
    if (rr < 0) {
      HDF5ErrMapper::ToException<FilterException>(
						  "Unable to set chunk size");
    }
    rr = H5Pset_filter(_hid, filter_id, H5Z_FLAG_OPTIONAL, 0, NULL);
    if (rr < 0) {
      HDF5ErrMapper::ToException<FilterException>(
						  "Unable to set filter");
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
  inline Filter Filter::From(const DataSpace &dataspace,const hid_t filter_id){
    return(Filter(guess_chunk(dataspace.getDimensions()),filter_id,1));
  }

  inline hid_t Filter::getId() const {
    return _hid;
  }

  inline std::vector<size_t> Filter::get_chunksizes()const{
    return(chunksizes);
  }



} // HighFive





