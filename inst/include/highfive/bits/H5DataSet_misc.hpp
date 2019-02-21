/*
 *  Copyright (c), 2017, Adrien Devresse <adrien.devresse@epfl.ch>
 *
 *  Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 *
 */
#pragma once

#include <algorithm>
#include <functional>
#include <numeric>
#include <sstream>
#include <string>

#ifdef H5_USE_BOOST
#include <boost/multi_array.hpp>
#endif

#include <H5Dpublic.h>
#include <H5Ppublic.h>

#include "../H5DataSet.hpp"
#include "../H5DataSpace.hpp"
#include "../H5DataType.hpp"

#include "H5Slice_traits_misc.hpp"
#include "H5Utils.hpp"

namespace HighFive {

  inline DataSet::DataSet() {}

  inline size_t DataSet::getStorageSize() const {
    return H5Dget_storage_size(_hid);
  }

  inline DataType DataSet::getDataType() const {
    DataType res;
    res._hid = H5Dget_type(_hid);
    return res;
  }






  inline std::vector<hsize_t> DataSet::get_num_chunks() const{


    auto dim_size = this->getSpace().getDimensions();
    auto filt =	this->getFilter();
    const size_t num_dims_ds = dim_size.size();

    const size_t num_dims_chunks = filt.chunksizes.size();

    std::vector<hsize_t> retvec(num_dims_chunks);


    if(num_dims_chunks==0){
      return(retvec);
    }

    if (num_dims_chunks != num_dims_ds) {
      HDF5ErrMapper::ToException<DataSetException>(
          "Filter chunksize rank does not match dataset rank");
    }

    std::transform(dim_size.begin(),dim_size.end(),filt.chunksizes.begin(),retvec.begin(),std::divides<hsize_t>());
    return(retvec);

  }


  inline std::vector<hsize_t> DataSet::get_chunk_offset(const std::vector<hsize_t> &chunk_idx) const{
    auto num_chunk_v=get_num_chunks();
    if(chunk_idx.size()!=num_chunk_v.size()){
        HDF5ErrMapper::ToException<DataSetException>(
            "Filter chunksize rank does not match chunk_idx.size()");
      }
    std::vector<hsize_t> retvec(chunk_idx.size());

    if(retvec.size()==0){
      return(retvec);
    }

    if (chunk_idx > num_chunk_v) {
      HDF5ErrMapper::ToException<DataSetException>(
						   "Chunk index greater than number of chunks!");
    }

    auto filt = this->getFilter();
    const size_t num_dims_chunks = filt.chunksizes.size();

    std::transform(chunk_idx.begin(), chunk_idx.end(), filt.chunksizes.begin(), retvec.begin(),
                   std::multiplies<hsize_t>());
    return (retvec);
  }


  inline std::optional<size_t> DataSet::read_raw_chunk(std::vector<std::byte> &data_buff, const std::vector<size_t> &offsets) const {
    hsize_t chunk_nbytes;

    //    T ret_vec(read_chunk_nbytes);
    uint32_t    read_filter_mask = 0; //filter mask after direct read
    std::vector<hsize_t> h_offsets(offsets.size());
    std::copy(offsets.begin(),offsets.end(),h_offsets.begin());

    auto ret =H5Dget_chunk_storage_size(_hid, h_offsets.data(), &chunk_nbytes);
    auto buff_size = data_buff.size();
    if(chunk_nbytes>buff_size){
      size_t newsize=chunk_nbytes;
      //      Rcpp::Rcerr<<"resizing from "<<data_buff.size()<<"to "<<newsize<<std::endl;
      data_buff.resize(newsize);
      // HDF5ErrMapper::ToException<DataSetException>(
      //     "Chunk larger than allocated storage!");
      //      return(
    }

    if ((H5Dread_chunk(_hid, H5P_DEFAULT, h_offsets.data(),
		       &read_filter_mask, data_buff.data())) < 0) {
      HDF5ErrMapper::ToException<DataSetException>(
						   "Unable to read direct chunk");
    }
    if(read_filter_mask!=0){
      return(std::nullopt);
    }

    return chunk_nbytes;
  }



  inline void DataSet::write_raw_chunk(std::vector<std::byte> &data_buff, const std::vector<size_t> &offsets,std::optional<size_t> buff_size) const {
    uint32_t    filter_mask = 0; //filter mask after direct read

    std::vector<hsize_t> h_offsets(offsets.size());
    std::copy(offsets.begin(),offsets.end(),h_offsets.begin());
    if(buff_size){
      if(H5Dwrite_chunk(_hid, H5P_DEFAULT, filter_mask,
			h_offsets.data(), buff_size.value(), data_buff.data())<0){
	      HDF5ErrMapper::ToException<DataSetException>(
							   "Unable to write direct chunk");
      }
    }else{
      filter_mask = 0x00000001;
      if(H5Dwrite_chunk(_hid, H5P_DEFAULT, filter_mask,
			h_offsets.data(), data_buff.size(), data_buff.data())){
	HDF5ErrMapper::ToException<DataSetException>(
						     "Unable to write direct chunk");
      }
    }
  }
  // template<typename T>
  // inline T DataSet::read_raw_chunk(const std::vector<hsize_t> &chunk_idx) const {

  //   hsize_t read_chunk_nbytes;


  //   auto offset_vec = get_chunk_offset(chunk_idx);
  //   auto ret =H5Dget_chunk_storage_size(_hid, offset_vec.data(), &read_chunk_nbytes);
  //   T ret_vec(read_chunk_nbytes);
  //   unsigned    read_filter_mask = 0; filter mask after direct read

  //   if ((H5Dread_chunk(_hid, H5P_DEFAULT, offset_vec.data(),
  //                      &read_filter_mask, (void *)&ret_vec[0])) < 0) {
  //     HDF5ErrMapper::ToException<DataSetException>(
  //         "Unable to read direct chunk");
  //   }
  //   return (ret_vec);
  // }

  inline bool DataSet::isTransposed() const {
    int transpose_int = 0;
    if (this->hasAttribute("doTranspose")) {
      this->getAttribute("doTranspose").read(transpose_int);
    }
    return (transpose_int != 0);
  }

  inline void DataSet::extend(const std::vector<size_t> &new_dims) {
    std::vector<hsize_t> real_new_dims(new_dims.size());
    std::copy(new_dims.begin(), new_dims.end(), real_new_dims.begin());
    if ((_hid =  H5Dset_extent(_hid,real_new_dims.data())) < 0) {
	HDF5ErrMapper::ToException<DataSetException>(
            "Unable to extend DataSet");
    }
  }

  inline void DataSet::setTranspose(const bool transpose) {
    int transpose_int = transpose ? 1 : 0;
    auto transpose_attr = this->createAttribute<int>("doTranspose", DataSpace::From(transpose_int));
    transpose_attr.write(transpose_int);
    doTranspose = transpose;
  }



inline DataSpace DataSet::getSpace() const {
    DataSpace space;
    if ((space._hid = H5Dget_space(_hid)) < 0) {
        HDF5ErrMapper::ToException<DataSetException>(
            "Unable to get DataSpace out of DataSet");
    }
    return space;
}

inline Filter DataSet::getFilter() const {
    Filter filt;
    const size_t rank = this->getMemSpace().getNumberDimensions();
    std::vector<hsize_t> tvec(rank);



    if ((filt._hid =  H5Dget_create_plist(_hid)) < 0) {
        HDF5ErrMapper::ToException<DataSetException>("Unable to get Filter out of DataSet");
    }
    if(H5Pget_layout(filt._hid) ==H5D_CHUNKED){
      filt.chunksizes.resize(rank);
      auto ret = H5Pget_chunk(filt._hid, rank, tvec.data());
      std::copy(tvec.begin(),tvec.end(),filt.chunksizes.begin());
    }

    return filt;
}

inline DataSpace DataSet::getMemSpace() const { return getSpace(); }
}

