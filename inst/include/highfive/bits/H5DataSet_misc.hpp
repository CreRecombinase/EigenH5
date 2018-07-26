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

