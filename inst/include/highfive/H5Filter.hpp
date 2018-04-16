//
// Created by nwknoblauch on 12/2/17.
//

#pragma once

#include "H5Object.hpp"
#include "H5PropertyList.hpp"

#ifdef H5_USE_EIGEN

#include <Eigen/Core>

#endif

namespace HighFive {

///
/// \brief Generic HDF5 property List
///
    class Filter {
    public:
        static const size_t CHUNK_BASE = 16*1024;
        static const size_t CHUNK_MIN = 8*1024;
        static const size_t CHUNK_MAX = 1024*1024;


      Filter(const std::vector<size_t> &chunk_dims, const hid_t filter_id, const int r);
      Filter();
      static std::vector<size_t> guess_chunk(const std::vector<size_t> data_shape);
      static Filter From(const DataSpace &dataspace,const hid_t filter_id);
#ifdef H5_USE_EIGEN

#endif
        hid_t getId() const;
      std::vector<size_t> get_chunksizes()const;

    protected:
      std::vector<size_t> chunksizes;
        // protected constructor
        hid_t _hid;


    };

} // HighFive

#include "bits/H5Filter_misc.hpp"


