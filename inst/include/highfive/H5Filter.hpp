//
// Created by nwknoblauch on 12/2/17.
//

#pragma once

#include "H5Object.hpp"
#include "H5PropertyList.hpp"



#include <Eigen/Core>



namespace HighFive {

///
/// \brief Generic HDF5 property List
///
    class Filter {
    public:
      static const hid_t gzip = 1;
      static const hid_t blosc = 32001;
      static const hid_t lzf = 32000;
      static const hid_t zstd = 32015;
      static const hid_t no_filter = 0;
      static const size_t CHUNK_BASE = 16*1024;
      static const size_t CHUNK_MIN = 8*1024;
      static const size_t CHUNK_MAX = 1024*1024;
      Filter(const std::vector<size_t> &chunk_dims, const hid_t filter_id, std::vector<unsigned int> cd_values);
      Filter();
      static std::vector<size_t> guess_chunk(const std::vector<size_t> data_shape);
      static Filter From(const DataSpace &dataspace,const hid_t filter_id,std::vector<unsigned int> cd_values={});
      hid_t getId() const;
      std::vector<size_t> get_chunksizes()const;
      std::pair<std::string,std::vector<unsigned int> > get_filter_info() const;
    protected:
      std::vector<size_t> chunksizes;
      hid_t _hid;
      friend class ::HighFive::DataSet;
    };


} // HighFive

#include "bits/H5Filter_misc.hpp"


