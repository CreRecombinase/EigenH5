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

        Filter(const std::vector<size_t> &chunk_dims, const hid_t filter_id, const int r,
               const bool doTranspose = false);
        Filter(const std::vector<size_t> &chunk_dims,
               const std::vector<size_t> &data_dims,
               const hid_t filter_id,
               const int r,
               const bool doTranspose = false);
        static std::vector<size_t> guess_chunk(const std::vector<size_t> data_shape, const size_t typesize);
#ifdef H5_USE_EIGEN

        template<typename Scalar, int RowsAtCompileTime, int ColsAtCompileTime, int Options>
        Filter(const std::vector<size_t> &chunk_dims,
               const Eigen::Matrix<Scalar, RowsAtCompileTime, ColsAtCompileTime, Options> &mat, const hid_t filter_id,
               const bool doTranspose = false);
        template<typename Scalar, int RowsAtCompileTime, int ColsAtCompileTime, int Options>
        Filter(const std::vector<size_t> &chunk_dims,
               const Eigen::Map<Eigen::Matrix<Scalar, RowsAtCompileTime, ColsAtCompileTime, Options> > &mat, const hid_t filter_id,
               const bool doTranspose = false);

#endif
        hid_t getId() const;
          static std::vector<size_t> reset_chunks_vec(const std::vector<size_t> &chunk_dims,
                                                     const std::vector<size_t> mat_dims);

    protected:

    
        template<typename Scalar, int RowsAtCompileTime, int ColsAtCompileTime, int Options>
        std::vector<size_t> reset_chunks(const std::vector<size_t> &chunk_dims,
                                                 const Eigen::Map<Eigen::Matrix<Scalar, RowsAtCompileTime, ColsAtCompileTime, Options> >&mat);
        template<typename Scalar, int RowsAtCompileTime, int ColsAtCompileTime, int Options>
        std::vector<size_t> reset_chunks(const std::vector<size_t> &chunk_dims,
                                                 const Eigen::Matrix<Scalar, RowsAtCompileTime, ColsAtCompileTime, Options> &mat);
        // protected constructor
        hid_t _hid;


    };

} // HighFive

#include "bits/H5Filter_misc.hpp"


