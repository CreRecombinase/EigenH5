/*
 *  Copyright (c), 2017, Adrien Devresse <adrien.devresse@epfl.ch>
 *
 *  Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 *
 */
#pragma once

#include "H5Slice_traits.hpp"

#include <algorithm>
#include <cassert>
#include <functional>
#include <numeric>
#include <sstream>
#include <string>

#ifdef H5_USE_BOOST
#include <boost/multi_array.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#endif

#include <H5Dpublic.h>
#include <H5Ppublic.h>

#include "../H5DataSpace.hpp"
#include "../H5DataType.hpp"
#include "../H5Selection.hpp"

#include "H5Converter_misc.hpp"

namespace HighFive {

namespace details {

// map the correct reference to the dataset depending of the layout
// dataset -> itself
// subselection -> parent dataset
inline const DataSet& get_dataset(const Selection* ptr) {
    return ptr->getDataset();
}

inline const DataSet& get_dataset(const DataSet* ptr) { return *ptr; }

// map the correct memspace identifier depending of the layout
// dataset -> entire memspace
// selection -> resolve space id
inline hid_t get_memspace_id(const Selection* ptr) {
    return ptr->getMemSpace().getId();
}

inline hid_t get_memspace_id(const DataSet* ptr) {
    (void)ptr;
    return H5S_ALL;
}
}

inline ElementSet::ElementSet(const std::vector<std::size_t>& element_ids)
    : _ids(element_ids) {}


    template<typename Derivate>
    inline Selection
    SliceTraits<Derivate>::select(const std::vector<size_t> &offset,
                                  const std::vector<size_t> &count,
                                  const std::vector<size_t> &stride) const {
        // hsize_t type convertion
        // TODO : normalize hsize_t type in HighFive namespace

        bool isTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
	if(isTranspose){
	  HDF5ErrMapper::ToException<DataSpaceException>(
							 "Transposed data has been deprecated");
	}


        std::vector<hsize_t> offset_local(offset.size());
        std::vector<hsize_t> count_local(count.size());
        std::vector<hsize_t> stride_local(stride.size());
        std::copy(offset.begin(), offset.end(), offset_local.begin());
        std::copy(count.begin(), count.end(), count_local.begin());
        std::copy(stride.begin(), stride.end(), stride_local.begin());

        DataSpace space = static_cast<const Derivate *>(this)->getSpace().clone();
        if (H5Sselect_hyperslab(space.getId(), H5S_SELECT_SET, offset_local.data(),
                                stride.empty() ? NULL : stride_local.data(),
                                count_local.data(), NULL) < 0) {
            HDF5ErrMapper::ToException<DataSpaceException>(
                    "Unable to select hyperslap");
        }

        return Selection(DataSpace(count), space,
                         details::get_dataset(static_cast<const Derivate *>(this)));
    }

#ifdef H5_USE_EIGEN

  template<typename Derivate>
  template<size_t Dims>
  inline Selection
  SliceTraits<Derivate>::selectRanges(const std::vector<std::array<size_t,Dims> > &offset,
				      const std::vector<std::array<size_t,Dims> > &count,
				      const std::array<size_t,Dims> &space_dim) const {
    // hsize_t type convertion
    const size_t num_ranges=offset.size();
    const size_t vec_size=Dims;
    if(num_ranges!=count.size()){
      HDF5ErrMapper::ToException<DataSpaceException>(
						     "offset and count must have the same number of elements");
    }

    bool isTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
    if(isTranspose){
      HDF5ErrMapper::ToException<DataSpaceException>(
						     "Transposed data has been deprecated");
    }

    std::array<hsize_t,Dims>  offset_local;
    std::array<hsize_t,Dims> count_local;
    std::vector<size_t> count_total(Dims);
    std::copy(space_dim.begin(),space_dim.end(),count_total.begin());
    //    count_total.fill(0);


    const DataSpace& space = static_cast<const Derivate*>(this)->getSpace();

    //    DataSpace space = static_cast<const Derivate *>(this)->getSpace().clone();
    H5Sselect_none(space.getId());
    for(size_t i=0; i<num_ranges;i++){



        std::copy(offset[i].begin(), offset[i].end(), offset_local.begin());
        std::copy(count[i].begin(), count[i].end(), count_local.begin());

	// for(size_t j=0; j<Dims;j++){
	//   count_total[j]=count_total[j]+count_local[j];
	// }


        if (H5Sselect_hyperslab(space.getId(),H5S_SELECT_OR, offset_local.data(),
                                nullptr,
                                count_local.data(), NULL) < 0) {
            HDF5ErrMapper::ToException<DataSpaceException>(
                    "Unable to select hyperslap");
        }
    }

    return Selection(DataSpace(count_total), space,
		     details::get_dataset(static_cast<const Derivate *>(this)));
  }


  template<typename Derivate>
  inline Selection
  SliceTraits<Derivate>::selectEigen(const std::vector<size_t> &offset,
				     const std::vector<size_t> &count,
                                       const std::vector<size_t> &stride) const {
        // hsize_t type convertion

        bool isTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
		if(isTranspose){
	  HDF5ErrMapper::ToException<DataSpaceException>(
							 "Transposed data has been deprecated");
	}

        std::vector<hsize_t> offset_local(offset.size());
        std::vector<hsize_t> count_local(count.size());
        std::vector<hsize_t> stride_local(stride.size());

        std::copy(offset.begin(), offset.end(), offset_local.begin());
        std::copy(count.begin(), count.end(), count_local.begin());
        std::copy(stride.begin(), stride.end(), stride_local.begin());


        DataSpace space = static_cast<const Derivate *>(this)->getSpace().clone();
        if (H5Sselect_hyperslab(space.getId(), H5S_SELECT_SET, offset_local.data(),
                                stride.empty() ? NULL : stride_local.data(),
                                count_local.data(), NULL) < 0) {
            HDF5ErrMapper::ToException<DataSpaceException>(
                    "Unable to select hyperslap");
        }

        return Selection(DataSpace(count), space,
                         details::get_dataset(static_cast<const Derivate *>(this)));
    }

#endif
    template <typename Derivate>
    inline Selection
    SliceTraits<Derivate>::select(const std::vector<size_t>& columns) const {

        const DataSpace& space = static_cast<const Derivate*>(this)->getSpace();
        const DataSet& dataset =
                details::get_dataset(static_cast<const Derivate*>(this));
        const bool isTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
		if(isTranspose){
	  HDF5ErrMapper::ToException<DataSpaceException>(
							 "Transposed data has been deprecated");
	}


        std::vector<size_t> dims = space.getDimensions();
        std::vector<hsize_t> counts(dims.size());
        std::copy(dims.begin(), dims.end(), counts.begin());
        const int woffset =  1;
        counts[dims.size() - woffset] = 1;
        std::vector<hsize_t> offsets(dims.size(), 0);

        H5Sselect_none(space.getId());
        for (std::vector<size_t>::const_iterator i = columns.begin();
             i != columns.end(); ++i) {

            offsets[offsets.size() - woffset] = *i;

            if (H5Sselect_hyperslab(space.getId(),
                                    H5S_SELECT_OR,
                                    offsets.data(),
                                    0, counts.data(), 0) < 0) {
                HDF5ErrMapper::ToException<DataSpaceException>(
                        "Unable to select hyperslap");
            }
        }

        dims[dims.size() - woffset] = columns.size();
        return Selection(DataSpace(dims), space, dataset);
    }
  template <typename Derivate>
  inline Selection
  SliceTraits<Derivate>::selectRows(const std::vector<size_t>& rows) const {

    const DataSpace& space = static_cast<const Derivate*>(this)->getSpace();
    const DataSet& dataset =
      details::get_dataset(static_cast<const Derivate*>(this));
    const bool isTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
    	if(isTranspose){
	  HDF5ErrMapper::ToException<DataSpaceException>(
							 "Transposed data has been deprecated");
	}

    std::vector<size_t> dims = space.getDimensions();
    std::vector<hsize_t> counts(dims.size());
    std::copy(dims.begin(), dims.end(), counts.begin());
    const int woffset =  dims.size();
    counts[dims.size() - woffset] = 1;
    std::vector<hsize_t> offsets(dims.size(), 0);

    H5Sselect_none(space.getId());
    for (std::vector<size_t>::const_iterator i = rows.begin();
             i != rows.end(); ++i) {

            offsets[offsets.size() - woffset] = *i;

            if (H5Sselect_hyperslab(space.getId(),
                                    H5S_SELECT_OR,
                                    offsets.data(),
                                    0, counts.data(), 0) < 0) {
                HDF5ErrMapper::ToException<DataSpaceException>(
                        "Unable to select hyperslap");
            }
        }

        dims[dims.size() - woffset] = rows.size();
        return Selection(DataSpace(dims), space, dataset);
    }

template <typename Derivate>
inline Selection
SliceTraits<Derivate>::select(const ElementSet& elements) const {
    hsize_t* data = NULL;
    const std::size_t length = elements._ids.size();
    std::vector<hsize_t> raw_elements;

    // optimised at compile time
    // switch for data conversion on 32bits platforms
    if (details::is_same<std::size_t, hsize_t>::value) {
        data = (hsize_t*)(&(elements._ids[0]));
    } else {
        raw_elements.resize(length);
        std::copy(elements._ids.begin(), elements._ids.end(),
                  raw_elements.begin());
        data = &(raw_elements[0]);
    }

    DataSpace space = static_cast<const Derivate*>(this)->getSpace().clone();
    if (H5Sselect_elements(space.getId(), H5S_SELECT_SET, length, data) < 0) {
        HDF5ErrMapper::ToException<DataSpaceException>(
            "Unable to select elements");
    }

    return Selection(DataSpace(length), space,
                     details::get_dataset(static_cast<const Derivate*>(this)));
}


template <typename Derivate>
inline std::vector<size_t> SliceTraits<Derivate>::getDataDimensions() const {
  const bool doTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
  // if(doTranspose){
  //   HDF5ErrMapper::ToException<DataSpaceException>(
  // 						     "Transposed data has been deprecated");
  // }
  std::vector<size_t> dims =static_cast<const Derivate*>(this)->getMemSpace().getDimensions();
  if (doTranspose) {
    std::reverse(dims.begin(), dims.end());
  }
  return (dims);
}


// template <typename Derivate>
// template <SEXPTYPE RTYPE,typename T>
// inline void SliceTraits<Derivate>::readR(Rcpp::Vector<RTYPE> dat) {

  // const size_t dim_array = details::array_dims<T>::value;
  // DataSpace space = static_cast<const Derivate*>(this)->getSpace();
  // DataSpace mem_space = static_cast<const Derivate*>(this)->getMemSpace();
  
  // if (!details::checkDimensions(mem_space, dim_array)) {
  //   std::ostringstream ss;
  //   ss << "Impossible to read DataSet of dimensions "
  //      << mem_space.getNumberDimensions() << " into arrays of dimensions "
  //      << dim_array;
  //   throw DataSpaceException(ss.str());
  // }
  
  // Apply pre read convertions (these will depend on the dataset, unfortunately)
//   const size_t dat_s = dat_s.size();
//   void* rdata= nullptr;
//   if constexpr(RTYPE==STRSXP){
//       std::vector<char*> wvec(dat_s,nullptr);;
//       for(int i=0; i<dat_s; i++){
// 	wvec[i]=CHAR(STRING_ELT(dat, i));
//       }
//       rdata=wvec.data();
//     }
//   if constexpr(
//   
//   // details::data_converter<T> converter(array, mem_space);
//   // Create mem datatype
//   const AtomicType<typename details::type_of_array<T>::type>
//     array_datatype;
//   
//   auto mem_datatype = array_datatype.getId();
//   
//   
//   if (H5Dread(
// 	      details::get_dataset(static_cast<const Derivate *>(this)).getId(),
// 	      mem_datatype,
// 	      details::get_memspace_id((static_cast<const Derivate *>(this))),
// 	      space.getId(), H5P_DEFAULT,
// 	      static_cast<void *>(converter.transform_read(array))) < 0) {
//     HDF5ErrMapper::ToException<DataSetException>(
// 						 "Error during HDF5 Read: ");
//   }
//   
//   // re-arrange results
//   converter.process_result(array);
// }
// 
// 
// }

  
template <typename Derivate>
template <typename T>
inline void SliceTraits<Derivate>::read(T &array) {
    // typedef typename details::remove_const<T>::type type_no_const;

    // type_no_const& nocv_array = const_cast<type_no_const&>(array);

    bool isTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
    if(isTranspose){
      HDF5ErrMapper::ToException<DataSpaceException>(
						     "Transposed data has been deprecated");
    }
    const size_t dim_array = details::array_dims<T>::value;
    DataSpace space = static_cast<const Derivate*>(this)->getSpace();
    DataSpace mem_space = static_cast<const Derivate*>(this)->getMemSpace();

    if (!details::checkDimensions(mem_space, dim_array)) {
        std::ostringstream ss;
        ss << "Impossible to read DataSet of dimensions "
           << mem_space.getNumberDimensions() << " into arrays of dimensions "
           << dim_array;
        throw DataSpaceException(ss.str());
    }

    // Apply pre read convertions (these will depend on the dataset, unfortunately)


    details::data_converter<T> converter(array, mem_space);
    // Create mem datatype
    const AtomicType<typename details::type_of_array<T>::type>
        array_datatype;


    auto mem_datatype = array_datatype.getId();


    if (H5Dread(
            details::get_dataset(static_cast<const Derivate *>(this)).getId(),
            mem_datatype,
            details::get_memspace_id((static_cast<const Derivate *>(this))),
            space.getId(), H5P_DEFAULT,
            static_cast<void *>(converter.transform_read(array))) < 0) {
        HDF5ErrMapper::ToException<DataSetException>(
                "Error during HDF5 Read: ");
    }

    // re-arrange results
    converter.process_result(array);
}

template <typename Derivate>
template <typename T>
inline void SliceTraits<Derivate>::read(T *array) {

    DataSpace space = static_cast<const Derivate*>(this)->getSpace();
    DataSpace mem_space = static_cast<const Derivate*>(this)->getMemSpace();

    // Create mem datatype
    const AtomicType<typename details::type_of_array<T>::type> array_datatype;

    if (H5Dread(
            details::get_dataset(static_cast<const Derivate*>(this)).getId(),
            array_datatype.getId(),
            details::get_memspace_id((static_cast<const Derivate*>(this))),
            space.getId(), H5P_DEFAULT,
            static_cast<void*>(array)) < 0) {
        HDF5ErrMapper::ToException<DataSetException>(
            "Error during HDF5 Read: ");
    }
}

template <typename Derivate>
template <typename T>
inline void SliceTraits<Derivate>::write(const T& buffer) {
    typedef typename details::remove_const<T>::type type_no_const;

    type_no_const& nocv_buffer = const_cast<type_no_const&>(buffer);

    const size_t dim_buffer = details::array_dims<type_no_const>::value;
    DataSpace space = static_cast<const Derivate*>(this)->getSpace();
    DataSpace mem_space = static_cast<const Derivate*>(this)->getMemSpace();

    if (!details::checkDimensions(mem_space, dim_buffer)) {
        std::ostringstream ss;
        ss << "Impossible to write buffer of dimensions " << dim_buffer
           << " into dataset of dimensions " << mem_space.getNumberDimensions();
        throw DataSpaceException(ss.str());
    }

    const AtomicType<typename details::type_of_array<type_no_const>::type>
        array_datatype;

    // Apply pre write convertions
    bool isTranspose = details::get_dataset(static_cast<const Derivate *>(this)).isTransposed();
    if(isTranspose){
      HDF5ErrMapper::ToException<DataSpaceException>(
						     "Transposed data has been deprecated");
    }
    details::data_converter<type_no_const> converter(nocv_buffer, mem_space);

    if (H5Dwrite(details::get_dataset(static_cast<Derivate*>(this)).getId(),
                 array_datatype.getId(),
                 details::get_memspace_id((static_cast<Derivate*>(this))),
                 space.getId(), H5P_DEFAULT,
                 static_cast<const void*>(
                     converter.transform_write(nocv_buffer))) < 0) {
        HDF5ErrMapper::ToException<DataSetException>(
            "Error during HDF5 Write: ");
    }
}

template <typename Derivate>
template <typename T>
inline void SliceTraits<Derivate>::write(const T* buffer) {

    DataSpace space = static_cast<const Derivate*>(this)->getSpace();
    DataSpace mem_space = static_cast<const Derivate*>(this)->getMemSpace();

    const AtomicType<typename details::type_of_array<T>::type> array_datatype;

    if (H5Dwrite(details::get_dataset(static_cast<Derivate*>(this)).getId(),
                 array_datatype.getId(),
                 details::get_memspace_id((static_cast<Derivate*>(this))),
                 space.getId(), H5P_DEFAULT,
                 static_cast<const void*>(buffer)) < 0) {
        HDF5ErrMapper::ToException<DataSetException>(
            "Error during HDF5 Write: ");
    }
}
}

