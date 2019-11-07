/*
 *  Copyright (c), 2017, Adrien Devresse <adrien.devresse@epfl.ch>
 *
 *  Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 *
 */
#pragma once

#include "H5Iterables_misc.hpp"
#include "H5Node_traits.hpp"
#include "../H5PropertyList.hpp"
#include <string>
#include <vector>
#include <boost/algorithm/string.hpp>

#include "../H5Attribute.hpp"
#include "../H5Filter.hpp"
#include "../H5DataSet.hpp"
#include "../H5DataSpace.hpp"
#include "../H5DataType.hpp"
#include "../H5Exception.hpp"
#include "../H5Utility.hpp"
#include "../H5Group.hpp"

#include <H5Apublic.h>
#include <H5Dpublic.h>
#include <H5Fpublic.h>
#include <H5Gpublic.h>
#include <H5Opublic.h>
#include <H5Ppublic.h>
#include <H5Tpublic.h>

namespace HighFive {

  template <typename Derivate>
  inline DataSet NodeTraits<Derivate>::createDataSet(const Path& dataset_name,
						     const DataSpace& space,
						     const DataType &dtype,
						     const Filter &filter) {
    auto chunksizes = filter.get_chunksizes();
    if(chunksizes.size()>0){
      auto space_dims=space.getDimensions();
      auto space_max_dims=space.getMaxDimensions();
      const size_t n_dims=space_dims.size();
      if (chunksizes.size() != n_dims) {
	HDF5ErrMapper::ToException<FilterException>(
						    "Filter and DataSpace are of different ranks!");
      }
      for(int i=0;i<n_dims;i++){
	if (chunksizes[i] > space_max_dims[i]) {
	  HDF5ErrMapper::ToException<FilterException>(
						      "Filter chunksize cannot be larger than DataSpace (max) "
						      "dimension!");
	}
      }
    }
    DataSet set;
    if ((set._hid = H5Dcreate2(static_cast<Derivate *>(this)->getId(),
			       dataset_name.c_str(), dtype._hid, space._hid,
			       H5P_DEFAULT, filter.getId(), H5P_DEFAULT)) < 0) {
      HDF5ErrMapper::ToException<DataSetException>(
						   std::string("Unable to create the dataset \"") + dataset_name.string() +
						   "\":");
    }
    set.setTranspose(false);
    return set;
  }

  template <typename Derivate>
  inline DataSet
  NodeTraits<Derivate>::getDataSet(const Path &dataset_name) const {


    DataSet set;
    if ((set._hid = H5Dopen2(static_cast<const Derivate *>(this)->getId(),
                             dataset_name.c_str(), H5P_DEFAULT)) < 0) {
      HDF5ErrMapper::ToException<DataSetException>(
                                                   std::string("Unable to open the dataset \"") + dataset_name.string() +
          "\":");
    }
    set.doTranspose = set.isTransposed();
    return set;
  }

  template <typename Derivate>
  inline std::optional<DataSet>
  NodeTraits<Derivate>::openDataSet(const Path & dataset_name) const {

    if(!this->exist(dataset_name)){
      return(std::nullopt);
    } else {
      return (this->getDataSet(dataset_name));
    }
  }

  template <typename Derivate>
  inline Group NodeTraits<Derivate>::createGroup(const Path & group_name) {


    auto gcpl = H5Pcreate (H5P_LINK_CREATE);
    H5Pset_create_intermediate_group (gcpl, 1);
    Group group;
    if ((group._hid = H5Gcreate2(static_cast<const Derivate *>(this)->getId(),
				 group_name.c_str(), gcpl, H5P_DEFAULT,
				 H5P_DEFAULT)) < 0) {
      HDF5ErrMapper::ToException<GroupException>(
						 std::string("Unable to create the group ") + group_name.string() + ":");
    }
    H5Pclose(gcpl);
    return (group);
  }

  template <typename Derivate>
  inline Group
  NodeTraits<Derivate>::getGroup(const Path  &group_name) const {

    Group group;
    if ((group._hid = H5Gopen2(static_cast<const Derivate *>(this)->getId(),
                               group_name.c_str(), H5P_DEFAULT)) < 0) {
      HDF5ErrMapper::ToException<GroupException>(
						 std::string("Unable to open the group \"") + group_name.string() +
						 "\":");
    }
    return group;
  }

  template <typename Derivate>
  inline std::optional<Group>
  NodeTraits<Derivate>::openGroup(const Path  &group_name) const {
    if(!this->exist(group_name)){
      return(std::nullopt);
    } else {
      return (this->getGroup(group_name));
    }
  }



  template<typename Derivate>
  inline bool NodeTraits<Derivate>::isGroup(const Path  & object_name) const{

    H5O_info_t tid;
#if defined(H5_USE_110_API) || defined(H5_USE_112_API_DEFAULT)
    auto ret = H5Oget_info_by_name2(static_cast<const Derivate *>(this)->getId(),object_name.c_str(),&tid,H5O_INFO_ALL,H5P_DEFAULT)<0;
#else
  
    auto ret = H5Oget_info_by_name(static_cast<const Derivate *>(this)->getId(), object_name.c_str(), &tid,H5P_DEFAULT) < 0;
#endif
    if (ret) {
      HDF5ErrMapper::ToException<DataSetException>(
						   std::string("Unable to open the object \"") + object_name.string() +
						   "\":");
    }
    // if ((tid.type == H5O_TYPE_GROUP) != (object_name.is_directory())) {
    //   Rcpp::Rcerr << "path indicates group, but is not stored as a group"
    //               << std::endl;
    // }

    return (tid.type == H5O_TYPE_GROUP);
    // namespace HighFive
    // HDF5ErrMapper::ToException<DataSetException>(
    //     std::string("Object does not exist \"") + object_name + "\":");
    // return (false);
  }

  template<typename Derivate>
  inline bool NodeTraits<Derivate>::isDataSet(const Path  & object_name) const{

    H5O_info_t tid;
    #if defined(H5_USE_110_API) || defined(H5_USE_112_API_DEFAULT)

    auto ret = H5Oget_info_by_name2(static_cast<const Derivate *>(this)->getId(),object_name.c_str(),&tid,H5O_INFO_ALL,H5P_DEFAULT)<0;
#else
    auto ret = H5Oget_info_by_name(static_cast<const Derivate *>(this)->getId(), object_name.c_str(), &tid,H5P_DEFAULT) < 0;
#endif
    if (ret) {
      HDF5ErrMapper::ToException<DataSetException>(
						   std::string("Unable to open the object \"") + object_name.string() +
						   "\":");
    }
    // if ((tid.type == H5O_TYPE_GROUP) != (object_name.is_directory())) {
    //   Rcpp::Rcerr << "path indicates group, but is not stored as a group"
    //               << std::endl;
    // }

    return (tid.type == H5O_TYPE_DATASET);
    // namespace HighFive
    // HDF5ErrMapper::ToException<DataSetException>(
    //     std::string("Object does not exist \"") + object_name + "\":");
    // return (false);
  }







  template <typename Derivate>
  inline size_t NodeTraits<Derivate>::getNumberObjects() const{
    hsize_t res;
    if (H5Gget_num_objs(static_cast<const Derivate*>(this)->getId(), &res) <
        0) {
      HDF5ErrMapper::ToException<GroupException>(
						 std::string("Unable to count objects in existing group or file"));
    }
    return res;
  }


  template<typename Derivate>
  inline std::variant<DataSet,Group> NodeTraits<Derivate>::getObject(const Path  & object_name) const{

    if(isGroup(object_name)){
      return (getGroup(object_name));
    }
    if (!isDataSet(object_name)) {
      HDF5ErrMapper::ToException<ObjectException>(std::string("Object is not a Group or DataSet: ") + object_name.string());
    }
    return (getDataSet(object_name));
  }

  template<typename Derivate>
  inline std::optional<std::variant<DataSet,Group> > NodeTraits<Derivate>::openObject(const Path & object_name) const{
    if(this->exist(object_name)){
      return(this->getObject(object_name));
    }else{
      return(std::nullopt);
    }
  }

  template <typename Derivate>
  inline std::vector<std::variant<DataSet,Group> > NodeTraits<Derivate>::getObjects() const {
    auto names = this->listObjectNames();
    const int num_objs=names.size();
    std::vector<std::variant<DataSet,Group> > retvec(num_objs);
    for (int i = 0; i < num_objs; i++) {
      retvec[i] = this->getObject(names[i]);
    }
    return (retvec);
  }

  template <typename Derivate>
  inline Path NodeTraits<Derivate>::getObjectName(size_t index) const {
    std::string retstring;
    const ssize_t maxLength = 1023;
    char buffer[maxLength + 1];
    ssize_t length =
      H5Lget_name_by_idx(static_cast<const Derivate*>(this)->getId(), ".",
			 H5_INDEX_NAME, H5_ITER_INC, index,
			 buffer, maxLength, H5P_DEFAULT);
    if (length < 0)
      HDF5ErrMapper::ToException<GroupException>(
						 "Error accessing object name");

    if (length <= maxLength){
      retstring= std::string(buffer, length);
    } else {

      std::vector<char> bigBuffer(length + 1, 0);
      H5Lget_name_by_idx(static_cast<const Derivate *>(this)->getId(), ".",
                         H5_INDEX_NAME, H5_ITER_INC, index, bigBuffer.data(),
                         length, H5P_DEFAULT);

      retstring = std::string(bigBuffer.data(), length);
    }

    return (Path(retstring));
  }

  template <typename Derivate>
  inline std::vector<Path> NodeTraits<Derivate>::listObjectNames() const {

    
    std::vector<Path> names;
    details::HighFiveIterateData<Path> iterateData(names);

    size_t num_objs = getNumberObjects();
    names.reserve(num_objs);

    if (H5Literate(static_cast<const Derivate *>(this)->getId(), H5_INDEX_NAME,
                   H5_ITER_NATIVE, NULL,
                   &details::internal_high_five_iterate<H5L_info_t,Path>,
                   static_cast<void *>(&iterateData)) < 0) {
      HDF5ErrMapper::ToException<GroupException>(
          std::string("Unable to list objects in group"));
    }
    return names;
  }


  template <typename Derivate>
  inline bool NodeTraits<Derivate>::_exist(const std::string& node_name) const {
    htri_t val = H5Lexists(static_cast<const Derivate*>(this)->getId(),
                           node_name.c_str(), H5P_DEFAULT);
    if (val < 0) {
      HDF5ErrMapper::ToException<GroupException>(
						 std::string("Invalid link for exist() "));
    }

    return (val > 0);
  }

  template <typename Derivate>
  inline bool NodeTraits<Derivate>::exist(const Path& group_path) const {
    // When there are slashes, first check everything is fine
    // so that subsequent errors are only due to missing intermediate groups
    if (group_path.has_parent_path()) {
      _exist("/");  // Shall not throw under normal circumstances
      try {
	SilenceHDF5 silencer;
	return _exist(group_path);
      } catch (const GroupException &) {
        return false;
      }
    }
    return _exist(group_path);
  }
}

  // template <typename Derivate>
  // inline bool NodeTraits<Derivate>::exist(Path  p) const {

  //   std::string	ret;
  //   const auto ttid=static_cast<const Derivate*>(this)->getId();
  //   for(auto &tb: p){
  //     ret+=tb.to_name();
  //     auto val = H5Lexists(ttid,ret.c_str(), H5P_DEFAULT);
  //     if(val<0){
  // 	HDF5ErrMapper::ToException<GroupException>(std::string("Invalid path while checking for existence of "+p));
  //     }
  //     if (val == 0) {
  //       return (false);
  //     }
  //   }
  //   return (true);
  // }


