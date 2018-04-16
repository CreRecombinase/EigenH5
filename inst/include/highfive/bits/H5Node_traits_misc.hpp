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
#include <experimental/filesystem>


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
#include <H5Ppublic.h>
#include <H5Tpublic.h>

namespace HighFive {

template <typename Derivate>
inline DataSet
NodeTraits<Derivate>::createDataSet(const std::string& dataset_name,
                                    const DataSpace& space,
                                    const DataType &dtype,
                                    const Filter &filter) {
  auto chunksizes = filter.get_chunksizes();
  if(chunksizes.size()>0){
    auto space_dims=space.getDimensions();
    const size_t n_dims=space_dims.size();
    if(chunksizes.size()!=n_dims){
      HDF5ErrMapper::ToException<FilterException>(
						  "Filter and DataSpace are of different ranks!");
    }
    for(int i=0;i<n_dims;i++){
      if(chunksizes[i]>space_dims[i]){
	HDF5ErrMapper::ToException<FilterException>(
						    "Filter chunksize cannot be larger than DataSpace dimension!");
      }
    }
  }
    DataSet set;
    if ((set._hid = H5Dcreate2(static_cast<Derivate*>(this)->getId(),
                               dataset_name.c_str(), dtype._hid, space._hid,
                               H5P_DEFAULT, filter.getId(), H5P_DEFAULT)) < 0) {
        HDF5ErrMapper::ToException<DataSetException>(
            std::string("Unable to create the dataset \"") + dataset_name +
            "\":");
    }
    set.setTranspose(false);
    return set;
}


    template<typename Derivate>
    inline DataSet
    NodeTraits<Derivate>::createDataSet(const std::string &dataset_name,
                                        const DataSpace &space,
                                        const DataType &dtype) {

      return createDataSet(dataset_name, space, dtype, Filter());
    }




template <typename Derivate>
template <typename Type>
inline DataSet
NodeTraits<Derivate>::createDataSet(const std::string& dataset_name,
                                    const DataSpace& space) {
    return createDataSet(dataset_name, space, AtomicType<Type>());
}

template <typename Derivate>
inline DataSet
NodeTraits<Derivate>::getDataSet(const std::string& dataset_name) const {
    DataSet set;
    if ((set._hid = H5Dopen2(static_cast<const Derivate*>(this)->getId(),
                             dataset_name.c_str(), H5P_DEFAULT)) < 0) {
        HDF5ErrMapper::ToException<DataSetException>(
            std::string("Unable to open the dataset \"") + dataset_name +
            "\":");
    }
    set.doTranspose = set.isTransposed();
    return set;
}

template <typename Derivate>
inline std::optional<DataSet>
NodeTraits<Derivate>::openDataSet(const std::string& dataset_name) const {
  DataSet set;
  SilenceHDF5 silence;
  if ((set._hid = H5Dopen2(static_cast<const Derivate*>(this)->getId(),
			   dataset_name.c_str(), H5P_DEFAULT)) < 0) {
    return(std::nullopt);
  }
  set.doTranspose = set.isTransposed();
  return set;
}


  template <typename Derivate>
  inline Group NodeTraits<Derivate>::createGroup(const std::string& group_name) {
    Group group;
    if ((group._hid = H5Gcreate2(static_cast<Derivate*>(this)->getId(),
				 group_name.c_str(), H5P_DEFAULT, H5P_DEFAULT,
				 H5P_DEFAULT)) < 0) {
      HDF5ErrMapper::ToException<GroupException>(
						 std::string("Unable to create the group \"") + group_name + "\":");
    }
    return group;
  }

  template<typename Derivate>
  inline Group NodeTraits<Derivate>::createOrGetGroup(const std::string &group_name) {
    if(group_name=="/" ){
      return(this->getGroup("/"));
    }
    if (this->exist(group_name)) {
      return (this->getGroup(group_name));
    } else {
      return (this->createGroup(group_name));
    }
  }


  template<typename Derivate>
  inline Group
  NodeTraits<Derivate>::createGroups_rec(std::experimental::filesystem::path::const_iterator g_name,const std::experimental::filesystem::path::const_iterator g_end) {
    Group group = this->createOrGetGroup(*g_name);
    g_name++;
    if(g_name==g_end){
      return(group);
    }else{
      return(group.createGroups_rec(g_name,g_end));
    }
  }


    template<typename Derivate>
    inline Group NodeTraits<Derivate>::createOrGetGroups(const std::string &group_name) {
        if(group_name=="/"){
            return(this->getGroup("/"));
        }
	namespace fs = std::experimental::filesystem;
	fs::path p = group_name;
	return(this->createGroups_rec(p.begin(),p.end()));
    }


template <typename Derivate>
inline Group
NodeTraits<Derivate>::getGroup(const std::string& group_name) const {

    Group group;
    if(group_name=="." || group_name==""){
      group._hid=static_cast<const Derivate*>(this)->getId();
    }else{
      if ((group._hid = H5Gopen2(static_cast<const Derivate*>(this)->getId(),
				 group_name.c_str(), H5P_DEFAULT)) < 0) {
        HDF5ErrMapper::ToException<GroupException>(
						   std::string("Unable to open the group \"") + group_name + "\":");
      }
    }
    return group;
}
template <typename Derivate>
inline std::optional<Group>
NodeTraits<Derivate>::openGroup(const std::string& group_name) const {
  SilenceHDF5 silence;
  Group group;
  if(group_name=="." || group_name==""){
    group._hid=static_cast<const Derivate*>(this)->getId();
    }else{
      if ((group._hid = H5Gopen2(static_cast<const Derivate*>(this)->getId(),
				 group_name.c_str(), H5P_DEFAULT)) < 0) {
	return(std::nullopt);
      }
    }
    return group;
}


template <typename Derivate>
inline size_t NodeTraits<Derivate>::getNumberObjects() const {
    hsize_t res;
    if (H5Gget_num_objs(static_cast<const Derivate*>(this)->getId(), &res) <
        0) {
        HDF5ErrMapper::ToException<GroupException>(
            std::string("Unable to count objects in existing group or file"));
    }
    return res;
}



  template<typename Derivate>
  inline std::variant<DataSet,Group> NodeTraits<Derivate>::getObject(const std::string & object_name) const{
    namespace fs = std::experimental::filesystem;
    fs::path p = object_name;
    Group tg = this->getGroup(p.parent_path());
    if(p.filename()=="."){
      return(tg);
    }else{
      H5O_info_t tid;
      if(H5Oget_info_by_name(tg.getId(),p.filename().c_str(),&tid,H5P_DEFAULT)<0){
	HDF5ErrMapper::ToException<DataSetException>(
						     std::string("Unable to open the object \"") + object_name +
						     "\":");
      }
      if(tid.type==H5O_TYPE_GROUP){
	return(tg.getGroup(p.filename()));
      }else{
	if(tid.type==H5O_TYPE_DATASET){
	  return(tg.getDataSet(p.filename()));
	}else{
	  HDF5ErrMapper::ToException<DataSetException>(
						       std::string("Unable to open object of type\"") + std::to_string(tid.type) +
						       "\":");
	}
      }
    }
  }


  template<typename Derivate>
  inline std::optional<std::variant<DataSet,Group> > NodeTraits<Derivate>::openObject(const std::string & object_name) const{
    namespace fs = std::experimental::filesystem;
      SilenceHDF5 silence;
    fs::path p = object_name;
    Group tg = this->getGroup(p.parent_path());
    if(p.filename()=="."){
      return(tg);
    }else{
      H5O_info_t tid;
      if(H5Oget_info_by_name(tg.getId(),p.filename().c_str(),&tid,H5P_DEFAULT)<0){
	return(std::nullopt);
      }
      if(tid.type==H5O_TYPE_GROUP){
	return(tg.getGroup(p.filename()));
      }else{
	if(tid.type==H5O_TYPE_DATASET){
	  return(tg.getDataSet(p.filename()));
	}else{
	  return(std::nullopt);
	}
      }
    }
  }
  template <typename Derivate>
  inline std::vector<std::variant<DataSet,Group> > NodeTraits<Derivate>::getObjects() const {
    std::vector<std::string> names = this->listObjectNames();
    const int num_objs=names.size();
    std::vector<std::variant<DataSet,Group> > retvec(num_objs);
    for(int i=0;i<num_objs;i++){
      retvec[i]=this->getObject(names[i]);
    }
    return(retvec);
  }

template <typename Derivate>
inline std::string NodeTraits<Derivate>::getObjectName(size_t index) const {
    const ssize_t maxLength = 1023;
    char buffer[maxLength + 1];
    ssize_t length =
        H5Lget_name_by_idx(static_cast<const Derivate*>(this)->getId(), ".",
                           H5_INDEX_NAME, H5_ITER_INC, index,
                           buffer, maxLength, H5P_DEFAULT);
    if (length < 0)
        HDF5ErrMapper::ToException<GroupException>(
            "Error accessing object name");
    if (length <= maxLength)
        return std::string(buffer, length);
    std::vector<char> bigBuffer(length + 1, 0);
    H5Lget_name_by_idx(static_cast<const Derivate*>(this)->getId(), ".",
                       H5_INDEX_NAME, H5_ITER_INC, index,
                       bigBuffer.data(), length, H5P_DEFAULT);
    return std::string(bigBuffer.data(), length);
}

template <typename Derivate>
inline std::vector<std::string> NodeTraits<Derivate>::listObjectNames() const {

    std::vector<std::string> names;
    details::HighFiveIterateData iterateData(names);

    size_t num_objs = getNumberObjects();
    names.reserve(num_objs);

    if (H5Literate(static_cast<const Derivate*>(this)->getId(), H5_INDEX_NAME,
                   H5_ITER_INC, NULL,
                   &details::internal_high_five_iterate<H5L_info_t>,
                   static_cast<void*>(&iterateData)) < 0) {
        HDF5ErrMapper::ToException<GroupException>(
            std::string("Unable to list objects in group"));
    }

    return names;
}

template <typename Derivate>
inline bool NodeTraits<Derivate>::exist(const std::string& node_name) const {
  namespace fs = std::experimental::filesystem;
  //    fs::path p = node_name;
    htri_t val = H5Lexists(static_cast<const Derivate*>(this)->getId(),
                     node_name.c_str(), H5P_DEFAULT);
    if( val < 0){
        HDF5ErrMapper::ToException<GroupException>(
            std::string("Invalid link for exist() "));
    }

    return (val > 0 );
}

}

