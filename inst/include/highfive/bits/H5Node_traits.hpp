/*
 *  Copyright (c), 2017, Adrien Devresse <adrien.devresse@epfl.ch>
 *
 *  Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 *
 */
#pragma once

#include <string>
#include <experimental/filesystem>
#include <variant>
#include <optional>

namespace HighFive {

class Attribute;
class DataSet;
class Group;
class DataSpace;
class DataType;

    class Properties;

template <typename Derivate>
class NodeTraits {
  public:


    DataSet createDataSet(const std::string &dataset_name,
                          const DataSpace &space,
                          const DataType &dtype,
                          const Filter &filter);
    ///
    /// \brief createDataSet Create a new dataset in the current file of
    /// datatype type and of size space
    /// \param dataset_name identifier of the dataset
    /// \param space Associated DataSpace, see \ref DataSpace for more
    /// informations
    /// \param type Type of Data
    /// \return DataSet Object
    DataSet createDataSet(const std::string& dataset_name,
                          const DataSpace& space, const DataType& type);

      ///
    /// \brief createDataSet create a new (virtual) dataset in the current
    ///  file with a vector of Selections (from this file or other files)
    /// \param datasets vector of datasets
    /// \param concat margin margin to concatenate along (default is along first dimension)
    /// \return DataSet Object
    ///
    ///
    ///
    // template <typename Type>
    // DataSet createDataSet(const std::string& dataset_name,
    //                       const DataSpace& space);



    ///
    /// \brief createDataSet create a new dataset in the current file with a
    /// size specified by space
    /// \param dataset_name identifier of the dataset
    /// \param space Associated DataSpace, see \ref DataSpace for more
    /// informations
    /// \return DataSet Object
    ///
    ///
    ///
    template <typename Type>
    DataSet createDataSet(const std::string& dataset_name,
                          const DataSpace& space);

    ///
    /// \brief get an existing dataset in the current file
    /// \param dataset_name
    /// \return return the named dataset, or throw exception if not found
    ///
  DataSet getDataSet(const std::string& dataset_name) const;
  std::optional<DataSet> openDataSet(const std::string& dataset_name) const;


    ///
    /// \brief create a new group with the name group_name
    /// \param group_name
    /// \return the group object
    ///
    Group createGroup(const std::string& group_name);

    ///
    /// \brief open an existing group with the name group_name
    /// \param group_name
    /// \return the group object
    ///
  Group getGroup(const std::string& group_name) const;
  std::optional<Group> openGroup(const std::string& group_name) const;


  bool isGroup(const std::string& group_name) const;
  bool isDataSet(const std::string& group_name) const;



  /// \brief return the number of leaf objects of the node / group
  /// \return number of leaf objects
  size_t getNumberObjects() const;

  ///
  /// \brief return the name of the object with the given index
  /// \return the name of the object
  std::string getObjectName(size_t index) const;

  ///
  /// \brief list all leaf objects name of the node / group
  /// \return number of leaf objects
  std::vector<std::string> listObjectNames() const;

  ///
  /// \brief return either a group or dataset
  /// \return variant containing either a group or dataset
  std::variant<DataSet,Group> getObject(const std::string & object_name) const;
  std::optional<std::variant<DataSet,Group>> openObject(const std::string & object_name) const;

  ///
  /// \brief return all leaf objects
  /// \return zero or more objects (represented as std::variants)
  std::vector<std::variant<DataSet,Group> > getObjects() const;

  ///
  /// \brief check a dataset or group exists in the current node / group
  ///
  /// \param dataset/group name to check
  /// \return true if a dataset/group with the asssociated name exist, or
  /// false
  bool exist(const std::string& node_name) const;
private:
  static Group grpCreate(const hid_t root_id, const char* name);
  std::variant<DataSet,Group> getObj(const hid_t root_id, const char* name) const;
  typedef Derivate derivate_type;
};
}

#include "H5Node_traits_misc.hpp"

