/*
 *  Copyright (c), 2017, Adrien Devresse <adrien.devresse@epfl.ch>
 *
 *  Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 *
 */
#pragma once

#include "H5Object.hpp"

namespace HighFive {

struct TypeMapper;

///
/// \brief HDF5 Data Type
///
class DataType : public Object {
  public:
    DataType();

    bool operator == (const DataType& other) const;

    bool operator != (const DataType& other) const;

  bool isVariable() const;

  protected:
    friend class Attribute;
    friend class File;
    friend class DataSet;
};

///
/// \brief create an HDF5 DataType from a C++ type
///
///  Support only basic data type
///
template <typename T>
class AtomicType : public DataType {
  public:
    AtomicType();

    typedef T basic_type;
};
}

/*
template<hid_t HT,is>
struct CppType {
    typedef HT type;
}*/


#include "bits/H5DataType_misc.hpp"

