/*
 *  Copyright (c), 2017, Adrien Devresse <adrien.devresse@epfl.ch>
 *
 *  Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 *
 */
#pragma once

#include <exception>
#include <string>
#include <vector>

#include "../H5Exception.hpp"

#include <H5Ipublic.h>
#include <H5Opublic.h>
#include <H5Ppublic.h>

#include <path/path.hpp>

namespace HighFive {

inline herr_t convert_to_string(void *elem, hid_t type_id, unsigned ndim,
                                const hsize_t *point, void *operator_data) {

  //  (void)id;
  (void)operator_data;

  std::vector<std::string> *data =
      static_cast<std::vector<std::string> *>(operator_data);
  const char *data_elem = static_cast<const char *>(elem);
  (*data)[point[0]] = data_elem;
  return (0);
}

namespace details {

// iterator for H5 iterate

struct HighFiveIterateData {
    inline HighFiveIterateData(std::vector<PathNode>& my_names)
        : names(my_names), err(NULL) {}

    std::vector<PathNode>& names;
    std::exception* err;

    inline void throwIfError() {
        if (err) {
            throw * err;
        }
    }
};

template <typename InfoType>
inline herr_t internal_high_five_iterate(hid_t id, const char* name,
                                         const InfoType* info, void* op_data) {
    (void)id;
    (void)info;

    HighFiveIterateData* data = static_cast<HighFiveIterateData*>(op_data);
    try {
      H5O_info_t tid;
#if defined(H5_USE_110_API)
      auto ret = H5Oget_info_by_name2(id,name,&tid,H5O_INFO_ALL,H5P_DEFAULT);
      #else
      auto ret = H5Oget_info_by_name(static_cast<const Derivate *>(this)->getId(), object_name.c_str(), &tid,H5P_DEFAULT);
#endif
      data->names.push_back(PathNode(std::string(name),tid.type == H5O_TYPE_GROUP));
        return 0;
    } catch (...) {
      data->err =
          new ObjectException("Exception during H5Iterate, abort listing");
    }
    return -1;
}

} // end details
}

