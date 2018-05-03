/*
 *  Copyright (c), 2017, Adrien Devresse <adrien.devresse@epfl.ch>
 *
 *  Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 *
 */
#pragma once

#include "../H5Exception.hpp"
#include "../H5File.hpp"

#include <H5Fpublic.h>

namespace HighFive {

namespace {

// libhdf5 uses a preprocessor trick on their oflags
// we can not declare them constant without a mapper
inline int convert_open_flag(int openFlags) {
    int res_open = 0;
    if (openFlags & File::ReadOnly)
        res_open |= H5F_ACC_RDONLY;
    if (openFlags & File::ReadWrite)
        res_open |= H5F_ACC_RDWR;
    if (openFlags & File::Create)
        res_open |= H5F_ACC_CREAT;
    if (openFlags & File::Truncate)
        res_open |= H5F_ACC_TRUNC;
    if (openFlags & File::Excl)
        res_open |= H5F_ACC_EXCL;
    return res_open;
}
}
    // inline bool file_exists(const std::string& name) {
    //     struct stat buffer;
    //     return (stat (name.c_str(), &buffer) == 0);
    // }

  inline bool File::start_swmr(){
    return(H5Fstart_swmr_write(_hid) > 0);
  }

  inline size_t File::getObjCount(unsigned int types) const{
    return (H5Fget_obj_count( _hid,types ));
  }




inline File::File(const std::string& filename, int openFlags,
                  const FileDriver& driver)
    : _filename(filename) {

    struct stat buffer;
    const bool file_exists = stat(filename.c_str(), &buffer) == 0 ;
    if(file_exists){
        if(!(openFlags & File::Truncate)){
            if(openFlags & File::Create) {
                openFlags = openFlags & ~File::Create;
            }
        }

    }

    openFlags = convert_open_flag(openFlags);

    //    auto fapl=    H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);


    if (openFlags & H5F_ACC_CREAT) {
        if ((_hid = H5Fcreate(_filename.c_str(), openFlags & (H5F_ACC_TRUNC),
                              H5P_DEFAULT, driver.getId())) < 0) {
            HDF5ErrMapper::ToException<FileException>(
                std::string("Unable to create file " + _filename));
        }
    } else {
        if ((_hid = H5Fopen(_filename.c_str(), openFlags, driver.getId())) <
            0) {
            HDF5ErrMapper::ToException<FileException>(
                std::string("Unable to open file " + _filename));
        }
    }
}

  inline std::optional<File> File::openFile(const std::string& filename, int openFlags,const FileDriver& driver){
    try{
      HighFive::SilenceHDF5 silence;
      File file(filename,openFlags,driver);
      return(file);
    }catch (HighFive::Exception& err) {
      // Rcpp::StringVector retvec(1);
      return(std::nullopt);
    }

  }


inline const std::string& File::getName() const {
    return _filename;
}

inline void File::flush()const  {
    if (H5Fflush(_hid, H5F_SCOPE_GLOBAL) < 0) {
        HDF5ErrMapper::ToException<FileException>(
            std::string("Unable to flush file " + _filename));
    }
}
}

