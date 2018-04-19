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
#include "../H5Object.hpp"

namespace HighFive {

inline Object::Object() : _hid(H5I_INVALID_HID) {}

inline Object::Object(const Object& other) : _hid(other._hid) {
    if (other.isValid() && H5Iinc_ref(_hid) < 0) {
        throw ObjectException("Reference counter increase failure");
    }
}

inline Object& Object::operator=(const Object& other) {
    if (this != &other) {
        if (_hid != H5I_INVALID_HID)
            H5Idec_ref(_hid);

        _hid = other._hid;
        if (other.isValid() && H5Iinc_ref(_hid) < 0) {
            throw ObjectException("Reference counter increase failure");
        }
    }
    return *this;
}




inline Object::~Object() {
    if (isValid() && H5Idec_ref(_hid) < 0) {
        std::cerr << "HighFive::~Object: reference counter decrease failure"
                  << std::endl;
    }
}

inline bool Object::isValid() const {
    return (_hid != H5I_INVALID_HID) && (H5Iis_valid(_hid) != false);
}

  inline int Object::getRefCt()const{
    return(H5Iget_ref( _hid));
  }
inline hid_t Object::getId() const { return _hid; }


inline haddr_t Object::getAddr() const{
   H5O_info_t tid;
   if(H5Oget_info(_hid,&tid)<0){
     throw ObjectException("Unable to obtain object info");
   }else{
     return(tid.addr);
   }
}
}
