# - Find c-blosc
# Find the native c-blosc includes and library.
#
# Pass in the following variables
#
#  CBLOSC_ROOT             - a hint where to find c-blosc (can be an ENV var)
#
# Once done this will define

#  CBLOSC_FOUND          - True if found.
#  CBLOSC_INCLUDE_DIRS   - where to find includes
#  CBLOSC_STATIC_LIBRARIES      - List of static libraries to link against
#  CBLOSC_LIBRARIES      - List of shared libraries to link against
#  CBLOSC_DLLS           - DLLs/shared libs required at runtime
#
#=============================================================================
# Copyright 2015 Spencer Stirling
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

if (NOT CBLOSC_ROOT)
  set (CBLOSC_ROOT  $ENV{CBLOSC_ROOT})
endif ()

# find include dir
find_path(CBLOSC_INCLUDE_DIR
  NAMES blosc.h
  HINTS
  "${CBLOSC_ROOT}"
  PATH_SUFFIXES include)

message (STATUS  "CBLOSC include dir: ${CBLOSC_INCLUDE_DIR}")
# find library to link against
if (WIN32)
  # FIXME
  if (ZLIB_USE_STATIC_LIBS)
    find_library(ZLIB_LIBRARY
      NAMES  zlibstatic  ${ZLIB_NAMES}
      HINTS
      "${ZLIB_ROOT}"
      "[HKEY_LOCAL_MACHINE\\SOFTWARE\\GnuWin32\\Zlib;InstallPath]"
      "$ENV{PROGRAMFILES}/zlib"
      PATH_SUFFIXES  lib)
  else ()
    find_library(ZLIB_LIBRARY
      NAMES  ${ZLIB_NAMES}
      HINTS
      "${ZLIB_ROOT}"
      "[HKEY_LOCAL_MACHINE\\SOFTWARE\\GnuWin32\\Zlib;InstallPath]"
      "$ENV{PROGRAMFILES}/zlib"
      PATH_SUFFIXES  lib)
  endif ()

else ()

  # find static lib first
  set (CMAKE_FIND_LIBRARY_SUFFIXES_KEEP  ${CMAKE_FIND_LIBRARY_SUFFIXES})
  set (CMAKE_FIND_LIBRARY_SUFFIXES ".a")
  find_library(CBLOSC_STATIC_LIBRARY
    NAMES  blosc
    HINTS
    "${CBLOSC_ROOT}"
    PATH_SUFFIXES  lib)
  set (CMAKE_FIND_LIBRARY_SUFFIXES  ${CMAKE_FIND_LIBRARY_SUFFIXES_KEEP})

  # now find shared lib
  find_library(CBLOSC_LIBRARY
    NAMES  blosc
    HINTS
    "${CBLOSC_ROOT}"
    PATH_SUFFIXES  lib)

endif ()

message (STATUS  "CBLOSC LIBRARY_DIR dir: ${CBLOSC_LIBRARY}")



mark_as_advanced(CBLOSC_STATIC_LIBRARY CBLOSC_LIBRARY CBLOSC_INCLUDE_DIR)
# handle the QUIETLY and REQUIRED arguments and set CBLOSC_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(CBLOSC REQUIRED_VARS CBLOSC_STATIC_LIBRARY CBLOSC_LIBRARY CBLOSC_INCLUDE_DIR)


if(CBLOSC_FOUND)
  set(CBLOSC_INCLUDE_DIRS ${CBLOSC_INCLUDE_DIR})
  set(CBLOSC_STATIC_LIBRARIES ${CBLOSC_STATIC_LIBRARY})
  set(CBLOSC_LIBRARIES ${CBLOSC_LIBRARY})
  set (CBLOSC_DLLS  ${CBLOSC_LIBRARIES})
else()
  message (STATUS  "INSTALLING c-blosc from external repo")


  # add blosc libraries
  add_library(blosc_shared STATIC IMPORTED)
  set_property(TARGET blosc_shared PROPERTY IMPORTED_LOCATION ${BLOSC_INSTALL_DIR}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}blosc${CMAKE_SHARED_LIBRARY_SUFFIX})
  add_dependencies(blosc_shared project_blosc)
    find_library(CBLOSC_STATIC_LIBRARY
    NAMES  blosc
    HINTS
    "${CBLOSC_ROOT}"
    PATH_SUFFIXES  lib)
  set (CMAKE_FIND_LIBRARY_SUFFIXES  ${CMAKE_FIND_LIBRARY_SUFFIXES_KEEP})

  # now find shared lib
  find_library(CBLOSC_LIBRARY
    NAMES  blosc
    HINTS
    "${BLOSC_INSTALL_DIR}"
    PATH_SUFFIXES  lib)

  include_directories(${BLOSC_INSTALL_DIR}/include)
  set(CBLOSC_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/src/libblosc/lib)
  set(CBLOSC_STATIC_LIBRARIES "${CMAKE_CURRENT_BINARY_DIR}/libblosc/lib/libblosc.a")
  set(CBLOSC_LIBRARIES ${CBLOSC_LIBRARY})
  set(CBLOSC_FOUND 1)
endif()

