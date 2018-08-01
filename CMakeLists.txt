cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
project(EigenH5)
include(ExternalProject)




add_compile_options(-Wno-ignored-attributes  -Wno-deprecated-declarations)
set(CMAKE_MODULE_PATH  "${CMAKE_SOURCE_DIR}/inst/cmake/Modules/"  ${CMAKE_MODULE_PATH})
set(HDF5_USE_STATIC_LIBRARIES ON)
option(FIND_HDF5
  "Search for a local installation of HDF5" ON)
option(FIND_BLOSC
  "Search for a local installation of blosc" ON)
option(FIND_ZSTD
  "Search for a local installation of zstd" OFF)
if(FIND_HDF5)
  find_package(HDF5 COMPONENTS C REQUIRED)
endif()
if(NOT HDF5_FOUND)
  set(ExternalProjectCMakeArgs
    -DHDF5_BUILD_CPP_LIB=OFF
    )
  file(MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/hdf5_local/src)
  ExternalProject_Add(hdf5_local
    GIT_REPOSITORY "https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git"
    GIT_TAG "hdf5-1_10_2"
    CMAKE_ARGS ${ExternalProjectCMakeArgs}
    SOURCE_DIR ${PROJECT_BINARY_DIR}/hdf5_local/src/hdf5-${_hdf5_version}
    BINARY_DIR ${PROJECT_BINARY_DIR}/hdf5_local/build
    STAMP_DIR  ${PROJECT_BINARY_DIR}/hdf5_local/stamp
    TMP_DIR    ${PROJECT_BINARY_DIR}/hdf5_local/tmp
    INSTALL_COMMAND ""
    )
  include_directories(
    ${PROJECT_BINARY_DIR}/hdf5_local/src/hdf5-${_hdf5_version}/src
    ${PROJECT_BINARY_DIR}/hdf5_local/build
    )
  set(_hdf5_libs
    ${PROJECT_BINARY_DIR}/hdf5_local/build/bin/libhdf5.a
    -ldl
    )
endif()

find_package(R)


set(package_list Rcpp RcppEigen RcppProgress RcppParallel BH testthat)
foreach(pack ${package_list})
  EXECUTE_PROCESS(
    COMMAND ${R_EXECUTABLE} "--slave" "--no-save" "-e" "cat(system.file(\"include\",package=\"${pack}\"))"
    OUTPUT_VARIABLE tv)
  message (STATUS  "${pack} directory: ${tv}")
  include_directories(${tv})
endforeach()



add_library(eigenh5 SHARED
  src/blosc_filter.c
  src/h5io.cpp
  src/lzf_d.cpp
  src/lzf_filter.cpp
  src/lzf_z.cpp
  src/RcppExports.cpp
  src/sexp_io.cpp
  src/sexp_utils.cpp
  src/singleton.cpp
  src/split_ld_region.cpp
  src/test-dataqueue.cpp
  src/test-dataset.cpp
  src/test-dimrange.cpp
  src/test-eigen.cpp
  src/test-groups.cpp
  src/test-runner.cpp
  src/utils.cpp
  src/zstd_h5plugin.c)




if(FIND_BLOSC)

  if (NOT CBLOSC_ROOT)
    set (CBLOSC_ROOT  $ENV{CBLOSC_ROOT})
  endif ()

  # find include dir
  find_path(CBLOSC_INCLUDE_DIR
    NAMES blosc.h
    HINTS
    "${CBLOSC_ROOT}"
    PATH_SUFFIXES include)
  if(CBLOSC_INCLUDE_DIR)
    include_directories(${CBLOSC_INCLUDE_DIR})
  endif()

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
    if(CBLOSC_LIBRARY)
      target_link_libraries(eigenh5 ${CBLOSC_LIBRARY})
    endif()
  endif()

  message (STATUS  "CBLOSC LIBRARY_DIR dir: ${CBLOSC_LIBRARY}")
endif()

if(NOT CBLOSC_LIBRARY)
  set(BLOSC_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/blosc")
  set(BLOSC_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/blosc")
  set(BLOSC_CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${BLOSC_INSTALL_DIR})
  ExternalProject_Add(project_blosc
    PREFIX ${BLOSC_PREFIX}
    GIT_REPOSITORY https://github.com/Blosc/c-blosc.git
    INSTALL_DIR ${BLOSC_INSTALL_DIR}
    CMAKE_ARGS ${BLOSC_CMAKE_ARGS}
    )
  add_library(blosc_shared STATIC IMPORTED)
  set_property(TARGET blosc_shared PROPERTY IMPORTED_LOCATION ${BLOSC_INSTALL_DIR}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}blosc${CMAKE_SHARED_LIBRARY_SUFFIX})
  add_dependencies(blosc_shared project_blosc)
  include_directories(${BLOSC_INSTALL_DIR}/include)
  add_dependencies(eigenh5 blosc_shared)
endif()



add_compile_options(-Wno-ignored-attributes  -Wno-deprecated-declarations)

if(FIND_ZSTD)
  find_path(ZSTD_INCLUDE_DIR zstd.h)
  find_library(ZSTD_LIBRARY NAMES zstd HINTS "${ZSTD_ROOT}")
endif()
if(ZSTD_INCLUDE_DIR AND ZSTD_LIBRARY)
  include_directories(${ZSTD_INCLUDE_DIR})
  target_link_libraries(eigenh5 ${ZSTD_LIBRARY})
  set(ZSTD_FOUND TRUE)
  message(STATUS "Found ZSTD library: ${ZSTD_LIBRARY}")
else()

  set (ZSTD_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/zstd")
  set(ZSTD_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/zstd")
  set(ZSTD_CMAKE_ARGS  -DCMAKE_INSTALL_PREFIX=${ZSTD_INSTALL_DIR})
  ExternalProject_Add(zstd_ext
    PREFIX ${ZSTD_PREFIX}
    GIT_REPOSITORY https://github.com/facebook/zstd.git
    SOURCE_SUBDIR build/cmake/
    INSTALL_DIR ${ZSTD_INSTALL_DIR}
    CMAKE_ARGS ${ZSTD_CMAKE_ARGS})

  add_library(zstd_shared STATIC IMPORTED)
  set_property(TARGET zstd_shared PROPERTY IMPORTED_LOCATION ${ZSTD_INSTALL_DIR}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}zstd${CMAKE_SHARED_LIBRARY_SUFFIX})
  add_dependencies(zstd_shared zstd_ext)
  include_directories(${ZSTD_INSTALL_DIR}/include)
  add_dependencies(eigenh5 zstd_shared)
endif()
  # force zstd make to be called on each time
  # ExternalProject_Add_Step(zstd_ext forcebuild
  #   DEPENDEES configure
  #   DEPENDERS build
  #   COMMAND "true"
  #   ALWAYS 1)

  # add_library(zstd STATIC IMPORTED)
  # set_property(TARGET zstd PROPERTY
  #   IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/libzstd-build/lib/libzstd.a")
  # add_dependencies(zstd zstd_ext)


#endif ()

if(NOT HDF5_FOUND)
  add_dependencies(eigenh5 hdf5_local)
endif()





set_property(TARGET eigenh5 PROPERTY CXX_STANDARD 17)
target_link_libraries(eigenh5 ${HDF5_LIBRARIES} ${HDF5_HL_LIBRARIES})
include_directories(${HDF5_INCLUDE_DIRS})
include_directories(${RCPP_INCLUDE_DIRS})
#include_directories(${PROGRESS_INCLUDE_DIRS})
#include_directories(${PARALLEL_INCLUDE_DIRS})
include_directories(${R_INCLUDEDIR})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories("${CMAKE_SOURCE_DIR}/inst/include")
target_link_libraries(eigenh5 ${R_SHAREDLIBDIR})








install(TARGETS eigenh5 DESTINATION lib)
