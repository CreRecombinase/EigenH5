cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
project(EigenH5)
include(ExternalProject)

option(FIND_HDF5
  "Search for a local installation of HDF5" ON)
option(FIND_BLOSC
  "Search for a local installation of blosc" ON)
option(FIND_ZSTD
  "Search for a local installation of zstd" ON)

message(STATUS "module path is : ${CMAKE_MODULE_PATH}")
add_compile_options(-Wno-ignored-attributes  -Wno-deprecated-declarations)
include(SelectLibraryConfigurations)
include(FindPackageHandleStandardArgs)
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/inst/cmake/Modules/" ${CMAKE_MODULE_PATH})


# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)


# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
ENDIF("${isSystemDir}" STREQUAL "-1")


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
target_link_libraries(eigenh5 stdc++fs)




if(FIND_HDF5)
#  set(HDF5_USE_STATIC_LIBRARIES ON)
  find_package(HDF5 COMPONENTS C HL REQUIRED)
  if(HDF5_FOUND)
    message (STATUS  "Linking HDF5 lib: ${HDF5_LIBRARIES}")
    if(NOT HDF5_C_HL_LIBRARIES)
      message (STATUS  "HDF5 HL LIBRARIES NOT FOUND: ${HDF5_C_HL_LIBRARIES}")
      set(HDF5_FOUND 0)
    else()
      message (STATUS  "Linking HDF5_HL lib: ${HDF5_C_HL_LIBRARIES}")
    endif()

    message (STATUS "Linking HDF5_C lib: ${HDF5_C_LIBRARIES}")
    target_link_libraries(eigenh5 ${HDF5_LIBRARIES} ${HDF5_C_HL_LIBRARIES})
    message (STATUS  "Including HDF5 directorie(s): ${HDF5_INCLUDE_DIRS}")
    include_directories(${HDF5_INCLUDE_DIRS})
  endif()
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






if(FIND_BLOSC)
#  set(CBLOSC_ROOT ${CMAKE_SOURCE_DIR}/blosc/)
#  set(CBLOSC_ROOT "/home/nwknoblauch/Downloads/c-blosc/build")
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
    message(STATUS "blosc header files are in: ${CBLOSC_INCLUDE_DIR}")
    include_directories(${CBLOSC_INCLUDE_DIR})
  else()
  endif()


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
    message (STATUS  "blosc shared library: ${CBLOSC_LIBRARY}")
    target_link_libraries(eigenh5 ${CBLOSC_LIBRARY})
  else()
    if(CBLOSC_STATIC_LIBRARY)
      message (STATUS  "blosc static library: ${CBLOSC_STATIC_LIBRARY}")
      target_link_libraries(eigenh5 ${CBLOSC_STATIC_LIBRARY})
    endif()
  endif()
endif()

if(NOT CBLOSC_LIBRARY AND NOT CBLOSC_STATIC_LIBRARY)
  set(BLOSC_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/blosc")
  set(BLOSC_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/blosc")
  set(BLOSC_CMAKE_ARGS
    -DCMAKE_INSTALL_PREFIX=${BLOSC_INSTALL_DIR}
    -DBUILD_SHARED=OFF
    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
    -DDEACTIVATE_SNAPPY=ON
    -DBUILD_BENCHMARKS=OFF
    -DBUILD_TESTS=OFF)
  ExternalProject_Add(project_blosc
    BUILD_COMMAND ${CMAKE_MAKE_PROGRAM} -j13 VERBOSE=1
    PREFIX ${BLOSC_PREFIX}
    GIT_REPOSITORY https://github.com/Blosc/c-blosc.git
    INSTALL_DIR ${BLOSC_INSTALL_DIR}
    CMAKE_ARGS ${BLOSC_CMAKE_ARGS}
    )
  add_library(blosc_static STATIC IMPORTED)
  set_property(TARGET blosc_static PROPERTY IMPORTED_LOCATION ${BLOSC_INSTALL_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}blosc${CMAKE_STATIC_LIBRARY_SUFFIX})
  set_property(TARGET blosc_static PROPERTY POSITION_INDEPENDENT_CODE True)
  message(STATUS "will create blosc static library:${BLOSC_INSTALL_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}blosc${CMAKE_STATIC_LIBRARY_SUFFIX}")
  message(STATUS  "will create blosc headers directory: ${BLOSC_INSTALL_DIR}/include")
  add_dependencies(blosc_static project_blosc)
  include_directories(${BLOSC_INSTALL_DIR}/include)

  add_dependencies(eigenh5 blosc_static)
  target_link_libraries(eigenh5 blosc_static)
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
    BUILD_COMMAND ${CMAKE_MAKE_PROGRAM} -j13
    PREFIX ${ZSTD_PREFIX}
    GIT_REPOSITORY https://github.com/facebook/zstd.git
    SOURCE_SUBDIR build/cmake/
    INSTALL_DIR ${ZSTD_INSTALL_DIR}
    CMAKE_ARGS ${ZSTD_CMAKE_ARGS})

  add_library(zstd_shared shared IMPORTED)
  set_property(TARGET zstd_shared PROPERTY IMPORTED_LOCATION ${ZSTD_INSTALL_DIR}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}zstd${CMAKE_SHARED_LIBRARY_SUFFIX})
  add_dependencies(zstd_shared zstd_ext)
  include_directories(${ZSTD_INSTALL_DIR}/include)
  add_dependencies(eigenh5 zstd_shared)
endif()


if(NOT HDF5_FOUND)
  add_dependencies(eigenh5 hdf5_local)
endif()





set_property(TARGET eigenh5 PROPERTY CXX_STANDARD 17)

include_directories(${RCPP_INCLUDE_DIRS})
#include_directories(${PROGRESS_INCLUDE_DIRS})
#include_directories(${PARALLEL_INCLUDE_DIRS})
include_directories(${R_INCLUDEDIR})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories("${CMAKE_SOURCE_DIR}/inst/include")
target_link_libraries(eigenh5 ${R_SHAREDLIBDIR})


install(TARGETS eigenh5 DESTINATION lib)
