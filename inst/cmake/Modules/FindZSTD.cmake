find_path(ZSTD_INCLUDE_DIR zstd.h)

find_library(ZSTD_LIBRARY NAMES zstd HINTS "${ZSTD_ROOT}")

if (ZSTD_INCLUDE_DIR AND ZSTD_LIBRARY)
    set(ZSTD_FOUND TRUE)
    message(STATUS "Found ZSTD library: ${ZSTD_LIBRARY}")
  else()
    ExternalProject_Add(zstd_ext
      GIT_REPOSITORY "https://github.com/facebook/zstd.git"
      PREFIX ${CMAKE_CURRENT_BINARY_DIR}/libzstd
      CMAKE_ARGS -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_C_FLAGS=${ZSTD_C_FLAGS}
      -DCMAKE_AR=${CMAKE_AR}
      BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/libzstd-build
      BUILD_COMMAND $(MAKE) libzstd_static
      INSTALL_COMMAND "true")

# force zstd make to be called on each time
ExternalProject_Add_Step(zstd_ext forcebuild
  DEPENDEES configure
  DEPENDERS build
  COMMAND "true"
  ALWAYS 1)

add_library(zstd STATIC IMPORTED)
set_property(TARGET zstd PROPERTY
  IMPORTED_LOCATION "${CMAKE_CURRENT_BINARY_DIR}/libzstd-build/lib/libzstd.a")
add_dependencies(zstd zstd_ext)
set(ZSTD_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/src/zstd/lib)


endif ()
