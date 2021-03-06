#ifndef FILTER_BLOSC_H
#define FILTER_BLOSC_H

#ifdef __cplusplus
extern "C" {
#endif

#include "hdf5.h"
#include "blosc.h"

  /* Filter revision number, starting at 1 */
  /* #define FILTER_BLOSC_VERSION 1 */
#define FILTER_BLOSC_VERSION 2	/* multiple compressors since Blosc 1.3 */

  /* Filter ID registered with the HDF Group */
#define FILTER_BLOSC 32001

  /* Registers the filter with the HDF5 library. */
#if defined(_MSC_VER)
  __declspec(dllexport)
#endif	/* defined(_MSC_VER) */
    int register_blosc(char **version, char **date);
herr_t blosc_set_local(hid_t dcpl, hid_t type, hid_t space);


#ifdef __cplusplus
}
#endif

#endif
