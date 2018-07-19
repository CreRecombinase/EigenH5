#include <stdlib.h>
#include "zstd/zstd_h5plugin.h"
#include "zstd/zstd.h"

#define ZSTD_FILTER 32015
#define PUSH_ERR(func, minor, str, ...) H5Epush(H5E_DEFAULT, __FILE__, func, __LINE__, H5E_ERR_CLS, H5E_PLINE, minor, str, ##__VA_ARGS__)

size_t zstd_filter(unsigned int flags, size_t cd_nelmts,
	const unsigned int cd_values[], size_t nbytes,
		   size_t *buf_size, void **buf);


int register_zstd(void){

      int retval;

    H5Z_class_t filter_class = {
        H5Z_CLASS_T_VERS,
        (H5Z_filter_t)(ZSTD_FILTER),
        1,1,
        "zstd",
        NULL,
        NULL,
        (H5Z_func_t)(zstd_filter)
    };




    retval = H5Zregister(&filter_class);
    if(retval<0){
        PUSH_ERR("register_zstd", H5E_CANTREGISTER, "Can't register zstd filter");
    }
    return retval;
}



DLL_EXPORT size_t zstd_filter(unsigned int flags, size_t cd_nelmts,
	const unsigned int cd_values[], size_t nbytes,
	size_t *buf_size, void **buf)
{
	void *outbuf = NULL;    /* Pointer to new output buffer */
	void *inbuf = NULL;    /* Pointer to input buffer */
	inbuf = *buf;

	size_t ret_value;
	size_t origSize = nbytes;     /* Number of bytes for output (compressed) buffer */

	if (flags & H5Z_FLAG_REVERSE)
	{
		size_t decompSize = ZSTD_getDecompressedSize(*buf, origSize);
		if (NULL == (outbuf = malloc(decompSize)))
			goto error;

		decompSize = ZSTD_decompress(outbuf, decompSize, inbuf, origSize);

		free(*buf);
		*buf = outbuf;
		outbuf = NULL;
		ret_value = (size_t)decompSize;
	}
	else
	{
		int aggression = 0;
		if (cd_nelmts > 0)
			aggression = (int)cd_values[0];
		if (aggression > 22)
			aggression = 22;

		size_t compSize = ZSTD_compressBound(origSize);
		if (NULL == (outbuf = malloc(compSize)))
			goto error;

		compSize = ZSTD_compress(outbuf, compSize, inbuf, origSize, aggression);

		free(*buf);
		*buf = outbuf;
		*buf_size = compSize;
		outbuf = NULL;
		ret_value = compSize;
	}
	if (outbuf != NULL)
		free(outbuf);
	return ret_value;

error:
	return 0;
}

const H5Z_class_t zstd_H5Filter =
{
	H5Z_CLASS_T_VERS,
	(H5Z_filter_t)(ZSTD_FILTER),
	1, 1,
	"Zstandard compression: http://www.zstd.net",
	NULL, NULL,
	(H5Z_func_t)(zstd_filter)
};

DLL_EXPORT H5PL_type_t H5PLget_plugin_type(void)
{
	return H5PL_TYPE_FILTER;
}

DLL_EXPORT const void* H5PLget_plugin_info(void)
{
	return &zstd_H5Filter;
}
