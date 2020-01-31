#include "eigenh5/indexers.hpp"
#include "path.hpp"
#include "sexp_io.hpp"
#include "utils.hpp"
#include <variant>

//[[Rcpp::plugins(cpp17)]]

#include <sys/types.h>
#include <aio.h>
#include <fcntl.h>
#include <errno.h>
#include <iostream>


// SEXP get_chunk_offsets(const std::string filename,const std::string datapath){

//     auto dp=root_path(datapath);

//     HighFive::File file(filename,HighFive::File::ReadOnly);

//     auto groupname = dp.parent_path();
//     auto dataname = dp.filename();
//     auto dset = file.getDataSet(dp);
//     auto dims = dset.getDataDimensions();
//     const auto csize = dp.getFilter().get_chunksizes();



// }
