// #include "EigenH5.h"
// //[[Rcpp::depends(BH)]]

// #include <boost/multi_array.hpp>

// #include <Rinternals.h>
// #include <R_ext/Rallocators.h>

// 
// typedef struct allocator_data {
//   size_t size;
// } allocator_data;
// 
// void* my_alloc(R_allocator_t *allocator, size_t size) {
//   ((allocator_data*)allocator->data)->size = size;
//   return (void*) numa_alloc_local(size);
// }
// 
// void my_free(R_allocator_t *allocator, void * addr) {
//   size_t size = ((allocator_data*)allocator->data)->size;
//   numa_free(addr, size);
// }
// 
// 
// 
// SEXP cuda_read_mat_h5(const std::string h5file, const std::string groupname, const std::string dataname){
//   
//   using namespace HighFive;
//   try {
//     // Create a new file using the default property lists.
//     File file(h5file, File::ReadOnly);
//     
//     // Define the size of our dataset: 2x6
// 
//     Group group=file.getGroup("/");
//     if(groupname!="/"){
//       group = group.getGroup(groupname);
//     }
//     
//     
//     typedef typename boost::multi_array_ref<double, 2> ArrayRef;
//     typedef typename boost::multi_array<double, 2> Array;
//     
//     ArrayRef mat_ref(data.data(),boost::extents[data.rows()][data.cols()],boost::fortran_storage_order());
//     Array write_mat(boost::extents[data.rows()][data.cols()]);
//     std::copy(mat_ref.begin(),mat_ref.end(),write_mat.begin());
//  
//     // Create the dataset
//     DataSet dataset =group.createDataSet<double>(dataname, DataSpace::From(write_mat));
//     
//     dataset.write(write_mat);
//     
//   } catch (Exception& err) {
//     // catch and print any HDF5 error
//     std::cerr << err.what() << std::endl;
//   }
//   
// }
