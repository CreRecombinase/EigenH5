#include <EigenH5.h>

//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]

#include <highfive/EigenUtils.hpp>
#include <highfive/H5DataSet.hpp>
#include <highfive/H5Filter.hpp>
#include <highfive/H5DataSpace.hpp>
#include <highfive/H5File.hpp>
#include <highfive/H5Attribute.hpp>
#include <highfive/H5Utility.hpp>
#include <highfive/H5DataType.hpp>
#include <highfive/H5Group.hpp>
#include <highfive/H5PropertyList.hpp>
#include <highfive/H5FileDriver.hpp>
#include <highfive/H5Object.hpp>
#include <highfive/H5Selection.hpp>
#include <blosc_filter.h>


// Rcpp::RObject read_h5(std::string filename, std::string dataname){
//   using namespace HighFive;
//   HighFive::File file(filename, File::ReadOnly);
//   auto dataset = file.getDataSet(dataname);
//   auto dataspace = dataset.getSpace();
//   auto dimvec=dataspace.getDimensions();
//   DataType dt= dataset.getDataType();
// }

//[[Rcpp::export]]
Eigen::ArrayXd read_vector(const std::string &filename,const std::string &groupname,const std::string &dataname, const Rcpp::IntegerVector offsets= Rcpp::IntegerVector::create(),
                           const Rcpp::IntegerVector chunksizes= Rcpp::IntegerVector::create()){
  using namespace HighFive;
  std::vector<size_t> local_chunksizes,local_offsets;
  int r = register_blosc(nullptr,nullptr);
  
  std::copy(offsets.begin(),offsets.end(),std::back_inserter(local_offsets));
  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));
  
  Eigen::VectorXd retvec;
  read_mat_h5(filename,groupname,dataname,retvec,local_offsets,local_chunksizes);
  return(retvec);
}

//[[Rcpp::export]]
Rcpp::IntegerVector guess_chunks(std::vector<int> dimensions){
  std::vector<size_t> l_dimensions(dimensions.size());
  std::copy(dimensions.begin(),dimensions.end(),l_dimensions.begin());
  return(Rcpp::wrap(HighFive::Filter::guess_chunk(l_dimensions,8)));
} 

//[[Rcpp::export]]
void create_matrix_h5(const std::string &filename,
                      const std::string &groupname,
                      const std::string &dataname,
                      const std::vector<int> &dimensions,
                      const bool doTranspose=false,
                      const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create()){
  
  int r = register_blosc(nullptr,nullptr);
  std::vector<size_t> chunk_dims(chunksizes.begin(),chunksizes.end());
  std::vector<size_t> mat_dims(dimensions.begin(),dimensions.end());
  HighFive::create_matrix_h5<double>(filename,groupname,dataname,mat_dims,doTranspose,chunk_dims);
}

//[[Rcpp::export]]
void create_vector_h5(const std::string &filename,const std::string &groupname,const std::string &dataname,const int dimension,const int chunksize=1000){
  int r = register_blosc(nullptr,nullptr);
  
  const size_t dim=dimension;
  const size_t chunk=chunksize;
  HighFive::create_vector_h5<double>(filename,groupname,dataname,dim,chunk);
}

//[[Rcpp::export]]
void write_vector(const std::string &filename,const std::string &groupname,const std::string &dataname,Eigen::VectorXd &data){
  int r = register_blosc(nullptr,nullptr);
  
  HighFive::write_vector_h5(filename,groupname,dataname,data);
}
// void write_vector(std::string filename,std::string dataname,Eigen::ArrayXd &data){
//   using namespace HighFive;
//   File file(filename, File::ReadWrite );
//   
//   int r;
//   
//   /* Register the filter with the library */
//   r = register_blosc(nullptr,nullptr);
//   
//   std::vector<size_t> cshape{100};
//   
//   Filter filter(cshape, FILTER_BLOSC, r);
//   // Create a dataset with double precision floating points
//   DataSet dataset = file.createDataSet(dataname, DataSpace({(unsigned long) data.size()}), AtomicType<double>(), filter.getId());
//   // Define the size of our dataset: 2x6
//   
//   // write it
//   double* dp=data.data();
//   dataset.write(dp);
//   
// }

//[[Rcpp::export]]
Eigen::MatrixXd read_mat_h5(std::string filename,
                            std::string groupname,
                            std::string dataname,
                            const Rcpp::IntegerVector offsets= Rcpp::IntegerVector::create(),
                            const Rcpp::IntegerVector chunksizes= Rcpp::IntegerVector::create()){
  int r = register_blosc(nullptr,nullptr);
  
  std::vector<size_t> local_chunksizes,local_offsets;
  std::copy(offsets.begin(),offsets.end(),std::back_inserter(local_offsets));
  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));
  
  
  
  
  Eigen::MatrixXd retmat;
  HighFive::read_mat_h5(filename,groupname,dataname,retmat,local_offsets,local_chunksizes);
  return(retmat);
  
}

//[[Rcpp::export]]
bool is_transposed(const std::string filename,
                   const std::string groupname,
                   const std::string dataname){
  return(HighFive::File(filename,HighFive::File::ReadOnly).getGroup(groupname).getDataSet(dataname).isTransposed());
}

//[[Rcpp::export]]
void copy_mat_h5(std::string infilename,
                            std::string outfilename,
                            std::string groupname,
                            std::string dataname,
                            const Rcpp::IntegerVector offsets= Rcpp::IntegerVector::create(),
                            const Rcpp::IntegerVector chunksizes= Rcpp::IntegerVector::create()){
  int r = register_blosc(nullptr,nullptr);
  
  std::vector<size_t> local_chunksizes,local_offsets;
  std::copy(offsets.begin(),offsets.end(),std::back_inserter(local_offsets));
  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));
  
  
  
  
  Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor>  retmat;
  HighFive::read_mat_h5(infilename,groupname,dataname,retmat,local_offsets,local_chunksizes);
  
  HighFive::write_mat_h5(infilename,groupname,dataname,retmat);
  
}



//[[Rcpp::export]]
Eigen::MatrixXd read_mat_cols_h5(std::string filename,
                            std::string groupname,
                            std::string dataname,
                            const Rcpp::IntegerVector cols= Rcpp::IntegerVector::create()){
  int r = register_blosc(nullptr,nullptr);
  
  std::vector<size_t> columns_local(cols.size());
  std::transform(cols.begin(), cols.end(), columns_local.begin(),
                 [](int c) -> size_t { return (c-1); });

  HighFive::File file(filename, HighFive::File::ReadOnly);
  
  
  auto group = file.getGroup(groupname);
  
  Eigen::MatrixXd retmat;
  group.getDataSet(dataname).select(columns_local).read(retmat);
  return(retmat);

}



//[[Rcpp::export]]
Eigen::MatrixXd read_mat_rows_h5(std::string filename,
                                 std::string groupname,
                                 std::string dataname,
                                 const Rcpp::IntegerVector rows= Rcpp::IntegerVector::create(),const bool read_transpose=false){
  int r = register_blosc(nullptr,nullptr);
  
  std::vector<size_t> rows_local(rows.size());
  std::transform(rows.begin(), rows.end(), rows_local.begin(),
                 [](int c) -> size_t { return (c-1); });
  
  HighFive::File file(filename, HighFive::File::ReadOnly);
  
  
  auto group = file.getGroup(groupname);
  
  Eigen::MatrixXd retmat;
 auto dataset=group.getDataSet(dataname);
  if(!read_transpose){
    dataset.selectRows(rows_local).read(retmat);
  }else{
    const size_t data_colnum=dataset.getDataDimensions()[1];
    const size_t data_rownum=rows_local.size();
    const bool is_t = dataset.isTransposed();
    if(!is_t){
      retmat.resize(data_colnum,data_rownum);
      Eigen::Map<Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic,Eigen::RowMajor> > read_mat(retmat.data(),data_rownum,data_colnum);
      group.getDataSet(dataname).selectRows(rows_local).read(read_mat);
    }else{
      retmat.resize(data_rownum,data_colnum);
      Eigen::Map<Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic,Eigen::ColMajor> > read_mat(retmat.data(),data_rownum,data_colnum);
      group.getDataSet(dataname).selectRows(rows_local).read(read_mat);
      retmat.transposeInPlace();
    }
  }
  return(retmat);
}

//[[Rcpp::export]]
void write_mat_h5(std::string filename,
                  std::string groupname,
                  std::string dataname,
                  Eigen::MatrixXd &data,const bool doTranspose=false,
                  const Rcpp::IntegerVector chunksizes=Rcpp::IntegerVector::create()){
  int r = register_blosc(nullptr,nullptr);
  std::vector<size_t> local_chunksizes;
  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));
  HighFive::write_mat_h5(filename,groupname,dataname,data,doTranspose,local_chunksizes);
  
  
  
}

//[[Rcpp::export]]
void write_mat_chunk_h5(std::string filename,
                        std::string groupname,
                        std::string dataname,
                        Eigen::MatrixXd &data,
                        const Rcpp::IntegerVector offsets= Rcpp::IntegerVector::create(),
                        const Rcpp::IntegerVector chunksizes= Rcpp::IntegerVector::create()){
  int r = register_blosc(nullptr,nullptr);
  
  std::vector<size_t> local_chunksizes,local_offsets;
  std::copy(offsets.begin(),offsets.end(),std::back_inserter(local_offsets));
  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));
  HighFive::write_mat_chunk_h5(filename,groupname,dataname,data,local_offsets,local_chunksizes);
}


//[[Rcpp::export]]
void write_vec_chunk_h5(std::string filename,
                        std::string groupname,
                        std::string dataname,
                        Eigen::VectorXd &data,
                        const Rcpp::IntegerVector offsets= Rcpp::IntegerVector::create(),
                        const Rcpp::IntegerVector chunksizes= Rcpp::IntegerVector::create()){
  int r = register_blosc(nullptr,nullptr);
  
  std::vector<size_t> local_chunksizes,local_offsets;
  std::copy(offsets.begin(),offsets.end(),std::back_inserter(local_offsets));
  std::copy(chunksizes.begin(),chunksizes.end(),std::back_inserter(local_chunksizes));
  HighFive::write_mat_chunk_h5(filename,groupname,dataname,data,local_offsets,local_chunksizes);
}

