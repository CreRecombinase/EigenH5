#include <EigenH5.h>


//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp17)]]

#include <highfive/EigenUtils.hpp>
#include <blosc_filter.h>


// [[Rcpp::interfaces(r,cpp)]]
//[[Rcpp::export]]
void start_blosc(){
  Rcpp::Environment env = Rcpp::Environment::global_env();
  auto r = register_blosc(nullptr,nullptr);
  Rcpp::LogicalVector bv(1);
  bv[0]=r==1;
  env["..blosc"]=bv;
  auto rr = register_lzf();
  bv[0]=rr==1;
  env["..lzf"]=bv;


  // Rcpp::LogicalVector bv = env["..blosc"];
  //Rcout << "Stooge Nb 2 is: " << v[1] << std::endl;



}

//[[Rcpp::export]]
bool check_blosc(){
  Rcpp::Environment env = Rcpp::Environment::global_env();
  return(env["..blosc"]);
}





//[[Rcpp::export]]
void create_vector_h5(const std::string &filename,const std::string &groupname,const std::string &dataname,const int dimension,const int chunksize=1000){
  // int r = register_blosc(nullptr,nullptr);
  
  const size_t dim=dimension;
  const size_t chunk=chunksize;
  HighFive::create_vector_h5<double>(filename,groupname,dataname,dim,chunk);
}

void write_vector(const std::string &filename,const std::string &groupname,const std::string &dataname,Eigen::VectorXd &data){
  // int r = register_blosc(nullptr,nullptr);
  
  HighFive::write_vector_h5(filename,groupname,dataname,data);
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
Rcpp::IntegerMatrix parse_mat(Rcpp::CharacterVector inp){
  // using namespace ranges;
  // inp_r ranges::make_iterator_range(inp.begin(), inp.end());
  const size_t n_cols=inp.size();
    
  const size_t n_rows=((inp(0).size()+1)/2);
  Rcpp::IntegerMatrix retmat(n_rows,n_cols);
  for(int i=0;i<n_cols;i++){
    auto tst=inp[i];
    for(int j=0;j<n_rows;j++){
      const int tj=(j*2);
      retmat(j,i)=tst[tj]-'0';
    }
  }
  return(retmat);
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

