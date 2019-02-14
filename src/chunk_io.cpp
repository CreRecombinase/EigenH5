#include "EigenH5.h"
//[[depends(RcppEigen)]]
//[[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::depends(BH)]]
#include <progress.hpp>
#include <array>
#include <Rcpp.h>
#include <Rinternals.h>
// [[Rcpp::interfaces(r, cpp)]]

template<typename T>
struct t2chunk_t{
  typedef std::false_type type;
};

template<> struct t2chunk_t<std::pair<int,int>>{
  typedef ChunkParser type;
}

template<> struct t2chunk_t<Rcpp::IntegerVector>>{
  typedef IndexParser type;
}







SEXP read_vector_c(std::string filename,
		   std::string datapath,
		   int	row_offset=0,
		   int	row_chunksize=-1){
 using namespace Rcpp;

  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);
  // auto dims = dset.getDataDimensions();

  // auto chunk_dims = dset.getFilter().get_chunksizes();


  ChunkParser rows(dset,{row_offset,row_chunksize});

  auto my_t = typeof_h5_dset(dset);


  switch (my_t){
  case INTSXP: {
    VecShuttle<ChunkParser,int> mover(rows,1);
    return(mover.read_vector());
    break;
  }
  case REALSXP: {
    VecShuttle<ChunkParser,double> mover(rows,1);
    return(mover.read_vector());
    break;
  }
  case STRSXP: {
    Rcpp::stop("sorry,can't convert string (yet)");
    //    VecShuttle<ChunkParser,std::string> mover(rows,1);
    //    return(mover.read_vector(dset));
    break;

    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}



SEXP read_vector_i(std::string filename,
		   std::string datapath,
		   Rcpp::IntegerVector index=Rcpp::IntegerVector::create()){
 using namespace Rcpp;

  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);

  IndexParser rows(dset,index);

  auto my_t = typeof_h5_dset(dset);


  switch (my_t){
  case INTSXP: {
    VecShuttle<IndexParser,int> mover(rows,1);
    return(mover.read_vector());
    break;
  }
  case REALSXP: {
    VecShuttle<IndexParser,double> mover(rows,1);
    return(mover.read_vector());
    break;
  }
  case STRSXP: {
    Rcpp::stop("sorry, can't convert string (yet)");
    //    VecShuttle<ChunkParser,std::string> mover(rows,1);
    //    return(mover.read_vector());
    break;

    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}








SEXP read_matrix_ci(std::string filename,
		   std::string datapath,
		    int	row_offset=0,
		    int row_chunksize=-1,
		    Rcpp::IntegerVector index=Rcpp::IntegerVector::create()){
  using namespace Rcpp;

  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);

  const ChunkParser rows(dset,row_offset,row_chunksize,0);
  const IndexParser cols(dset,index,1);

  auto my_t = typeof_h5_dset(dset);


  switch (my_t){
  case INTSXP: {
    MatrixShuttle<ChunkParser,IndexParser,int> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case REALSXP: {
    MatrixShuttle<ChunkParser,IndexParser,double> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case STRSXP: {
    Rcpp::stop("sorry, can't convert string (yet)");
    //    VecShuttle<ChunkParser,std::string> mover(rows,1);
    //    return(mover.read_matrix());
    break;

    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}




SEXP read_matrix_ic(std::string filename,
		    std::string datapath,
		    Rcpp::IntegerVector row_index=Rcpp::IntegerVector::create(),
		    int	col_offset=0,
		    int col_chunksize=-1){
  using namespace Rcpp;

  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);


  const IndexParser rows(dset,row_index,0);
  const ChunkParser cols(dset,col_offset,col_chunksize,1);

  auto my_t = typeof_h5_dset(dset);


  switch (my_t){
  case INTSXP: {
    MatrixShuttle<IndexParser,ChunkParser,int> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case REALSXP: {
    MatrixShuttle<IndexParser,ChunkParser,double> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case STRSXP: {
    Rcpp::stop("sorry, can't convert string (yet)");
    //    VecShuttle<ChunkParser,std::string> mover(rows,1);
    //    return(mover.read_matrix());
    break;

    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}





SEXP read_matrix_ii(std::string filename,
		    std::string datapath,
		    Rcpp::IntegerVector row_index=Rcpp::IntegerVector::create(),
		    Rcpp::IntegerVector col_index=Rcpp::IntegerVector::create()){
  using namespace Rcpp;

  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);


  const IndexParser rows(dset,row_index,0);
  const IndexParser cols(dset,col_index,1);

  auto my_t = typeof_h5_dset(dset);


  switch (my_t){
  case INTSXP: {
    MatrixShuttle<IndexParser,IndexParser,int> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case REALSXP: {
    MatrixShuttle<IndexParser,IndexParser,double> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case STRSXP: {
    Rcpp::stop("sorry, can't convert string (yet)");
    //    VecShuttle<ChunkParser,std::string> mover(rows,1);
    //    return(mover.read_matrix());
    break;

    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}






SEXP read_matrix_cc(std::string filename,
		    std::string datapath,
		    int	row_offset=0,
		    int row_chunksize=-1,
		    int	col_offset=0,
		    int col_chunksize=-1){
  using namespace Rcpp;

  auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);

  const ChunkParser rows(dset,row_offset,row_chunksize,0);
  const ChunkParser cols(dset,col_offset,col_chunksize,1);


  auto my_t = typeof_h5_dset(dset);


  switch (my_t){
  case INTSXP: {
    MatrixShuttle<ChunkParser,ChunkParser,int> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case REALSXP: {
    MatrixShuttle<ChunkParser,ChunkParser,double> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case STRSXP: {
    Rcpp::stop("sorry, can't convert string (yet)");
    //    VecShuttle<ChunkParser,std::string> mover(rows,1);
    //    return(mover.read_matrix());
    break;

    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}








template<typename RT,typename CT,typename RRT=t2chunk_t<RT>::type,typename CCT=t2chunk_t<CT>::type>
SEXP read_matrix_tt(std::string filename,
		    std::string datapath,
		    const RT rowsub,
		    const CT colsub){

   auto dp=root_path(datapath);

  HighFive::File file(filename,HighFive::File::ReadOnly);

  auto groupname = dp.parent_path();
  auto dataname = dp.filename();
  auto dset = file.getDataSet(dp);

  const RRT rows(dset,rowsub,0);
  const CCT cols(dset,colsub,1);


  auto my_t = typeof_h5_dset(dset);


  switch (my_t){
  case INTSXP: {
    MatrixShuttle<RRT,CCT,int> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case REALSXP: {
    MatrixShuttle<RRT,CCT,double> mover(rows,cols,1);
    return(mover.read_matrix());
    break;
  }
  case STRSXP: {
    Rcpp::stop("sorry, can't convert string (yet)");
    //    VecShuttle<ChunkParser,std::string> mover(rows,1);
    //    return(mover.read_matrix());
    break;

    break;
  }
  default: {
    warning(
	    "Invalid SEXPTYPE %d.\n",
	    my_t
	    );
    Rcpp::Rcerr<<dataname<<" has type that can't be read"<<std::endl;

    Rcpp::stop("Can't read type");
    return R_NilValue;
  }
  }
}



class VisitMatrix
  :public
  boost::static_visitor<SEXP>
{
  const std::string &filename;
  const std::string &datapath;
  VisitMatrix(const std::string &filename_,
	      const std::string &datapath_):filename(filename_),datapath(datapath_){}
  template<typename RT,typename CT>
  SEXP operator()(RT rowdat,CT coldat){
    return(read_matrix_tt<RT,CT>(filename,datapath,rowdat,coldat));
  }
}


//[[Rcpp::export]]
  SEXP read_matrix_v(const std::string filename,  const std::string datapath, SEXP rows,SEXP cols){

    return(boost:apply_visitor(VisitMatrix(filename,datapath),dispatch_subset(rows),dispatch_subset(cols)));
  }
