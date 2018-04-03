/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.
 */

// All test files should include the <testthat.h>
// header file.
#include <testthat.h>

#include "EigenH5.h"
//[[depends(RcppEigen)]]
// Initialize a unit test context. This is similar to how you
// might begin an R test file with 'context()', expect the
// associated context should be wrapped in braced.
context("data dimensions"){
  test_that("We can get the dimensions of various R objects"){

    const int p=15;
    const int n=3;


    Rcpp::NumericMatrix tmat(p,n);
    auto td = EigenH5::dims(tmat);
    expect_true(tmat.nrow()==p);
    expect_true(tmat.ncol()==n);
    expect_true(td(0)==tmat.nrow());
    expect_true(td(1)==tmat.ncol());
    expect_true(td(0)==p);
    expect_true(td(1)==n);
    std::vector<int> ntd = Rcpp::as<std::vector<int> >(td);
    expect_true(ntd[0]==tmat.nrow());
    expect_true(ntd[1]==tmat.ncol());
    expect_true(ntd[0]==p);
    expect_true(ntd[1]==n);
    Rcpp::IntegerMatrix ttmat(n,p);
    td = EigenH5::dims(ttmat);
    expect_true(td[0]==n);
    expect_true(td[1]==p);
  }
}



context("Reading and writing Eigen Matrices"){

  std::vector<int> dims ={2,3};
  std::vector<std::string> mv =	{"won't","you",
				 "spend","some",
				 "time", "with"};
  std::vector<std::string> tr(6);
  Eigen::Map<Eigen::Matrix<std::string,Eigen::Dynamic,Eigen::Dynamic> > temp_wmat(mv.data(),2,3);
  Eigen::Map<Eigen::Matrix<std::string,Eigen::Dynamic,Eigen::Dynamic> > temp_rmat(tr.data(),2,3);
  std::string fname = std::tmpnam(nullptr);
  std::string groupname = "testg";
  std::string dataname = "test";
  using namespace HighFive;
  HighFive::File file(fname,HighFive::File::ReadWrite|HighFive::File::Create);

  //  auto grp = file.createOrGetGroups(groupname);
  auto  dataset = file.createDataSet< std::string >("/dataset_one",  DataSpace::From(temp_wmat));
  auto  ndataset = file.createDataSet< std::string >("/dataset_two",  DataSpace::From(mv));
  ndataset.write(mv);
  std::vector<std::string> ntv;
  dataset.write(temp_wmat);
  dataset.read(temp_rmat);
  ndataset.read(ntv);
  int ii=0;
  for(int i=0;i<2;i++){
    for(int j=0;j<3;j++){
      expect_true(mv[ii]==tr[ii]);
      expect_true(temp_wmat(i,j)==temp_rmat(i,j));
      expect_true(ntv[ii]==mv[ii]);
      ii++;
    }
  }



}


// let's write our vector of int to the HDF5 dataset
//dataset.write(data);







context("find_cont works as advertised") {
    Rcpp::IntegerVector tv= Rcpp::IntegerVector::create(1,2,3,4,5);

  test_that("(forward) contiguous selection") {
    auto dsv = DimRange(tv.begin(),tv.end());
    expect_true(dsv.get_num_selections()==1);
    expect_true(dsv.isCompact());
    expect_true(dsv.isCompact());
    auto tds=dsv.dim_sels[0];
    expect_true(tds.in_start==0);
    expect_true(tds.in_stop==4);
    expect_true(tds.out_start==0);
    expect_true(tds.out_stop==4);
  }

}
