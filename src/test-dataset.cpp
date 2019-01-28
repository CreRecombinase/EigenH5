

/*
 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
//  * `LinkingTo: testthat` within your DESCRIPTION file.
//  */

// // All test files should include the <testthat.h>
// // header file.

#include <stdlib.h>
#include "EigenH5.h"
#include <stdio.h>
#include <testthat.h>
#include <R.h>
#include <Rinternals.h>


//[[depends(RcppEigen)]]


context("optionals"){
  using namespace Rcpp;
  LogicalVector tdat_a = {true,false,true};
  StringVector tdat_b = {"hi","my","friend"};
  IntegerVector tdat_c = {3,5,9};
  NumericVector tdat_d = {3.5,5.3,9.9};



  List tlist= List::create(_["a"]=tdat_a,
			   _["b"]=tdat_b,
			   _["c"]=tdat_c,
			   _["d"]=tdat_d);
  test_that("We can get scalars back for 4 POD types"){
    auto op_a= get_list_scalar<bool>(tlist,"a");
    expect_true(op_a.value()==true);

    auto op_b= get_list_scalar<std::string>(tlist,"b");
    expect_true(op_b.value()=="hi");

    auto op_c= get_list_scalar<int>(tlist,"c");
    expect_true(op_c.value()==3);

    auto op_d= get_list_scalar<double>(tlist,"d");
    expect_true(op_d.value()==3.5);
  }
}






context("datasets"){


  std::string tfname = std::tmpnam(nullptr);
  auto ntf = HighFive::File(tfname,HighFive::File::ReadWrite|HighFive::File::Create);
  std::vector<int> tvec = {1,2,3};
  using namespace HighFive;
  auto space=	DataSpace::From(tvec);
  auto dset =	ntf.createDataSet(Path("ntest"),space,AtomicType<int>(),Filter::From(space,filter_zstd));
  test_that("We can check for existence of datasets"){


    expect_true(ntf.exist(Path("ntest")));
    expect_true(ntf.isDataSet(Path("ntest")));
    expect_false(ntf.isGroup(Path("ntest")));

    expect_true(ntf.exist(Path("ntest")));
    expect_true(ntf.isDataSet(Path("ntest")));
    expect_false(ntf.isGroup(Path("ntest")));

    dset = ntf.createGroup(Path("tgrp")).createDataSet(Path("test2"),space,AtomicType<int>(),Filter::From(space,filter_zstd));
    expect_true(ntf.exist(Path("tgrp/test2")));
    expect_true(ntf.isDataSet(Path("tgrp/test2")));
    expect_false(ntf.isGroup(Path("tgrp/test2")));
    expect_true(ntf.getGroup(Path("tgrp")).exist(Path("test2")));
    expect_true(ntf.getGroup(Path("tgrp")).isDataSet(Path("test2")));
    expect_false(ntf.getGroup(Path("tgrp")).isGroup(Path("test2")));
    expect_false(ntf.getGroup(Path("tgrp")).exist(Path("ntest")));
    // expect_true(ntf.openDataSet("tgrp/test2"));
    // expect_true(ntf.openGroup("tgrp")->openDataSet("test2"));
  }


}

// context("ensuring const"){

//   std::string tfname = std::tmpnam(nullptr);
//   using namespace HighFive;
//   auto ntf = HighFive::File(tfname,HighFive::File::ReadWrite|HighFive::File::Create);
//   Eigen::MatrixXi tmat(2,3);
//   tmat<<0,1,2,3,4,5;

//   auto space=	DataSpace::From(tmat);
//   auto dset =	ntf.createDataSet("ntest",space,AtomicType<int>(),Filter::From(space,filter_zstd));
//   Eigen::MatrixXi copy_t=tmat.eval();
//   test_that("matrix copied succesffully"){
//     expect_true(copy_t(0,0)==tmat(0,0));
//     expect_false(&copy_t.coeffRef(1,1)==&tmat.coeffRef(1,1));
//   }
//   dset.write(tmat);
//   test_that("Matrix remains the same after writing") {
//     expect_true(tmat == copy_t);
//   }
// }
