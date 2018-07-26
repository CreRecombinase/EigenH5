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


context("Matrix Move constructor"){
  using namespace Eigen;
  MatrixXd m0 = MatrixXd::Random(9,9);
  auto m0m = m0.data();
  MatrixXd em;
  test_that("constructor works for simple ranges"){
    expect_true(em.data()!=m0.data());
    em = std::move(m0);
    expect_true(em.data()!=m0.data());
    expect_true(em.data()==m0m);
    Rcpp::Rcout<<"testing ran"<<std::endl;
  }
}
    

