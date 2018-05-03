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
Eigen::MatrixXi slow_subset_cols(const Eigen::MatrixXi input, Eigen::ArrayXi perm,const bool sort_first=false){
  const size_t ps=perm.size();
  if(ps > input.cols()){
    Rcpp::stop("error in permutation");
  }
  Eigen::MatrixXi retmat(input.rows(),ps);

  if(sort_first){
    std::sort(perm.data(),perm.data()+perm.size());
  }

  for(int ti=0; ti<ps; ti++){
    retmat.col(ti)=input.col(perm(ti));
  }
  return(retmat);
}







context("DimRange constructors"){


  test_that("constructor works for simple ranges"){
    Rcpp::IntegerVector my_ints = {1,2,3,4,5};
    Eigen::ArrayXi permi(5);
    permi<<0,1,2,3,4;
    DimRange dr(my_ints.begin(),my_ints.end());
    expect_true(dr.isCompact());
    expect_true(dr.isSorted());
    expect_false(dr.isRepeated());
    expect_true(dr.get_num_selections()==1);
    expect_true(dr.get_n_elem()==my_ints.size());
    expect_true(dr.permutation_order().isApprox(permi));
  }

  test_that("constructor works for sorted, non-compact ranges"){
    Rcpp::IntegerVector my_ints = {1,2,3,5};
    Eigen::ArrayXi permi(my_ints.size());
    permi<<0,1,2,3;
    DimRange dr(my_ints.begin(),my_ints.end());
    expect_false(dr.isCompact());
    expect_true(dr.isSorted());
    expect_false(dr.isRepeated());
    expect_true(dr.get_num_selections()==2);
    expect_true(dr.get_n_elem()==my_ints.size());
    expect_true(dr.permutation_order().isApprox(permi));
  }

  test_that("constructor works for non-sorted, compact ranges"){
    Rcpp::IntegerVector my_ints = {5,4,3,2,1};
    Eigen::ArrayXi permi(my_ints.size());
    permi<<4,3,2,1,0;
    DimRange dr(my_ints.begin(),my_ints.end());
    expect_true(dr.isCompact());
    expect_false(dr.isSorted());
    expect_false(dr.isRepeated());
    expect_true(dr.get_num_selections()==1);
    expect_true(dr.get_n_elem()==my_ints.size());
    expect_true(dr.permutation_order().isApprox(permi));
  }

  test_that("constructor works for non-sorted, non-compact ranges with compact substructure"){
    Rcpp::IntegerVector my_ints = {6,5,3,2,1};
    Eigen::ArrayXi permi(my_ints.size());
    permi<<4,3,2,1,0;
    DimRange dr(my_ints.begin(),my_ints.end());
    expect_false(dr.isCompact());
    expect_false(dr.isSorted());
    expect_false(dr.isRepeated());
    expect_true(dr.get_num_selections()==2);
    expect_true(dr.get_n_elem()==my_ints.size());

    expect_true(dr.permutation_order().isApprox(permi));
  }

  test_that("constructor works for non-sorted, non-compact ranges with non-compact substructure"){
    Rcpp::IntegerVector my_ints = {1,5,3,2,6};
    Eigen::ArrayXi permi(my_ints.size());
    permi<<0,3,2,1,4;
    DimRange dr(my_ints.begin(),my_ints.end());
    expect_false(dr.isCompact());
    expect_false(dr.isSorted());
    expect_false(dr.isRepeated());
    expect_true(dr.get_num_selections()==4);
    expect_true(dr.get_n_elem()==my_ints.size());
    expect_true(dr.permutation_order().isApprox(permi));
  }
}

context("PermutationMatrices"){


  const int n=2;
  const int p=5;

  Eigen::MatrixXi test_mat(n,p);
  test_mat<<0,1,2,3,4, 5,6,7,8,9;
  test_that("Can get back original matrix by permuting with identity"){
    Eigen::ArrayXi permi(p);
    permi<<0,1,2,3,4,5;
    Eigen::PermutationMatrix<Eigen::Dynamic> pm(permi.matrix());

    expect_true(test_mat*pm==test_mat);
  }
  test_that("Can flip matrix by reversing permutation vector"){
    Eigen::ArrayXi permi(p);
    permi<<0,1,2,3,4,5;
    std::reverse(permi.data(),permi.data()+p);
    Eigen::PermutationMatrix<Eigen::Dynamic> pm(permi.matrix());
    Eigen::MatrixXi ret_tm = test_mat.rowwise().reverse();
    expect_true((test_mat*pm)==(ret_tm));
  }
  test_that("Permutation works the way I think it works(simple reverse)"){
    Rcpp::IntegerVector my_ints = {6,5,3,2,1};
    Eigen::ArrayXi true_sub(5);
    Eigen::ArrayXi perm_i(5);
    perm_i<<4,3,2,1,0;
    true_sub<<5,4,3,1,0;
    auto sub_mat=slow_subset_cols(test_mat,true_sub,true);
    auto true_sub_mat=slow_subset_cols(test_mat,true_sub,false);
    Eigen::PermutationMatrix<Eigen::Dynamic> pm(perm_i.matrix());
    expect_true(sub_mat*pm==true_sub_mat);
  }

  test_that("Permutation works the way I think it works (non-trivial permutation)"){
    Rcpp::IntegerVector my_ints = {1,5,3,2,6};
    Eigen::ArrayXi true_sub(5);
    Eigen::ArrayXi perm_i(5);
    perm_i<<0,3,2,1,4;
    true_sub<<0,4,3,1,5;
    auto sub_mat=slow_subset_cols(test_mat,true_sub,true);
    auto true_sub_mat=slow_subset_cols(test_mat,true_sub,false);
    Eigen::PermutationMatrix<Eigen::Dynamic> pm(perm_i.matrix());
    expect_true(sub_mat*pm==true_sub_mat);
  }
  test_that("(simple) Permutation works on vectors"){
    Eigen::Matrix<int,4,1> testv {2,3,4,5};
    Eigen::Matrix<int,4,1> resv	{5,4,3,2};
    Eigen::ArrayXi true_sub(5);
    Eigen::Matrix<int,4,1> perm_i(5);
    perm_i<<3,2,1,0;
    Eigen::PermutationMatrix<4> pm(perm_i.matrix());
    expect_true(pm*testv==resv);
  }

  test_that("(arbitrary) Permutation works on vectors"){
    Eigen::Matrix<int,4,1> testv {2,3,4,5};
    Eigen::Matrix<int,4,1> resv	{2,4,3,5};
    Eigen::ArrayXi true_sub(5);
    Eigen::Matrix<int,4,1> perm_i(5);
    perm_i<<0,2,1,3;
    Eigen::PermutationMatrix<4> pm(perm_i.matrix());
    expect_true(pm*testv==resv);
  }



}
