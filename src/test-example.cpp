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
#include "testthat.h"
#include <stdlib.h>
#include "EigenH5.h"
#include <stdio.h>
#include <R.h>
#include <Rinternals.h>

//[[depends(RcppEigen)]]

HighFive::File test_file_w(){
  std::string fname = std::tmpnam(nullptr);
  return(HighFive::File(fname,HighFive::File::ReadWrite|HighFive::File::Create));
}

inline HighFive::File file_cw(const std::string filename){
  //  auto gr = EigenH5::get_singleton();
  return(HighFive::File(filename,HighFive::File::ReadWrite|HighFive::File::Create));
}









context("traversing datapath"){
  test_that("We can check for existence of groups"){

    auto tf = test_file_w();

    auto tf2 = test_file_w();
    expect_true(tf.getObjCount(H5F_OBJ_FILE)==1);
    expect_true(tf.getObjCount(H5F_OBJ_GROUP)==0);
    expect_false(tf.exist("test"));
    expect_true(tf.getObjCount(H5F_OBJ_GROUP)==0);
    expect_false(tf.openGroup("test"));
    expect_true(tf.getObjCount(H5F_OBJ_GROUP)==0);
    expect_true(tf.getRefCt()==1);
  }


  test_that("We can create groups and maintain ref ct"){
    auto tf = test_file_w();
    auto tf2 = test_file_w();
    auto tgrp2 =	tf2.createGroup("test/test2/test3");
    expect_true(tf2.getObjCount(H5F_OBJ_GROUP)==1);
    expect_true(tgrp2.getRefCt()==1);
    auto ttgrp2 = tgrp2.getGroup(".");
    auto tgrp =	tf.createGroup("/test");
    expect_true(tf.getObjCount(H5F_OBJ_GROUP)==1);



    expect_true(tf.isGroup("test"));
    expect_true(tf.exist("test/"));
    expect_true(tf.exist("/test/"));
    expect_true(tf.isGroup("/test/"));
    expect_true(tf.isGroup("test/"));
    expect_true(tf.getObjCount(H5F_OBJ_GROUP)==1);
    expect_true(tf.isGroup("/test"));
    expect_true(tf.openGroup("test"));
    expect_true(tf.getObjCount(H5F_OBJ_GROUP)==1);
    expect_false(tf.getGroup("test").exist("test"));

    auto ttgrp = tf.getGroup("test");
    expect_true(tgrp.getAddr()==ttgrp.getAddr());
    ttgrp = tf.getGroup("/test");
    expect_true(tgrp.getAddr()==ttgrp.getAddr());
    tgrp = tf.createGroup("/test2");
    expect_true(tf.openGroup("test2"));

    tgrp = tf.createGroup("test2/test3");
    expect_true(tf.isGroup("test2/test3"));
    expect_true(tf.isGroup("/test2/test3"));
    expect_true(tf.getGroup("test2").isGroup("test3"));
    expect_true(tf.getGroup("test2").isGroup("/test2/test3"));
    expect_false(tf.getGroup("test2").exist("test2"));

    ttgrp = tf.getGroup("test2").getGroup("/test2/test3");
    expect_true(ttgrp.getAddr()==tgrp.getAddr());

  }

  test_that("We can check for existence of datasets"){
    auto tf = test_file_w();
    std::vector<int> tvec = {1,2,3};
    using namespace HighFive;
    auto space=	DataSpace::From(tvec);
    auto dset =	tf.createDataSet("test",space,AtomicType<int>(),Filter::From(space,FILTER_BLOSC));
    expect_true(tf.exist("test"));
    expect_true(tf.isDataSet("test"));
    expect_false(tf.isGroup("test"));

    expect_true(tf.exist("/test"));
    expect_true(tf.isDataSet("/test"));
    expect_false(tf.isGroup("/test"));

    dset = tf.createGroup("tgrp").createDataSet("test2",space,AtomicType<int>(),Filter::From(space,FILTER_BLOSC));
    expect_true(tf.exist("tgrp/test2"));
    expect_true(tf.isDataSet("tgrp/test2"));
    expect_false(tf.isGroup("tgrp/test2"));

    expect_true(tf.getGroup("tgrp").exist("test2"));
    expect_true(tf.getGroup("tgrp").isDataSet("test2"));
    expect_false(tf.getGroup("tgrp").isGroup("test2"));
    expect_false(tf.getGroup("tgrp").exist("test"));
    expect_true(tf.openDataSet("tgrp/test2"));
    expect_true(tf.openGroup("tgrp")->openDataSet("test2"));


  }


}

context("We can read and write matrices using the DataQueue"){


    std::string fname = std::tmpnam(nullptr);
    const int n=2;
    const int p=3;
    Eigen::MatrixXi test_mat(n,p);
    Eigen::MatrixXi test_mat_t(n,p);
    int tt=0;
    for(int i=0; i<n; i++){
      for(int j=0; j<p; j++){
	test_mat(i,j)=tt++;
	test_mat_t(i,j)=tt*tt;
      }
    }
    {
      auto tf = file_cw(fname);


      using namespace HighFive;
      auto space=	DataSpace::From(test_mat);
      auto dset =	tf.createDataSet("test",space,AtomicType<int>(),Filter::From(space,FILTER_BLOSC));
      auto dset_t =	tf.createDataSet("test_t",space,AtomicType<int>(),Filter::From(space,FILTER_BLOSC));


      dset.write(test_mat);
      dset_t.write(test_mat_t);
    }
    FileManager fm(Rcpp::wrap(fname),true);
    test_that("We can read matrices (and transposes)"){
      using namespace Rcpp;


      auto tl = List::create(_["filename"]=Rcpp::wrap(fname),
			     _["datapath"]=Rcpp::wrap("test"));
      auto tl_t = List::create(_["filename"]=Rcpp::wrap(fname),
			       _["datapath"]=Rcpp::wrap("test_t"));
      auto ttl = List::create(_["1"]=tl,_["2"]=tl_t);
      Eigen::MatrixXi tm;
      Eigen::MatrixXi tmt;
      Eigen::MatrixXi tm_t;
      DataQueue<2,int> test_queue(ttl,true,fm);


      test_queue.readMat(0,tm,false);
      test_queue.readMat(1,tm_t,false);
      test_queue.readMat(0,tmt,true);
      for(int i=0; i<n; i++){
	for(int j=0; j<p; j++){
	  expect_true(test_mat(i,j)==tm(i,j));
	  expect_true(test_mat_t(i,j)==tm_t(i,j));
	  expect_true(test_mat(i,j)==tmt(j,i));

	}
      }
    }



    test_that("We can read subsets of matrices (and transposes)"){
      using namespace Rcpp;
      auto tl = List::create(_["filename"]=Rcpp::wrap(fname),
			     _["datapath"]=Rcpp::wrap("test"));
      auto tl_t = List::create(_["filename"]=Rcpp::wrap(fname),
			       _["datapath"]=Rcpp::wrap("test_t"),
			       _["subset_cols"]=Rcpp::IntegerVector::create(1,3));
      auto ttl = List::create(_["1"]=tl,_["2"]=tl_t);
      Eigen::MatrixXi tm;
      Eigen::MatrixXi tm_t;
      DataQueue<2,int> test_queue(ttl,true,fm);


      test_queue.readMat(0,tm,false);
      test_queue.readMat(1,tm_t,false);


      expect_true(test_mat_t.col(0)==tm_t.col(0));
      expect_true(test_mat_t.col(2)==tm_t.col(1));

      for(int i=0; i<n; i++){
	for(int j=0; j<p; j++){
	  expect_true(test_mat(i,j)==tm(i,j));

	}
      }
    }


}

			 







// context("making several selections simultaneously"){



//   // std::string fname = std::tmpnam(nullptr);
//   // std::vector<int> tvec = {1,2,3};
//   // using namespace HighFive;
//   // auto space=	DataSpace::From(tvec);
//   // auto dset =	tf.createDataSet("test",space,AtomicType<int>(),Filter::From(space,FILTER_BLOSC));
//   // expect_true(tf.exist("test"));
//   // expect_true(tf.isDataSet("test"));
//   // expect_false(tf.isGroup("test"));



//   auto tf = file_cw(fname);
//     //    expect_true(rf==nullptr);
//     H5Fclose(tf->getId());
//     rf = file_r(tf->getName());
//     expect_false(rf==nullptr);

//   }
//   remove(fname.c_str());
// }



// // Initialize a unit test context. This is similar to how you
// // might begin an R test file with 'context()', expect the
// // associated context should be wrapped in braced.
// context("data dimensions"){
//   test_that("We can get the dimensions of various R objects"){

//     const int p=15;
//     const int n=3;


//     Rcpp::NumericMatrix tmat(p,n);
//     auto td = EigenH5::dims(tmat);
//     expect_true(tmat.nrow()==p);
//     expect_true(tmat.ncol()==n);
//     expect_true(td(0)==tmat.nrow());
//     expect_true(td(1)==tmat.ncol());
//     expect_true(td(0)==p);
//     expect_true(td(1)==n);
//     std::vector<int> ntd = Rcpp::as<std::vector<int> >(td);
//     expect_true(ntd[0]==tmat.nrow());
//     expect_true(ntd[1]==tmat.ncol());
//     expect_true(ntd[0]==p);
//     expect_true(ntd[1]==n);
//     Rcpp::IntegerMatrix ttmat(n,p);
//     td = EigenH5::dims(ttmat);
//     expect_true(td[0]==n);
//     expect_true(td[1]==p);
//   }
// }



// context("Reading and writing Eigen Matrices"){

//   std::vector<int> dims ={3,4};
//   Eigen::MatrixXd wmat(3,4);
//   double ti=0;
//   for(int i=0;i<3;i++){
//     for(int j=0;j<4;j++){
//       wmat(i,j)=ti;
//       ti=ti+1;
//     }
//   }


//   std::string groupname = "testg";
//   std::string dataname = "test";
//   using namespace HighFive;


//   HighFive::File file(fname,HighFive::File::ReadWrite|HighFive::File::Create);

//   //  auto grp = file.createOrGetGroups(groupname);
//   auto  dataset = file.createDataSet< std::string >("/dataset_one",  DataSpace::From(wmat));
//   //  auto  ndataset = file.createDataSet< std::string >("/dataset_two",  DataSpace::From(mv));

//   std::vector<std::string> ntv;
//   dataset.write(temp_wmat);
//   dataset.read(temp_rmat);
//   ndataset.read(ntv);
//   int ii=0;
//   for(int i=0;i<2;i++){
//     for(int j=0;j<3;j++){
//       //      expect_true(temp_wmat(i,j)==temp_rmat(i,j));
//       expect_true(ntv[ii]==mv[ii]);
//       ii++;
//     }
//   }
// }


// // let's write our vector of int to the HDF5 dataset
// //dataset.write(data);







// context("find_cont works as advertised") {
//     Rcpp::IntegerVector tv= Rcpp::IntegerVector::create(1,2,3,4,5);

//   test_that("(forward) contiguous selection") {
//     auto dsv = DimRange(tv.begin(),tv.end());
//     expect_true(dsv.get_num_selections()==1);
//     expect_true(dsv.isCompact());
//     expect_true(dsv.isCompact());
//     auto tds=dsv.dim_sels[0];
//     expect_true(tds.in_start==0);
//     expect_true(tds.in_stop==4);
//     expect_true(tds.out_start==0);
//     expect_true(tds.out_stop==4);
//   }

// }



// template<SEXPTYPE RTYPE>
// SEXP generate_sexp(const int n){

//   //  int n = length(x);

//   if constexpr(RTYPE==REALSXP){
//       double *pout_b;
//       double *pout_e;

//       SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
//       pout_b = REAL(out);
//       pout_e = pout_b + n;
//       std::iota(pout_b,pout_e,0);
//       UNPROTECT(1);

//       return out;
//     }
//   if constexpr(RTYPE==INTSXP){
//       int *pout_b;
//       int *pout_e;

//       SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
//       pout_b = INTEGER(out);
//       pout_e = pout_b + n;
//       std::iota(pout_b,pout_e,0);
//       UNPROTECT(1);
//       return out;
//     }
//   if constexpr(RTYPE==STRSXP){
//       // char** *pout_b;
//       // char** *pout_e;
//    //      SEXP ret, ans1, ans2, cls, nam, rownam;
//    // PROTECT(ret = Rf_allocVector(VECSXP, 2)); // a list with two elements
//    // PROTECT(ans1 = Rf_allocVector(INTSXP, 3)); // first column
//    // PROTECT(ans2 = Rf_allocVector(INTSXP, 3)); // second column
//    // for (int i=0; i<3; ++i) { // some data
//    //    INTEGER(ans1)[i] = i+1;
//    //    INTEGER(ans2)[i] = -(i+1);
//    // }
//    // SET_VECTOR_ELT(ret, 0, ans1);
//    // SET_VECTOR_ELT(ret, 1, ans2);

//    // PROTECT(cls = allocVector(STRSXP, 1)); // class attribute
//    // SET_STRING_ELT(cls, 0, mkChar(\"data.frame\"));
//    // classgets(ret, cls);

//    // PROTECT(nam = allocVector(STRSXP, 2)); // names attribute (column names)
//    // SET_STRING_ELT(nam, 0, mkChar(\"a\"));
//    // SET_STRING_ELT(nam, 1, mkChar(\"b\"));
//    // namesgets(ret, nam);

//    // PROTECT(rownam = allocVector(STRSXP, 3)); // row.names attribute
//    // SET_STRING_ELT(rownam, 0, mkChar(\"1\"));
//    // SET_STRING_ELT(rownam, 1, mkChar(\"2\"));
//    // SET_STRING_ELT(rownam, 2, mkChar(\"3\"));
//    // setAttrib(ret, R_RowNamesSymbol, rownam);

//    // UNPROTECT(6);
//    // return ret;


//       SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
//       // pout_b = REAL(out);
//       // pout_e = pout_b + n;
//       for(int i=0; i<n; i++){
// 	std::string tstring= std::to_string(i);
// 	SET_STRING_ELT(out, i, mkChar(tstring.c_str()));
//       }
//       UNPROTECT(1);

//       return out;
//     }
// }
