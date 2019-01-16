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

HighFive::File test_file_w(){
  std::string fname = std::tmpnam(nullptr);
  return(HighFive::File(fname,HighFive::File::ReadWrite|HighFive::File::Create));

}

inline HighFive::File file_cw(const std::string filename){
  //  auto gr = EigenH5::get_singleton();
  return(HighFive::File(filename,HighFive::File::ReadWrite|HighFive::File::Create));
}



// context("Moving groups"){
//   test_that("We can mount a file and move data from that file"){
//        auto tf = test_file_w();

//        auto tf2 = test_file_w();
//        auto std::vector	df{1,2,3};
//        using namespace HighFive;
//        tf.createGroup("test").createDataSet("testd",DataSpace::From(df)).write(df);

//        H5Fmount(tf2.getId(), "/G", fid2.getId(), H5P_DEFAULT);
//        auto ng=tf2.getGroup("G");
//        H5Lmove( hid_t src_loc_id, const char *src_name, hid_t dest_loc_id, const char *dest_name, hid_t lcpl_id, hid_t lapl_id )







// context("traversing datapath"){

//   // test_that("We can check for existence of groups"){

//
//   //   expect_true(tf.getObjCount(H5F_OBJ_FILE)==1);
//   //   expect_true(tf.getObjCount(H5F_OBJ_GROUP)==0);
//   //   expect_false(tf.exist("btest"));
//   //   expect_true(tf.getObjCount(H5F_OBJ_GROUP)==0);
//   //   expect_false(tf.openGroup("btest"));
//   //   expect_true(tf.getObjCount(H5F_OBJ_GROUP)==0);
//   //   expect_true(tf.getRefCt()==1);
//   // }


//   // auto tf = test_file_w();
//   // auto tf2 = test_file_w();
//   // test_that("We can create groups and maintain ref ct"){


//   //   auto tgrp2 = tf2.createGroup("btest/test2/test3");
//   //   expect_true(tf2.getObjCount(H5F_OBJ_GROUP)==1);
//   //   expect_true(tgrp2.getRefCt()==1);
//   //   auto ttgrp2 = tgrp2.getGroup(".");
//   //   auto tgrp =	tf.createGroup("/btest");
//   //   expect_true(tf.getObjCount(H5F_OBJ_GROUP)==1);
//   // }


//   // test_that("Checking for groups is robust to syntax of groupname"){
//   //   expect_true(tf.isGroup("btest"));
//   //   expect_true(tf.exist("btest/"));
//   //   expect_true(tf.exist("/btest/"));
//   //   expect_true(tf.isGroup("/btest/"));
//   //   expect_true(tf.isGroup("btest/"));
//   //   expect_true(tf.getObjCount(H5F_OBJ_GROUP)==1);
//   //   expect_true(tf.isGroup("/btest"));
//   //   expect_true(tf.openGroup("btest"));
//   //   expect_true(tf.getObjCount(H5F_OBJ_GROUP)==1);
//   //   expect_false(tf.getGroup("btest").exist("btest"));
//   // }

//   // test_that("Can compare addresses of groups"){
//   //   auto tgrp = tf.getGroup("btest");
//   //   auto ttgrp = tf.getGroup("/btest");
//   //   expect_true(tgrp.getAddr()==ttgrp.getAddr());
//   //   tgrp = tf.createGroup("/test2");
//   //   expect_true(tf.openGroup("test2"));

//   //   tgrp = tf.createGroup("test2/test3");
//   //   expect_true(tf.isGroup("test2/test3"));
//   //   expect_true(tf.isGroup("/test2/test3"));
//   //   expect_true(tf.getGroup("test2").isGroup("test3"));
//   //   expect_true(tf.getGroup("test2").isGroup("/test2/test3"));
//   //   expect_false(tf.getGroup("test2").exist("test2"));

//   //   ttgrp = tf.getGroup("test2").getGroup("/test2/test3");
//   //   expect_true(ttgrp.getAddr()==tgrp.getAddr());



//   // }
// }
