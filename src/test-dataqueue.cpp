
#include <stdlib.h>
#include "EigenH5.h"
#include <stdio.h>
#include <testthat.h>
#include <R.h>
#include <Rinternals.h>


context("We can read and write matrices using the DataQueue"){


    std::string fname = std::tmpnam(nullptr);
    std::string fnamew = std::tmpnam(nullptr);

    const int n=2;
    const int p=3;
    Eigen::MatrixXi test_mat(n,p);
    Eigen::MatrixXi test_mat_t(n,p);
    Eigen::VectorXi test_vec_t(n*p);
    test_vec_t<<0,1,2,3,4,5;
    test_mat<<0,1,2,3,4,5;
    test_mat_t<<0,1,2,3,4,5;

    {
      auto tf = HighFive::File(fname,HighFive::File::ReadWrite|HighFive::File::Create);
      auto tfw = HighFive::File(fnamew,HighFive::File::ReadWrite|HighFive::File::Create);



      using namespace HighFive;
      auto space=	DataSpace::From(test_mat);
      auto space_v=	DataSpace::From(test_vec_t);

      auto dset =	tf.createDataSet("test",space,AtomicType<int>(),Filter::From(space,FILTER_BLOSC));
      auto dset_t =	tf.createDataSet("test_t",space,AtomicType<int>(),Filter::From(space,FILTER_BLOSC));
      auto dset_v =	tfw.createDataSet("test_v",space_v,AtomicType<int>(),Filter::From(space_v,FILTER_BLOSC));


      dset.write(test_mat);
      dset_t.write(test_mat_t);
    }


    FileManager fm(Rcpp::wrap(fname),true);
    FileManager fw(Rcpp::wrap(fnamew),false);


    test_that("We can read matrices (and transposes)"){
      using namespace Rcpp;


      auto tl = List::create(_["filename"]=Rcpp::wrap(fname),
    			     _["datapath"]=Rcpp::wrap("test"));

      auto tl_t = List::create(_["filename"]=Rcpp::wrap(fname),
    			       _["datapath"]=Rcpp::wrap("test_t"));
      auto ttl = List::create(_["1"]=tl,_["2"]=tl_t);

      auto tv = List::create(_["filename"]=Rcpp::wrap(fnamew),
    			     _["datapath"]=Rcpp::wrap("test_v"));
      auto tlv = List::create(_["1"]=tv);

      auto fmb = fm.get_files();
      auto fwb = fw.get_files();

      expect_true(fmb->second.getName()==fname);
      expect_true(fwb->second.getName()==fnamew);
      if(!fmb->second.exist("test")){
	Rcpp::stop("can't proceed if 'test' does not exist!");
      }
      if(!fmb->second.exist("test_t")){
	Rcpp::stop("can't proceed if 'test_t' does not exist!");
      }
      if(!fwb->second.exist("test_v")){
	Rcpp::stop("can't proceed if 'test_v' does not exist!");
      }

      Eigen::MatrixXi ttm_m;
      fmb->second.getDataSet("test").read(ttm_m);
      expect_true(ttm_m==test_mat);



      Eigen::MatrixXi tm;
      Eigen::MatrixXi tmt;
      Eigen::MatrixXi tm_t;
      Eigen::VectorXi tm_v;

      DataQueue<2,int> test_queue(ttl,true,fm);
      DataQueue<1,int> test_queue_v(tlv,false,fw);


      test_queue.readMat(0,tm,false);
      test_queue.readMat(1,tm_t,false);
      test_queue.readMat(0,tmt,true);
      test_queue_v.writeVector(0,test_vec_t);
      fwb->second.flush();

      Eigen::VectorXi ttm_v;
      fwb->second.getDataSet("test_v").read(ttm_v);
      expect_true(ttm_v==test_vec_t);
      test_queue_v.readVector(0,tm_v);
      expect_true(tm_v==test_vec_t);
      expect_true(test_mat==tm);
      expect_true(test_mat==tmt.transpose());

    }



    test_that("We can read subsets of matrices (and transposes)"){

      using namespace Rcpp;
      auto tl_t = List::create(_["filename"]=Rcpp::wrap(fname),
    			       _["datapath"]=Rcpp::wrap("test_t"),
    			       _["subset_cols"]=Rcpp::IntegerVector::create(1,3));

      auto tl_tt = List::create(_["filename"]=Rcpp::wrap(fname),
    			       _["datapath"]=Rcpp::wrap("test_t"),
    			       _["subset_cols"]=Rcpp::IntegerVector::create(3,2));

      auto tl_ttt = List::create(_["filename"]=Rcpp::wrap(fname),
    			       _["datapath"]=Rcpp::wrap("test_t"),
    			       _["subset_cols"]=Rcpp::IntegerVector::create(3,1));

      auto tl_tttt = List::create(_["filename"]=Rcpp::wrap(fname),
				 _["datapath"]=Rcpp::wrap("test_t"),
				  _["subset_cols"]=Rcpp::IntegerVector::create(3,1),
				  _["subset_rows"]=Rcpp::IntegerVector::create(2,1));


      auto ttl = List::create(_["1"]=tl_t,
			      _["2"]=tl_tt,
			      _["3"]=tl_ttt,
			      _["4"]=tl_tttt
			      );

      Eigen::MatrixXi tm_t;
      Eigen::MatrixXi tm_tt;
      Eigen::MatrixXi tm_ttt;
      Eigen::MatrixXi tm_tttt;
      DataQueue<2,int> test_queue(ttl,true,fm);


      test_queue.readMat(0,tm_t,false);
      test_queue.readMat(1,tm_tt,false);
      test_queue.readMat(2,tm_ttt,false);
      test_queue.readMat(3,tm_tttt,false);

      Eigen::MatrixXi true_tm(2,2);;

      true_tm<<test_mat_t.col(0),test_mat_t.col(2);
      expect_true(tm_t==true_tm);

      true_tm<<test_mat_t.col(2),test_mat_t.col(1);
      expect_true(tm_tt==true_tm);

      true_tm<<test_mat_t.col(2),test_mat_t.col(0);
      expect_true(tm_ttt==true_tm);


      true_tm<<test_mat_t.col(2),test_mat_t.col(0);
      true_tm.colwise().reverseInPlace();
      expect_true(tm_tttt==true_tm);







    }



}
