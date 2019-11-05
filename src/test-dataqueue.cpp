
#include <stdlib.h>
#include "EigenH5.h"
#include "eigenh5/indexers.hpp"
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

      auto dset =	tf.createDataSet(Path("test"),space,AtomicType<int>(),Filter::From(space,filter_zstd));
      auto dset_t =	tf.createDataSet(Path("test_t"),space,AtomicType<int>(),Filter::From(space,filter_zstd));
      auto dset_v =	tfw.createDataSet(Path("test_v"),space_v,AtomicType<int>(),Filter::From(space_v,filter_zstd));


      dset.write(test_mat);
      dset_t.write(test_mat_t);
    }


    FileManager<true> fm(Rcpp::wrap(fname));
    FileManager<false> fw(Rcpp::wrap(fnamew));


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
      if(!fmb->second.exist(Path("test"))){
	Rcpp::stop("can't proceed if 'test' does not exist!");
      }
      if(!fmb->second.exist(Path("test_t"))){
	Rcpp::stop("can't proceed if 'test_t' does not exist!");
      }
      if(!fwb->second.exist(Path("test_v"))){
	Rcpp::stop("can't proceed if 'test_v' does not exist!");
      }

      Eigen::MatrixXi ttm_m;
      fmb->second.getDataSet(Path("test")).read(ttm_m);
      expect_true(ttm_m==test_mat);



      Eigen::MatrixXi tm;
      Eigen::MatrixXi tmt;
      Eigen::MatrixXi tm_t;
      Eigen::VectorXi tm_v;

      DataQueue<2,int,true> test_queue(ttl,fm);
      DataQueue<1,int,false> test_queue_v(tlv,fw);


      test_queue.readMat(0,tm,false);
      test_queue.readMat(1,tm_t,false);
      test_queue.readMat(0,tmt,true);
      test_queue_v.writeVector(0,test_vec_t);
      fwb->second.flush();

      Eigen::VectorXi ttm_v;
      fwb->second.getDataSet(Path("test_v")).read(ttm_v);
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
      DataQueue<2,int,true> test_queue(ttl,fm);


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

context("Subsetting works"){

  constexpr SubInterval inter(0,100);
  constexpr SubInterval o_inter(10,100);
  static_assert(inter==SubInterval::front_back(0,99));
  static_assert(inter.get_offset()==0,"offset works");
  static_assert(inter.get_size()==100,"size works");
  static_assert(inter.get_back()==99,"size works");
  static_assert(inter.num_chunks(10)==10,"chunking works");
  static_assert(inter.num_chunks(11)==10,"ceiling division works");
  static_assert(inter.chunk_i(0,10)==SubInterval(0,10),"first chunk is first chunk");
  static_assert(o_inter.chunk_i(0,10)==SubInterval(10,10),"first offset chunk is first offset chunk");
  static_assert(o_inter.chunk_i(1,10)==SubInterval(20,10),"second offset chunk is first offset chunk");
  static_assert(inter.chunk_i(1,10)==SubInterval(10,10),"second chunk is second chunk");
  static_assert(inter.chunk_i(3,10)==SubInterval(30,10),"third chunk is third chunk");

  static_assert(inter.new_offset(4).get_offset()==4);
  static_assert(inter.new_offset(4).get_back()==99);

  static_assert(inter.get_back()==99);
  static_assert(inter.new_back(4)==SubInterval(0,5));

  static_assert(inter.chunk_i(0,10).sub_chunk(SubInterval(5,20))==SubInterval(5,5));
  static_assert(inter.chunk_i(1,10).sub_chunk(SubInterval(5,20))==SubInterval(0,10));
  static_assert(inter.chunk_i(2,10).sub_chunk(SubInterval(5,20))==SubInterval(0,5));
  static_assert(inter.chunk_selection(SubInterval(3,10),10).get_offset()==0);

  static_assert(SubInterval(3,10).closest_boundary_below(10)==0);
  static_assert(SubInterval(3,10).num_chunks(10)==1);
  static_assert(SubInterval(3,10).num_chunks_total(10)==2);
  static_assert(SubInterval(3,10).closest_boundary_below(10)==0);
  static_assert(SubInterval(3,10).closest_boundary_above(10)==19);

  static_assert(inter.truncate_back(SubInterval(0,20))==SubInterval(0,20));

  static_assert(inter.chunk_selection(SubInterval(3,10),10).get_offset()==0);
  static_assert(inter.chunk_selection(SubInterval(3,10),10).get_offset()==0);
  static_assert(inter.chunk_selection(SubInterval(3,10),10).get_back()==19);

  static_assert(inter.chunk_selection(SubInterval(3,10),10).get_size()==20);
  static_assert(inter.chunk_selection(SubInterval(0,10),10).get_size()==10);
  static_assert(inter.chunk_selection(SubInterval(11,10),10).get_size()==20);
  static_assert(inter.chunk_selection(SubInterval(11,10),10).get_offset()==10);

  static_assert(inter.truncate_back(SubInterval(50,100)).get_size()==50);
  static_assert(inter.truncate_back(SubInterval(50,100)).get_offset()==50);
  static_assert(inter.truncate_back(SubInterval(50,50)).get_size()==50);


  // constexpr SubInterval big_inter(0,  2370249);
  // static_assert(inter.chunk_selection(SubInterval(1127221,1243028),150000).get_offset()==2);
  const int datasize =6000;
  const int offset =2135;
  const int selsize=2000;
  const size_t chunksize = 1000;

  Rcpp::IntegerVector inpv(selsize,0);
  std::iota(inpv.begin(),inpv.end(),offset+1);
  const IndexParser inp(datasize,chunksize,inpv);
  const ChunkParser cp(datasize,chunksize,{offset,selsize},false);
  expect_true(inp.total_size()==cp.total_size());
  expect_true(inp.total_chunksize()==cp.total_chunksize());
  expect_true(inp.n_chunks()==cp.n_chunks());
  int itcc=0;
  auto chunk_b = cp.begin();
  for(auto ind_b :inp){
    expect_true(ind_b.disk_offset()==chunk_b->disk_offset());
    expect_true(ind_b.disk_size()==chunk_b->disk_size());
    Rcpp::Rcerr<<itcc++<<std::endl;
    Rcpp::Rcerr<<ind_b<<std::endl<<*chunk_b<<std::endl;
    expect_true(ind_b.chunk_offset()==chunk_b->chunk_offset());
    expect_true(ind_b.chunk_size()==chunk_b->chunk_size());
    chunk_b++;
  }


}
