context("chunk-based API")



test_that("can read a vector chunk", {
  
  tvec <- as.integer(1:5)
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",filter="none", tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_c(filename = tempf, datapath="grp/dat",row_offset = 1,row_chunksize = 2)
  expect_equal(trd, tvec[2:3])
  
  
  tvec <- as.integer(1:5)
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat", tvec)
  
  
  trd <- read_vector_h5(filename = tempf, datapath="grp/dat")
  expect_equal(trd, tvec)
})


test_that("can read a vector index", {
  
  tvec <- as.integer(1:100000)
  iv <- sort(sample(1:length(tvec),3,replace=F))
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",filter="none", tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_i(filename = tempf, datapath="grp/dat",iv)
  expect_equal(trd, tvec[iv])
  
  
  riv <- sample(1:length(tvec),5000,replace=T)
  
  trd <- read_vector_i(filename = tempf, datapath="grp/dat",riv)
  expect_equal(trd, tvec[riv])
  
})



test_that("can read a matrix rowchunk",{
  
  tmat <- matrix(1:27,9,3)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test")  
  
  check_mat <- read_matrix_cc(tf,"test",row_chunksize = 9,col_chunksize = 3)
  expect_equal(tmat,check_mat)
  check_imat <- read_matrix_ii(tf,"test",1:9,1:3)
  expect_equal(check_imat,tmat)  
  
})



test_that("can read a matrix that's actually chunked",{
  
  library(testthat)
  library(EigenH5)
  tmat <- matrix(1:2700,90,30)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(10L,3L))  
  
  check_mat <- read_matrix_cc(tf,"test",row_chunksize = 90,col_chunksize = 30)
  expect_equal(tmat,check_mat)
  check_imat <- read_matrix_ii(tf,"test",1:9,1:3)
  expect_equal(check_imat,tmat)  
  
})

