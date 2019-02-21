context("chunk-based API")



test_that("can read a vector chunk", {
 
  tvec <- as.integer(1:5)
  tempf <- tempfile()
  write_vector_h5v(filename = tempf, datapath = "grp/dat",filter="none",data= tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",1:2)
  expect_equal(trd, tvec[1:2],check.attributes=F)
  

  tvec <- as.integer(1:5)
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",data =  tvec)
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",2:5)
  expect_equal(trd, tvec[2:5],check.attributes=F)
})


test_that("can read a vector index", {
  
  tvec <- as.integer(1:100000)
  iv <- sort(sample(1:length(tvec),3,replace=F))
  tempf <- tempfile()
  write_vector_h5v(filename = tempf, datapath = "grp/dat",filter="none",data =  tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",iv)
  expect_equal(trd, tvec[iv])
  
  
  riv <- sample(1:length(tvec),5000,replace=T)
  
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",riv)
  expect_equal(trd, tvec[riv])
  
})


test_that("can read a vector index out of order", {
  
  tvec <- as.integer(1:100)
  iv <- sort(sample(1:length(tvec),300,replace=T))
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",filter="none", tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",iv)
  expect_equal(c(trd), tvec[iv])
  
  
  riv <- sample(1:length(tvec),5000,replace=T)
  
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",riv)
  expect_equal(trd, tvec[riv])
  
})




test_that("can read a matrix rowchunk",{
  
  library(EigenH5)
  tmat <- matrix(1:18,6,3)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(3L,3L))  
  
  check_mat <- read_matrix_h5v(tf,"test",1:6,1:3)
  expect_equal(tmat[1:6,1:3],check_mat)
  check_imat <- read_matrix_h5v(tf,"test",c(1:6),c(1:3))
  expect_equal(check_imat,tmat)  
})



test_that("can read a matrix rowchunk",{
  
  library(EigenH5)
  tmat <- matrix(1:18,6,3)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(3L,3L))  
  
  check_mat <- read_matrix_h5v(tf,"test",1:6,1:3)
  expect_equal(tmat[1:6,1:3],check_mat)
  check_imat <- read_matrix_h5v(tf,"test",c(1:6),c(1:3))
  expect_equal(check_imat,tmat)  
})




test_that("can read a matrix that's actually chunked",{
  
  library(testthat)
  library(EigenH5)
  tmat <- matrix(1:2700,90,30)
  tf <- tempfile()
  write_matrix_h5v(tmat,tf,"test",chunksizes=c(10L,3L))  
  
  check_mat <- read_matrix_h5v(tf,"test")
  expect_equal(tmat,check_mat)
  
})




test_that("can write a matrix chunkwise",{

    
    library(testthat)
    library(EigenH5)
    tmat <- matrix(1:2700,90,30)
    tf <- tempfile()
    write_matrix_h5v(tmat,tf,"test",chunksizes=c(10L,3L))  
    
    check_mat <- read_matrix_h5v(tf,"test",1:90,1:30)
    expect_equal(tmat,check_mat)

  
  
})



