context("h5")

test_that("can write string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_s_vec_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})

test_that("can read string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vec_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})


test_that("can read/write numeric vector",{
  tvec <- runif(100)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vec_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})
test_that("can read/write integer vector",{
  tvec <- sample(1:100)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vec_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})


test_that("can read string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  SeqSupport::write_vec(h5filename = tempf,
                        groupname = "grp",
                        dataname = "tdat",
                        data = tvec
  )
  rd <- read_s_vec_h5(tempf,"grp","tdat")
  check_dtype(tempf,"grp","tdat")
  expect_equal(rd,tvec)
})

test_that("writing a matrix works as in RcppEigenH5",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_mat_h5(tempf,groupname = "testg",dataname = "testd",data = tmat)  
  rmat <- RcppEigenH5::read_2d_mat_h5(tempf,"testg","testd")  
  expect_equal(rmat,tmat)  
})

test_that("doTranspose is respected in reading",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  RcppEigenH5::write_mat_h5(tempf,groupname = "testg",dataname = "testd",data = tmat,doTranspose = T)  
  
 
  r_mat <- read_mat_h5(tempf,"testg","testd")
  expect_equal(r_mat,tmat)
})

test_that("doTranspose is respected in writing",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_mat_h5(tempf,groupname = "testg",dataname = "testd",data = tmat,doTranspose = T)  
  r_mat <- RcppEigenH5::read_2d_mat_h5(tempf,"testg","testd")
  expect_equal(r_mat,tmat)
})



test_that("writing 2 matrix blocks works",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_mat_h5(tempf,groupname = "testg",dataname = "testd",data = tmat)
  write_mat_h5(tempf,groupname = "testg",dataname = "testd2",data = tmat)
  sub_mat <- tmat[3:5,2:3]
  sub_mat <- tmat[3:5,2:3]
  r_sub_mat <- read_mat_h5(tempf,"testg","testd",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
  r_sub_mat <- read_mat_h5(tempf,"testg","testd2",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})

test_that("writing matrix blocks works alright",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd",dimensions = c(9L,3L),doTranspose = F)
  sub_mat <- tmat[3:5,2:3]
  write_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  r_sub_mat <- read_mat_h5(tempf,"testg","testd",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})



test_that("writing matrix blocks works alright",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",dimensions = c(9L,3L),doTranspose = F)
  sub_mat <- tmat[3:5,2:3]
  write_mat_chunk_h5(filename = tempf,groupname = "/",dataname = "testd",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  r_sub_mat <- read_mat_h5(tempf,"/","testd",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})
test_that("writing 2 matrix blocks works alright",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd",dimensions = c(9L,3L),doTranspose = T)
  sub_mat <- tmat[3:5,2:3]
  write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  r_sub_mat <- read_mat_h5(tempf,"testg","testd",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd2",dimensions = c(9L,3L),doTranspose = T)
  sub_mat <- tmat[3:5,2:3]
  write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd2",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  r_sub_mat <- read_mat_h5(tempf,"testg","testd2",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
  
})

test_that("writing a vector chunk works",{
  tvec <- runif(200)
  tempf <- tempfile()
  create_vector_h5(tempf,"testg","test",200L)
  sub_t <- tvec[5:195]
  write_vec_chunk_h5(filename = tempf,groupname = "testg",dataname = "test",data = sub_t,offsets = 4,chunksizes = 191)
  retvec <- read_vector(tempf,"testg","test",offsets = 4L,chunksizes = 191L)
  expect_equal(retvec,sub_t)
})


test_that("writing a vector works as in RcppEigenH5",{
  tvec <- runif(200)
  tempf <- tempfile()
  write_vector(filename = tempf,groupname = "testg",dataname = "test",data = tvec)
  retvec <- read_vector(tempf,"testg","test")
  expect_equal(tvec,retvec)
})
