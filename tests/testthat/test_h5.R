context("h5")

test_that("writing a matrix works as in RcppEigenH5",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_mat_h5(h5file = tempf,groupname = "testg",dataname = "testd",data = tmat,deflate_level = 0L)  
  rmat <- RcppEigenH5::read_2d_mat_h5(tempf,"testg","testd")  
  expect_equal(rmat,tmat)  
})