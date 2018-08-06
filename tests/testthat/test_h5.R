context("h5")

test_that("Can append a dataframe",{
  
  p_a <- 3
  tdf <- tibble::data_frame(a=runif(p_a),b=sample(1:p_a,p_a))
  tdfb <- tibble::data_frame(a=runif(p_a),b=sample(1:p_a,p_a))
  tf <- tempfile()
  write_df_h5(tdf,tf,"test",max_dim=c(NA_integer_))
  write_df_h5(tdfb,tf,"test",append=T)
  expect_equal(read_df_h5(tf,"test"),dplyr::bind_rows(tdf,tdfb))
  
  
})


test_that("Can read mach dosage files (in random order)",{
# 
# library(tidyverse)
N <- 100
p <- 400
sample_ids <- as.character(sample(1:(N*p),N,replace=F))
ntsnpf <- paste0(tempfile(),".txt.gz")
tsnp_mat <- matrix(sprintf("%.3f",runif(min=0,max=2,N*p)),nrow = N,byrow=T)
expect_true(all(nchar(tsnp_mat)==5))
wtsnp_mat <- cbind(cbind(sample_ids,rep("DOSE",N)),tsnp_mat)
readr::write_delim(tibble::as_data_frame(wtsnp_mat[sample(1:N),]),path = ntsnpf,delim = "\t",col_names = F)
atsnp_mat <-wtsnp_mat[,-c(1,2)]
class(atsnp_mat) <- "numeric"

sample_names <- wtsnp_mat[,1]
p <- ncol(atsnp_mat)
t_idx <- sort(sample(1:p,min(100,as.integer(p/2)),replace=F))
# t_idx <- c(3,10,19,33,34,46,56,79,80)
tf <- tempfile()
EigenH5::mach2h5(dosagefile = ntsnpf,h5file = tf,datapath = "test",snp_idx = t_idx-1,names=sample_names,p=p,options=list(buffer_size = 18,SNPfirst = T))
 
ttsnp_mat <- t(atsnp_mat[,t_idx])
attr(ttsnp_mat,"dimnames") <- NULL
# class(ttsnp_mat) <- "numeric"
mrd <- EigenH5::read_matrix_h5(tf,"/","test")
testthat::expect_equal(mrd,ttsnp_mat)
# which(mrd!=ttsnp_mat,arr.ind = T) 
})


test_that("check for groups/datasets",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tmat,tempf,"/grp/grp2/tmat")
  # expect_false(isDataSet(tempf,"grp/grp2/"))
  expect_true(isGroup(tempf,"grp"))
  expect_true(isGroup(tempf,"grp/grp2"))
  expect_true(isGroup(tempf,"grp/grp2/"))
  
  expect_true(isGroup(tempf,"/"))
  expect_false(isGroup(tempf,"grp/grp2/tmat"))
  expect_true(isDataSet(tempf,"grp/grp2/tmat"))
 
})

test_that("check for groups/datasets",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(filename = tempf,datapath="grp/grp2/tmat",data=tmat)
  data <- ls_h5(tempf,"grp/grp2",full_names = T)
  expect_equal(dim_h5(tempf,data),dim(tmat))

})



test_that("can read and write NA",{
  
  tmat <- matrix(runif(9*10),9,10)
  tmat[sample(1:(9*10),3,replace=F)] <- NA_real_
  tempf <- tempfile()
  write_matrix_h5(filename = tempf,datapath="testg/testd",data=tmat)
  rmat <- read_matrix_h5(filename = tempf,datapath="testg/testd")
  expect_equal(rmat,tmat)
})



test_that("can create nested groups",{
  tvec <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_matrix_h5(filename = tempf,datapath = "testg/tg2/test",data =tvec)
  rvec <- read_matrix_h5(tempf,"testg/tg2/test")
  expect_equal(tvec,rvec)
})

