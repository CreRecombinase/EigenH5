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

# library(EigenH5)
# library(testthat)
test_that("Can read mach dosage files (in random order)",{
# 
# library(tidyverse)
N <- 50
p <- 6000
sample_ids <- as.character(sample(1:(N*p),N,replace=F))
ntsnpf <- paste0(tempfile(),".txt.gz")
tsnp_mat <- matrix(sprintf("%.3f",runif(min=0,max=2,N*p)),nrow = N,byrow=T)
expect_true(all(nchar(tsnp_mat)==5))
wtsnp_mat <- cbind(cbind(sample_ids,rep("DOSE",N)),tsnp_mat)
readr::write_delim(tibble::as_data_frame(wtsnp_mat[sample(1:N),]),path = ntsnpf,delim = "\t",col_names = F)



atsnp_mat <-wtsnp_mat[,-c(1,2)]
class(atsnp_mat) <- "numeric"
hdf5_f <- 
sample_names <- wtsnp_mat[,1]

p <- ncol(atsnp_mat)
t_idx <- sort(sample(1:p,min(101,as.integer(p/2)),replace=F))
# t_idx <- c(3,10,19,33,34,46,56,79,80)
tf <- tempfile()
create_dataset_h5(tf,"test",numeric(),options=list(dims=c(length(t_idx),N)))
EigenH5::mach2h5(dosagefile = ntsnpf,
                 h5file = tf,
                 datapath = "test",
                 snp_idx = t_idx-1,
                 names=sample_names,
                 p=p,options=list(progress=T))
ttsnp_mat <- t(atsnp_mat[,t_idx])
attr(ttsnp_mat,"dimnames") <- NULL
# class(ttsnp_mat) <- "numeric"
mrd <- EigenH5::read_matrix_h5(tf,"test")
testthat::expect_equal(mrd,ttsnp_mat)
# which(mrd!=ttsnp_mat,arr.ind = T) atsn  
})




test_that("Can read mach dosage files (in random order split between 2 files)",{
  # 
  # library(tidyverse)
  N <- 50
  p <- 40000
  sample_ids <- as.character(sample(1:(N*p),N,replace=F))
  ntsnpf_a <- paste0(tempfile(),".txt.gz")
  ntsnpf_b <- paste0(tempfile(),".txt.gz")
  
  tsnp_mat <- matrix(sprintf("%.3f",runif(min=0,max=2,N*p)),nrow = N,byrow=T)
  expect_true(all(nchar(tsnp_mat)==5))
  
  split_pt <- sample(1:N,floor(N/2),replace=F)
  N_a <- length(split_pt)
  N_b <- length(sample_ids[-split_pt])
  expect_equal(N_a+N_b,N)
  tsnp_mat_a <- tsnp_mat[split_pt,]
  tsnp_mat_b <- tsnp_mat[-split_pt,]
  
  wtsnp_mat_a <- cbind(cbind(sample_ids[split_pt],rep("DOSE",N_a)),tsnp_mat_a)
  wtsnp_mat_b <- cbind(cbind(sample_ids[-split_pt],rep("DOSE",N_b)),tsnp_mat_b)
  
  expect_equal(nrow(wtsnp_mat_a),N_a)
  expect_equal(nrow(wtsnp_mat_b),N_b)
  readr::write_delim(tibble::as_data_frame(wtsnp_mat_a[sample(1:N_a),]),path = ntsnpf_a,delim = "\t",col_names = F)
  readr::write_delim(tibble::as_data_frame(wtsnp_mat_b[sample(1:N_b),]),path = ntsnpf_b,delim = "\t",col_names = F)
  
  class(tsnp_mat) <- "numeric"

  t_idx <- sort(sample(1:p,min(100,as.integer(p/2)),replace=F))
  # t_idx <- c(3,10,19,33,34,46,56,79,80)
  tf <- tempfile()
  n_p <- length(t_idx)
  create_dataset_h5(filename = tf,datapath="test",data=numeric(),options=list(dims=c(n_p,N)))
  
  EigenH5::mach2h5(dosagefile = ntsnpf_a,
                   h5file = tf,
                   datapath = "test",
                   snp_idx = t_idx-1,
                   names=sample_ids,
                   p=p,options=list(progress=T))
  EigenH5::mach2h5(dosagefile = ntsnpf_b,
                   h5file = tf,
                   datapath = "test",
                   snp_idx = t_idx-1,
                   names=sample_ids,
                   p=p,options=list(progress=T))
  ttsnp_mat <- t(tsnp_mat[,t_idx])
  attr(ttsnp_mat,"dimnames") <- NULL
  # class(ttsnp_mat) <- "numeric"
  mrd <- EigenH5::read_matrix_h5(tf,"test")
  testthat::expect_equal(mrd,ttsnp_mat)
  # which(mrd!=ttsnp_mat,arr.ind = T) atsn  
})




test_that("Can read mach dosage files (in random order split between 2 files with extra lines)",{
  # 
  # library(tidyverse)
  N <- 50
  p <- 400
  
  gen_mach <- function(N,p){
    sample_ids <- as.character(sample(1:(N*p),N,replace=F))
    tsnp_mat <- matrix(sprintf("%.3f",runif(min=0,max=2,N*p)),nrow = N,byrow=T)
    expect_true(all(nchar(tsnp_mat)==5))
    wtsnp_mat <- cbind(cbind(sample_ids,rep("DOSE",N)),tsnp_mat)
    return(wtsnp_mat)
  }
  
  
  ntsnpf_a <- paste0(tempfile(),".txt.gz")
  ntsnpf_b <- paste0(tempfile(),".txt.gz")
  
  all_tsnp <- gen_mach(4*N,p)
  wtsnp_a <- all_tsnp[1:N,]
  wtsnp_b <- all_tsnp[(1:N)+N,]
  wtsnp_c <- all_tsnp[(1:N)+2*N,]
  wtsnp_d <- all_tsnp[(1:N)+3*N,]
  
  expect_equal(rbind(wtsnp_a,wtsnp_b,wtsnp_c,wtsnp_d),all_tsnp)
  
  good_tsnp <- rbind(wtsnp_a,wtsnp_c)
  
  
  
  all_tsnp_a <- rbind(wtsnp_a,wtsnp_b)
  all_tsnp_b <- rbind(wtsnp_c,wtsnp_d)

  readr::write_delim(tibble::as_data_frame(all_tsnp_a[sample(1:(2*N)),]),path = ntsnpf_a,delim = "\t",col_names = F)
  readr::write_delim(tibble::as_data_frame(all_tsnp_b[sample(1:(2*N)),]),path = ntsnpf_b,delim = "\t",col_names = F)
  
  tsnp_mat <- good_tsnp[,-c(1,2)]
  class(tsnp_mat) <- "numeric"
  
  t_idx <- sort(sample(1:p,min(100,as.integer(p/2)),replace=F))
  # t_idx <- c(3,10,19,33,34,46,56,79,80)
  tf <- tempfile()
  n_p <- length(t_idx)
  create_dataset_h5(filename = tf,datapath="test",data=numeric(),options=list(dims=c(n_p,2*N)))
  sample_ids <- good_tsnp[,1]
  
  EigenH5::mach2h5(dosagefile = ntsnpf_a,
                   h5file = tf,
                   datapath = "test",
                   snp_idx = t_idx-1,
                   names=sample_ids,
                   p=p,options=list(progress=T))
  EigenH5::mach2h5(dosagefile = ntsnpf_b,
                   h5file = tf,
                   datapath = "test",
                   snp_idx = t_idx-1,
                   names=sample_ids,
                   p=p,options=list(progress=T))
  ttsnp_mat <- t(tsnp_mat[,t_idx])
  attr(ttsnp_mat,"dimnames") <- NULL
  # class(ttsnp_mat) <- "numeric"
  mrd <- EigenH5::read_matrix_h5(tf,"test")
  testthat::expect_equal(mrd,ttsnp_mat)
  # which(mrd!=ttsnp_mat,arr.ind = T) atsn  
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


library(EigenH5)
library(testthat)
test_that("can create nested groups",{
  tvec <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_matrix_h5(filename = tempf,datapath = "/testg/tg2/test",data =tvec)
  write_matrix_h5(filename=tempf,datapath="/testg/tg3/test",data=tvec)
  rvec <- read_matrix_h5(tempf,"/testg/tg2/test")
  expect_equal(tvec,rvec)
})

test_that("can create nested vector groups",{
  tvec <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_vector_h5(filename = tempf,datapath = "testg/tg2/test",data =c(tvec))
  write_vector_h5(filename=tempf,datapath="testg/tg3/test",data=c(tvec))
  rvec <- read_vector_h5(tempf,"testg/tg2/test")
  expect_equal(c(tvec),rvec)
})

