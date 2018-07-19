context("h5")

test_that("Can read dosage files (in random order)",{
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
EigenH5::mach2h5(dosagefile = ntsnpf,h5file = tf,datapath = "test",snp_idx = t_idx-1,names=sample_names,p=p,buffer_size = 18,SNPfirst = T)
 
ttsnp_mat <- t(atsnp_mat[,t_idx])
attr(ttsnp_mat,"dimnames") <- NULL
# class(ttsnp_mat) <- "numeric"
mrd <- EigenH5::read_matrix_h5(tf,"/","test")
testthat::expect_equal(mrd,ttsnp_mat)
# which(mrd!=ttsnp_mat,arr.ind = T) 
})


test_that("can write string vector",{
  library(testthat)
  library(EigenH5)
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  testthat::expect_true(EigenH5::write_vector_h5(tempf,datapath="grp/dat",data=tvec))
  expect_equal(typeof_h5(tempf,"grp"),"list")
  expect_equal(typeof_h5(tempf,"grp/dat"),"character")

  expect_equal(get_dims_h5(tempf,"grp/dat"),length(tvec))
  expect_equal(get_dims_h5(tempf,"grp/dat"),length(tvec))
  
  rd <- read_vector_h5(tempf,datapath="grp/dat")
  expect_equal(rd,tvec)
  trd <- read_vector_h5(tempf,datapath="grp/dat",datasize=2)
  expect_equal(head(tvec,2),trd)  
  write_vector_h5(tempf,datapath="/grp/dat2",data=tvec)
  
  trd <- read_vector_h5(tempf,"grp","dat2")
  expect_equal(trd,tvec)
  
  tvec <- c("allb","allc","alld")
  write_vector_h5(tempf,"/grp2/grp3","dat2",data=tvec)
  
  
  trd <- read_vector_h5(tempf,"grp","dat",subset=2:3)
  
  expect_equal(tail(tvec,2),trd)  
  trd <- read_vector_h5(tempf,"grp","dat",subset=c(1,3))
  expect_equal(tvec[c(1,3)],trd) 

})

test_that("can check type of vectors",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  expect_equal(typeof_h5(tempf,"grp"),"list")
  expect_equal(typeof_h5(tempf,"grp/dat"),"character")
  
  tvec <- runif(3)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp/grp2","dat",tvec)
  
  
  tvec <- sample(1:10)
  tempf <- tempfile()
  write_vector_h5(tempf,"/","dat",tvec)
  expect_equal(typeof_h5(tempf,datapath = "dat"),"integer")
  expect_equal(get_dims_h5(tempf,datapath="dat"),10)
})

test_that("can write a vector subset",{
  tvec <- numeric(3)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  write_vector_h5(tempf,"grp","dat",.5,subset=c(3))
  trd <- read_vector_h5(tempf,"grp","dat",subset=3)
  expect_equal(trd,trd)
  trd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(trd,c(0,0,0.5))
})

test_that("can write a vector out of order",{
  tvec <- c(1.0,2.0,3.0)
  tempf <- tempfile()
  ind <- c(3,1,2)
  write_vector_h5(tempf,"grp","dat",tvec,subset=ind)
  trd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(trd,tvec[ind])
})


test_that("can read a vector out of order",{
  tvec <- 1:3
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  trd <- read_vector_h5(tempf,"grp","dat",subset=c(3,1,2))
  expect_equal(trd,tvec[c(3,1,2)])
  
})


test_that("can write REAL vector",{
  tvec <- runif(3)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  expect_equal(dim_h5(tempf,"grp/dat"),length(tvec))
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
  trd <- read_vector_h5(tempf,"grp","dat",datasize=2)
  expect_equal(head(tvec,2),trd)  
  trd <- read_vector_h5(tempf,"grp","dat",subset=2:3)
  expect_equal(tail(tvec,2),trd)  
  trd <- read_vector_h5(tempf,"grp","dat",subset=c(1,3))
  expect_equal(tvec[c(1,3)],trd) 
})

test_that("we can read subsets out of order",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,datapath="grp/dat",data=tvec)
  strd <- read_vector_h5(tempf,datapath="grp/dat")
  expect_equal(strd,tvec)
  trd <- read_vector_h5(tempf,datapath="grp/dat",subset=c(2,1))
  expect_equal(tvec[c(2,1)],trd) 
  trd <- read_vector_h5(tempf,datapath="grp/dat",subset=c(3,1))
  expect_equal(tvec[c(3,1)],trd) 
})


test_that("can read string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})

test_that("can create a vector and then write to it",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  create_vector_h5(tempf,"grp","dat",character(),dim=3L)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,c("","",""))
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})

test_that("can read int matrix",{
  library(EigenH5)
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"/","tmat",tmat)
  rd <- read_matrix_h5(tempf,"/","tmat")
  expect_equal(get_dims_h5(tempf,"/","tmat"),c(100,9))
  expect_equal(tmat,rd)
})


test_that("can concatenate matrices virtually (along columns)",{
  # library(EigenH5)
  # library(testthat)
  pvec <- sample(10:50,4)
  n <- 8
  mats <- purrr::map(pvec,~matrix(sample(1:(n*.x)),n,.x))
  # tfs <- purrr::map(pvec,~tempfile())
  tfs <- purrr::rerun(length(pvec),filename=tempfile(),datapath="/temp")
  purrr::walk2(tfs,mats,~write_matrix_h5(.x$filename,"/","temp",.y))
  purrr::walk2(tfs,mats,~write_matrix_h5(.x$filename,"/","t_temp",t(.y)))
  
  # am <- map(tfs,~update_list(.x,datapath="/temp"))
  t_tfs <- purrr::map(tfs,~purrr::list_modify(.x,datapath="t_temp"))
  
  nf <- tempfile()
  concat_mats(nf,"all_temp_t",t_tfs,margin = 0)
  
  concat_mats(nf,"all_temp",tfs,margin = 1)
  
  
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  # write_matrix_h5(tempf,"/","tmat",tmat)
  rd <- read_matrix_h5(nf,"/","all_temp")
  rd_t <- read_matrix_h5(nf,"/","all_temp_t")
  
  all_mats <- purrr::reduce(mats,cbind)
  all_mats_t <- purrr::map(mats,t) %>% purrr::reduce(rbind)
  expect_equal(all_mats,rd)
  expect_equal(all_mats_t,rd_t)

})



test_that("can read int matrix transpose",{
  rown <- 10
  coln <- 60
  
  tmat <- matrix(1:(rown*coln),rown,coln)
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"/","tmat",tmat)
  rd <- read_matrix_h5(tempf,"/","tmat",doTranspose=T)
  expect_equal(t(rd),tmat)
  
  sr <- sample(1:nrow(tmat),nrow(tmat)-1,replace=F)
  sc <- sample(1:ncol(tmat),ncol(tmat)-1,replace=F)
  # sc <- c(1,4,2,5,3)
  srd <- read_matrix_h5(tempf,"/","tmat",subset_rows=sr,doTranspose=T)
  expect_equal(t(srd),tmat[sr,,drop=F])
  src <- read_matrix_h5(tempf,"/","tmat",subset_cols=sc,doTranspose=F)
  expect_equal(src,tmat[,sc])
  src <- read_matrix_h5(tempf,"/","tmat",subset_cols=sc,doTranspose=T)
  expect_equal(t(src),tmat[,sc])
  srrc <- read_matrix_h5(tempf,"/","tmat",subset_rows=sr,subset_cols=sc,doTranspose=T)
  expect_equal(t(srrc),tmat[sr,sc])
  # mb <- microbenchmark::microbenchmark(rd=read_matrix_h5(tempf,"/","tmat",doTranspose=T),
  #                                      trd=t(read_matrix_h5(tempf,"/","tmat")))
  # expect_equal(tmat,rd)
})



test_that("can read int matrix(one column)",{
  library(EigenH5)
  tmat <- matrix(sample(1:900),100,9)
  smat <- tmat[,3,drop=F]
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"/","tmat",tmat)
  rd <- read_matrix_h5(tempf,"/","tmat",subset_cols=3)
  expect_equal(get_dims_h5(tempf,"/","tmat"),c(100,9))
  expect_equal(smat,rd)
})


test_that("can read int matrix(one row)",{
  library(EigenH5)
  tmat <- matrix(sample(1:900),100,9)
  smat <- tmat[3,,drop=F]
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"/","tmat",tmat)
  rd <- read_matrix_h5(tempf,"/","tmat",subset_rows=3)
  expect_equal(get_dims_h5(tempf,"/","tmat"),c(100,9))
  expect_equal(smat,rd)
})

test_that("check for groups/datasets",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"/grp/grp2","tmat",tmat)
  expect_false(isDataSet(tempf,"grp/grp2/"))
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
  write_matrix_h5(tempf,datapath="grp/grp2/tmat",data=tmat)
  data <- ls_h5(tempf,"grp/grp2",full_names = T)
  expect_equal(dim_h5(tempf,data),dim(tmat))
  

})


test_that("can read int matrix rows",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp/grp2","tmat",tmat)
  ind <- c(1,3,5)
  ttmat <- tmat[ind,]
  rd <- read_matrix_h5(tempf,"grp/grp2","tmat",subset_rows = ind)
  nttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,"grp/grp2","tmat",subset_cols = ind)
  expect_equal(nttmat,rd)
  # expect_equal(ind,c(1,3,5))
  # expect_equal(ttmat,rd)
  ind <- c(1,3,2,100,4)
  ttmat <- tmat[ind,]
  rd <- read_matrix_h5(tempf,datapath="grp/grp2/tmat",subset_rows = ind)
  # expect_equal(ind,c(1,2,3,2,100,4))
  expect_equal(ttmat,rd)
})


test_that("can read int matrix cols",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ind <- c(3,1,5)
  ttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,datapath="grp/tmat",subset_cols = ind)
  expect_equal(ttmat,rd)
  ind <- c(1,2,7,3)
  ttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,datapath="grp/tmat",subset_cols = ind)
  expect_equal(ttmat,rd)
})



test_that("can read int matrix rows & cols",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ttmat <- tmat[c(5,3,1),c(3,5,6)]
  rd <- read_matrix_h5(tempf,datapath="grp/tmat",
                       subset_rows = c(5,3,1),
                       subset_cols=c(3,5,6))
  expect_equal(ttmat,rd)
})



test_that("can read int matrix cols",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ttmat <- tmat[,c(1,3,5)]
  rd <- read_matrix_h5(tempf,"grp","tmat",subset_cols = c(1,3,5))
  expect_equal(ttmat,rd)
})



test_that("can read/write numeric vector",{
  tvec <- runif(100)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})
test_that("can read/write integer vector",{
  tvec <- sample(1:100)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})


test_that("can read string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,
                        groupname = "grp",
                        dataname = "tdat",
                        data = tvec
  )
  rd <- read_vector_h5(tempf,"grp","tdat")
  expect_equal(rd,tvec)
})


test_that("can read and write NA",{
  
  tmat <- matrix(runif(9*10),9,10)
  tmat[sample(1:(9*10),3,replace=F)] <- NA_real_
  tempf <- tempfile()
  write_matrix_h5(tempf,datapath="testg/testd",data=tmat)
  rmat <- read_matrix_h5(tempf,datapath="testg/testd")
  expect_equal(rmat,tmat)
})







test_that("writing 2 matrix blocks works",{
  tmat <- matrix(1:27,9,3)
  tempf <- tempfile()
  create_matrix_h5(tempf,groupname = "testg",dataname = "testd",data=numeric(),dim=c(9,3))
  sub_mat <- tmat[1:5,1:2]
  write_matrix_h5(tempf,
                     groupname = "testg",
                     dataname = "testd",
                     data = sub_mat,offsets = c(0,0),subset_rows = 1:5,subset_cols = 1:2)
  r_sub_mat <- read_matrix_h5(tempf,groupname = "testg",dataname = "testd",offset = c(0L,0L),datasize=c(5L,2L))
  expect_equal(sub_mat,r_sub_mat)
  sub_mat <- tmat[-(1:5),-(1:2),drop=F]
  write_matrix_h5( tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,offset = c(5,2))
  sub_mat <- tmat[(1:5),-(1:2),drop=F]
  write_matrix_h5( tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,subset_cols = c(3L),subset_rows = 1:5)
  sub_mat <- tmat[-(1:5),(1:2),drop=F]
  write_matrix_h5(tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,offsets = c(5,0))
  r_mat <- read_matrix_h5(tempf,"testg","testd")
  expect_equal(tmat,r_mat)
})




test_that("can write a chunk smaller than total (disk) data dimension, specifying only offsets",{
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(tempf,groupname = "testg",dataname = "testd",data=numeric(),dim=c(9,3))
  sub_mat <- tmat[1:5,1:2]
  write_matrix_h5(tempf,groupname="testg",dataname="testd",data=sub_mat,offsets=c(0,0))
  trm <- read_matrix_h5(tempf,"testg","testd",datasizes=c(5L,2L))
  expect_equal(sub_mat,trm)
  
  
})



test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5( tempf,groupname = "testg",dataname = "testd",data=numeric(),dims = c(9L,3L))
  sub_mat <- tmat[3:5,2:3]
  write_matrix_h5(tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(2L,1L))
  r_sub_mat <- read_matrix_h5(tempf,"testg","testd",offsets = c(2L,1L),datasizes = c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})



test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",data=numeric(),dims = c(9L,3L))
  sub_mat <- tmat[3:5,2:3]
  write_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",data = sub_mat,offsets = c(2L,1L))
  r_sub_mat <- read_matrix_h5(tempf,"/","testd",offsets = c(2L,1L),datasizes = c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})
test_that("writing 2 matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd0",data=numeric(),dims = c(9L,3L))
  write_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd0",data = tmat,offsets = c(0L,0L))
  r_sub_mat <- read_matrix_h5(tempf,"testg","testd0")
  expect_equal(tmat,r_sub_mat)
  
})


test_that("can create nested groups",{
  tvec <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_matrix_h5(tempf,"testg/tg2","test",data =tvec)
  rvec <- read_matrix_h5(tempf,"testg/tg2","test")
  expect_equal(tvec,rvec)
})

