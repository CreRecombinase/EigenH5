context("h5")

library(testthat)
library(EigenH5)
test_that("can write string vector",{
  tvec <- c("allb","allc","alld","alle")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  expect_equal(get_dims_h5(tempf,"grp","dat"),length(tvec))
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})


# test_that("can use R interface for files",{
#   tvec <- c("allb","allc","alld")
#   tempf <- tempfile()
#   write_vector_h5(tempf,"grp","dat",tvec)
#   mf <-  new(H5File, tempf, TRUE)
# })


test_that("can read string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})

test_that("can read int matrix",{
  library(EigenH5)
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"grp","tmat",tmat)
  rd <- read_matrix_h5(tempf,"grp","tmat")
  expect_equal(get_dims_h5(tempf,"grp","tmat"),c(100,9))
  expect_equal(get_dims_h5(tempf,"grp","tmat"),c(100,9))
  expect_equal(tmat,rd)
})

# cont_reg(c(1:10,15:12),chunksize=5)
# cont_diff(c(1:10,15:12),chunksize = 5)

test_that("can read int matrix to an 'array'",{
  library(EigenH5)
  tmat <- matrix(letters[1:6],2,3)
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"grp","tmat",tmat)
  rd <- read_array_h5(tempf,"grp","tmat",subset_indices = list(integer(),1:2))
  expect_equal(get_dims_h5(tempf,"grp","tmat"),c(2,3))
  expect_equal(tmat,rd)
})

# test_that("can read allel_h5",{
#   tempf <- '/home/nwknoblauch/Desktop/t_19.h5'
#   rd <- data.matrix(read_delim("/home/nwknoblauch/Desktop/t_19.impute.hap",delim=" ",col_names = F))
#   attr(rd,"dimnames")<- NULL
#   tal <- do.call("cbind",array_branch(trd,c(1,2)))
#   expect_equal(tal,rd)
#   snp_df <- read_df_h5("/home/nwknoblauch/Desktop/scratch/polyg_scratch/impute/EUR.chr19.h5","variants",subcols = c("CHROM"))
#   
# 
#   trd <- read_array_h5(tempf,"calldata","GT",offsets=c(0L,0L,0L),chunksizes=c(500L,503L,2L))
# })

test_that("can read int matrix rows",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ind <- c(1,3,5)
  ttmat <- tmat[ind,]
  rd <- read_matrix_h5(tempf,"grp","tmat",subset_rows = ind)
  expect_equal(ind,c(1,3,5))
  expect_equal(ttmat,rd)
  ind <- c(1,2,3,2,100,4)
  ttmat <- tmat[ind,]
  rd <- read_matrix_h5(tempf,"grp","tmat",subset_rows = ind)
  expect_equal(ind,c(1,2,3,2,100,4))
  expect_equal(ttmat,rd)
})
test_that("can read int matrix cols",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ind <- c(3,1,5)
  ttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,"grp","tmat",subset_cols = ind)
  expect_equal(ttmat,rd)
  ind <- c(1,2,7,3)
  ttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,"grp","tmat",subset_cols = ind)
  expect_equal(ttmat,rd)
})

test_that("can read int matrix rows",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ttmat <- tmat[c(1,3,5),]
  rd <- read_matrix_h5(tempf,"grp","tmat",subset_rows = c(1,3,5))
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

test_that("can read a string matrix",{
  tm <- matrix(c("allb","allc","alld","alle"),2,2)
  tempf <- tempfile()
  
  write_matrix_h5(tempf,
                  groupname = "grp",
                  dataname = "tdat",
                  data = tm
  )
  rd <- read_matrix_h5(tempf,"grp","tdat")
  expect_equal(rd,tm)
  
  #check_dtype(tempf,"grp","tdat")
  
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
  check_dtype(tempf,"grp","tdat")
  expect_equal(rd,tvec)
})

test_that("writing a matrix works as in RcppEigenH5",{
  
  tmat <- matrix(runif(9*10),9,10)
  tempf <- tempfile()
  EigenH5::write_matrix_h5(tempf,groupname = "testg",dataname = "testd",data = tmat)
  sub_t <- tmat[,c(1,3,5)]
  rmat <- read_matrix_h5(tempf,"testg","testd",subset_cols = c(1,3,5))
  expect_equal(rmat,sub_t)  
  write_matrix_h5(tempf,groupname = "testg",dataname = "testd2",data = tmat)  
  rmat <- read_matrix_h5(tempf,"testg","testd2",subset_cols=c(1L,3L,5L))
  expect_equal(rmat,sub_t)
})





test_that("writing 2 matrix blocks works",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(tempf,groupname = "testg",dataname = "testd",data=numeric(),dims=c(9,3))
  sub_mat <- tmat[1:5,1:2]
  write_matrix_h5(filename = tempf,
                     groupname = "testg",
                     dataname = "testd",
                     data = sub_mat,offsets = c(0,0))
  r_sub_mat <- read_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd",offsets = c(0L,0L),chunksizes = c(5L,2L))
  
  expect_equal(sub_mat,r_sub_mat)
  sub_mat <- tmat[-(1:5),-(1:2),drop=F]
  write_matrix_h5(filename = tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,
                  offsets = c(5,2))
  sub_mat <- tmat[(1:5),-(1:2),drop=F]
  write_matrix_h5(filename = tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,offsets = c(0,2))
  sub_mat <- tmat[-(1:5),(1:2),drop=F]
  write_matrix_h5(filename = tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,offsets = c(5,0))
  r_mat <- read_matrix_h5(tempf,"testg","testd")
  expect_equal(tmat,r_mat)
})



test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd",data=numeric(),dims = c(9L,3L),doTranspose = F)
  sub_mat <- tmat[3:5,2:3]
  write_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(2L,1L))
  r_sub_mat <- read_matrix_h5(tempf,"testg","testd",offsets = c(2L,1L),chunksizes = c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})



test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",data=numeric(),dims = c(9L,3L),doTranspose = F)
  sub_mat <- tmat[3:5,2:3]
  write_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  r_sub_mat <- read_matrix_h5(tempf,"/","testd",offsets = c(2L,1L),chunksizes = c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})
test_that("writing 2 matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd0",data=numeric(),dims = c(9L,3L),doTranspose = F)
  write_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd0",data = tmat,offsets = c(0L,0L),doTranspose = F)
  r_sub_mat <- read_matrix_h5(tempf,"testg","testd0")
  expect_equal(tmat,r_sub_mat)
  
  # write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(1L,2L),chunksizes = c(9L,2L))
  # r_sub_mat <- read_mat_h5(tempf,"testg","testd",c(2L,1L),c(3L,2L))
  # expect_equal(sub_mat,r_sub_mat)
  # create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd2",dimensions = c(9L,3L),doTranspose = F)
  # sub_mat <- tmat[3:5,2:3]
  # write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd2",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  # r_sub_mat <- read_mat_h5(tempf,"testg","testd2",c(2L,1L),c(3L,2L))
  # expect_equal(sub_mat,r_sub_mat)
  
})

test_that("writing a vector chunk works",{
  tvec <- runif(200)
  tempf <- tempfile()
  create_vector_h5(tempf,"testg","test",200L)
  sub_t <- tvec[5:195]
  write_vec_chunk_h5(filename = tempf,groupname = "testg",dataname = "test",data = sub_t,offsets = 4,chunksizes = 191)
  retvec <- read_vector_h5(tempf,"testg","test",offset = 4L,chunksize = 191L)
  expect_equal(retvec,sub_t)
})


test_that("can create nested groups",{
  tvec <- runif(200)
  tempf <- tempfile()
  tm <- matrix(runif(9*3),9,3)
  write_matrix_h5(tempf,"testg/tg2","test",data =tm)
  ntv <- read_matrix_h5(tempf,"testg/tg2","test")
  expect_equal(ntv,tm)
})




test_that("writing a vector works as in RcppEigenH5",{
  tvec <- runif(200)
  tempf <- tempfile()
  write_vector(filename = tempf,groupname = "testg",dataname = "test",data = tvec)
  retvec <- read_vector(tempf,"testg","test")
  expect_equal(tvec,retvec)
})

test_that("guessing chunks works like in python",{
  data_dims <- c(10000,10)
  guess_chunks(data_dims)
  
})
