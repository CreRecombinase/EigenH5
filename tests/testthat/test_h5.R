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

x<-runif(1000) #sampling from the uniform(0,1) 1000 times
xvals<--log(x) #apply the -log transformation
xvals <- 1/exp(x)

t<-seq(0,max(xvals),length.out = 100) #vector of X values used to generate the theoretical exponential distribution

plot(ecdf(xvals),ylab='F(x)',main="Exponential Cumulative Density") #plot empirical values
points(t,pexp(t),col='pink') #superimpose the theoretical distribution
plot(pexp(t))


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
  
  tmat <- matrix(runif(9*10),9,10)
  tempf <- tempfile()
  write_mat_h5(tempf,groupname = "testg",dataname = "testd",data = tmat,doTranspose = T)  
  sub_t <- tmat[,c(1,3,5)]
  rmat <- read_mat_cols_h5(tempf,"testg","testd",c(1L,3L,5L))
  expect_equal(rmat,sub_t)  
  write_mat_h5(tempf,groupname = "testg",dataname = "testd2",data = tmat,doTranspose = F)  
  rmat <- read_mat_cols_h5(tempf,"testg","testd2",c(1L,3L,5L))
  expect_equal(rmat,sub_t)
})


test_that("Can read a subset of rows",{
  
  tmat <- matrix(runif(9*10),9,10)
  tempf <- tempfile()
  write_mat_h5(tempf,groupname = "testg",dataname = "testd",data = tmat,doTranspose = F)  
  sub_t <- tmat[c(1,3,5),]
  rmat <- read_mat_rows_h5(tempf,"testg","testd",c(1L,3L,5L))
  expect_equal(rmat,sub_t)  
  write_mat_h5(tempf,groupname = "testg",dataname = "testd2",data = tmat,doTranspose = T)  
  rmat <- read_mat_rows_h5(tempf,"testg","testd2",c(1L,3L,5L))
  expect_equal(rmat,sub_t)
})

test_that("Can read a subset of rows and transpose",{
  tmat <- matrix(as.numeric(1:90),9,10)
  tempf <- tempfile()
  write_mat_h5(tempf,groupname = "testg",dataname = "testd",data = tmat,doTranspose = F)  
  sub_t <- tmat[c(1,3,5),]
  rmat <- read_mat_rows_h5(tempf,"testg","testd",c(1L,3L,5L),read_transpose = T)
  expect_equal(rmat,t(sub_t))  
  write_mat_h5(tempf,groupname = "testg",dataname = "testd2",data = tmat,doTranspose = T)
  rmat <- read_mat_rows_h5(tempf,"testg","testd2",c(1L,3L,5L),read_transpose=T)
  expect_equal(rmat,t(sub_t))
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

test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd",dimensions = c(9L,3L),doTranspose = F)
  sub_mat <- tmat[3:5,2:3]
  write_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  r_sub_mat <- read_mat_h5(tempf,"testg","testd",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})



test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",dimensions = c(9L,3L),doTranspose = F)
  sub_mat <- tmat[3:5,2:3]
  write_mat_chunk_h5(filename = tempf,groupname = "/",dataname = "testd",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  r_sub_mat <- read_mat_h5(tempf,"/","testd",c(2L,1L),c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})
test_that("writing 2 matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd0",dimensions = c(9L,3L),doTranspose = T)
  write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd0",data = tmat,offsets = c(0L,0L),chunksizes = c(9L,3L))
  r_sub_mat <- read_mat_h5(tempf,"testg","testd0",c(0L,0L),c(9L,3L))
  expect_equal(tmat,r_sub_mat)
  
  write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(1L,2L),chunksizes = c(9L,2L))
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




test_that("ld_region splitting works as",{
  
  
  num_ld_r <- 20
  region_id <- integer()
  result_matrix <- matrix(0,num_ld_r,3)
  for(i in 1:num_ld_r){
    result_matrix[i,1] <- i
    result_matrix[i,2] <- length(region_id)
    isize <- sample(1:1000,1)
    result_matrix[i,3] <- isize
    region_id <- c(region_id,rep(i,isize))
  }
  c_rm <- split_ldd(region_id)
  expect_equal(c_rm,result_matrix)
  
  
  
  
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
