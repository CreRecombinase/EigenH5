context("compression")

test_that("datasets are zstd by default",{
  tvec <- runif(3)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"zstd")
})

# 
# test_that("can write without compression",{
#   tvec <- as.integer(1:10)
#   tempf <- tempfile()
#   write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="none",chunksizes=9L)
#   expect_equal(dataset_chunks(tempf,"grp/grp2/dat"),9L)
#   
#   h5f = rhdf5::H5Fopen(tempf)
#   
#   test_d <- rhdf5::h5read(tempf,"grp/grp2/dat")
#   expect_equal(test_d,tvec)
#   
#   retl <- get_datset_filter(tempf,"grp/grp2/dat")
#   expect_equal(retl$name,"no_filter")
# })
# 
# test_that("can write without compression and without chunking",{
#   tvec <- as.integer(1:10)
#   tempf <- tempfile()
#   write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="none",chunksize=integer())
#   expect_equal(dataset_chunks(tempf,"grp/grp2/dat"),integer())
#   retl <- get_datset_filter(tempf,"grp/grp2/dat")
#   expect_equal(retl$name,"no_filter")
# })

test_that("can write a matrix without compression and without chunking",{
  tmat <- matrix(sample(1:00),10,10)
  tempf <- tempfile()
  write_matrix_h5(tmat,tempf,"grp/grp2/mdat", filter = "none",chunksizes = integer())
  expect_equal(dataset_chunks(tempf,"grp/grp2/mdat"),integer()) 
  retl <- get_datset_filter(tempf,"grp/grp2/mdat")
  expect_equal(retl$name,"no_filter")
  expect_equal(retl$options,numeric())
})


# 
# test_that("can read raw chunks of data without compression",{
#   tvec <- as.integer(1:10)
#   tempf <- tempfile()
#   
#   write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="none",chunksizes=10L)
#   
#   trv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = 0L)
#   expect_identical(tvec,readBin(trv,what=integer(),n=length(tvec)))
#   
#   tvec <- as.numeric(1:10)+.Machine$double.eps*10
#   write_vector_h5(tvec,tempf,"grp/grp2/dat2",filter="none",chunksizes=10L)
#   trv <- read_raw_chunk(tempf,"grp/grp2/dat2",chunk_idx = 0L)
#   expect_identical(tvec,readBin(trv,what=numeric(),n=length(tvec)))
#   
#   
#   })

# 
# test_that("can read whole dataset with zstd compression",{
#   tvec <- as.integer(1:10)
#   tempf <- tempfile()
#   
#   write_vector_h5(tvec,tempf,"grp/grp2/dat",chunksizes=10L)
#   
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = 0L)
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   expect_identical(tvec,readBin(trv,what=integer(),n=length(tvec)))
#   
#   tvec <- as.numeric(1:10)+.Machine$double.eps*10
#   write_vector_h5(tvec,tempf,"grp/grp2/dat2",chunksizes=10L)
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat2",chunk_idx = 0L)
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   expect_identical(tvec,readBin(trv,what=numeric(),n=length(tvec)))
#   
#   
# })


# 
# test_that("can read raw chunks of data with zstd compression",{
#   tvec <- as.integer(1:10000)
#   tempf <- tempfile()
#   
#   write_vector_h5(tvec,tempf,"grp/grp2/dat",chunksizes=100L)
#   
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = 0L)
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   expect_identical(tvec[1:100L],readBin(trv,what=integer(),n=100L))
#   
#   
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = 1L)
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   expect_identical(tvec[101L:200L],readBin(trv,what=integer(),n=100L))
#   
#   
#   tvec <- as.numeric(1:10000)+.Machine$double.eps*10
#   write_vector_h5(tvec,tempf,"grp/grp2/dat2",chunksizes=100L)
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat2",chunk_idx = 0L)
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   expect_identical(tvec[1:100L],readBin(trv,what=numeric(),n=100L))
#   
#   
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat2",chunk_idx = 1L)
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   expect_identical(tvec[101L:200L],readBin(trv,what=numeric(),n=100L))
# })
# 
# test_that("can read chunks when data is uncompressable",{
#   tvec <- sample(-.Machine$integer.max:.Machine$integer.max,10000)
#   tempf <- tempfile()
#   
#   write_vector_h5(tvec,tempf,"grp/grp2/dat",chunksizes=100L)
#   
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = 0L)
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_lt(length(trv),length(crv))
#   expect_identical(tvec[1:100L],readBin(trv,what=integer(),n=100L))
# })


# 
# test_that("can read raw chunks from matrices",{
#   
#   tmat <- matrix(as.integer(1:1000),100L,10L)
#   tempf <- tempfile()
#   
#   write_matrix_h5(tmat,tempf,"grp/grp2/dat",chunksizes=c(10L,5L))
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = c(0L,0L))
#   
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   ttmat <- matrix(readBin(trv,what=integer(),n=50L),10L,5L,byrow = T)
#   expect_identical(tmat[1:10,1:5],ttmat)
# 
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = c(0L,1L))
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   ttmat <- matrix(readBin(trv,what=integer(),n=50L),10L,5L,byrow = T)
#   expect_identical(tmat[1:10,6:10],ttmat)
#   
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = c(1L,0L))
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   ttmat <- matrix(readBin(trv,what=integer(),n=50L),10L,5L,byrow = T)
#   expect_identical(tmat[11:20,1:5],ttmat)
#   
#   crv <- read_raw_chunk(tempf,"grp/grp2/dat",chunk_idx = c(1L,1L))
#   trv <- rzstdlib::zstdDecompressImpl(crv)
#   expect_gt(length(trv),length(crv))
#   ttmat <- matrix(readBin(trv,what=integer(),n=50L),10L,5L,byrow = T)
#   expect_identical(tmat[11:20,6:10],ttmat)
#   
# })
# 



test_that("can compress with gzip",{
  data(iris)
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="gzip")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"deflate")
  
})


test_that("can compress dataframe with gzip",{
  data(iris)
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_df_h5(iris,filename = tempf,datapath = "iris",filter="gzip")
  retl <- get_datset_filter(tempf,"iris/Species")
  expect_equal(retl$name,"deflate")
  
})

test_that("can read a matrix compressed with gzip (using chunk iterator)",{
  tmat <- matrix(as.integer(1:100),1000,100)
  tempf <- tempfile()
  write_matrix_h5(tmat,tempf,"grp/grp2/dat",filter="gzip")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"deflate")
  rm <- read_matrix_h5v(tempf,"grp/grp2/dat")
  expect_equal(rm,tmat)
})





test_that("can compress with gzip level 9",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="gzip",filter_options=9L)
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$options,9L)
})
# 
# test_that("can compress with lzf",{
#   tvec <- as.integer(1:10)
#   tempf <- tempfile()
#   write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="lzf")
#   retl <- get_datset_filter(tempf,"grp/grp2/dat")
#   expect_equal(retl$name,"lzf")
#   expect_equal(retl$options,numeric())
# })
# 
# test_that("can compress with lzf and read with chunk iterator",{
#   # library(EigenH5)
#   
#   tmat <- matrix(as.integer(1:100),1000,100)
#   tempf <- tempfile()
#   write_matrix_h5(tmat,tempf,"grp/grp2/dat",filter="lzf")
#   retl <- get_datset_filter(tempf,"grp/grp2/dat")
#   expect_equal(retl$name,"lzf")
#   rm <- read_matrix_h5v(tempf,"grp/grp2/dat")
#   expect_equal(rm,tmat)
# })

test_that("can compress with zstd",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="zstd")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"zstd")
})

test_that("can compress with nothing",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="none")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"no_filter")
})


test_that("can compress with zstd with high compression level",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="zstd",filter_options=19L)
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$options,19L)
})


test_that("Higher compression leads to smaller file size",{
  tvec <- unlist(purrr::rerun(1900,sample(as.numeric(1:50))))
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="zstd",filter_options=1L)
  file.size(tempf)
  tempf2 <- tempfile()
  write_vector_h5(tvec,tempf2,"grp/grp2/dat",filter="zstd",filter_options=22L)
  file.size(tempf2)
  expect_gte(file.size(tempf),file.size(tempf2))
})



