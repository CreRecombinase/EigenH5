context("compression")

test_that("datasets are zstd by default",{
  tvec <- runif(3)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"zstd")
})

test_that("can compress with blosc",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="blosc")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"blosc")
})

test_that("can write without compression",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="none",chunksizes=9L)
  expect_equal(dataset_chunks(tempf,"grp/grp2/dat"),9L)
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"no_filter")
})

test_that("can write without compression and without chunking",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="none",chunksize=integer())
  expect_equal(dataset_chunks(tempf,"grp/grp2/dat"),integer())
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"no_filter")
})

test_that("can write a matrix without compression and without chunking",{
  tmat <- matrix(sample(1:00),10,10)
  tempf <- tempfile()
  write_matrix_h5(tmat,tempf,"grp/grp2/mdat",filter="none",chunksizes=integer())
  expect_equal(dataset_chunks(tempf,"grp/grp2/mdat"),integer())
  retl <- get_datset_filter(tempf,"grp/grp2/mdat")
  expect_equal(retl$name,"no_filter")
  expect_equal(retl$options,numeric())
})





test_that("can compress with gzip",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="gzip")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"deflate")
})


test_that("can compress with gzip level 9",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="gzip",filter_options=9L)
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$options,9L)
})

test_that("can compress with lzf",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="lzf")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"lzf")
  expect_equal(retl$options,numeric())
})

test_that("can compress with zstd",{
  tvec <- as.integer(1:10)
  tempf <- tempfile()
  write_vector_h5(tvec,tempf,"grp/grp2/dat",filter="zstd")
  retl <- get_datset_filter(tempf,"grp/grp2/dat")
  expect_equal(retl$name,"zstd")
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



