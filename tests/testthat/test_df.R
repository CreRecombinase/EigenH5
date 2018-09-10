context("dataframes")


test_that("we can write an empty data_frame",{
  
  td <- tibble::data_frame(a=1:5,b=letters[1:5],c=runif(5))
  ntd <- dplyr::slice(td,integer())
  tf <- tempfile()
  write_df_h5(ntd,"test",tf,chunksizes=9L,max_dims=NA_integer_)
  expect_equal(c(dim_h5(tf,"test/a"),dim_h5(tf,"test/b"),dim_h5(tf,"test/c")),c(0L,0L,0L))
  write_df_h5(td,"test",tf,append=T)
  srtd <- read_df_h5(tf,"test")
  expect_equal(td,srtd)
})


test_that("We can read and write dataframes correctly",{
  
  td <- tibble::data_frame(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,3:5)
  tf <- tempfile()
  write_df_h5(td,"test",tf)
  rtd <- read_df_h5(tf,"test")
  expect_equal(ls_h5(tf),"test")
  dn <- ls_h5(tf,"test")
  expect_equal(dn,c("a","b","c"))
  srtd <- read_df_h5(tf,"test",offset=2,chunksize = 3)
  expect_equal(td,rtd)
  expect_equal(std,srtd)
})


test_that("We can read and write dataframe subcols correctly",{
  
  
  td <- tibble::data_frame(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,3:5)
  tf <- tempfile()
  write_df_h5(td,"test",tf)
  rtd <- read_df_h5(tf,"test",subcols=c("a","c"))
  expect_equal(dplyr::select(td,a,c),rtd)
  # expect_equal(ls_h5(tf),"test")
  # dn <- ls_h5(tf,"test")
  # expect_equal(dn,c("a","b","c"))
  # srtd <- read_df_h5(tf,"test",offset=2,chunksize = 3)
  # expect_equal(td,rtd)
  # expect_equal(std,srtd)
})


test_that("We can read and dataframe slices correctly",{
  
  
  td <- tibble::data_frame(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,c(1,3,5))
  tf <- tempfile()
  write_df_h5(td,tf,"test")
  rtd <- read_df_h5(tf,"test")
  srtd <- read_df_h5(tf,"test",filtervec = c(1,3,5))
  nstrd <- read_df_h5(tf,"test",subset=c(1,3,5))
  expect_equal(td,rtd)
  expect_equal(std,srtd)
  expect_equal(srtd,nstrd)
})


