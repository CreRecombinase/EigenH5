context("dataframes")


test_that("we can write an empty tibble",{
  
  td <- tibble::tibble(a=1:5,b=letters[1:5],c=runif(5))
  ntd <- dplyr::slice(td,integer())
  tf <- tempfile()
  expect_equal(file_acc_ct(tf)$count,rep(0L,5))
  write_df_h5(ntd,tf,"test",chunksizes=9L,max_dims=NA_integer_)
  expect_equal(file_acc_ct(tf)$count,rep(0L,5))
  dim_h5(tf,"test/a")
  expect_equal(file_acc_ct(tf)$count,rep(0L,5))
  expect_equal(c(dim_h5(tf,"test/a"),dim_h5(tf,"test/b"),dim_h5(tf,"test/c")),c(0L,0L,0L))
  expect_equal(file_acc_ct(tf)$count,rep(0L,5))
  write_df_h5(td,tf,"test",append=T)
  expect_equal(file_acc_ct(tf)$count,rep(0L,5))
  srtd <- read_df_h5(tf,"test")
  expect_equal(file_acc_ct(tf)$count,rep(0L,5))
  expect_equal(td,srtd)
})


test_that("we can start with appending",{
  
  td <- tibble::tibble(a=1:5,b=letters[1:5],c=runif(5))
  tf <- tempfile()
  write_df_h5(td,tf,"test",append=TRUE)
  write_df_h5(td,tf,"test",append=TRUE)
  srtd <- read_df_h5(tf,"test")
  expect_equal(dplyr::bind_rows(td,td),srtd)
})


test_that("We can read and write dataframes correctly",{
  
  td <- tibble::tibble(a=1:5,b=paste0(letters[1:5],letters[5:1]),c=runif(5))
  std <- dplyr::slice(td,3:5)
  tf <- tempfile()
  write_df_h5(td,tf,"test",filter="none")
  rtd <- read_df_h5(tf,"test")
  expect_equal(td,rtd)
  expect_equal(ls_h5(tf),"test")
  dn <- ls_h5(tf,"test")
  expect_equal(dn,c("a","b","c"))
  srtd <- read_df_h5(tf,"test",subset=3:5)
  
  expect_equal(std,srtd)
})


test_that("We can read and write dataframe subcols correctly",{
  
  
  td <- tibble::tibble(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,3:5)
  tf <- tempfile()
  write_df_h5(td,tf,"test")
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

  td <- tibble::tibble(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,c(1,3,5))
  tf <- tempfile()
  write_df_h5(td,tf,"test")
  rtd <- read_df_h5(tf,"test")
  srtd <- read_df_h5(tf,"test",subset = c(1,3,5))
  nstrd <- read_df_h5(tf,"test",subset=c(1,3,5))
  expect_equal(td,rtd)
  expect_equal(std,srtd)
  expect_equal(srtd,nstrd)
})


test_that("We can convert mtcars to hdf5",{
  tf <- tempfile()
  mtcf <- readr::readr_example("mtcars.csv")
  x <- delim2h5(input_file =mtcf ,output_file = tf,delim=",",h5_args=list(datapath="mtcars"))
  check_df <- readr::read_delim(mtcf,delim=",")
  tdf <- read_df_h5(tf,datapath = "mtcars")
  expect_equal(check_df,tdf)
})

