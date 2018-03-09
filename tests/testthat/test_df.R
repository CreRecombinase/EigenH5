context("dataframes")



test_that("We can read and write dataframes correctly",{
  
  
  td <- tibble::data_frame(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,3:5)
  tf <- tempfile()
  write_df_h5(td,"test",tf)
  rtd <- read_df_h5(tf,"test")
  
  expect_equal(get_objs_h5(tf),"test")
  dn <- get_objs_h5(tf,"test")
  expect_equal(dn,c("a","b","c"))
  srtd <- read_df_h5(tf,"test",offset=2,chunksize = 3)
  expect_equal(td,rtd)
  expect_equal(std,srtd)
})


test_that("We can read and dataframe slices correctly",{
  
  
  td <- tibble::data_frame(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,c(1,3,5))
  tf <- tempfile()
  write_df_h5(td,"test",tf)
  rtd <- read_df_h5(tf,"test")
  srtd <- read_df_h5(tf,"test",filtervec = c(1,3,5))
  expect_equal(td,rtd)
  expect_equal(std,srtd)
})





