context("dataframes")



test_that("We can read and write dataframes correctly",{
  
  
  td <- tibble::data_frame(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,3:5)
  tf <- tempfile()
  write_df_h5(td,"test",tf)
  rtd <- read_df_h5(tf,"test")
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



test_that("finding the intersection of 3 SNPinfo dfs",{
  library(tidyverse)
  p <- 10
  master_snpi <- data_frame(chr=sample(1:3,p,replace=T),pos=sample(1:1000,p,replace=F)) %>% arrange(chr,pos) %>% mutate(snp_id=1:n())
  
  slice_a <- c(2,4,6,8)
  slice_b <- c(1,2,4,5,7)
  slice_c <- c(2,4,8)
  
  tempfa <- tempfile()
  tempfb <- tempfile()
  tempfc <- tempfile()
  
  tdf_a <- slice(master_snpi,slice_a) %>% mutate(snp_id=1:n())
  tdf_b <- slice(master_snpi,slice_b) %>% mutate(snp_id=1:n())
  tdf_c <- slice(master_snpi,slice_c) %>% mutate(snp_id=1:n())
  
  write_df_h5(tdf_a,"SNPinfo",tempfa)
  write_df_h5(tdf_b,"SNPinfo",tempfb)
  write_df_h5(tdf_c,"SNPinfo",tempfc)
  
  resl <- intersect_snpinfo_h5(c(tempfa,tempfb,tempfc))
  
  
  
  
})


