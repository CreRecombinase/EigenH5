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
  crtd <- read_tibble_h5(tf,"test",list())
  expect_equal(td,rtd)
  expect_equal(rtd,crtd)
  
  
  expect_equal(ls_h5(tf),"test")
  dn <- ls_h5(tf,"test")
  expect_equal(dn,c("a","b","c"))
  srtd <- read_df_h5(tf,"test",subset=3:5)
  crtd <- read_tibble_h5(tf,"test",list(subset=as.integer(3:5)))
  
  expect_equal(std,srtd)
  expect_equal(srtd,crtd,ignore_row_order=FALSE)
})


test_that("We can read and write dataframe subcols correctly",{
  
  
  td <- tibble::tibble(a=1:5,b=letters[1:5],c=runif(5))
  std <- dplyr::slice(td,3:5)
  tf <- tempfile()
  write_df_h5(td,tf,"test")
  rtd <- read_df_h5(tf,"test",subcols=c("a","c"))
  expect_equal(dplyr::select(td,a,c),rtd)
  expect_equal(ls_h5(tf),"test")
  dn <- ls_h5(tf,"test")
   expect_equal(dn,c("a","b","c"))
   srtd <- read_df_h5(tf,"test",offset=2,chunksize = 3)
   tsrtd <- read_tibble_h5(tf,"test",list(offset=2,chunksize=3))
   expect_equal(std,srtd,ignore_row_order=FALSE)
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



test_that("chunked sorting workflow works like an on-disk grouped-sort",{

  p <- 10000
  levels_a <- 23
  sa <- integer()
  while(length(unique(sa))<levels_a){
    sa <- sort(sample(levels_a,p,replace=T))
  }
  sb <- sample(p,p,replace=F)
  sc <- runif(p)
  tf <- tempfile(fileext=".tsv.gz")
  df <- tibble::tibble(a=sa,b=sb,c=sc)
  rledf <- rle2offset(df$a)
  readr::write_tsv(df,tf)
  
  otf <- tempfile(fileext=".h5")
  delim2h5(tf,otf,h5_args=list(datapath="test"),delim="\t",col_types=readr::cols(
  a = readr::col_integer(),
  b = readr::col_integer(),
  c = readr::col_double()
))
  expect_equal(read_df_h5(otf,"test"),df)
  
  purrr::pwalk(rledf,function(value,offset,datasize){
    tdf <- read_df_h5(otf,"test",subset=seq(offset+1,length.out = datasize)) %>% dplyr::arrange(b)
    expect_equal(tdf$a,rep(value,datasize))
    write_df_h5(tdf,otf,datapath = "test",subset=seq(offset+1,length.out = datasize))
  })
  expect_equal(dplyr::arrange(df,a,b),read_df_h5(otf,"test"))
})



test_that("we can read from big dataframes",{
  
  tf <- tempfile()
    smc <- readr::cols(
  mpg = readr::col_double(),
  cyl = readr::col_double(),
  disp = readr::col_double(),
  hp = readr::col_double(),
  drat = readr::col_double(),
  wt = readr::col_double(),
  qsec = readr::col_double(),
  vs = readr::col_double(),
  am = readr::col_double(),
  gear = readr::col_double(),
  carb = readr::col_double()
)
  mtcf <- readr::readr_example("mtcars.csv")
  mtc_df <- readr::read_delim(mtcf,delim=",",col_types = smc)

  rx <- replicate(n=20,delim2h5(input_files =mtcf ,output_file = tf,delim=",",col_names=TRUE,col_types=smc,h5_args=list(datapath="mtcars",append=TRUE)))
  delim2h5(input_files =rep(mtcf,20),output_file = tf,delim=",",col_names=TRUE,col_types=readr::spec(mtc_df),h5_args=list(datapath="mtcars2",append=TRUE),id_col = "index")
  idxd <- read_vector_h5(tf,"mtcars2/index")  
  max_d <- nrow(mtc_df)*20
  expect_equal(idxd,1L:as.integer(max_d))
  
  fr_df <- dplyr::bind_rows(replicate(20,mtc_df,simplify=FALSE))
  
  gen_rslice <- function(offset,size){
    seq(offset+1,length.out = size)
  }
  
  r_rslice <- function(max_s){
    offset <- sample(max_s,size=1)
    size <- sample(max_s-offset,size=1)
    return(tibble::tibble(offset=offset,size=size))
  }
  offset_df <- dplyr::bind_rows(replicate(50,r_rslice(max_d),simplify = FALSE))
  
alt_read_df_h5 <- function(f,datapath,offset,datasize){
  inp_cols <- ls_h5(f,datapath,full_names = T)
  inpc <- inp_cols[1]
  tvec <- read_vector_v(f,inpc,offset:(offset+datasize))
  tvec <- read_vector_v(f,inpc,seq(offset+1L,length.out = datasize))
}

  for(i in seq_len(nrow(offset_df))){
    tdf <- read_df_h5(tf,"mtcars",offset=offset_df$offset[i],datasize=offset_df$size[i])

    grs <- gen_rslice(offset_df$offset[i],offset_df$size[i])
    ttdf <- read_df_h5(tf,"mtcars",subset=grs)
    expect_lt(max(grs),max_d)
    cdf <- dplyr::slice(fr_df,grs)
    expect_equal(ttdf,cdf,ignore_row_order=FALSE)
    stopifnot()
  }
  
})




test_that("We can convert mtcars to hdf5",{
  tf <- tempfile()
  mtcf <- readr::readr_example("mtcars.csv")
  x <- delim2h5(input_file =mtcf ,output_file = tf,delim=",",h5_args=list(datapath="mtcars"))
  check_df <- readr::read_delim(mtcf,delim=",")
  tdf <- read_df_h5(tf,datapath = "mtcars")
  testthat::expect_equal(check_df,tdf)
  delim2h5(input_file =mtcf ,output_file = tf,delim=",",h5_args=list(datapath="mtcars_id"),id_col = TRUE,chunk_size=5)
  tdf_id <- read_df_h5(tf,datapath = "mtcars_id")
  testthat::expect_equal(tdf_id$id_col,1:nrow(check_df))
  delim2h5(input_file =mtcf ,output_file = tf,delim=",",h5_args=list(datapath="mtcars_idn"),id_col = "IdenT",chunk_size=2)
  tdf_id2 <- read_df_h5(tf,datapath = "mtcars_idn")
  testthat::expect_equal(tdf_id2$IdenT,1:nrow(check_df))
})

