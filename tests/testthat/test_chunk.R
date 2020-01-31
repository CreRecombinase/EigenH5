context("chunk-based API")



test_that("can read a vector chunk", {
  tvec <- as.integer(1:5)
  tempf <- tempfile()
  write_vector_h5(data = tvec,filename = tempf,"grp/dat",filter="none")
  #write_vector_h5v(filename = tempf, datapath = "grp/dat",filter="none",data= tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",1:2)
  expect_equal(trd, tvec[1:2],check.attributes=F)
  

  tvec <- as.integer(1:5)
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",data =  tvec)
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",2:5)
  expect_equal(trd, tvec[2:5],check.attributes=F)
})




test_that("can read a (string) vector chunk", {
  
  fruit <- c("apple", "apricot", "avocado", "banana", "bell pepper", "bilberry", 
             "blackberry", "blackcurrant", "blood orange", "blueberry", "boysenberry", 
             "breadfruit", "canary melon", "cantaloupe", "cherimoya", "cherry", 
             "chili pepper", "clementine", "cloudberry", "coconut", "cranberry", 
             "cucumber", "currant", "damson", "date", "dragonfruit", "durian", 
             "eggplant", "elderberry", "feijoa", "fig", "goji berry", "gooseberry", 
             "grape", "grapefruit", "guava", "honeydew", "huckleberry", "jackfruit", 
             "jambul", "jujube", "kiwi fruit", "kumquat", "lemon", "lime", 
             "loquat", "lychee", "mandarine", "mango", "mulberry", "nectarine", 
             "nut", "olive", "orange", "pamelo", "papaya", "passionfruit", 
             "peach", "pear", "persimmon", "physalis", "pineapple", "plum", 
             "pomegranate", "pomelo", "purple mangosteen", "quince", "raisin", 
             "rambutan", "raspberry", "redcurrant", "rock melon", "salal berry", 
             "satsuma", "star fruit", "strawberry", "tamarillo", "tangerine", 
             "ugli fruit", "watermelon")
  library(EigenH5)
  library(testthat)
  tvec <- as.character(c(1,2,10,4))
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",filter="none",data= tvec)
  read_vector_h5(tempf,"grp/dat")
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",1:2)
  expect_equal(trd, tvec[1:2],check.attributes=F)
  
  
  tvec <- fruit
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",data =  tvec)
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",2:5)
  expect_equal(trd, tvec[2:5],check.attributes=F)
})




test_that("can read a vector index", {
  
  tvec <- as.integer(1:100000)
  iv <- sort(sample(1:length(tvec),3,replace=F))
  tempf <- tempfile()
  write_vector_h5v(filename = tempf, datapath = "grp/dat",filter="none",data =  tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",iv)
  expect_equal(trd, tvec[iv])
  
  
  riv <- sample(1:length(tvec),5000,replace=T)
  
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",riv)
  expect_equal(trd, tvec[riv])
  
})


test_that("can read a vector index out of order", {
  
  tvec <- as.integer(1:100)
  iv <- sort(sample(1:length(tvec),300,replace=T))
  tempf <- tempfile()
  write_vector_h5(filename = tempf, datapath = "grp/dat",filter="none", tvec)
  #rd <- read_vector_h5(tempf,datapath="grp/dat")
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",iv)
  expect_equal(c(trd), tvec[iv],check.attributes=F)
  
  riv <- sample(1:length(tvec),5000,replace=T)
  
  trd <- read_vector_h5v(filename = tempf, datapath="grp/dat",riv)
  expect_equal(trd, tvec[riv],check.attributes=F)
  
})


test_that("can read a matrix rowchunk",{
  
  tmat <- matrix(1:180,60,3)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(3L,3L),filter="none")  
  
  check_mat <- read_matrix_h5v(tf,"test",1:6,1:3)
  expect_equal(tmat[1:6,1:3],check_mat)
  check_imat <- read_matrix_h5v(tf,"test",c(1:6),c(1:3))
  expect_equal(check_imat,tmat[c(1:6),c(1:3)])  
})


test_that("can read a matrix rowchunk to a list",{
  
  tmat <- matrix(1:180,60,3)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(3L,3L),filter="none")  
  
  check_mat <- read_matrix_rl(tf,"test",1:6,1:3)
   
  expect_equal(check_mat,list(tmat[1:3,1:3],tmat[4:6,1:3]))
  check_mat <- read_matrix_rl(tf,"test",1:60,c(1L,3L))
  tmat_l <- purrr::map(split(1:60,gl(n = 20,k = 3,length = 60)),~tmat[.x,c(1,3)])
  names(tmat_l) <- NULL
  # check_imat <- read_matrix_h5v(tf,"test",c(1:6),c(1:3))
  expect_equal(tmat_l,check_mat)  
})




test_that("can read a (string) matrix rowchunk",{
  gen_rstrings <- function(n,max_len=300){
    replicate(n,paste0(sample(letters,size=sample(1:max_len,1),replace=T),collapse = ""))
    
  }

  tmat <- matrix(as.character(1:(6*3)),6,3)
  tf <- tempfile()
  write_matrix_h5v(tmat,tf,"test",chunksizes=c(1L,3L),filter="none")  
  r_matrix <- read_matrix_h5v(tf,"test")
  
  check_mat <- read_matrix_h5v(tf,"test",1:6,1:3)
  expect_equal(tmat[1:6,1:3],check_mat)
  check_imat <- read_matrix_h5v(tf,"test",c(1:6),c(1:3))
  expect_equal(check_imat,tmat[c(1:6),c(1:3)])  
})


test_that("can read an off-center matrix rowchunk",{
  
  
  tmat <- matrix(1:18,6,3)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(3L,3L))  
  
  check_mat <- read_matrix_h5v(tf,"test",2:6,2:3)
  expect_equal(tmat[2:6,2:3],check_mat)
  check_imat <- read_matrix_h5v(tf,"test",c(2:6),c(2:3))
  expect_equal(check_imat,tmat[2:6,2:3])  
})




test_that("can read an off-center matrix rowchunk smaller than disk chunk",{
  
  
  tmat <- matrix(1:18,6,3)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(3L,3L))  
  
  check_mat <- read_matrix_h5v(tf,"test",2:2,2:2)
  expect_equal(tmat[2:2,2:2,drop=F],check_mat)
  check_imat <- read_matrix_h5v(tf,"test",c(2L),c(2L))
  expect_equal(check_imat,tmat[2,2,drop=F])  
  
  tmat <- matrix(1:(60*30),60,30)
  tf <- tempfile()
  write_matrix_h5(tmat,tf,"test",chunksizes=c(30L,30L))  
  
  check_mat <- read_matrix_h5v(tf,"test",20:25,20:25)
  expect_equal(tmat[20:25,20:25,drop=F],check_mat)
  check_imat <- read_matrix_h5v(tf,"test",c(20:25),c(20:25))
  expect_equal(check_imat,tmat[20:25,20:25,drop=F])
  
})




test_that("can read a matrix that's actually chunked",{
  
  tmat <- matrix(1:2700,90,30)
  tf <- tempfile()
  write_matrix_h5v(tmat,tf,"test",chunksizes=c(10L,3L))  
  
  check_mat <- read_matrix_h5v(tf,"test")
  expect_equal(tmat,check_mat)
  
})




test_that("can write a matrix chunkwise",{

    tmat <- matrix(1:2700,90,30)
    # rownames(tmat) <- paste0("r",1:90)
    # colnames(tmat) <- paste0("c",1:30)
    # 
    tf <- tempfile()
    write_matrix_h5v(tmat,tf,"test",chunksizes=c(10L,3L))  
    
    trm <- read_matrix_h5v(tf,"test")
    expect_equal(trm,tmat)
    check_mat <- read_matrix_h5v(tf,"test",j=1:30)
    expect_equal(tmat,check_mat)
    
    
    check_mat <- read_matrix_h5v(tf,"test",j=1L:15L)
    expect_equal(tmat[,1:15],check_mat)

  
  
})



