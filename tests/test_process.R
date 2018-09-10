context("concurrency")


test_that("we can open a file twice (for reading, from the same process)",{
  
  
  p_a <- 3
  tdf <- tibble::data_frame(a=runif(p_a),b=sample(1:p_a,p_a))
  #tdfb <- tibble::data_frame(a=runif(p_a),b=sample(1:p_a,p_a))
  tf <- tempfile()
  tf2 <- tempfile()
  write_df_h5(tdf,tf,"test")
  write_df_h5(tdf,tf2,"test")
  
  ret_id <- openFileHandleRead(tf)
  file_acc <- file_acc_ct(tf)
  file_acc_2 <- file_acc_ct(tf2)
  expect_equal(file_acc$count[1],1)
  expect_equal(file_acc_2$count[1],0)
  ret_id2 <- openFileHandleRead(tf)
  file_acc <- file_acc_ct(tf)
  expect_equal(file_acc$count[1],2)
  closeFileHandle(ret_id)
  file_acc <- file_acc_ct(tf)
  expect_equal(file_acc$count[1],1)
  closeFileHandle(ret_id2)
  file_acc <- file_acc_ct(tf)
  expect_equal(file_acc$count[1],0)
})

test_that("we can open a file twice (for reading, from another process)",{
  
  
  p_a <- 3
  tdf <- tibble::data_frame(a=runif(p_a),b=sample(1:p_a,p_a))
  #tdfb <- tibble::data_frame(a=runif(p_a),b=sample(1:p_a,p_a))
  tf <- tempfile()
  tf2 <- tempfile()
  write_df_h5(tdf,tf,"test")
  write_df_h5(tdf,tf2,"test")
  library(future)
  plan(multiprocess)
  
  f_ret_id <- future(openFileHandleRead(tf),packages = "EigenH5",lazy=T)
  f_ret_id2 <- future(openFileHandleRead(tf),packages = "EigenH5",lazy=T)
  file_acc <- file_acc_ct(tf)
  expect_equal(file_acc$count[1],0)
  ret_id3 <- openFileHandleRead(tf)
  file_acc <- file_acc_ct(tf)
  expect_equal(file_acc$count[1],1)
  ret_id4 <- openFileHandleRead(tf)
  mv <- value(f_ret_id)
  file_acc <- file_acc_ct(tf)
  expect_equal(file_acc$count[1],2)
  f_file_acc <- value(future({
    cat(value(f_ret_id),"\n")
    file_acc_ct(tf)},packages="EigenH5"))
  
})

