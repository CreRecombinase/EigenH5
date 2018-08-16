context("metadata/utilities")

test_that("Can get back chunksize",{
  
  tv <- sample(1:100,10)
  tm <- matrix(1:9,3,3)
  tf <- tempfile()
  write_vector_h5(tf,"/","ntest",tv,chunksize=2)
  write_vector_h5(tf,"/","test",tv)
  write_matrix_h5(tf,"/","testm",tm)
  ltm <- matrix(42,3000,3000)
  write_matrix_h5(tf,"/","ltestm",ltm)
  dataset_chunks(tf,"ltestm")
  
  #ltv <- rep(42,
  
  expect_equal(dataset_chunks(tf,"testm"),c(3,3))
  #Verified using h5ls -rv
  expect_equal(dataset_chunks(tf,"test"),10)
  ltv <- rep(42,9000000)
  write_vector_h5(tf,"/","test2",ltv)
  expect_equal(dataset_chunks(tf,"test2"),562500)
  
})


# test_that("I can concatenate sparse matrices",{
#   
#   mm <- Matrix::Matrix(toeplitz(c(10, 0, 1, 0, 3)), sparse = TRUE)
#   mymm <- attributes(mm)
#   newM <- purrr::lift(purrr::partial(new,Class="dsCMatrix"))
#   spmat <- mymm[names(mymm)!="class"] %>%  newM
#   all(spmat==mm)
#   identical(spmat,mm)
# })
# 



test_that("I can create an extendable dataset and extend it",{
  
    tf <- tempfile()
    create_dataset_h5(filename = tf,datapath = "/test",data=numeric(),list(dims=c(3L,4L),max_dims=c(NA_integer_,NA_integer_)))
    expect_equal(get_dims_h5(tf,"/test"),c(3L,4L))
    extend_dataset(tf,"/test",c(4L,5L))
    expect_equal(get_dims_h5(tf,"/test"),c(4L,5L))
    
    tf <- tempfile()
    create_dataset_h5(filename = tf,datapath = "/test",data=numeric(),list(dims=c(3L,4L),max_dims=c(NA_integer_,NA_integer_)))
    extend_dataset_by(tf,"/test",c(2L,2L))
    expect_equal(get_dims_h5(tf,"/test"),c(5L,6L))
    
  
})


test_that("We can write a vector",{
  
  EigenH5::write_vector_h5(numeric(3),tempfile(),"test")
  
})

test_that("root group is created upon file creation",{
  
  tf <- tempfile()
  create_file_h5(tf)  
  ls_h5(tf)
  isGroup(tf,"/")
  
})
