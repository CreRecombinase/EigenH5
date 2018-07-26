context("Attributes")

test_that("we can create attributes on datasets",{
  
  
  tf <- tempfile()
  tvec <- runif(3)
  write_vector_h5(tf,"/","test",tvec)
  attvec <- 1:3
  write_attribute_h5(tf,"/test/attr",attvec)  
  rattr <- read_attribute_h5(tf, "/test/attr")
  expect_equal(rattr,attvec)
})

test_that("we can create attributes on root group",{
  
  tf <- tempfile()
  tvec <- runif(3)
  write_vector_h5(tf,"/","test",tvec)
  attvec <- 1:3
  write_attribute_h5(tf,"/attr",attvec)  
  rattr <- read_attribute_h5(tf, "/attr")
  expect_equal(rattr,attvec)
})