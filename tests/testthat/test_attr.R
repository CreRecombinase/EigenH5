context("Attributes")

test_that("we can create attributes on datasets",{
  tf <- tempfile()
  tvec <- runif(3)
  write_vector_h5(tvec,tf,"test")
  attvec <- 1:3
  write_attribute_h5(attvec,tf,"test/attr")  
  rattr <- read_attribute_h5(tf, "/test/attr")
  expect_equal(rattr,attvec)
})

test_that("we can create attributes on root group",{
  
  tf <- tempfile()
  tvec <- runif(3)
  write_vector_h5(tvec,tf,"test")
  attvec <- 1:3
  write_attribute_h5(attvec,tf,"/attr")
  rattr <- read_attribute_h5(tf, "/attr")
  expect_equal(rattr,attvec)
})
