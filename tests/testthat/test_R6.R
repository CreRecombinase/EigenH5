context("R6")

# test_that("can create a file",{
# 
#   fn <- fs::file_temp()
#   create_file_h5(fn)
#   expect_equal(file_acc_ct(fn)$count[1],0)
#   mf <- H5File$new(filename = fn)
#   expect_equal(file_acc_ct(fn)$count[1],1)
#   rm(mf)
#   gc()
#   expect_equal(file_acc_ct(fn)$count[1],0)
#   fs::file_exists(fn)
# })

# 
# test_that("can access a dataset",{
#   
#   mtf <- fs::file_temp()
#   mv <- runif(30)  
#   write_vector_h5(mv,mtf,"t3/t2")  
#   mf <- H5File$new(filename = mtf)  
#   md <- get_file_object(mf$file_id,"t3")
#   mo <- get_file_object(mf$file_id,"t3/t2")
#   file_acc_ct(mtf)
# })
#   