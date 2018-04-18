context("reading and writing lists")


# 
# test_that("we can read and write a simple list with one element",{
#   
#   tempf <- tempfile()
#   my_list <- list(a=1:10)
#   write_l_h5(tempf,"/",my_list)
#   rl <- read_l_h5(tempf,"/")  
#   expect_equal(rl,my_list)
# })
# 
# test_that("we can read and write a simple list with two element",{
#   
#   tempf <- tempfile()
#   my_list <- list(a=1:10,b=matrix(1:10,5,2))
#   write_l_h5(tempf,"sub",my_list)
#   rl <- read_l_h5(tempf,"sub")  
#   expect_equal(rl,my_list)
# })
# 
# test_that("we can read and write a nested list",{
#   
#   tempf <- tempfile()
#   my_list <- list(a=1:10,b=matrix(1:10,5,2),c=list(d=1:10,e=letters))
#   write_l_h5(tempf,"/",my_list)
#   rl <- read_l_h5(tempf,"/")  
#   expect_equal(rl,my_list)
# })

# test_that("we can read and write a nested list within a group",{
#   
#   tempf <- tempfile()
#   my_list <- list(a=1:10,b=matrix(1:10,5,2),c=list(d=1:10,e=letters))
#   # names(my_list) <- 
#   
#   write_l_h5(tempf,"/subgroup",my_list)
#   rl <- read_l_h5(tempf,"/subgroup")  
#   expect_equal(rl,my_list)
# })



# test_that("can read int matrix rows",{
#   tmat <- matrix(sample(1:900),100,9)
#   tempf <- tempfile()
#   write_matrix_h5(tempf,"grp/grp2","tmat",tmat)
#   ind <- c(1,3,5)
#   ttmat <- tmat[ind,]
#   trd <- read_object_h5(tempf,"grp/grp2","tmat",subset_rows = ind)
#   expect_equal(ttmat,trd)
#   rd <- read_matrix_h5(tempf,"grp/grp2","tmat",subset_rows = ind)
#   # nttmat <- tmat[,ind]
#   # rd <- read_matrix_h5(tempf,"grp/grp2","tmat",subset_cols = ind)
#   # 
#   # # expect_equal(ind,c(1,3,5))
#   # # expect_equal(ttmat,rd)
#   # ind <- c(1,3,2,100,4)
#   # ttmat <- tmat[ind,]
#   # rd <- read_matrix(tempf,"grp/grp2/tmat",subset_rows = ind)
#   # # expect_equal(ind,c(1,2,3,2,100,4))
#   # expect_equal(ttmat,rd)
# })