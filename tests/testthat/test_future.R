# context("reading the future")
# 
# test_that("we can read a matrix in 2 chunks using futures",{
#   
#   tmat <- matrix(sample(1:900),100,9)
#   tempf <- tempfile()
#   write_matrix_h5(tempf,"grp","tmat",tmat)
# 
#   indl <- BBmisc::chunk(1:100,n.chunks = 9)
#   names(indl) <- letters[1:9]
#   matl <- purrr::map(indl,~tmat[.x,])
#   rl <- read_mat_row_futures(tempf,"grp/tmat",indl)
#   expect_equal(names(rl),names(indl))
#   for(i in 1:length(indl)){
#     expect_equal(future::value(rl[[i]]),matl[[i]])
#   }
#   indl <- BBmisc::chunk(1:9,n.chunks = 2)
#   cl <- read_mat_col_futures(tempf,"grp/tmat",indl)
#   cmatl <- map(indl,~tmat[,.x])
# })
#   
# test_that("we can read a df in 2 chunks using futures",{
#   
#   td <- tibble::data_frame(a=1:300,b=rep(letters[1:5],60),c=runif(300))
#   
#   tempf <- tempfile()
#   write_df_h5(td,"grp",tempf)
#   
#   indl <- BBmisc::chunk(1:300,n.chunks = 9)
#   matl <- purrr::map(indl,~dplyr::slice(td,.x))
#   rl <- read_df_futures(tempf,"grp",filtervec = indl)
#   for(i in 1:length(indl)){
#     expect_equal(future::value(rl[[i]]),matl[[i]])
#   }
# })
#   