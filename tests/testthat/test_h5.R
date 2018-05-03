context("h5")



test_that("check out singleton class",{
  ms <- get_singleton()

})


test_that("can write string vector",{
  library(testthat)
  library(EigenH5)
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  testthat::expect_true(EigenH5::write_vector_h5(tempf,datapath="grp/dat",data=tvec))
  file_acc_ct(tempf)
  expect_equal(typeof_h5(tempf,"grp"),"list")
  file_acc_ct(tempf)
  expect_equal(typeof_h5(tempf,"grp/dat"),"character")
  file_acc_ct(tempf)
  
  expect_equal(get_dims_h5(tempf,"grp/dat"),length(tvec))
  expect_equal(get_dims_h5(tempf,"grp/dat"),length(tvec))
  rd <- read_vector_h5(tempf,datapath="grp/dat")
  expect_equal(rd,tvec)
  trd <- read_vector_h5(tempf,datapath="grp/dat",chunksize=2)
  expect_equal(head(tvec,2),trd)  
  write_vector_h5(tempf,datapath="/grp/dat2",data=tvec)
  trd <- read_vector_h5(tempf,"grp","dat2")
  expect_equal(trd,tvec)
  tvec <- c("allb","allc","alld")
  
  write_vector_h5(tempf,"/grp2/grp3","dat2",data=tvec)
  
  
  trd <- read_vector_h5(tempf,"grp","dat",subset=2:3)
  
  expect_equal(tail(tvec,2),trd)  
  trd <- read_vector_h5(tempf,"grp","dat",subset=c(1,3))
  expect_equal(tvec[c(1,3)],trd) 

})

test_that("can check type of vectors",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  expect_equal(typeof_h5(tempf,"grp"),"list")
  expect_equal(typeof_h5(tempf,"grp/dat"),"character")
  
  tvec <- runif(3)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp/grp2","dat",tvec)
  
  
  tvec <- sample(1:10)
  tempf <- tempfile()
  write_vector_h5(tempf,"/","dat",tvec)
  expect_equal(typeof_h5(tempf,datapath = "dat"),"integer")
  expect_equal(get_dims_h5(tempf,datapath="dat"),10)
})

test_that("can write a vector subset",{
  tvec <- runif(3)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  trd <- read_vector_h5(tempf,"grp","dat",subset=2:3)
  expect_equal(trd,tvec[2:3])
  
})


# test_that("can guess chunks for compression",{
#   
#   tdim <- c(10,1024^3)
#   guess_chunks(tdim)  
# })


test_that("can write REAL vector",{
  tvec <- runif(3)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  expect_equal(dim_h5(tempf,"grp/dat"),length(tvec))
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
  trd <- read_vector_h5(tempf,"grp","dat",chunksize=2)
  expect_equal(head(tvec,2),trd)  
  trd <- read_vector_h5(tempf,"grp","dat",subset=2:3)
  expect_equal(tail(tvec,2),trd)  
  trd <- read_vector_h5(tempf,"grp","dat",subset=c(1,3))
  expect_equal(tvec[c(1,3)],trd) 
  
  
})

test_that("we can read subsets out of order",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,datapath="grp/dat",data=tvec)
  strd <- read_vector_h5(tempf,datapath="grp/dat")
  expect_equal(strd,tvec)
  trd <- read_vector_h5(tempf,datapath="grp/dat",subset=c(2,1))
  expect_equal(tvec[c(2,1)],trd) 
  trd <- read_vector_h5(tempf,datapath="grp/dat",subset=c(3,1))
  expect_equal(tvec[c(3,1)],trd) 
})


test_that("can read string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})

test_that("can create a vector and then write to it",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  create_vector_h5(tempf,"grp","dat",character(),dim=3L)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,c("","",""))
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})

test_that("can read int matrix",{
  library(EigenH5)
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"/","tmat",tmat)
  rd <- read_matrix_h5(tempf,"/","tmat")
  expect_equal(get_dims_h5(tempf,"/","tmat"),c(100,9))
  expect_equal(tmat,rd)
})


test_that("can read int matrix(one column)",{
  library(EigenH5)
  tmat <- matrix(sample(1:900),100,9)
  smat <- tmat[,3,drop=F]
  tempf <- tempfile()
  #write_matrix_h5(tempf,"grp","tmat_t",tmat,doTranspose = T)
  write_matrix_h5(tempf,"/","tmat",tmat)
  rd <- read_matrix_h5(tempf,"/","tmat",subset_cols=3)
  expect_equal(get_dims_h5(tempf,"/","tmat"),c(100,9))
  expect_equal(tmat,rd)
})

test_that("check for groups/datasets",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"/grp/grp2","tmat",tmat)
  expect_false(isDataSet(tempf,"grp/grp2/"))
  expect_true(isGroup(tempf,"grp"))
  expect_true(isGroup(tempf,"grp/grp2"))
  expect_true(isGroup(tempf,"grp/grp2/"))
  
  expect_true(isGroup(tempf,"/"))
  expect_false(isGroup(tempf,"grp/grp2/tmat"))
  expect_true(isDataSet(tempf,"grp/grp2/tmat"))
 
})

test_that("check for groups/datasets",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,datapath="grp/grp2/tmat",data=tmat)
  data <- ls_h5(tempf,"grp/grp2",full_names = T)
  expect_equal(dim_h5(tempf,data),dim(tmat))
  

})


test_that("can read int matrix rows",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp/grp2","tmat",tmat)
  ind <- c(1,3,5)
  ttmat <- tmat[ind,]
  rd <- read_matrix_h5(tempf,"grp/grp2","tmat",subset_rows = ind)
  nttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,"grp/grp2","tmat",subset_cols = ind)
  expect_equal(nttmat,rd)
  # expect_equal(ind,c(1,3,5))
  # expect_equal(ttmat,rd)
  ind <- c(1,3,2,100,4)
  ttmat <- tmat[ind,]
  rd <- read_matrix_h5(tempf,datapath="grp/grp2/tmat",subset_rows = ind)
  # expect_equal(ind,c(1,2,3,2,100,4))
  expect_equal(ttmat,rd)
})


test_that("can read int matrix cols",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ind <- c(3,1,5)
  ttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,datapath="grp/tmat",subset_cols = ind)
  expect_equal(ttmat,rd)
  ind <- c(1,2,7,3)
  ttmat <- tmat[,ind]
  rd <- read_matrix_h5(tempf,datapath="grp/tmat",subset_cols = ind)
  expect_equal(ttmat,rd)
})



test_that("can read int matrix rows & cols",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ttmat <- tmat[c(5,3,1),c(3,5,6)]
  rd <- read_matrix_h5(tempf,datapath="grp/tmat",
                       subset_rows = c(5,3,1),
                       subset_cols=c(3,5,6))
  expect_equal(ttmat,rd)
})



test_that("can read int matrix cols",{
  tmat <- matrix(sample(1:900),100,9)
  tempf <- tempfile()
  write_matrix_h5(tempf,"grp","tmat",tmat)
  ttmat <- tmat[,c(1,3,5)]
  rd <- read_matrix_h5(tempf,"grp","tmat",subset_cols = c(1,3,5))
  expect_equal(ttmat,rd)
})



test_that("can read/write numeric vector",{
  tvec <- runif(100)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})
test_that("can read/write integer vector",{
  tvec <- sample(1:100)
  tempf <- tempfile()
  write_vector_h5(tempf,"grp","dat",tvec)
  rd <- read_vector_h5(tempf,"grp","dat")
  expect_equal(rd,tvec)
})


test_that("can read string vector",{
  tvec <- c("allb","allc","alld")
  tempf <- tempfile()
  write_vector_h5(tempf,
                        groupname = "grp",
                        dataname = "tdat",
                        data = tvec
  )
  rd <- read_vector_h5(tempf,"grp","tdat")
  expect_equal(rd,tvec)
})


test_that("can read and write NA",{
  
  tmat <- matrix(runif(9*10),9,10)
  tmat[sample(1:(9*10),3,replace=F)] <- NA_real_
  tempf <- tempfile()
  write_matrix_h5(tempf,datapath="testg/testd",data=tmat)
  rmat <- read_matrix_h5(tempf,datapath="testg/testd")
  expect_equal(rmat,tmat)
})







test_that("writing 2 matrix blocks works",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(tempf,groupname = "testg",dataname = "testd",data=numeric(),dim=c(9,3))
  sub_mat <- tmat[1:5,1:2]
  write_matrix_h5(tempf,
                     groupname = "testg",
                     dataname = "testd",
                     data = sub_mat,offsets = c(0,0),subset_rows = 1:5,subset_cols = 1:2)
  r_sub_mat <- read_matrix_h5(tempf,groupname = "testg",dataname = "testd",offset = c(0L,0L),chunksize=c(5L,2L))
  expect_equal(sub_mat,r_sub_mat)
  sub_mat <- tmat[-(1:5),-(1:2),drop=F]
  write_matrix_h5( tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,offset = c(5,2))
  sub_mat <- tmat[(1:5),-(1:2),drop=F]
  write_matrix_h5( tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,subset_cols = c(3L),subset_rows = 1:5)
  sub_mat <- tmat[-(1:5),(1:2),drop=F]
  
  write_matrix_h5(tempf,
                  groupname = "testg",
                  dataname = "testd",
                  data = sub_mat,offsets = c(5,0))
  r_mat <- read_matrix_h5(tempf,"testg","testd")
  expect_equal(tmat,r_mat)
})




test_that("can write a chunk smaller than total (disk) data dimension, specifying only offsets",{
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(tempf,groupname = "testg",dataname = "testd",data=numeric(),dim=c(9,3))
  sub_mat <- tmat[1:5,1:2]
  write_matrix_h5(tempf,groupname="testg",dataname="testd",data=sub_mat,offsets=c(0,0))
  trm <- read_matrix_h5(tempf,"testg","testd",chunksizes=c(5L,2L))
  expect_equal(sub_mat,trm)
  
  
})



test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5( tempf,groupname = "testg",dataname = "testd",data=numeric(),dims = c(9L,3L))
  sub_mat <- tmat[3:5,2:3]
  write_matrix_h5(tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(2L,1L))
  r_sub_mat <- read_matrix_h5(tempf,"testg","testd",offsets = c(2L,1L),chunksizes = c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})



test_that("writing matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",data=numeric(),dims = c(9L,3L))
  sub_mat <- tmat[3:5,2:3]
  write_matrix_h5(filename = tempf,groupname = "/",dataname = "testd",data = sub_mat,offsets = c(2L,1L))
  r_sub_mat <- read_matrix_h5(tempf,"/","testd",offsets = c(2L,1L),chunksizes = c(3L,2L))
  expect_equal(sub_mat,r_sub_mat)
})
test_that("writing 2 matrix blocks works ",{
  
  tmat <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd0",data=numeric(),dims = c(9L,3L))
  write_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd0",data = tmat,offsets = c(0L,0L))
  r_sub_mat <- read_matrix_h5(tempf,"testg","testd0")
  expect_equal(tmat,r_sub_mat)
  
  # write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd",data = sub_mat,offsets = c(1L,2L),chunksizes = c(9L,2L))
  # r_sub_mat <- read_mat_h5(tempf,"testg","testd",c(2L,1L),c(3L,2L))
  # expect_equal(sub_mat,r_sub_mat)
  # create_matrix_h5(filename = tempf,groupname = "testg",dataname = "testd2",dimensions = c(9L,3L),doTranspose = F)
  # sub_mat <- tmat[3:5,2:3]
  # write_mat_chunk_h5(filename = tempf,groupname = "testg",dataname = "testd2",data = sub_mat,offsets = c(2L,1L),chunksizes = c(3L,2L))
  # r_sub_mat <- read_mat_h5(tempf,"testg","testd2",c(2L,1L),c(3L,2L))
  # expect_equal(sub_mat,r_sub_mat)
  
})

# test_that("writing a vector chunk works",{
#   tvec <- runif(200)
#   tempf <- tempfile()
#   create_vector_h5(tempf,"testg","test",200L)
#   sub_t <- tvec[5:195]
#   write_vector(filename = tempf,groupname = "testg",dataname = "test",data = sub_t,offsets = 4,chunksizes = 191)
#   retvec <- read_vector_h5(tempf,"testg","test",offset = 4L,chunksize = 191L)
#   expect_equal(retvec,sub_t)
# })


test_that("can create nested groups",{
  tvec <- matrix(runif(9*3),9,3)
  tempf <- tempfile()
  write_matrix_h5(tempf,"testg/tg2","test",data =tvec)
  rvec <- read_matrix_h5(tempf,"testg/tg2","test")
  expect_equal(tvec,rvec)
})

# test_that("ld_region splitting works as",{
#   
#   
#   num_ld_r <- 20
#   region_id <- integer()
#   result_matrix <- matrix(0,num_ld_r,3)
#   for(i in 1:num_ld_r){
#     result_matrix[i,1] <- i
#     result_matrix[i,2] <- length(region_id)
#     isize <- sample(1:1000,1)
#     result_matrix[i,3] <- isize
#     region_id <- c(region_id,rep(i,isize))
#   }
#   N <- 100
#   lr <- length(region_id)
#   test_mat <- matrix(0.01,lr,N)
#   for(i in 1:nrow(result_matrix)){
#     row_start <- (result_matrix[i,2]+1)
#     row_stop <- row_start+result_matrix[i,3]-1
#     row_val <- result_matrix[i,1]
#     test_mat[row_start:row_stop,] <- as.numeric(row_val)
#   }
#   
#   tempf <- tempfile()
#   write_vector_h5(filename = tempf,groupname = "grp",dataname = "region_id",data = region_id)
#   tv <- read_vec_h5(tempf,"grp","region_id")
#   write_matrix_h5(filename=tempf,groupname="/",dataname = "data",data = test_mat)
#   tM <- apply(result_matrix,1,function(x,filename,groupname,dataname,N){
#     tX <- read_mat_h5(filename,groupname,dataname,
#                 offsets=as.integer(c(x[2],0)),
#                 chunksizes = as.integer(c(x[3],N)))
#     expect_equal(all(tX==x[1]),T)
#     return(tX)
#     
#   },filename=tempf,groupname="/",dataname="data",N=N)
#   
#   
# })


## test_that("can convert flat matrices to hdf5 files ",{
  
##   p <- 5
##   N <- 3
##   tf <- tempfile(fileext=".txt.gz")
##   tm <- matrix(sample(0:9,p*N,replace=T),p,N)
##   readr::write_delim(tibble::as_data_frame(tm),path = tf,delim=" ",col_names = F)
##   nhf <- tempfile()
##   gz2hdf5(tf,output_filename = nhf,"/","test",p = p,N = N,chunk_size = 3)
##   res <- read_matrix_h5(nhf,"/","test")
##   expect_equal(res,t(tm))
## })
# 
# gwas_names <- c("bd","cad","cd","ht","ra","t1d","t2d")
# # gwas_names <- c("t1d","ht")
# # snpif <- sprintf("/home/nwknoblauch/Desktop/scratch/polyg_scratch/h5/%s_seq_wtcc_geno.h5",gwas_names)
# resl <- intersect_snpinfo_h5(snpif)
# tmap_df <- imap(resl,~read_df_h5(.y,"SNPinfo",subcols=c("chr","pos","snp_id","SNP"))) %>% set_names(gwas_names)
# 
# nt_df <- imap_dfr(tmap_df,function(x,y){
#    rdf <- slice(x,84:88) %>% mutate(n=y)
#   stopifnot(!is.unsorted(rdf$pos))
#   return(rdf)
# })


test_that("writing a vector works as in RcppEigenH5",{
  tvec <- runif(200)
  tempf <- tempfile()
  write_vector_h5(tempf,"testg","test",data = tvec)
  retvec <- read_vector_h5(tempf,"testg","test")
  expect_equal(tvec,retvec)
})

# test_that("guessing chunks works like in python",{
#   data_dims <- c(10000,10)
#   guess_chunks(data_dims)
#   
# })
