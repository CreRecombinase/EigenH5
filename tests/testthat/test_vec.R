context("vectors")



test_that("I can overwrite a vector", {
        tf <- tempfile()
        EigenH5::write_vector_h5(integer(0), tf, "empty_test", filter = "none", chunksizes = integer())
        # read_vector_h5(tf,"empty_test")
        tv <- runif(3)
        write_vector_h5(tv, tf, "test")
        expect_equal(read_vector_h5(tf, "test"), tv)
        ntv <- runif(3)
        write_vector_h5(ntv, tf, "test")
        expect_equal(read_vector_h5(tf, "test"), ntv)
})



testthat::test_that("I can write a factor", {
        tf <- tempfile()
        factor_d <- gl(n = 15,k = 4)
        EigenH5::write_vector_h5(factor_d, tf, "factor")
        ret <- EigenH5::read_vector_h5(tf,"factor")
        expect_equal(factor_d,ret)
})



test_that("I can append a vector", {
        tf <- tempfile()
        tv <- runif(3)
        write_vector_h5(data = tv, filename = tf, datapath = , "test", max_dims = c(NA_integer_))
        expect_equal(read_vector_h5(tf, "test"), tv)
        ntv <- runif(3)
        write_vector_h5(filename = tf, datapath = "test", data = ntv, append = T)
        expect_equal(read_vector_h5(tf, "test"), c(tv, ntv))
})


test_that("can write and read long strings", {
        tvec <- paste0(sample(letters, 254, replace = T), collapse = "")
        tempf <- tempfile()
        write_vector_h5(tvec, tempf, "testw")
        
        rvec <- read_vector_h5(filename = tempf, "testw")
        expect_equal(rvec, tvec)
        
        tvec <- paste0(rep(rawToChar(as.raw(1:126)),3),collapse="")
        tempf <- tempfile()
        write_vector_h5(tvec, tempf, "testw",filter="none")
        res_vec <- read_vector_h5(filename = tempf, "testw")
        expect_equal(res_vec, tvec)
})


test_that("can write short strings then long strings", {
        tvec <- paste0(sample(letters, 25, replace = T), collapse = "")
        tempf <- tempfile()
        write_vector_h5(tvec, tempf, "testw",max_dims=NA_integer_,min_string_size=27L)
        expect_equal(ArrayTypeSize(tempf,"testw"),28L)
        tvec2 <- paste0(sample(letters, 30, replace = T), collapse = "")
        expect_error(write_vector_h5(tvec2,tempf,"testw",append=TRUE),"string will not fit in dataset")
        
        write_vector_h5(tvec, tempf, "testw2",max_dims=NA_integer_,min_string_size=31)
        expect_equal(ArrayTypeSize(tempf,"testw2"),32)
        tvec2 <- paste0(sample(letters, 30, replace = T), collapse = "")
        write_vector_h5(tvec2,tempf,"testw2",append=TRUE)
        rvec <- read_vector_h5(filename = tempf, "testw2",subset=2)
        expect_equal(rvec, tvec2)
})


test_that("can write string vector", {
        tvec <- c("allb", "allc", "alld")
        tempf <- tempfile()
        testthat::expect_true(EigenH5::write_vector_h5(filename = tempf, datapath = "grp/dat", data = tvec))
        expect_equal(typeof_h5(filename = tempf, "grp"), "list")
        expect_equal(typeof_h5(filename = tempf, "grp/dat"), "character")

        expect_equal(dim_h5(filename = tempf, "grp/dat"), length(tvec))
        expect_equal(dim_h5(filename = tempf, "grp/dat"), length(tvec))

        rd <- read_vector_h5(filename = tempf, datapath = "grp/dat")
        expect_equal(rd, tvec)
        trd <- read_vector_h5(filename = tempf, datapath = "grp/dat", datasize = 2)
        expect_equal(head(tvec, 2), trd)
        write_vector_h5(filename = tempf, datapath = "/grp/dat2", data = tvec)

        trd <- read_vector_h5(filename = tempf, "grp/dat2")
        expect_equal(trd, tvec)

        tvec <- c("allb", "allc", "alld")
        write_vector_h5(filename = tempf, "/grp2/grp3", "dat2", data = tvec)


        trd <- read_vector_h5(filename = tempf, "grp/dat", subset = 2:3)

        expect_equal(tail(tvec, 2), trd)
        trd <- read_vector_h5(filename = tempf, "grp/dat", subset = c(1, 3))
        expect_equal(tvec[c(1, 3)], trd)
})

test_that("can check type of vectors", {
        tvec <- c("allb", "allc", "alld")
        tempf <- tempfile()
        write_vector_h5(filename = tempf,datapath =  "grp/dat", tvec)
        expect_equal(typeof_h5(filename = tempf, "grp"), "list")
        expect_equal(typeof_h5(filename = tempf, "grp/dat"), "character")

        tvec <- runif(3)
        tempf <- tempfile()
        write_vector_h5(filename = tempf, datapath = "grp/grp2/dat", tvec)


        tvec <- sample(1:10)
        tempf <- tempfile()
        write_vector_h5(filename = tempf,datapath = "dat", tvec)
        expect_equal(typeof_h5(filename = tempf, datapath = "dat"), "integer")
        expect_equal(dim_h5(filename = tempf, datapath = "dat"), 10)
})






test_that("can write a vector out of order", {
        tvec <- c(1.0, 2.0, 3.0)
        tempf <- tempfile()
        ind <- c(3, 1, 2)
        write_vector_h5(filename = tempf, datapath="grp/dat", tvec, subset = ind)
        trd <- read_vector_h5(filename = tempf, datapath="grp/dat")
        expect_equal(trd, tvec[ind])
})


test_that("can read a vector out of order", {
        tvec <- 1:3
        tempf <- tempfile()
        write_vector_h5(filename = tempf, datapath="grp/dat", tvec)
        trd <- read_vector_h5(filename = tempf, datapath="grp/dat", subset = c(3, 1, 2))
        expect_equal(trd, tvec[c(3, 1, 2)])
})

# test_that("can read an empty subset", {
#         tvec <- runif(3)
#         tempf <- tempfile()
#         write_vector_h5(filename = tempf, datapath="grp/dat", tvec)
#         rd <- read_vector_h5(filename = tempf, datapath="grp/dat", subset = integer())
#         expect_equal(rd,integer())
# })


test_that("can write REAL vector", {
        tvec <- runif(3)
        tempf <- tempfile()
        write_vector_h5(filename = tempf, datapath="grp/dat", tvec)
        expect_equal(dim_h5(filename = tempf, "grp/dat"), length(tvec))
        rd <- read_vector_h5(filename = tempf, datapath="grp/dat")
        expect_equal(rd, tvec)
        trd <- read_vector_h5(filename = tempf, datapath="grp/dat", datasize = 2)
        expect_equal(head(tvec, 2), trd)
        trd <- read_vector_h5(filename = tempf, datapath="grp/dat", subset = 2:3)
        expect_equal(tail(tvec, 2), trd)
        trd <- read_vector_h5(filename = tempf, datapath="grp/dat", subset = c(1, 3))
        expect_equal(tvec[c(1, 3)], trd)
})

test_that("we can read subsets out of order", {
        tvec <- c("allb", "allc", "alld")
        tempf <- tempfile()
        write_vector_h5(filename = tempf, datapath = "grp/dat", data = tvec)
        strd <- read_vector_h5(filename = tempf, datapath = "grp/dat")
        expect_equal(strd, tvec)
        trd <- read_vector_h5(filename = tempf, datapath = "grp/dat", subset = c(2, 1))
        expect_equal(tvec[c(2, 1)], trd)
        trd <- read_vector_h5(filename = tempf, datapath = "grp/dat", subset = c(3, 1))
        expect_equal(tvec[c(3, 1)], trd)
})


test_that("can read string vector", {
        tvec <- c("allb", "allc", "alld")
        tempf <- tempfile()
        write_vector_h5(filename = tempf, datapath="grp/dat", tvec)
        rd <- read_vector_h5(filename = tempf, datapath="grp/dat")
        expect_equal(rd, tvec)
})

test_that("can create a vector and then write to it", {
        tvec <- c("allb", "allc", "alld")
        tempf <- tempfile()
        create_vector_h5(filename = tempf, datapath="grp/dat", character(), dim = 3L)
        rd <- read_vector_h5(filename = tempf, datapath="grp/dat")
        expect_equal(rd, c("", "", ""))
        write_vector_h5(filename = tempf, datapath="grp/dat", tvec)
        rd <- read_vector_h5(filename = tempf, datapath="grp/dat")
        expect_equal(rd, tvec)
})




test_that("can read/write numeric vector", {
        tvec <- runif(100)
        tempf <- tempfile()
        write_vector_h5(filename = tempf, datapath="grp/dat", tvec)
        rd <- read_vector_h5(filename = tempf, datapath="grp/dat")
        expect_equal(rd, tvec)
})
test_that("can read/write integer vector", {
        tvec <- sample(1:100)
        tempf <- tempfile()
        write_vector_h5(filename = tempf, datapath="grp/dat", tvec)
        rd <- read_vector_h5(filename = tempf, datapath="grp/dat")
        expect_equal(rd, tvec)
})


test_that("can read string vector", {
        tvec <- c("allb", "allc", "alld")
        tempf <- tempfile()
        write_vector_h5(filename = tempf,
                datapath="grp/tdat",
                data = tvec
        )
        # otvec <- tvec
        # otvec[2] <- NA_character_
        # write_vector_h5(filename = tempf,
        #         datapath="grp/otdat",
        #         data = otvec
        # )
        # 
        # ord <- read_vector_h5(filename = tempf, datapath="grp/otdat")
        # expect_equal(ord,otvec,na.rm=T)
        rd <- read_vector_h5(filename = tempf, datapath="grp/tdat")
        expect_equal(rd, tvec)
})
