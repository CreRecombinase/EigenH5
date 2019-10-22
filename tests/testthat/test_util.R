context("metadata/utilities")



test_that("Can get back chunksize", {
        tv <- sample(1:100, 10)
        tm <- matrix(1:9, 3, 3)
        tf <- tempfile()
        write_vector_h5(filename = tf, datapath = "ntest", data = tv, chunksize = 2)
        write_vector_h5(filename = tf, datapath = "test", data = tv)
        write_matrix_h5(filename = tf, datapath = "testm", data = tm)
        ltm <- matrix(42, 3000, 3000)
        write_matrix_h5(filename = tf, datapath = "ltestm", data = ltm)
        dataset_chunks(tf, "ltestm")



        expect_equal(dataset_chunks(tf, "testm"), c(3, 3))
        # Verified using h5ls -rv
        expect_equal(dataset_chunks(tf, "test"), 10)
        ltv <- rep(42, 9000000)
        write_vector_h5(filename = tf, datapath = "test2", ltv)
        expect_equal(dataset_chunks(tf, "test2"), 562500)
})




test_that("I can create an extendable dataset and extend it", {
        tf <- tempfile()
        create_dataset_h5(filename = tf, datapath = "/test", data = numeric(), list(dims = c(3L, 4L), max_dims = c(NA_integer_, NA_integer_)))
        expect_equal(get_dims_h5(tf, "/test"), c(3L, 4L))
        extend_dataset(tf, "/test", c(4L, 5L))
        expect_equal(get_dims_h5(tf, "/test"), c(4L, 5L))

        tf <- tempfile()
        create_dataset_h5(filename = tf, datapath = "/test", data = numeric(), list(dims = c(3L, 4L), max_dims = c(NA_integer_, NA_integer_)))
        extend_dataset_by(tf, "/test", c(2L, 2L))
        expect_equal(get_dims_h5(tf, "/test"), c(5L, 6L))
})




test_that("fast conversion works", {
        letter_d <- sample(letters,100,replace=T)
        int_d <- sample(as.character(1:100),100,replace=TRUE)
        ascii_i <- purrr::map_int(letter_d,utf8ToInt)
        int_i <- as.integer(int_d)
        
        testthat::expect_equal(fast_str2ascii(letter_d),ascii_i)
        testthat::expect_equal(fast_str2int(int_d),int_i)
        
        for(i in 1:10){
                testthat::expect_equal(fast_str2ascii(paste0(paste0(rep("p",i),collapse=""),letter_d),offset = i),ascii_i)
                testthat::expect_equal(fast_str2int(paste0(paste0(rep("p",i),collapse=""),int_d),offset = i),int_i)
        }
        
        
        
})

test_that("We can write a vector", {
        expect_true(EigenH5::write_vector_h5(numeric(3), tempfile(), "test"))
})

test_that("root group is created upon file creation", {
        tf <- tempfile()
        create_file_h5(tf)
        ls_h5(tf)
        expect_true(isGroup(tf, "/"))
})


test_that("we can symlink an entire root", {
        tv <- sample(1:100, 10)
        tf <- tempfile()
        write_vector_h5(tv, tf, "testg/test")
        ntf <- tempfile()

        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/", datapath_to = "/t1")
        retv <- read_vector_h5(ntf, "/t1/testg/test")
        expect_equal(retv, tv)
})


test_that("we can symlink a vector of roots", {
        num_objs <- 10
        tfs <- replicate(num_objs, tempfile())
        tvl <- purrr::rerun(num_objs, sample(1:100, 10))
        purrr::walk2(tfs, tvl, ~ write_vector_h5(.y, .x, "testg/te"))
        ntf <- tempfile()
        link_objects_h5(filename_from = tfs, filename_to = ntf, datapath_from = rep("/", num_objs), datapath_to = as.character(1:num_objs))
        retl <- purrr::map(as.character(1:num_objs), ~ read_vector_h5(ntf, paste0(.x, "/testg/te")))
        expect_equal(retl, tvl)
})




test_that("we can symlink a single group", {
        tv <- sample(1:100, 10)
        tf <- tempfile()
        write_vector_h5(tv, tf, "testg/test")
        ntf <- tempfile()

        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/testg", datapath_to = "/t1")
        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/testg", datapath_to = "/testg")
        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/testg/test", datapath_to = "/newtest")
        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/testg/test", datapath_to = "/testg/subset")
        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/testg/test", datapath_to = "/testg/subset2")




        retv <- read_vector_h5(ntf, "/t1/test")
        expect_equal(retv, tv)
        retv <- read_vector_h5(ntf, "/testg/test")
        expect_equal(retv, tv)
        retv <- read_vector_h5(ntf, "newtest")
        expect_equal(retv, tv)
        retv <- read_vector_h5(ntf, "testg/subset")
        expect_equal(retv, tv)
        retv <- read_vector_h5(ntf, "testg/subset2")
        expect_equal(retv, tv)
})


test_that("we can symlink a vector of datapaths", {
        num_objs <- 10
        tfs <- replicate(num_objs, tempfile())
        tvl <- purrr::rerun(num_objs, sample(1:100, 10))
        purrr::walk2(tfs, tvl, ~ write_vector_h5(.y, .x, "testg/te"))
        ntf <- tempfile()
        link_objects_h5(filename_from = tfs, filename_to = ntf, datapath_from = rep("/testg/te", num_objs), datapath_to = as.character(1:num_objs))
        retl <- purrr::map(as.character(1:num_objs), ~ read_vector_h5(ntf, paste0(.x)))
        expect_equal(retl, tvl)
})

test_that("we can reorder stuff using link_objects", {
        tv <- sample(1:100, 10)
        tf <- tempfile()
        write_vector_h5(tv, tf, "testg/test")
        otv <- runif(50)
        write_vector_h5(otv, tf, "test_d")
        ntf <- tempfile()

        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/testg", datapath_to = "/t1")
        link_objects_h5(filename_from = tf, filename_to = ntf, datapath_from = "/testg", datapath_to = "/testg")

        retv <- read_vector_h5(ntf, "/t1/test")
        expect_equal(retv, tv)
        retv <- read_vector_h5(ntf, "/testg/test")
        expect_equal(retv, tv)
})



test_that("crazy bit packing works",{
        p <- 1e5
        chrom <- sample(1:23,p,replace=TRUE)
        pos <- sample(2^43,p,replace=T)
        ref <- purrr::map_int(sample(c("A","C","T","G",NA_character_),p,replace=T),utf8ToInt)
        alt <- purrr::map_int(sample(c("A","C","T","G",NA_character_),p,replace=T),utf8ToInt)
        ret <- fast_snp_pos_struct(chrom,pos,ref,alt)        
        cdf <- snp_struct2df(ret)
        expect_equal(cdf$chrom,chrom)
        expect_equal(cdf$pos,pos)
        expect_equal(cdf$ascii_ref,ref)
        expect_equal(cdf$ascii_alt,alt)
})

