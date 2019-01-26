context("matrices")


test_that("can read int matrix", {
        tmat <- matrix(sample(1:900), 100, 9)
        tempf <- tempfile()

        write_matrix_h5(tmat, filename = tempf, "tmat")
        rd <- read_matrix_h5(tempf, "tmat")
        expect_equal(get_dims_h5(tempf, "/tmat"), c(100, 9))
        expect_equal(tmat, rd)
})


test_that("can concatenate matrices virtually (along columns)", {
        pvec <- sample(10:50, 4)
        n <- 8
        mats <- purrr::map(pvec, ~matrix(sample(1:(n * .x)), n, .x))

        tfs <- purrr::rerun(length(pvec), filename = tempfile(), datapath = "/temp")
        purrr::walk2(tfs, mats, ~write_matrix_h5(.y, .x$filename, "temp"))
        purrr::walk2(tfs, mats, ~write_matrix_h5(t(.y), .x$filename, "t_temp"))


        t_tfs <- purrr::map(tfs, ~purrr::list_modify(.x, datapath = "t_temp"))

        nf <- tempfile()
        concat_mats(nf, "all_temp_t", t_tfs, margin = 0)
        concat_mats(nf, "all_temp", tfs, margin = 1)



        rd <- read_matrix_h5(nf, "all_temp")
        rd_t <- read_matrix_h5(nf, "all_temp_t")

        all_mats <- purrr::reduce(mats, cbind)
        all_mats_t <- purrr::map(mats, t) %>% purrr::reduce(rbind)
        expect_equal(all_mats, rd)
        expect_equal(all_mats_t, rd_t)
})



test_that("can read int matrix transpose", {
        rown <- 10
        coln <- 60

        tmat <- matrix(1:(rown * coln), rown, coln)
        tempf <- tempfile()

        write_matrix_h5(tmat, tempf, "tmat")
        rd <- read_matrix_h5(tempf, "tmat", doTranspose = T)
        expect_equal(t(rd), tmat)

        sr <- sample(1:nrow(tmat), nrow(tmat) - 1, replace = F)
        sc <- sample(1:ncol(tmat), ncol(tmat) - 1, replace = F)

        srd <- read_matrix_h5(tempf, "tmat", subset_rows = sr, doTranspose = T)
        expect_equal(t(srd), tmat[sr, , drop = F])
        src <- read_matrix_h5(tempf, "tmat", subset_cols = sc, doTranspose = F)
        expect_equal(src, tmat[, sc])
        src <- read_matrix_h5(tempf, "tmat", subset_cols = sc, doTranspose = T)
        expect_equal(t(src), tmat[, sc])
        srrc <- read_matrix_h5(tempf, "tmat", subset_rows = sr, subset_cols = sc, doTranspose = T)
        expect_equal(t(srrc), tmat[sr, sc])
})



test_that("can read int matrix(one column)", {
        tmat <- matrix(sample(1:900), 100, 9)
        smat <- tmat[, 3, drop = F]
        tempf <- tempfile()

        write_matrix_h5(tmat, tempf, "tmat")
        rd <- read_matrix_h5(tempf, "tmat", subset_cols = 3)
        expect_equal(get_dims_h5(tempf, "tmat"), c(100, 9))
        expect_equal(smat, rd)
})


test_that("can read int matrix(one row)", {
        tmat <- matrix(sample(1:900), 100, 9)
        smat <- tmat[3, , drop = F]
        tempf <- tempfile()

        write_matrix_h5(tmat, tempf, "tmat")
        rd <- read_matrix_h5(tempf, "tmat", subset_rows = 3)
        expect_equal(get_dims_h5(tempf, "tmat"), c(100, 9))
        expect_equal(smat, rd)
})

test_that("can read int matrix rows", {
        tmat <- matrix(sample(1:900), 100, 9)
        tempf <- tempfile()
        write_matrix_h5(tmat, tempf, "grp/grp2/tmat")
        ind <- c(1, 3, 5)
        ttmat <- tmat[ind, ]
        rd <- read_matrix_h5(tempf, "grp/grp2/tmat", subset_rows = ind)
        nttmat <- tmat[, ind]
        rd <- read_matrix_h5(tempf, "grp/grp2/tmat", subset_cols = ind)
        expect_equal(nttmat, rd)
        ind <- c(1, 3, 2, 100, 4)
        ttmat <- tmat[ind, ]
        rd <- read_matrix_h5(tempf, datapath = "grp/grp2/tmat", subset_rows = ind)
        expect_equal(ttmat, rd)
})


test_that("can read int matrix cols", {
        tmat <- matrix(sample(1:900), 100, 9)
        tempf <- tempfile()
        write_matrix_h5(tmat, tempf, "grp/tmat")
        ind <- c(3, 1, 5)
        ttmat <- tmat[, ind]
        rd <- read_matrix_h5(tempf, datapath = "grp/tmat", subset_cols = ind)
        expect_equal(ttmat, rd)
        ind <- c(1, 2, 7, 3)
        ttmat <- tmat[, ind]
        rd <- read_matrix_h5(tempf, datapath = "grp/tmat", subset_cols = ind)
        expect_equal(ttmat, rd)
})



test_that("can read int matrix rows & cols", {
        tmat <- matrix(sample(1:900), 100, 9)
        tempf <- tempfile()
        write_matrix_h5(tmat, tempf, "grp/tmat")
        ttmat <- tmat[c(5, 3, 1), c(3, 5, 6)]
        rd <- read_matrix_h5(tempf,
                datapath = "grp/tmat",
                subset_rows = c(5, 3, 1),
                subset_cols = c(3, 5, 6)
        )
        expect_equal(ttmat, rd)
})


test_that("can read int matrix cols", {
        tmat <- matrix(sample(1:900), 100, 9)
        tempf <- tempfile()
        write_matrix_h5(tmat, tempf, "tmat")
        ttmat <- tmat[, c(1, 3, 5)]
        rd <- read_matrix_h5(tempf, "tmat", subset_cols = c(1, 3, 5))
        expect_equal(ttmat, rd)
})




test_that("writing 2 matrix blocks works", {
        tmat <- matrix(1:27, 9, 3)
        tempf <- tempfile()

        create_matrix_h5(
                filename = tempf,
                datapath = "testg/testd",
                data = numeric(), dim = c(9, 3)
        )
        sub_mat <- tmat[1:5, 1:2]
        write_matrix_h5(
                filename = tempf,
                datapath = "testg/testd",
                data = sub_mat,
                subset_rows = 1:5,
                subset_cols = 1:2
        )
        r_sub_mat <- read_matrix_h5(
                filename = tempf,
                datapath = "testg/testd",
                offset = c(0L, 0L),
                datasize = c(5L, 2L)
        )
        expect_equal(sub_mat, r_sub_mat)

        sub_mat <- tmat[6:9, 3, drop = F]
        
        write_matrix_h5(
                filename = tempf, datapath = "testg/testd",
                data = sub_mat, offset = c(5, 2),datasize=dim(sub_mat)
        )


        sub_mat <- tmat[(1:5), -(1:2), drop = F]



        write_matrix_h5(
                filename = tempf, datapath = "testg/testd",
                data = sub_mat, subset_cols = c(3L), subset_rows = 1:5
        )
        sub_mat <- tmat[-(1:5), (1:2), drop = F]

        write_matrix_h5(
                filename = tempf,
                datapath = "testg/testd",
                data = sub_mat, offsets = c(5, 0), datasize = dim(sub_mat)
        )



        r_mat <- read_matrix_h5(tempf, "testg/testd")
        expect_equal(tmat, r_mat)

})


# 
# 
# test_that("can write a chunk smaller than total (disk) data dimension, specifying only offsets", {
#         tmat <- matrix(runif(9 * 3), 9, 3)
#         tempf <- tempfile()
#         create_matrix_h5(tempf,
#                 groupname = "testg",
#                 dataname = "testd",
#                 data = numeric(),
#                 dim = c(9, 3)
#         )
#         sub_mat <- tmat[1:5, 1:2]
#         write_matrix_h5(tempf,
#                 groupname = "testg",
#                 dataname = "testd",
#                 data = sub_mat, offsets = c(0, 0)
#         )
#         trm <- read_matrix_h5(tempf, "testg", "testd", datasizes = c(5L, 2L))
#         expect_equal(sub_mat, trm)
# })


# test_that("writing matrix blocks works ", {
#         tmat <- matrix(runif(9 * 3), 9, 3)
#         tempf <- tempfile()
#         create_matrix_h5(
#                 filename = tempf,
#                 datapath = "testg/testd",
#                 data = numeric(),
#                 dims = c(9L, 3L)
#         )
#         sub_mat <- tmat[3:5, 2:3]
#         write_matrix_h5(
#                 filename = tempf,
#                 datapath = "testg/testd",
#                 data = sub_mat,
#                 offsets = c(2L, 1L)
#         )
#         r_sub_mat <- read_matrix_h5(
#                 filename = tempf,
#                 datapath = "testg/testd",
#                 offsets = c(2L, 1L),
#                 datasizes = c(3L, 2L)
#         )
#         expect_equal(sub_mat, r_sub_mat)
# })


# 
# test_that("writing matrix blocks works ", {
#         tmat <- matrix(runif(9 * 3), 9, 3)
#         tempf <- tempfile()
#         create_matrix_h5(
#                 filename = tempf,
#                 datapath = "testd",
#                 data = numeric(),
#                 dims = c(9L, 3L)
#         )
#         sub_mat <- tmat[3:5, 2:3]
#         write_matrix_h5(
#                 filename = tempf,
#                 datapath = "testd",
#                 data = sub_mat,
#                 offsets = c(2L, 1L)
#         )
#         r_sub_mat <- read_matrix_h5(
#                 filename = tempf,
#                 datapath = "testd",
#                 offsets = c(2L, 1L),
#                 datasizes = c(3L, 2L)
#         )
#         expect_equal(sub_mat, r_sub_mat)
# })
# test_that("writing 2 matrix blocks works ", {
#         tmat <- matrix(
#                 runif(9 * 3), 9, 3
#         )
#         tempf <- tempfile()
#         create_matrix_h5(
#                 filename = tempf,
#                 datapath = "testg/testd0",
#                 data = numeric(),
#                 dims = c(9L, 3L)
#         )
#         write_matrix_h5(
#                 filename = tempf,
#                 datapath = "testg/testd0",
#                 data = tmat,
#                 offsets = c(0L, 0L)
#         )
#         r_sub_mat <- read_matrix_h5(tempf, "testg/testd0")
#         expect_equal(tmat, r_sub_mat)
# })
