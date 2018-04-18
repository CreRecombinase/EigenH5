context("C++")
test_that("Catch unit tests pass", {
    testthat::expect_cpp_tests_pass("EigenH5")
})
