
ignoreMe <- setMethod("show", "Rcpp_H5File", function (object) {
  cat("\n Hi, I am an H5File object!\n")
  cat("\n I was initialized with filename", object$filename)
  cat("\n and my readOnly status is  ", object$readOnly, ".\n", sep = "")
  #cat("\n Therefore my range is ", object$range(), ".", sep = "")
  cat("\n\n")
})