H5File <- R6Class("H5File", list(
  filename = NULL,
  read_only = TRUE,
  file_id = NULL,
  initialize = function(filename=fs::file_temp(), read_only = TRUE) {
      stopifnot(is.character(filename), length(filename) == 1,
                !(read_only && !fs::file_exists(filename)))
      stopifnot(is.logical(read_only), length(read_only) == 1)
      self$filename <- filename
      self$read_only <- read_only
      if (read_only) {
          self$file_id  <- open_file_ro(filename)
      }else{
          self$file_id  <- open_file_rw(filename)
      }
  },
  finalize = function() {
      print("Finalizer has been called!")
      release_file(self$file_id)
  }
))



H5DataSet <- R6Class("H5DataSet", list(
  dataset_name = NULL,
  dataset_id = NULL,
  dims=NULL,
  initialize = function(file, dataset_name) {
    stopifnot(is.logical(read_only), length(read_only) == 1)
    self$dataset_name <- dataset_name
    self$dataset_id <- get_dataset(file$file_id,dataset_name)
  },
  finalize = function() {
    print("Finalizer has been called for a dataset: ",dataset_name,"\n")
    release_dataset(dataset_id)
  }
  ))


H5Group <- R6Class("H5Group", list(
  group_name = NULL,
  group_id = NULL,
  dims=NULL,
  initialize = function(file, group_name) {
    stopifnot(is.logical(read_only), length(read_only) == 1)
    self$group_name <- group_name
    self$group_id <- get_group(file$file_id,group_name)
  },
  finalize = function() {
    print("Finalizer has been called for a group: ",group_name,"\n")
    release_group(group_id)
  }
))


