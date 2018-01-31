.onLoad <- function(libname, pkgname){
  start_blosc()
}

read_df_h5 <- function(h5filepath,groupname,subcols = character(),filtervec = integer(),offset=integer(),chunksize=integer()){
  return(tibble::as_data_frame(read_l_h5(h5filepath = h5filepath,groupname = groupname,subcols = subcols,offset = offset,chunksize = chunksize,filtervec = filtervec)))
}


datatype_h5 <- function(h5filename,groupname,dataname){
  dt <- check_dtype(input_filenames[1],groupname,dataname)
  if(dt==14){
    return(numeric())
  }
  return(integer())
}

concat_cols_h5 <- function(input_filenames,output_filename,groupname,dataname,input_rows=NULL){
  all_dims <- purrr::map(input_filenames,~get_dims_h5(.x,groupname=groupname,dataname=dataname))
  all_p <- purrr::map_int(all_dims,~.x[1])
  p <- unique(all_p)
  stopifnot(length(p)==1)
  all_colno <- purrr::map_int(all_dims,~.x[2])
  tot_colno <- sum(all_colno)
  if(is.null(input_rows)){
    input_rows <- purrr::rerun(length(input_filenames),1:p)
  }
  create_matrix_h5(filename = output_filename,
                   groupname = groupname,
                   dataname = dataname,
                   data = datatype_h5(h5filename = input_filenames[1],groupname = groupname,dataname = dataname),
                   doTranspose = F,
                   dims = c(p,tot_colno))
  cur_N <- 0
  for(i in 1:length(input_filenames)){
    tinf <- input_filenames[i]
    tinr <- input_rows[[i]]
    if(is.null(tinr)){
      tinr <- integer()
    }
    stopifnot(min(tinr)>0)
    tmat <- read_matrix_h5(filename = tinf,groupname = groupname,dataname = dataname,subset_rows = tinr)
    write_matrix_h5(filename = output_filename,groupname = groupname,dataname = dataname,data = tmat,doTranspose = F,offsets = c(0L,cur_N))
    cur_N <- cur_N+ncol(tmat)
    gc()
  }
  
  
}


