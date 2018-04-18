read_mat_col_futures<- function(h5filename,filepath,subset_cols=list(),doTranspose=F){
  n_futures <- length(subset_cols)
  stopifnot(n_futures>0)
  #    retl <- listenv::listenv()
  if(doTranspose){
    return(purrr::map(subset_cols,~future::future(t(EigenH5::read_matrix(h5filename,filepath,subset_cols=.x)),packages="EigenH5")))
  }else{
    return(purrr::map(subset_cols,~future::future(EigenH5::read_matrix(h5filename,filepath,subset_cols=.x),packages="EigenH5")))
  }
}

read_df_futures <- function(h5filename,filepath,subcols=character(),filtervec=list()){
    return(purrr::map(filtervec,~future::future(EigenH5::read_df_h5(h5filename,subcols=subcols,filepath,filtervec=.x)),packages="EigenH5"))
}

read_mat_row_futures<- function(h5filename,filepath,subset_rows=list(),doTranspose=F){
  n_futures <- length(subset_rows)
  stopifnot(n_futures>0)
  #    retl <- listenv::listenv()
  if(doTranspose){
    return(purrr::map(subset_rows,~future::future(t(EigenH5::read_matrix(h5filename,filepath,subset_rows=.x)),packages="EigenH5")))
  }else{
    return(purrr::map(subset_rows,~future::future(EigenH5::read_matrix(h5filename,filepath,subset_rows=.x),packages="EigenH5")))
  }
}

read_vec_futures <- function(h5filename,filepath,subset=list()){
    return(purrr::map(subset,~future::future(EigenH5::read_vector(h5filename,filepath,subset=.x)),packages="EigenH5"))
}
