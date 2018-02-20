.onLoad <- function(libname, pkgname){
  start_blosc()
}

read_df_h5 <- function(h5filepath,groupname,subcols = character(),filtervec = integer(),offset=integer(),chunksize=integer()){
  stopifnot(file.exists(h5filepath))
  return(tibble::as_data_frame(read_l_h5(h5filepath = normalizePath(h5filepath),groupname = groupname,subcols = subcols,offset = offset,chunksize = chunksize,filtervec = filtervec)))
}

get_sub_obj <- function(h5filepath,tpath="/"){
  res <- purrr::possibly(get_objs_h5,otherwise=NULL,quiet = T)(h5filepath,tpath)
  if(is.null(res)){
    return(tpath)
  }
  return(paste0(ifelse(tpath=="/","",tpath),"/",res))
}

split_chunk_df<- function(info_df,pos_id,group_id,rowsel=T,colsel=T){
  q_pos <- dplyr::enquo(pos_id)
  q_group <- dplyr::enquo(group_id)
  # stopifnot(!is.unsorted(as.integer(info_df[[group_id]])),
  #           !is.unsorted(as.integer(info_df[[pos_id]])),
  #           rowsel||colsel)
  sel_df <- dplyr::group_by(info_df,!!q_group) %>% 
    dplyr::summarise(offset=as.integer(min(!!q_pos)-1),chunksize=as.integer(n()))
  if(rowsel){
    sel_df <- dplyr::mutate(sel_df,row_offsets=offset,row_chunksizes=chunksize)
  }
  if(colsel){
    sel_df <- dplyr::mutate(sel_df,col_offsets=offset,col_chunksizes=chunksize)
  }
  sel_df <- dplyr::select(sel_df,-offset,-chunksize)
  return(sel_df)
}

# h5ls_df <- function(h5filepath){
#   root_objs <- get_sub_obj(h5filepath =h5filepath)
#   bg_objs <- purrr::possibly(get_objs_h5,otherwise = NULL)
#   
#   node_objs <- purrr::map(root_objs,~paste0(ifelse(.x=="/","",.x),"/",bg_objs(h5filepath=h5filepath,groupname = .x)))
#   
# }

gz2hdf5 <- function(input_filename,output_filename,output_groupname,output_dataname,p=NULL,N=NULL,chunk_size=10000){
  
  stopifnot(!is.null(p),!is.null(N))
  if(!file.exists(output_filename)){
    create_matrix_h5(output_filename,output_groupname,output_dataname,integer(),doTranspose=F,dims=as.integer(c(N,p)),chunksizes=as.integer(c(N,min(200,p))))
  }else{
    if(!data_exists(output_filename,output_groupname,output_dataname)){
      create_matrix_h5(output_filename,output_groupname,output_dataname,integer(),doTranspose=F,dims=as.integer(c(N,p)),chunksizes=as.integer(c(N,min(200,p))))
    }
  }
  

  write_f <- function(x,pos){
    tm <- parse_mat(x)
    write_matrix_h5(filename = output_filename,groupname = output_groupname,dataname = output_dataname,data = tm,offsets = c(0,pos-1))
  }
  readr::read_lines_chunked(input_filename,callback=SideEffectChunkCallback$new(write_f),chunk_size=chunk_size,progress=T)
}

read_mat_h5 <- function(filename,groupname,dataname,offset_rows=0,offset_cols=0,chunksize_rows=NULL,chunksize_cols=NULL){
  mat_dims <- get_dims_h5(filename,groupname,dataname)
  stopifnot(length(mat_dims)==2)
  if(is.null(chunksize_cols)){
    chunksize_cols <- mat_dims[2]-offset_cols
  }
  if(is.null(chunksize_rows)){
    chunksize_rows <- mat_dims[1]-offset_rows
  }
  return(read_matrix_h5(filename = filename,
                        groupname = groupname,
                        dataname = dataname,
                        offsets = c(offset_rows,offset_cols),
                        chunksizes = c(chunksize_rows,chunksize_cols)))
}


read_mat_l <- function(dff){
  return(purrr::pmap(dff,function(filenames,
                                  groupnames,
                                  datanames,
                                  row_offsets=0,
                                  col_offsets=0,
                                  row_chunksizes=NULL,
                                  col_chunksizes=NULL,...){
    
    return(EigenH5::read_mat_h5(filenames,groupnames,datanames,row_offsets,col_offsets,row_chunksizes,col_chunksizes))
  }))
}

create_mat_l <- function(dff){
  tl <- list(integer=integer(),numeric=numeric())
  return(purrr::pwalk(dff,function(filenames,
                                  groupnames,
                                  datanames,
                                  datatypes,
                                  row_chunksizes,
                                  col_chunksizes,
                                  row_c_chunksizes=NULL,
                                  col_c_chunksizes=NULL,
                                  ...){
    EigenH5::create_matrix_h5(
      filenames,
      groupnames,
      datanames,
      tl[[datatypes]],
      doTranspose=F,
      dims=c(row_chunksizes,col_chunksizes),
      chunksizes=c(row_c_chunksizes,col_c_chunksizes))
  }))
}

datatype_h5 <- function(h5filename,groupname,dataname){
  dt <- check_dtype(h5filename,groupname,dataname)
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


