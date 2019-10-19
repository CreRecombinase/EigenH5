.onLoad <- function(libname, pkgname){
    pkgconfig::set_config("EigenH5::use_blosc" = has_blosc())
    pkgconfig::set_config("EigenH5::use_lzf" = has_lzf())
    start_blosc()
    # start_singleton()
}


ls_h5 <- function(filename,groupname="/",full_names=FALSE,details=FALSE){
  if(!details){
    fs::path_norm(ls_h5_exp(filename = fs::path_expand(filename),
                            groupname = groupname,
                            full_names = full_names))
  }else{
    full_n <- ls_h5_exp(filename = fs::path_expand(filename),
                            groupname = groupname,
                            full_names = TRUE)
    id_type=purrr::map_chr(full_n,~typeof_h5(filename,.x))
    id_dim=purrr::map(full_n,~dim_h5(filename,.x))
    if(all(lengths(id_dim)==length(id_dim[[1]]))){
      id_dim <- purrr::flatten_int(id_dim)
    }
    if(!full_names){
      full_n <- fs::path_rel(full_n,start=groupname)
    }
    tibble::tibble(name=full_n,dims=id_dim,type=id_type)
  }
}

construct_data_path <- function(...){
    arguments <- list(...)
    retpath <- gsub("^/","",paste(arguments,collapse="/"))
    retpath <- gsub("//","/",retpath)
    return(retpath)
}

## lockf <- function(filename){
##   return(paste0(filename,".lck"))
## }

isObject_h5 <- function(filename,datapath){

  stopifnot(file.exists(filename))
  if(!hasArg(timeout)){
    timeout <- Inf
  }

  ret <- isObject(filename,datapath)

  return(ret)
}




gen_matslice_df <- function(filename,group_prefix,dataname){
  sub_grps <- ls_h5(filename,group_prefix)
  retdf <- dplyr::data_frame(filenames=filename,
                    groupnames=paste0(group_prefix,"/",sub_grps),
                    datanames=dataname) %>% dplyr::arrange(as.integer(sub_grps))
  return(retdf)
}



get_dims_h5 <- function(f,...){
  return(dim_h5(f,construct_data_path(...)))
}

write_h5 <- function(data,filename,datapath,offset=0L,subsets=list(subset_rows=integer(),subset_cols=integer())){
  if(is.list(data)){
    write_l_h5(h5filepath=h5filepath,datapath=datapath,datal=data)
  }else{
    if(is.vector(data)){
      write_vector_h5(filename =  h5filepath,datapath=datapath,data = data,offset=offset,subset = subsets[["subset_rows"]])
    }else{
      if(is.matrix(data)){
        write_matrix_h5(filename = h5filepath,datapath=datapath,data = data,
                     subset_rows = subsets[["subset_rows"]],
                     subset_cols = subsets[["subset_cols"]])
      }else{
        if(!is.null(data)){
        stop("data is of unknown type!")
        }
      }
    }
  }
}


get_objs_h5 <- function(f,gn,full_names=F){
  return(ls_h5(f,gn,full_names))
}

split_chunk_df<- function(info_df,pos_id,group_id,rowsel=T,colsel=T){
  q_pos <- dplyr::enquo(pos_id)
  q_group <- dplyr::enquo(group_id)
  
  
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


# read_h

write_l_h5 <- function(data,filename,datapath,...){
  stopifnot(is.list(data))
  if(datapath=="/"){
    datapath <- ""
  }
  purrr::iwalk(datal,~write_h5(filename,normalizePath(paste(datapath,.y,sep="/"),mustWork = F),data = .x))
}


## path_exists_h5  <- function(h5filepath,datapath){
##     retvec <- c(FALSE,FALSE)
##     retvec[1]  <- file.exists(h5filepath)
##     if(retvec[1]){
        
##     return(c(file.exists(h5filepath







# get_sub_obj <- function(h5filepath,tpath="/"){
#   res <- purrr::possibly(get_objs_h5,otherwise=NULL,quiet = T)(h5filepath,tpath)
#   if(is.null(res)){
#     return(tpath)
#   }
#   return(paste0(ifelse(tpath=="/","",tpath),"/",res))
# }

# split_chunk_df<- function(info_df,pos_id,group_id,rowsel=T,colsel=T){
#   q_pos <- dplyr::enquo(pos_id)
#   q_group <- dplyr::enquo(group_id)
#   sel_df <- dplyr::group_by(info_df,!!q_group) %>% 
#     dplyr::summarise(offset=as.integer(min(!!q_pos)-1),chunksize=as.integer(n()))
#   if(rowsel){
#     sel_df <- dplyr::mutate(sel_df,row_offsets=offset,row_chunksizes=chunksize)
#   }
#   if(colsel){
#     sel_df <- dplyr::mutate(sel_df,col_offsets=offset,col_chunksizes=chunksize)
#   }
#   sel_df <- dplyr::select(sel_df,-offset,-chunksize)
#   return(sel_df)
# }

# h5ls_df <- function(h5filepath){
#   root_objs <- get_sub_obj(h5filepath =h5filepath)
#   bg_objs <- purrr::possibly(get_objs_h5,otherwise = NULL)
#   
#   node_objs <- purrr::map(root_objs,~paste0(ifelse(.x=="/","",.x),"/",bg_objs(h5filepath=h5filepath,groupname = .x)))
#   
# }

# 
# read_mat_h5 <- function(filename,groupname,dataname,offset_rows=0,offset_cols=0,chunksize_rows=NULL,chunksize_cols=NULL){
#   mat_dims <- get_dims_h5(filename,groupname,dataname)
#   stopifnot(length(mat_dims)==2)
#   if(is.null(chunksize_cols)){
#     chunksize_cols <- mat_dims[2]-offset_cols
#   }
#   if(is.null(chunksize_rows)){
#     chunksize_rows <- mat_dims[1]-offset_rows
#   }
#   return(read_matrix_h5(filename = filename,
#                         groupname = groupname,
#                         dataname = dataname,
#                         offsets = c(offset_rows,offset_cols),
#                         chunksizes = c(chunksize_rows,chunksize_cols)))
# }


## read_l_h5 <- function(filename,h5path="/",...){
##   all_objs <- ls_h5(filename,h5path,full_names = T)
##   names(all_objs) <- basename(all_objs)
##   purrr::map(all_objs,function(fp){
##     if(isGroup(filename,fp)){
##       return(read_l_h5(filename,fp))
##     }
##     md <- dims_h5(filename,fp)
##     if(length(md)>1){
##       return(read_matrix(filename,fp))
##     }
##     return(read_vector(filename,datapath = fp))
##   })
## }


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




delim2h5 <- function(input_file,output_file,h5_args=list(),...){
  h5_args[["append"]] <- TRUE
  wf <- function(x,pos){
    rlang::exec(write_df_h5,df=x,filename=output_file,!!!h5_args)}
  readr::read_delim_chunked(file = input_file,callback = readr::SideEffectChunkCallback$new(wf),...)
}






