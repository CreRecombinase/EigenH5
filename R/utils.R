.onLoad <- function(libname, pkgname){
    start_blosc()
    # start_singleton()
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

## gen_matslice_chunk_l <- function(input_rows,chunksize=10000,...){
##     output_l <- BBmisc::chunk(input_rows,chunk.size=chunksize)  %>% purrr::map(~tibble::data_frame(input_rows=.x)afunction(x){
##       stopifnot(all(x==seq(x[1],x[length(x)])))
##       return(data_frame(row_offsets=x[1]-1,row_chunksizes=length(x)))
##       }) %>% dplyr::mutate(...)
##     return(input_df)
## }


gen_matslice_df <- function(filename,group_prefix,dataname){
  sub_grps <- ls_h5(filename,group_prefix)
  retdf <- dplyr::data_frame(filenames=filename,
                    groupnames=paste0(group_prefix,"/",sub_grps),
                    datanames=dataname) %>% arrange(as.integer(sub_grps))
  return(retdf)
}



get_dims_h5 <- function(f,...){
  return(dim_h5(f,construct_data_path(...)))
}

write_h5 <- function(h5filepath,datapath,data,offset=0L,subsets=list(subset_rows=integer(),subset_cols=integer())){
  if(is.list(data)){
    write_l_h5(h5filepath=h5filepath,datapath=datapath,datal=data)
  }else{
    if(is.vector(data)){
      write_vector(h5filepath = h5filepath,datapath=datapath,data = data,offset=offset,subset = subsets[["subset_rows"]])
    }else{
      if(is.matrix(data)){
        write_matrix(h5filepath = h5filepath,datapath=datapath,data = data,
                     subset_rows = subsets[["subset_rows"]],
                     subset_cols = subsets[["subset_cols"]])
      }else{
        stop("data is of unknown type!")
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

write_l_h5 <- function(h5filepath,datapath,datal){
  stopifnot(is.list(datal))
  if(datapath=="/"){
    datapath <- ""
  }
  purrr::iwalk(datal,~write_h5(h5filepath,normalizePath(paste(datapath,.y,sep="/"),mustWork = F),data = .x))
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


