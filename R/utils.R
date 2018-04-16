.onLoad <- function(libname, pkgname){
  start_blosc()
}

read_df_h5 <- function(h5filepath,groupname,subcols = character(),filtervec = integer(),offset=c(0L),chunksize=NA_integer_){
  stopifnot(file.exists(h5filepath))
  dsets <- ls_h5(h5filepath,groupname = groupname)
  if(length(subcols)>0){
    dsets <- dsets[dsets %in% subcols]
  }
  if(groupname=="/"){
    groupname <- ""
  }
  dsp <- normalizePath(paste(groupname,dsets,sep="/"),mustWork = F)
  names(dsp) <- basename(dsp)
  return(purrr::map_dfc(dsp,~read_vector(h5filepath,datapath = .x,offset = offset,chunksize = chunksize,subset = filtervec)))
  #return(tibble::as_data_frame(read_l_h5(h5filepath = normalizePath(h5filepath),groupname = groupname,subcols = subcols,offset = offset,chunksize = chunksize,filtervec = filtervec)))
}


construct_data_path <- function(...){
    arguments <- list(...)
    retpath <- gsub("^/","",paste(arguments,collapse="/"))
    retpath <- gsub("//","/",retpath)
    return(retpath)
}

gen_matslice_chunk_df <- function(input_rows,chunksize=10000,...){
    input_df <- BBmisc::chunk(input_rows,chunk.size=chunksize) %>% purrr::map_df(~data_frame(row_offsets=.x[1]-1,row_chunksizes=length(.x))) %>% dplyr::mutate(...)
    return(input_df)
}


gen_matslice_df <- function(filename,group_prefix,dataname){
  sub_grps <- ls_h5(filename,group_prefix)
  retdf <- dplyr::data_frame(filenames=filename,
                    groupnames=paste0(group_prefix,"/",sub_grps),
                    datanames=dataname) %>% arrange(as.integer(sub_grps))
  return(retdf)
}

write_df_h5 <- function(df,groupname="/",h5filepath){
  # stopifnot(file.exists(h5filepath))
  purrr::iwalk(df,~write_vector(h5filepath = h5filepath,datapath = normalizePath(paste(groupname,.y,sep="/"),mustWork = F),data=.x))
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




write_vector_h5 <- function(data,h5filepath,datapath,...){
    argl <- list(...)
    ssr <- FALSE
    ssc <- FALSE
    
    if(!isObject(h5filepath,datapath)){
        create_dataset(h5filepath,datapath,data,argl)
    }
    
    if(!is.null(argl[["subset_rows"]])){
        ssr <- T
        stopifnot(!any(duplicated(argl[["subset_rows"]])))
        ossr <- order(argl[["subset_rows"]])

        argl[["subset_rows"]] <- sort(argl[["subset_rows"]])

    }
    if(!is.null(argl[["subset_cols"]])){
        ssc <- T
        stopifnot(!any(duplicated(argl[["subset_cols"]])))
        ossc <- order(argl[["subset_cols"]])
        argl[["subset_cols"]] <- sort(argl[["subset_cols"]])
    }
    if(ssc && ssr){
        stop("arguments subset_rows and subset_cols cannot both be specified in `write_vector`")
    }
    if(ssr){
        return(update_vector(data[ossr],h5filepath,datapath,argl))
    }
    if(ssc){
        return(update_vector(data[ossc],h5filepath,datapath,argl))
    }
    return(update_vector(data,h5filepath,datapath,argl))
}


read_matrix_h5 <- function(h5filepath,datapath,...){
    argl <- list(...)
    ssr <- FALSE
    ssc <- FALSE
    if(!is.null(argl[["subset_rows"]])){
        ssr <- T
        stopifnot(!any(duplicated(argl[["subset_rows"]])))
        ossr <- order(argl[["subset_rows"]])

        argl[["subset_rows"]] <- sort(argl[["subset_rows"]])

    }
    if(!is.null(argl[["subset_cols"]])){
        ssc <- T
        stopifnot(!any(duplicated(argl[["subset_cols"]])))
        ossc <- order(argl[["subset_cols"]])
        argl[["subset_cols"]] <- sort(argl[["subset_cols"]])
    }
    if(ssr && ssc){
        return(read_matrix(filename =  h5filepath,datapath,argl)[ossr,ossc])
    }
    if(ssr){
        return(read_matrix(filename =  h5filepath,datapath,argl)[ossr,])
    }
    if(ssc){
        return(read_matrix(filename =  h5filepath,datapath,argl)[,ossc])
    }
    return(read_matrix(filename =  h5filepath,datapath,argl))
}


read_vector_h5 <- function(h5filepath,datapath,...){
    argl <- list(...)
    ssr <- FALSE
    ssc <- FALSE
    if(!is.null(argl[["subset_rows"]])){
        ssr <- T
        stopifnot(!any(duplicated(argl[["subset_rows"]])))
        ossr <- order(argl[["subset_rows"]])

        argl[["subset_rows"]] <- sort(argl[["subset_rows"]])

    }
    if(!is.null(argl[["subset_cols"]])){
        ssc <- T
        stopifnot(!any(duplicated(argl[["subset_cols"]])))
        ossc <- order(argl[["subset_cols"]])
        argl[["subset_cols"]] <- sort(argl[["subset_cols"]])
    }
    if(ssr && ssc){
        stop("arguments subset_rows and subset_cols cannot both be specified in `write_vector`")
    }
    if(ssr){
        return(read_vector(filename =  h5filepath,datapath,argl)[ossr])
    }
    if(ssc){
        return(read_vector(filename =  h5filepath,datapath,argl)[ossc])
    }
    return(read_vector(filename =  h5filepath,
                       datapath,argl))
}




write_matrix_h5 <- function(data,h5filepath,datapath,...){
    argl <- list(...)
    ssr <- FALSE
    ssc <- FALSE
    if(!isObject(h5filepath,datapath)){
        create_dataset(h5filepath,datapath,data,argl)
    }
    
    if(!is.null(argl[["subset_rows"]])){
        ssr <- T
        stopifnot(!any(duplicated(argl[["subset_rows"]])))
        ossr <- order(argl[["subset_rows"]])

        argl[["subset_rows"]] <- sort(argl[["subset_rows"]])

    }
    if(!is.null(argl[["subset_cols"]])){
        ssc <- T
        stopifnot(!any(duplicated(argl[["subset_cols"]])))
        ossc <- order(argl[["subset_cols"]])
        argl[["subset_cols"]] <- sort(argl[["subset_cols"]])
    }
    ## offsets=c(0L,0L),chunksizes=c(NA_integer_,NA_integer_),
    ## subset_rows=as.integer(c()),
    ## subset_cols=as.integer(c())){
    if(ssr && ssc){
        return(update_matrix(data[ossr,ossc],filename =  h5filepath,datapath,argl))
    }
    if(ssr){
        return(update_matrix(data[ossr,],filename =  h5filepath,datapath,argl))
    }
    if(ssc){
        return(update_matrix(data[,ossc],filename =  h5filepath,datapath,argl))
    }
    return(update_matrix(data,filename =  h5filepath,datapath,argl))
}





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


read_l_h5 <- function(filename,h5path="/"){
  all_objs <- ls_h5(filename,h5path,full_names = T)
  names(all_objs) <- basename(all_objs)
  purrr::map(all_objs,function(fp){
    if(isGroup(filename,fp)){
      return(read_l_h5(filename,fp))
    }
    md <- dims_h5(filename,fp)
    if(length(md)>1){
      return(read_matrix(filename,fp))
    }
    return(read_vector(h5filepath = filename,datapath = fp))
  })
}

read_h5 <- function(filename,h5path="/",subset=list()){
  groupname <- dirname(h5path)
  dataname <- basename(h5path)
  if(dataname==""){
    dataname <- groupname
  }
  if(isGroup(filename = filename,groupname = h5path)){
    read_l_h5(filename,h5path=h5path)
  }else{
    get_dims_h5()
  }
  objs <- ls_h5(filename,groupname=groupname,full_names = T)
  groups <- purrr::map_lgl(objs,~isGroup(filename,.x))
  
  # obj_dim <- get_dims_h5(filename,groupname,dataname)

  isvec <- length(obj_dim)==1
  if(isvec){
    stopifnot(is.null(subset_cols))
    return(read_vector_h5(filename,groupname,dataname,filtervec=subset_rows))
  }else{
    if(is.null(subset_rows)){
      subset_rows <- integer()
    }
    if(is.null(subset_cols)){
      subset_cols <- integer()
    }
    return(read_matrix_h5(filename,groupname,dataname,subset_rows = subset_rows,subset_cols=subset_cols))
  }
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


