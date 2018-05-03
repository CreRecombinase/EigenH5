
read_df_h5 <- function(filename,groupname,...){
  stopifnot(file.exists(filename))
  dsets <- ls_h5(filename,groupname = groupname)
  argl <- list(...)
  if(!hasArg(subcols)){
    argl[["subcols"]] <- dsets
    # subcols <- dsets
  }
  dsets <- dsets[dsets %in% argl[["subcols"]]]
  if(groupname=="/"){
    groupname <- ""
  }

  # argl[["filename"]] <- filename
  dsp <- normalizePath(paste(groupname,dsets,sep="/"),mustWork = F)
  names(dsp) <- basename(dsp)
  return(purrr::map_dfc(dsp,~read_vector_h5(filename,datapath = .x,...),...=...))
  #return(tibble::as_data_frame(read_l_h5(filename = normalizePath(filename),groupname = groupname,subcols = subcols,offset = offset,chunksize = chunksize,filtervec = filtervec)))
}


write_df_h5 <- function(df,groupname="/",filename,...){
  # stopifnot(file.exists(filename))
  purrr::iwalk(df,~write_vector_h5(filename = filename,datapath = normalizePath(paste(groupname,.y,sep="/"),mustWork = F),data=.x,...))
}


create_vector_h5 <- function(filename,groupname="/",dataname,data,...){
  
  argl <- list(...)
  ssr <- FALSE
  
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  tf <- lockf(filename)
  if(!hasArg(timeout)){
    timeout <- Inf
  }
  if(file.exists(filename)){
      stopifnot(!isObject_h5(filename,datapath))
  }
  ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
  create_dataset_h5(filename,datapath,data,argl)
  filelock::unlock(ml)
}

update_matrix_h5 <- function(filename,datapath,data,...){
  stopifnot(file.exists(filename),
            isObject_h5(filename,datapath))
  
  
}



create_matrix_h5 <- function(filename,groupname="/",dataname,data,...){
  
  argl <- list(...)
  ssr <- FALSE
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  tf <- lockf(filename)
  if(!hasArg(timeout)){
    timeout <- Inf
  }
  if(file.exists(filename)){
    stopifnot(!isObject_h5(filename,datapath))
  }
  ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
  create_dataset_h5(filename,datapath,data,argl)
  filelock::unlock(ml)
}
  

write_vector_h5 <- function(filename,groupname="/",dataname,data,...){
  argl <- list(...)
  ssr <- FALSE
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  tf <- lockf(filename)
  if(!hasArg(timeout)){
    timeout <- Inf
  }
  
  if(!file.exists(filename)){
    ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
    create_dataset_h5(filename,datapath,data,argl)
    filelock::unlock(ml)
  }
  if(!isObject(filename,datapath)){
    ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
    create_dataset_h5(filename,datapath,data,argl)
    filelock::unlock(ml)
  }
  
  
  if(!is.null(argl[["subset"]])){
    ssr <- T
    stopifnot(!any(duplicated(argl[["subset"]])))
    ossr <- order(argl[["subset"]])
    
    argl[["subset"]] <- sort(argl[["subset"]])
  }
  # 
  # if(ssc && ssr){
  #   stop("arguments subset_rows and subset_cols cannot both be specified in `write_vector`")
  # }
  if(ssr){
    ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
    ret <- update_vector(data[ossr],filename,datapath,argl)
    filelock::unlock(ml)
  }else{
    ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
    ret <- update_vector(data,filename,datapath,argl)
    filelock::unlock(ml)
  }
  return(ret)
}


write_matrix_h5 <- function(filename,groupname="/",dataname,data,...){
  
  argl <- list(...)
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  if(!hasArg(chunksizes)){
    if(hasArg(chunksize)){
      argl[["chunksizes"]] <- chunksize
    }else{
      argl[["chunksizes"]] <- dim(data)
    }
  }
  tf <- lockf(filename)
  if(!hasArg(timeout)){
    timeout <- Inf
  }
  
  ssr <- FALSE
  ssc <- FALSE
  if(!file.exists(filename)){
    ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
    create_dataset_h5(filename,datapath,data,argl)
    filelock::unlock(ml)
  }
  if(!isObject(filename,datapath)){
    ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
    create_dataset_h5(filename,datapath,data,argl)
    filelock::unlock(ml)
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
  
  
  ml <- filelock::lock(tf,exclusive = T,timeout=timeout)
  if(ssr && ssc){
    retv <- update_matrix(data[ossr,ossc,drop=F],filename =  filename,datapath,argl)
  }else{
    if(ssr){
      retv <- update_matrix(data[ossr,,drop=F],filename =  filename,datapath,argl)
    }else{
      if(ssc){
        retv <- update_matrix(data[,ossc,drop=F],filename =  filename,datapath,argl)
      }else{
        retv <- update_matrix(data,filename =  filename,datapath,argl)
      }
    }
  }
  filelock::unlock(ml)
  return(retv)
}



read_vector_h5 <- function(filename,groupname="/",dataname,...){
  
  argl <- list(...)
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  ssr <- FALSE
  if(!is.null(argl[["subset"]])){
    ssr <- T
    stopifnot(!any(duplicated(argl[["subset"]])))
    ossr <- order(argl[["subset"]])
    argl[["subset"]] <- sort(argl[["subset"]])
    
  }else{
    if(!is.null(argl[["filtervec"]])){
      ssr <- T
      stopifnot(!any(duplicated(argl[["filtervec"]])))
      ossr <- order(argl[["filtervec"]])
      argl[["filtervec"]] <- sort(argl[["filtervec"]])
    }
  }
  tf <- lockf(filename)
  if(!hasArg(timeout)){
    timeout <- Inf
  }
  ml <- filelock::lock(tf,exclusive = F,timeout=timeout)
  if(ssr){
    tr <- read_vector(filename =  filename,datapath,argl)[ossr]
  }else{
  tr <- read_vector(filename =  filename,
                     datapath,argl)
  }
  filelock::unlock(ml)
  return(tr)
}


read_matrix_h5 <- function(filename,groupname="/",dataname,...){
  argl <- list(...)
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  tf <- lockf(filename)
  if(!hasArg(timeout)){
    timeout <- Inf
  }
  ml <- filelock::lock(tf,exclusive = F,timeout=timeout)
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
    retr <- read_matrix(filename =  filename,datapath,argl)[ossr,ossc,drop=F]
  }else{
    if(ssr){
    retr <- read_matrix(filename =  filename,datapath,argl)[ossr,,drop=F]
    }else{
      if(ssc){
        retr <- read_matrix(filename =  filename,datapath,argl)[,ossc,drop=F]
      }else{
        retr <- read_matrix(filename =  filename,datapath,argl)
      }
    }
  }
  filelock::unlock(ml)
  return(retr)
}






