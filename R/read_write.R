
read_df_h5 <- function(filename,groupname,...){
  stopifnot(file.exists(filename))
  dsets <- ls_h5(filename,groupname = groupname)
  if(hasArg(subcols)){
    dsets <- dsets[dsets %in% subcols]
  }
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
  stopifnot(!isObject(filename,datapath))
    create_dataset(filename,datapath,data,argl)

}

create_matrix_h5 <- function(filename,groupname="/",dataname,data,...){
  
  argl <- list(...)
  ssr <- FALSE
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  stopifnot(!isObject(filename,datapath))
  create_dataset(filename,datapath,data,argl)

}
  

write_vector_h5 <- function(filename,groupname="/",dataname,data,...){
  argl <- list(...)
  ssr <- FALSE
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  
  if(!isObject(filename,datapath)){
    create_dataset(filename,datapath,data,argl)
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
    return(update_vector(data[ossr],filename,datapath,argl))
  }
  return(update_vector(data,filename,datapath,argl))
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
  if(ssr){
    return(read_vector(filename =  filename,datapath,argl)[ossr])
  }
  return(read_vector(filename =  filename,
                     datapath,argl))
}


read_matrix_h5 <- function(filename,groupname="/",dataname,...){
  argl <- list(...)
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
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
    return(read_matrix(filename =  filename,datapath,argl)[ossr,ossc])
  }
  if(ssr){
    return(read_matrix(filename =  filename,datapath,argl)[ossr,])
  }
  if(ssc){
    return(read_matrix(filename =  filename,datapath,argl)[,ossc])
  }
  return(read_matrix(filename =  filename,datapath,argl))
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
  
  ssr <- FALSE
  ssc <- FALSE
  if(!isObject(filename,datapath)){
    create_dataset(filename,datapath,data,argl)
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
    return(update_matrix(data[ossr,ossc,drop=F],filename =  filename,datapath,argl))
  }
  if(ssr){
    return(update_matrix(data[ossr,,drop=F],filename =  filename,datapath,argl))
  }
  if(ssc){
    return(update_matrix(data[,ossc,drop=F],filename =  filename,datapath,argl))
  }
  return(update_matrix(data,filename =  filename,datapath,argl))
}

