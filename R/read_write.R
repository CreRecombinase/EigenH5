
read_df_h5 <- function(filename,datapath,...){
  stopifnot(file.exists(filename))
  dsets <- ls_h5(filename,groupname = datapath)
  argl <- list(...)
  if(!hasArg(subcols)){
    argl[["subcols"]] <- dsets
    # subcols <- dsets
  }
  dsets <- dsets[dsets %in% argl[["subcols"]]]
  if(datapath=="/"){
    groupname <- ""
  }
  dsp <- normalizePath(paste(datapath,dsets,sep="/"),mustWork = F)
  names(dsp) <- basename(dsp)
  return(purrr::map_dfc(dsp,~read_vector_h5(filename,datapath = .x,...),...=...))
}


write_df_h5 <- function(df,filename,datapath="/",...){
  purrr::iwalk(df,~write_vector_h5(filename = filename,datapath = normalizePath(paste(datapath,.y,sep="/"),mustWork = F),data=.x,...))
}


create_vector_h5 <- function(filename,datapath,data,...){
  argl <- list(...)
  if(file.exists(filename)){
      stopifnot(!isObject_h5(filename,datapath))
  }
  create_dataset_h5(filename,datapath,data,argl)
}

update_matrix_h5 <- function(filename,datapath,data,...){
  stopifnot(file.exists(filename),
            isObject_h5(filename,datapath))
}

create_matrix_h5 <- function(filename,datapath,data,...){
  argl <- list(...)
  if(file.exists(filename)){
    stopifnot(!isObject_h5(filename,datapath))
  }
  create_dataset_h5(filename = filename ,datapath = datapath , data = data , options = argl)
}
  

write_vector_h5 <- function(data,filename,datapath,...){
  argl <- list(...)
  if(!file.exists(filename)){
    create_dataset_h5(filename,datapath,data,argl)
  }
  if(!isObject(filename,datapath)){
    create_dataset_h5(filename,datapath,data,argl)
  }
  ret <- update_vector(data,filename,datapath,argl)
  return(ret)
}


write_matrix_h5 <- function(data,filename,datapath,...){
    
    argl <- list(...)
    if(!file.exists(filename)){
        create_dataset_h5(filename,datapath,data,argl)
    }
    if(!isObject(filename,datapath)){
        create_dataset_h5(filename = filename ,datapath = datapath, data = data, options = argl)
    }
    retv <- update_matrix(data,filename =  filename,datapath,argl)
    return(retv)
}



read_vector_h5 <- function(filename,datapath,...){
    
    argl <- list(...)
    if(!is.null(argl[["filtervec"]])){
        argl[["subset"]] <- argl[["filtervec"]]
    }
    tr <- read_vector(filename =  filename,
                      datapath = datapath,
                      options = argl)

    return(tr)
}


read_matrix_h5 <- function(filename,datapath,...){
    argl <- list(...)
    retr <- read_matrix(filename =  filename,
                        datapath = datapath,
                        option = argl)
    return(retr)
}
