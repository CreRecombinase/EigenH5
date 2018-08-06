
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
  dsp <- normalizePath(paste(datapath,dsets,sep="/"), mustWork = F)
  names(dsp) <- basename(dsp)
  return(purrr::map_dfc(dsp,~read_vector_h5(filename, datapath = .x, ...), ...=...))
}


write_df_h5 <- function(df, filename, datapath="/", ...){
  argl <- list(...)
  purrr::iwalk(df, ~purrr::invoke(write_vector_h5,
                                  filename = filename,
                                  datapath = normalizePath(paste(datapath, .y, sep="/"),mustWork = F),
                                  data=.x,
                                  argl))
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

create_matrix_h5 <- function(filename, datapath, data, ...){
  argl <- list(...)
  if(file.exists(filename)){
    stopifnot(!isObject_h5(filename, datapath))
  }
  create_dataset_h5(filename = filename , datapath = datapath, data = data, options = argl)
}
  


write_vector_h5 <- function(data, filename, datapath, ...){
  argl <- list(...)
  ret <- FALSE
  app_v <- TRUE
  if(!file.exists(filename)){
    create_dataset_h5(filename, datapath, data, argl)
    app_v <- FALSE
  }
  if(!isObject(filename, datapath)){
    create_dataset_h5(filename, datapath, data, argl)
  }else{
    if(hasArg(append)){
      if(app_v & argl[["append"]]){
        ret <- append_vector_h5(data = data,filename = filename, datapath = datapath,  ... = argl)
      }
    }
  }
  if(!ret){
      if(length(data)>0){
          ret <- update_vector(data = data, filename = filename, datapath = datapath, options = argl)
          return(ret)
      }else{
      }
  }
}


append_vector_h5 <- function(data, filename, datapath, ...){
  argl <- list(...)
  filename <- file.path(filename)
  argl[["offset"]]<- get_dims_h5(filename, datapath)
  extend_dataset_by(filename, datapath, newdims = length(data))
  ret <- update_vector(data, filename, datapath, argl)
  return(ret)
}




write_matrix_h5 <- function(data, filename, datapath, ...){
    
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
