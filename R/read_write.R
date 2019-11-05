read_df_h5 <- function(filename, datapath = "/", ...){
    
    filename <- fs::path_expand(filename)
    stopifnot(file.exists(filename))
    return(read_tibble_h5(filename,datapath,list(...)))
    dsets <- ls_h5(filename, groupname = datapath)
    argl <- list(...)
    if(!hasArg(subcols)){
        argl[["subcols"]] <- dsets
                                        # subcols <- dsets
    }
    dsets <- dsets[dsets %in% argl[["subcols"]]]
    dsp <- fix_paths(datapath, dsets)
    names(dsp) <- basename(dsp)
    if(!hasArg(subset)){
        return(purrr::map_dfc(dsp,~read_vector_h5v(filename = filename,datapath =  .x)))
    }else{
        return(purrr::map_dfc(dsp,~read_vector_h5v(filename = filename,datapath = .x,i=as.integer(argl[["subset"]]))))
    }
}



write_df_h5 <- function(df, filename, datapath="/", ...){
    argl <- list(...)
 if(datapath=="/"){
        datapath <- ""
    }
  purrr::iwalk(df, ~purrr::invoke(write_vector_h5,
                                  filename = filename,
                                  datapath = fix_paths(datapath, .y),
                                  data=.x,
                                  argl))
}



create_vector_h5 <- function(filename,datapath,data,...){
    argl <- list(...)
        filename <- fs::path_expand(filename)
  if(file.exists(filename)){
      stopifnot(!isObject_h5(filename,datapath))
  }
  create_dataset_h5(filename,datapath,data,argl)
}


create_matrix_h5 <- function(filename, datapath, data, ...){
    argl <- list(...)
    filename <- fs::path_expand(filename)
  if(file.exists(filename)){
    stopifnot(!isObject_h5(filename, datapath))
  }
  create_dataset_h5(filename = filename , datapath = datapath, data = data, options = argl)
}
  


write_vector_h5 <- function(data, filename, datapath, ...){
  argl <- list(...)
  ret <- FALSE
  app_v <- TRUE
  filename <- fs::path_expand(filename)
  if(!file.exists(filename)){
    if(isTRUE(argl[["append"]]) && is.null(argl[["max_dims"]])){
      argl[["max_dims"]] <- NA_integer_
    }
    create_dataset_h5(filename, datapath, data, argl)
    app_v <- FALSE
  }
  if(!isObject(filename, datapath)){
    if(isTRUE(argl[["append"]]) && is.null(argl[["max_dims"]])){
      argl[["max_dims"]] <- NA_integer_
    }
    create_dataset_h5(filename, datapath, data, argl)
  }else{
    if(app_v && isTRUE(argl[["append"]])){
      ret <- append_vector_h5(data = data,filename = filename, datapath = datapath,  ... = argl)
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
  filename <- fs::path_expand(filename)
  argl[["offset"]]<- get_dims_h5(filename, datapath)
  extend_dataset_by(filename, datapath, newdims = length(data))
  ret <- update_vector(data, filename, datapath, argl)
  return(ret)
}




write_matrix_h5 <- function(data, filename, datapath, ...){
    
    argl <- list(...)
    filename <- fs::path_expand(filename)
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
    filename <-fs::path_expand(filename)
    tr <- read_vector(filename =  filename,
                      datapath = datapath,
                      options = argl)

    return(tr)
}


read_matrix_h5 <- function(filename,datapath,...){
    argl <- list(...)
    retr <- read_matrix(filename =  fs::path_expand(filename),
                        datapath = datapath,
                        option = argl)
    return(retr)
}
