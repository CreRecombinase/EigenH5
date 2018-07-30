
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

write_S4_h5 <- function(filename,datapath,data,...){
  stopifnot(isS4(data))
  sl <- slotNames(data)
  dl <- purrr::map(sl,~slot(data,.x))
  names(dl) <- sl
  write_l_h5(h5filepath = filename,datapath = datapath,datal = dl)
}

write_df_h5 <- function(df,groupname="/",filename,...){
  argl <- list(...)
  purrr::iwalk(df,~purrr::invoke(write_vector_h5,filename = filename,groupname=groupname,dataname=.y,data=.x,argl))
}


create_vector_h5 <- function(filename,groupname="/",dataname,data,...){
  
  argl <- list(...)

  
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }

  if(file.exists(filename)){
      stopifnot(!isObject_h5(filename,datapath))
  }

  create_dataset_h5(filename,datapath,data,argl)

}

update_matrix_h5 <- function(filename,datapath,data,...){
  stopifnot(file.exists(filename),
            isObject_h5(filename,datapath))
  
  
}



create_matrix_h5 <- function(filename,groupname="/",dataname,data,...){
  
  argl <- list(...)

  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }

  if(file.exists(filename)){
    stopifnot(!isObject_h5(filename,datapath))
  }
  create_dataset_h5(filename,datapath,data,argl)
}
  
append_vector_h5 <- function(filename,datapath,data,...){
  argl <- list(...)
  filename <- file.path(filename)
  argl[["offset"]]<- get_dims_h5(filename,datapath)
  
  extend_dataset_by(filename,datapath,newdims = length(data))
  ret <- update_vector(data,filename,datapath,argl)
  return(ret)
}



write_vector_h5 <- function(filename,groupname="/",dataname,data,...){
  argl <- list(...)
  # pre_ct <- file_acc_ct(filename)
  ret <- FALSE
  if(!hasArg(datapath)){
    datapath <- construct_data_path(groupname,dataname)
  }else{
    datapath <- argl[["datapath"]]
  }
  app_v <- TRUE
  if(!file.exists(filename)){
    create_dataset_h5(filename,datapath,data,argl)
    # post_ct <- file_acc_ct(filename)
    # stopifnot(all.equal(pre_ct,post_ct))
    app_v <- FALSE
  }
  if(!isObject(filename,datapath)){
    create_dataset_h5(filename,datapath,data,argl)
    # post_ct <- file_acc_ct(filename)
    # stopifnot(all.equal(pre_ct,post_ct))
  }else{
    if(hasArg(append)){
      if(app_v & argl[["append"]]){
        ret <- append_vector_h5(filename = filename,datapath = datapath,data = data,... = argl)
        # post_ct <- file_acc_ct(filename)
      }
    }
  }
  if(!ret){
    if(length(data)>0){
      ret <- update_vector(data,filename,datapath,argl)
      return(ret)
    }else{
    }
  }
}


write_matrix_h5 <- function(filename,groupname="/",dataname,data,...){
    
    argl <- list(...)
    if(!hasArg(datapath)){
        datapath <- construct_data_path(groupname,dataname)
    }else{
        datapath <- argl[["datapath"]]
    }
    if(!hasArg(datasizes)){
        if(hasArg(datasize)){
            argl[["datasizes"]] <- datasize
        }else{
            argl[["datasizes"]] <- dim(data)
        }
    }
    if(!file.exists(filename)){

        create_dataset_h5(filename,datapath,data,argl)

    }
    if(!isObject(filename,datapath)){

        create_dataset_h5(filename,datapath,data,argl)

    }
    


    retv <- update_matrix(data,filename =  filename,datapath,argl)


    return(retv)
}



read_vector_h5 <- function(filename,groupname="/",dataname,...){
    
    argl <- list(...)
    if(!hasArg(datapath)){
        datapath <- construct_data_path(groupname,dataname)
    }else{
        datapath <- argl[["datapath"]]
    }
    if(!is.null(argl[["filtervec"]])){
        argl[["subset"]] <- argl[["filtervec"]]
    }
    


    tr <- read_vector(filename =  filename,
                      datapath,argl)

    return(tr)
}


read_matrix_h5 <- function(filename,groupname="/",dataname,...){
    argl <- list(...)
    if(!hasArg(datapath)){
        datapath <- construct_data_path(groupname,dataname)
    }else{
        datapath <- argl[["datapath"]]
    }

    retr <- read_matrix(filename =  filename,datapath,argl)
    return(retr)
}






