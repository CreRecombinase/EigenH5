read_matrix_h5v <- function(filename, datapath,i, j, ...){
    has.i <- !missing(i)
    has.j <- !missing(j)
    if (!has.j){
        j <- integer()
    }
    if (!has.i){
        i <- integer()
    }
    argl <- list(...)
    stopifnot(length(argl) == 0)

    return(read_matrix_v(filename, datapath, i, j))

}


read_vector_h5v <- function(filename, datapath, i, ...){
    has.i <- !missing(i)

    if (!has.i){
        i <- integer()
    }
    argl <- list(...)
    stopifnot(length(argl) == 0)

    return(read_vector_v(filename, datapath, i))

}




write_matrix_h5v <- function(data, filename, datapath, ...){
    
    argl <- list(...)
    filename <- fs::path_expand(filename)
    if(!file.exists(filename)){
        #   argl[["dim"]] <- argl[["dim"]] %||% dim(data)
        create_dataset_h5(filename,datapath,data,argl)
    }
    if(!isObject(filename,datapath)){
        create_dataset_h5(filename = filename ,datapath = datapath, data = data, options = argl)
    }
    i <- argl[["i"]] %||% 1:NROW(data)
    j <- argl[["j"]] %||% 1:NCOL(data)
    update_matrix_v(data,filename =  filename,datapath,i,j)
}



write_vector_h5v <- function(data, filename, datapath, ...){
    
    argl <- list(...)
    filename <- fs::path_expand(filename)
    if(!file.exists(filename)){
        #   argl[["dim"]] <- argl[["dim"]] %||% dim(data)
        create_dataset_h5(filename,datapath,data,argl)
    }
    if(!isObject(filename,datapath)){
        create_dataset_h5(filename = filename ,datapath = datapath, data = data, options = argl)
    }
    i <- argl[["i"]] %||% 1:NROW(data)
    #j <- argl[["j"]] %||% 1:ncol(data)
    update_vector_v(data,filename =  filename,datapath,i)
}





