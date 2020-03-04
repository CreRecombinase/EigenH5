rhdf5 <- suppressWarnings(require(Rhdf5lib, quietly = TRUE, character.only = FALSE, warn.conflicts = FALSE))
if(rhdf5){
    Rhdf5lib::
fh5cc <- system2("which", "h5cc")
