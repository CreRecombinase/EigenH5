# Prepare your package for installation here.
# Use 'define()' to define configuration variables.
# Use 'configure_file()' to substitute configuration values.
rhdf5 <- suppressWarnings(require(Rhdf5lib, quietly = TRUE, character.only = FALSE, warn.conflicts = FALSE))
if (rhdf5) {
        HDF5R_CPPFLAGS <- paste0(
                "-I",
                system.file("include",
                        package = "Rhdf5lib",
                        mustWork = TRUE
                )
        )
        HDF5R_LIBS <- capture.output(Rhdf5lib::pkgconfig("PKG_C_HL_LIBS"))
}else{
    h5cc <- suppressWarnings(system2("which", "h5cc", stdout = TRUE, stderr = FALSE))
    if (h5cc == "") {
            stop(
                    "could not find an HDF5 installation.",
                    "(tried both h5cc and Rhdf5lib).",
                    "\nPlease either install Rhdf5lib",
                    "(BiocManager::install(\"Rhdf5lib\"))",
                    "or install HDF5 using either your ",
                    "system package manager, or install ",
                    "from source at  ",
                    "https://www.hdfgroup.org/downloads/hdf5/",
                    "(login is required, but no payment is necessary)"
            )
    }
    h5cc_args <- system2(h5cc, "-show", stdout = TRUE)
    R_cc <- read_r_config("CC")$CC
    ac <- read_r_config("--all")
    rgcc_v <- system(paste0(R_cc, " --version"), intern = TRUE)
    hgcc_v <- system(paste0(h5cc, " --version"), intern = TRUE)
    if (!all.equal(rgcc_v, hgcc_v)) {
            warning(
                    "It looks like R and hdf5 might have been built with different compilers,\n$(R CMD config CC) --version\n: ",
                    paste0(rgcc_v, collapse = "\n"),
                    "vs h5cc --version\n:",
                    paste0(hgcc_v, collapse = "\n")
            )
    }
#    deep_config <-  system2("R", "CMD libtool --config"
    h5cc_arg_v <- unlist(strsplit(h5cc_args, " "))
    static_libs <- h5cc_arg_v[grepl("\\.a$", h5cc_arg_v)]
    dyn_libs <- h5cc_arg_v[grepl("^-l", h5cc_arg_v)]
    if (any(grepl("libhdf5_hl", static_libs)) && any(grepl("libhdf5\\.a", static_libs))) {
        HDF5R_LIBS <- paste0(c(static_libs, dyn_libs), collapse = " ")
    }else{
        stop("Dynamic linking of HDF5 libraries is not configured yet, (please file a github issue if you need this)")
    }
    HDF5R_CPPFLAGS <- h5cc_arg_v[grepl("^-I", h5cc_arg_v)]
    if(length(HDF5R_CPPFLAGS)==0)
        HDF5R_CPPFLAGS <- " "
}
HDF5R_CPPFLAGS <- paste0(HDF5R_CPPFLAGS, " -D_LIBCPP_DISABLE_AVAILABILITY")

define(
        HDF5R_CPPFLAGS = HDF5R_CPPFLAGS,
        HDF5R_LIBS = HDF5R_LIBS,
        HDF5R_CFLAGS = " "
)
configure_file("src/Makevars.in")
