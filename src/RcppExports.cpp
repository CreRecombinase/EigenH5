// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/EigenH5.h"
#include <RcppEigen.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// start_blosc
void start_blosc();
static SEXP _EigenH5_start_blosc_try() {
BEGIN_RCPP
    start_blosc();
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_start_blosc() {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_start_blosc_try());
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// check_blosc
bool check_blosc();
static SEXP _EigenH5_check_blosc_try() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(check_blosc());
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_check_blosc() {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_check_blosc_try());
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// create_vector_h5
void create_vector_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, const int dimension, const int chunksize);
static SEXP _EigenH5_create_vector_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dimensionSEXP, SEXP chunksizeSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< const int >::type dimension(dimensionSEXP);
    Rcpp::traits::input_parameter< const int >::type chunksize(chunksizeSEXP);
    create_vector_h5(filename, groupname, dataname, dimension, chunksize);
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_create_vector_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dimensionSEXP, SEXP chunksizeSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_create_vector_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, dimensionSEXP, chunksizeSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// is_transposed
bool is_transposed(const std::string filename, const std::string groupname, const std::string dataname);
static SEXP _EigenH5_is_transposed_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string >::type dataname(datanameSEXP);
    rcpp_result_gen = Rcpp::wrap(is_transposed(filename, groupname, dataname));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_is_transposed(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_is_transposed_try(filenameSEXP, groupnameSEXP, datanameSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// write_vec_chunk_h5
void write_vec_chunk_h5(std::string filename, std::string groupname, std::string dataname, Eigen::VectorXd& data, const Rcpp::IntegerVector offsets, const Rcpp::IntegerVector chunksizes);
static SEXP _EigenH5_write_vec_chunk_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< std::string >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< std::string >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< Eigen::VectorXd& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type offsets(offsetsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type chunksizes(chunksizesSEXP);
    write_vec_chunk_h5(filename, groupname, dataname, data, offsets, chunksizes);
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_write_vec_chunk_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_write_vec_chunk_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, dataSEXP, offsetsSEXP, chunksizesSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// data_exists
bool data_exists(const std::string& filename, const std::string& groupname, const std::string& dataname);
static SEXP _EigenH5_data_exists_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    rcpp_result_gen = Rcpp::wrap(data_exists(filename, groupname, dataname));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_data_exists(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_data_exists_try(filenameSEXP, groupnameSEXP, datanameSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// check_dtype
SEXPTYPE check_dtype(const std::string& filename, const std::string& groupname, const std::string& dataname);
static SEXP _EigenH5_check_dtype_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    rcpp_result_gen = Rcpp::wrap(check_dtype(filename, groupname, dataname));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_check_dtype(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_check_dtype_try(filenameSEXP, groupnameSEXP, datanameSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// read_vector_h5
SEXP read_vector_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, Rcpp::IntegerVector offset, Rcpp::IntegerVector chunksize, Rcpp::IntegerVector filtervec);
static SEXP _EigenH5_read_vector_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP offsetSEXP, SEXP chunksizeSEXP, SEXP filtervecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type chunksize(chunksizeSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type filtervec(filtervecSEXP);
    rcpp_result_gen = Rcpp::wrap(read_vector_h5(filename, groupname, dataname, offset, chunksize, filtervec));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_read_vector_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP offsetSEXP, SEXP chunksizeSEXP, SEXP filtervecSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_read_vector_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, offsetSEXP, chunksizeSEXP, filtervecSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// get_dims_h5
Rcpp::IntegerVector get_dims_h5(const std::string& filename, const std::string& groupname, const std::string& dataname);
static SEXP _EigenH5_get_dims_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    rcpp_result_gen = Rcpp::wrap(get_dims_h5(filename, groupname, dataname));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_get_dims_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_get_dims_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// cont_diff
Rcpp::DataFrame cont_diff(Rcpp::IntegerVector inp, int chunksize);
static SEXP _EigenH5_cont_diff_try(SEXP inpSEXP, SEXP chunksizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type inp(inpSEXP);
    Rcpp::traits::input_parameter< int >::type chunksize(chunksizeSEXP);
    rcpp_result_gen = Rcpp::wrap(cont_diff(inp, chunksize));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_cont_diff(SEXP inpSEXP, SEXP chunksizeSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_cont_diff_try(inpSEXP, chunksizeSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// cont_reg
Rcpp::DataFrame cont_reg(Rcpp::IntegerVector input, int chunksize);
static SEXP _EigenH5_cont_reg_try(SEXP inputSEXP, SEXP chunksizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type input(inputSEXP);
    Rcpp::traits::input_parameter< int >::type chunksize(chunksizeSEXP);
    rcpp_result_gen = Rcpp::wrap(cont_reg(input, chunksize));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_cont_reg(SEXP inputSEXP, SEXP chunksizeSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_cont_reg_try(inputSEXP, chunksizeSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// multi_array_variant
void multi_array_variant(SEXP input_mat);
static SEXP _EigenH5_multi_array_variant_try(SEXP input_matSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< SEXP >::type input_mat(input_matSEXP);
    multi_array_variant(input_mat);
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_multi_array_variant(SEXP input_matSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_multi_array_variant_try(input_matSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// read_matrix_h5
SEXP read_matrix_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, const Rcpp::IntegerVector offsets, const Rcpp::IntegerVector chunksizes, const Rcpp::IntegerVector subset_rows, const Rcpp::IntegerVector subset_cols);
static SEXP _EigenH5_read_matrix_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP, SEXP subset_rowsSEXP, SEXP subset_colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type offsets(offsetsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type chunksizes(chunksizesSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type subset_rows(subset_rowsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type subset_cols(subset_colsSEXP);
    rcpp_result_gen = Rcpp::wrap(read_matrix_h5(filename, groupname, dataname, offsets, chunksizes, subset_rows, subset_cols));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_read_matrix_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP, SEXP subset_rowsSEXP, SEXP subset_colsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_read_matrix_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, offsetsSEXP, chunksizesSEXP, subset_rowsSEXP, subset_colsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// read_array_h5
SEXP read_array_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, const Rcpp::IntegerVector offsets, const Rcpp::IntegerVector chunksizes);
static SEXP _EigenH5_read_array_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type offsets(offsetsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type chunksizes(chunksizesSEXP);
    rcpp_result_gen = Rcpp::wrap(read_array_h5(filename, groupname, dataname, offsets, chunksizes));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_read_array_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_read_array_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, offsetsSEXP, chunksizesSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// write_vector_h5
void write_vector_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, SEXP data);
static SEXP _EigenH5_write_vector_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    write_vector_h5(filename, groupname, dataname, data);
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_write_vector_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_write_vector_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, dataSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// create_matrix_h5
void create_matrix_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, SEXP data, const bool doTranspose, const Rcpp::IntegerVector dims, const Rcpp::IntegerVector chunksizes);
static SEXP _EigenH5_create_matrix_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP, SEXP doTransposeSEXP, SEXP dimsSEXP, SEXP chunksizesSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const bool >::type doTranspose(doTransposeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type dims(dimsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type chunksizes(chunksizesSEXP);
    create_matrix_h5(filename, groupname, dataname, data, doTranspose, dims, chunksizes);
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_create_matrix_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP, SEXP doTransposeSEXP, SEXP dimsSEXP, SEXP chunksizesSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_create_matrix_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, dataSEXP, doTransposeSEXP, dimsSEXP, chunksizesSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// write_matrix_h5
void write_matrix_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, SEXP data, const bool doTranspose, const Rcpp::IntegerVector offsets, const Rcpp::IntegerVector chunksizes);
static SEXP _EigenH5_write_matrix_h5_try(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP, SEXP doTransposeSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< const std::string& >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type dataname(datanameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const bool >::type doTranspose(doTransposeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type offsets(offsetsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type chunksizes(chunksizesSEXP);
    write_matrix_h5(filename, groupname, dataname, data, doTranspose, offsets, chunksizes);
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_write_matrix_h5(SEXP filenameSEXP, SEXP groupnameSEXP, SEXP datanameSEXP, SEXP dataSEXP, SEXP doTransposeSEXP, SEXP offsetsSEXP, SEXP chunksizesSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_write_matrix_h5_try(filenameSEXP, groupnameSEXP, datanameSEXP, dataSEXP, doTransposeSEXP, offsetsSEXP, chunksizesSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// write_df_h5
bool write_df_h5(Rcpp::DataFrame& df, const std::string groupname, const std::string outfile, Rcpp::IntegerVector deflate_level);
static SEXP _EigenH5_write_df_h5_try(SEXP dfSEXP, SEXP groupnameSEXP, SEXP outfileSEXP, SEXP deflate_levelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame& >::type df(dfSEXP);
    Rcpp::traits::input_parameter< const std::string >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< const std::string >::type outfile(outfileSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type deflate_level(deflate_levelSEXP);
    rcpp_result_gen = Rcpp::wrap(write_df_h5(df, groupname, outfile, deflate_level));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_write_df_h5(SEXP dfSEXP, SEXP groupnameSEXP, SEXP outfileSEXP, SEXP deflate_levelSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_write_df_h5_try(dfSEXP, groupnameSEXP, outfileSEXP, deflate_levelSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// get_objs_h5
Rcpp::StringVector get_objs_h5(Rcpp::CharacterVector h5filepath, Rcpp::CharacterVector groupname);
static SEXP _EigenH5_get_objs_h5_try(SEXP h5filepathSEXP, SEXP groupnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type h5filepath(h5filepathSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type groupname(groupnameSEXP);
    rcpp_result_gen = Rcpp::wrap(get_objs_h5(h5filepath, groupname));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_get_objs_h5(SEXP h5filepathSEXP, SEXP groupnameSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_get_objs_h5_try(h5filepathSEXP, groupnameSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// read_l_h5
Rcpp::List read_l_h5(const std::string h5filepath, const std::string groupname, Rcpp::CharacterVector subcols, Rcpp::IntegerVector offset, Rcpp::IntegerVector chunksize, Rcpp::IntegerVector filtervec);
static SEXP _EigenH5_read_l_h5_try(SEXP h5filepathSEXP, SEXP groupnameSEXP, SEXP subcolsSEXP, SEXP offsetSEXP, SEXP chunksizeSEXP, SEXP filtervecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const std::string >::type h5filepath(h5filepathSEXP);
    Rcpp::traits::input_parameter< const std::string >::type groupname(groupnameSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type subcols(subcolsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type chunksize(chunksizeSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type filtervec(filtervecSEXP);
    rcpp_result_gen = Rcpp::wrap(read_l_h5(h5filepath, groupname, subcols, offset, chunksize, filtervec));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_read_l_h5(SEXP h5filepathSEXP, SEXP groupnameSEXP, SEXP subcolsSEXP, SEXP offsetSEXP, SEXP chunksizeSEXP, SEXP filtervecSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_read_l_h5_try(h5filepathSEXP, groupnameSEXP, subcolsSEXP, offsetSEXP, chunksizeSEXP, filtervecSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// intersect_snpinfo_h5
Rcpp::ListOf<Rcpp::IntegerVector> intersect_snpinfo_h5(std::vector<std::string> h5files);
static SEXP _EigenH5_intersect_snpinfo_h5_try(SEXP h5filesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type h5files(h5filesSEXP);
    rcpp_result_gen = Rcpp::wrap(intersect_snpinfo_h5(h5files));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _EigenH5_intersect_snpinfo_h5(SEXP h5filesSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_EigenH5_intersect_snpinfo_h5_try(h5filesSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// split_ldd
Rcpp::IntegerMatrix split_ldd(const std::vector<int>& region_ids);
RcppExport SEXP _EigenH5_split_ldd(SEXP region_idsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type region_ids(region_idsSEXP);
    rcpp_result_gen = Rcpp::wrap(split_ldd(region_ids));
    return rcpp_result_gen;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _EigenH5_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("void(*start_blosc)()");
        signatures.insert("bool(*check_blosc)()");
        signatures.insert("void(*create_vector_h5)(const std::string&,const std::string&,const std::string&,const int,const int)");
        signatures.insert("bool(*is_transposed)(const std::string,const std::string,const std::string)");
        signatures.insert("void(*write_vec_chunk_h5)(std::string,std::string,std::string,Eigen::VectorXd&,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
        signatures.insert("bool(*data_exists)(const std::string&,const std::string&,const std::string&)");
        signatures.insert("SEXPTYPE(*check_dtype)(const std::string&,const std::string&,const std::string&)");
        signatures.insert("SEXP(*read_vector_h5)(const std::string&,const std::string&,const std::string&,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector)");
        signatures.insert("Rcpp::IntegerVector(*get_dims_h5)(const std::string&,const std::string&,const std::string&)");
        signatures.insert("Rcpp::DataFrame(*cont_diff)(Rcpp::IntegerVector,int)");
        signatures.insert("Rcpp::DataFrame(*cont_reg)(Rcpp::IntegerVector,int)");
        signatures.insert("void(*multi_array_variant)(SEXP)");
        signatures.insert("SEXP(*read_matrix_h5)(const std::string&,const std::string&,const std::string&,const Rcpp::IntegerVector,const Rcpp::IntegerVector,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
        signatures.insert("SEXP(*read_array_h5)(const std::string&,const std::string&,const std::string&,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
        signatures.insert("void(*write_vector_h5)(const std::string&,const std::string&,const std::string&,SEXP)");
        signatures.insert("void(*create_matrix_h5)(const std::string&,const std::string&,const std::string&,SEXP,const bool,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
        signatures.insert("void(*write_matrix_h5)(const std::string&,const std::string&,const std::string&,SEXP,const bool,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
        signatures.insert("bool(*write_df_h5)(Rcpp::DataFrame&,const std::string,const std::string,Rcpp::IntegerVector)");
        signatures.insert("Rcpp::StringVector(*get_objs_h5)(Rcpp::CharacterVector,Rcpp::CharacterVector)");
        signatures.insert("Rcpp::List(*read_l_h5)(const std::string,const std::string,Rcpp::CharacterVector,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector)");
        signatures.insert("Rcpp::ListOf<Rcpp::IntegerVector>(*intersect_snpinfo_h5)(std::vector<std::string>)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _EigenH5_RcppExport_registerCCallable() { 
    R_RegisterCCallable("EigenH5", "_EigenH5_start_blosc", (DL_FUNC)_EigenH5_start_blosc_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_check_blosc", (DL_FUNC)_EigenH5_check_blosc_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_create_vector_h5", (DL_FUNC)_EigenH5_create_vector_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_is_transposed", (DL_FUNC)_EigenH5_is_transposed_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_write_vec_chunk_h5", (DL_FUNC)_EigenH5_write_vec_chunk_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_data_exists", (DL_FUNC)_EigenH5_data_exists_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_check_dtype", (DL_FUNC)_EigenH5_check_dtype_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_read_vector_h5", (DL_FUNC)_EigenH5_read_vector_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_get_dims_h5", (DL_FUNC)_EigenH5_get_dims_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_cont_diff", (DL_FUNC)_EigenH5_cont_diff_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_cont_reg", (DL_FUNC)_EigenH5_cont_reg_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_multi_array_variant", (DL_FUNC)_EigenH5_multi_array_variant_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_read_matrix_h5", (DL_FUNC)_EigenH5_read_matrix_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_read_array_h5", (DL_FUNC)_EigenH5_read_array_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_write_vector_h5", (DL_FUNC)_EigenH5_write_vector_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_create_matrix_h5", (DL_FUNC)_EigenH5_create_matrix_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_write_matrix_h5", (DL_FUNC)_EigenH5_write_matrix_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_write_df_h5", (DL_FUNC)_EigenH5_write_df_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_get_objs_h5", (DL_FUNC)_EigenH5_get_objs_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_read_l_h5", (DL_FUNC)_EigenH5_read_l_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_intersect_snpinfo_h5", (DL_FUNC)_EigenH5_intersect_snpinfo_h5_try);
    R_RegisterCCallable("EigenH5", "_EigenH5_RcppExport_validate", (DL_FUNC)_EigenH5_RcppExport_validate);
    return R_NilValue;
}

RcppExport SEXP _rcpp_module_boot_File_module();
RcppExport SEXP _rcpp_module_boot_Group_module();

static const R_CallMethodDef CallEntries[] = {
    {"_EigenH5_start_blosc", (DL_FUNC) &_EigenH5_start_blosc, 0},
    {"_EigenH5_check_blosc", (DL_FUNC) &_EigenH5_check_blosc, 0},
    {"_EigenH5_create_vector_h5", (DL_FUNC) &_EigenH5_create_vector_h5, 5},
    {"_EigenH5_is_transposed", (DL_FUNC) &_EigenH5_is_transposed, 3},
    {"_EigenH5_write_vec_chunk_h5", (DL_FUNC) &_EigenH5_write_vec_chunk_h5, 6},
    {"_EigenH5_data_exists", (DL_FUNC) &_EigenH5_data_exists, 3},
    {"_EigenH5_check_dtype", (DL_FUNC) &_EigenH5_check_dtype, 3},
    {"_EigenH5_read_vector_h5", (DL_FUNC) &_EigenH5_read_vector_h5, 6},
    {"_EigenH5_get_dims_h5", (DL_FUNC) &_EigenH5_get_dims_h5, 3},
    {"_EigenH5_cont_diff", (DL_FUNC) &_EigenH5_cont_diff, 2},
    {"_EigenH5_cont_reg", (DL_FUNC) &_EigenH5_cont_reg, 2},
    {"_EigenH5_multi_array_variant", (DL_FUNC) &_EigenH5_multi_array_variant, 1},
    {"_EigenH5_read_matrix_h5", (DL_FUNC) &_EigenH5_read_matrix_h5, 7},
    {"_EigenH5_read_array_h5", (DL_FUNC) &_EigenH5_read_array_h5, 5},
    {"_EigenH5_write_vector_h5", (DL_FUNC) &_EigenH5_write_vector_h5, 4},
    {"_EigenH5_create_matrix_h5", (DL_FUNC) &_EigenH5_create_matrix_h5, 7},
    {"_EigenH5_write_matrix_h5", (DL_FUNC) &_EigenH5_write_matrix_h5, 7},
    {"_EigenH5_write_df_h5", (DL_FUNC) &_EigenH5_write_df_h5, 4},
    {"_EigenH5_get_objs_h5", (DL_FUNC) &_EigenH5_get_objs_h5, 2},
    {"_EigenH5_read_l_h5", (DL_FUNC) &_EigenH5_read_l_h5, 6},
    {"_EigenH5_intersect_snpinfo_h5", (DL_FUNC) &_EigenH5_intersect_snpinfo_h5, 1},
    {"_EigenH5_split_ldd", (DL_FUNC) &_EigenH5_split_ldd, 1},
    {"_rcpp_module_boot_File_module", (DL_FUNC) &_rcpp_module_boot_File_module, 0},
    {"_rcpp_module_boot_Group_module", (DL_FUNC) &_rcpp_module_boot_Group_module, 0},
    {"_EigenH5_RcppExport_registerCCallable", (DL_FUNC) &_EigenH5_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_EigenH5(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
