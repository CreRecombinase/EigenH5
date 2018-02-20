// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_EigenH5_RCPPEXPORTS_H_GEN_
#define RCPP_EigenH5_RCPPEXPORTS_H_GEN_

#include <RcppEigen.h>
#include <Rcpp.h>

namespace EigenH5 {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("EigenH5", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("EigenH5", "_EigenH5_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in EigenH5");
            }
        }
    }

    inline void start_blosc() {
        typedef SEXP(*Ptr_start_blosc)();
        static Ptr_start_blosc p_start_blosc = NULL;
        if (p_start_blosc == NULL) {
            validateSignature("void(*start_blosc)()");
            p_start_blosc = (Ptr_start_blosc)R_GetCCallable("EigenH5", "_EigenH5_start_blosc");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_start_blosc();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline bool check_blosc() {
        typedef SEXP(*Ptr_check_blosc)();
        static Ptr_check_blosc p_check_blosc = NULL;
        if (p_check_blosc == NULL) {
            validateSignature("bool(*check_blosc)()");
            p_check_blosc = (Ptr_check_blosc)R_GetCCallable("EigenH5", "_EigenH5_check_blosc");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_check_blosc();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline void create_vector_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, const int dimension, const int chunksize = 1000) {
        typedef SEXP(*Ptr_create_vector_h5)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_create_vector_h5 p_create_vector_h5 = NULL;
        if (p_create_vector_h5 == NULL) {
            validateSignature("void(*create_vector_h5)(const std::string&,const std::string&,const std::string&,const int,const int)");
            p_create_vector_h5 = (Ptr_create_vector_h5)R_GetCCallable("EigenH5", "_EigenH5_create_vector_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_create_vector_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(dimension)), Shield<SEXP>(Rcpp::wrap(chunksize)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline bool is_transposed(const std::string filename, const std::string groupname, const std::string dataname) {
        typedef SEXP(*Ptr_is_transposed)(SEXP,SEXP,SEXP);
        static Ptr_is_transposed p_is_transposed = NULL;
        if (p_is_transposed == NULL) {
            validateSignature("bool(*is_transposed)(const std::string,const std::string,const std::string)");
            p_is_transposed = (Ptr_is_transposed)R_GetCCallable("EigenH5", "_EigenH5_is_transposed");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_is_transposed(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline void copy_mat_h5(std::string infilename, std::string outfilename, std::string groupname, std::string dataname, const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_copy_mat_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_copy_mat_h5 p_copy_mat_h5 = NULL;
        if (p_copy_mat_h5 == NULL) {
            validateSignature("void(*copy_mat_h5)(std::string,std::string,std::string,std::string,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
            p_copy_mat_h5 = (Ptr_copy_mat_h5)R_GetCCallable("EigenH5", "_EigenH5_copy_mat_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_copy_mat_h5(Shield<SEXP>(Rcpp::wrap(infilename)), Shield<SEXP>(Rcpp::wrap(outfilename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(offsets)), Shield<SEXP>(Rcpp::wrap(chunksizes)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline Rcpp::IntegerMatrix parse_mat(Rcpp::CharacterVector inp) {
        typedef SEXP(*Ptr_parse_mat)(SEXP);
        static Ptr_parse_mat p_parse_mat = NULL;
        if (p_parse_mat == NULL) {
            validateSignature("Rcpp::IntegerMatrix(*parse_mat)(Rcpp::CharacterVector)");
            p_parse_mat = (Ptr_parse_mat)R_GetCCallable("EigenH5", "_EigenH5_parse_mat");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_parse_mat(Shield<SEXP>(Rcpp::wrap(inp)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::IntegerMatrix >(rcpp_result_gen);
    }

    inline void write_mat_chunk_h5(std::string filename, std::string groupname, std::string dataname, Eigen::MatrixXd& data, const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_write_mat_chunk_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_write_mat_chunk_h5 p_write_mat_chunk_h5 = NULL;
        if (p_write_mat_chunk_h5 == NULL) {
            validateSignature("void(*write_mat_chunk_h5)(std::string,std::string,std::string,Eigen::MatrixXd&,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
            p_write_mat_chunk_h5 = (Ptr_write_mat_chunk_h5)R_GetCCallable("EigenH5", "_EigenH5_write_mat_chunk_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_write_mat_chunk_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(offsets)), Shield<SEXP>(Rcpp::wrap(chunksizes)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void write_vec_chunk_h5(std::string filename, std::string groupname, std::string dataname, Eigen::VectorXd& data, const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_write_vec_chunk_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_write_vec_chunk_h5 p_write_vec_chunk_h5 = NULL;
        if (p_write_vec_chunk_h5 == NULL) {
            validateSignature("void(*write_vec_chunk_h5)(std::string,std::string,std::string,Eigen::VectorXd&,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
            p_write_vec_chunk_h5 = (Ptr_write_vec_chunk_h5)R_GetCCallable("EigenH5", "_EigenH5_write_vec_chunk_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_write_vec_chunk_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(offsets)), Shield<SEXP>(Rcpp::wrap(chunksizes)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline bool data_exists(const std::string& filename, const std::string& groupname, const std::string& dataname) {
        typedef SEXP(*Ptr_data_exists)(SEXP,SEXP,SEXP);
        static Ptr_data_exists p_data_exists = NULL;
        if (p_data_exists == NULL) {
            validateSignature("bool(*data_exists)(const std::string&,const std::string&,const std::string&)");
            p_data_exists = (Ptr_data_exists)R_GetCCallable("EigenH5", "_EigenH5_data_exists");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_data_exists(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline SEXPTYPE check_dtype(const std::string& filename, const std::string& groupname, const std::string& dataname) {
        typedef SEXP(*Ptr_check_dtype)(SEXP,SEXP,SEXP);
        static Ptr_check_dtype p_check_dtype = NULL;
        if (p_check_dtype == NULL) {
            validateSignature("SEXPTYPE(*check_dtype)(const std::string&,const std::string&,const std::string&)");
            p_check_dtype = (Ptr_check_dtype)R_GetCCallable("EigenH5", "_EigenH5_check_dtype");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_check_dtype(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXPTYPE >(rcpp_result_gen);
    }

    inline SEXP read_vector_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(), Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(), Rcpp::IntegerVector filtervec = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_read_vector_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_read_vector_h5 p_read_vector_h5 = NULL;
        if (p_read_vector_h5 == NULL) {
            validateSignature("SEXP(*read_vector_h5)(const std::string&,const std::string&,const std::string&,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector)");
            p_read_vector_h5 = (Ptr_read_vector_h5)R_GetCCallable("EigenH5", "_EigenH5_read_vector_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_vector_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(offset)), Shield<SEXP>(Rcpp::wrap(chunksize)), Shield<SEXP>(Rcpp::wrap(filtervec)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline Rcpp::IntegerVector get_dims_h5(const std::string& filename, const std::string& groupname, const std::string& dataname) {
        typedef SEXP(*Ptr_get_dims_h5)(SEXP,SEXP,SEXP);
        static Ptr_get_dims_h5 p_get_dims_h5 = NULL;
        if (p_get_dims_h5 == NULL) {
            validateSignature("Rcpp::IntegerVector(*get_dims_h5)(const std::string&,const std::string&,const std::string&)");
            p_get_dims_h5 = (Ptr_get_dims_h5)R_GetCCallable("EigenH5", "_EigenH5_get_dims_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_get_dims_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::IntegerVector >(rcpp_result_gen);
    }

    inline SEXP read_matrix_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector subset_rows = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector subset_cols = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_read_matrix_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_read_matrix_h5 p_read_matrix_h5 = NULL;
        if (p_read_matrix_h5 == NULL) {
            validateSignature("SEXP(*read_matrix_h5)(const std::string&,const std::string&,const std::string&,const Rcpp::IntegerVector,const Rcpp::IntegerVector,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
            p_read_matrix_h5 = (Ptr_read_matrix_h5)R_GetCCallable("EigenH5", "_EigenH5_read_matrix_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_matrix_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(offsets)), Shield<SEXP>(Rcpp::wrap(chunksizes)), Shield<SEXP>(Rcpp::wrap(subset_rows)), Shield<SEXP>(Rcpp::wrap(subset_cols)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP read_array_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_read_array_h5)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_read_array_h5 p_read_array_h5 = NULL;
        if (p_read_array_h5 == NULL) {
            validateSignature("SEXP(*read_array_h5)(const std::string&,const std::string&,const std::string&,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
            p_read_array_h5 = (Ptr_read_array_h5)R_GetCCallable("EigenH5", "_EigenH5_read_array_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_array_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(offsets)), Shield<SEXP>(Rcpp::wrap(chunksizes)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline void write_vector_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, SEXP data) {
        typedef SEXP(*Ptr_write_vector_h5)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_write_vector_h5 p_write_vector_h5 = NULL;
        if (p_write_vector_h5 == NULL) {
            validateSignature("void(*write_vector_h5)(const std::string&,const std::string&,const std::string&,SEXP)");
            p_write_vector_h5 = (Ptr_write_vector_h5)R_GetCCallable("EigenH5", "_EigenH5_write_vector_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_write_vector_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(data)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void create_matrix_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, SEXP data, const bool doTranspose = false, const Rcpp::IntegerVector dims = Rcpp::IntegerVector::create(), const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_create_matrix_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_create_matrix_h5 p_create_matrix_h5 = NULL;
        if (p_create_matrix_h5 == NULL) {
            validateSignature("void(*create_matrix_h5)(const std::string&,const std::string&,const std::string&,SEXP,const bool,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
            p_create_matrix_h5 = (Ptr_create_matrix_h5)R_GetCCallable("EigenH5", "_EigenH5_create_matrix_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_create_matrix_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(doTranspose)), Shield<SEXP>(Rcpp::wrap(dims)), Shield<SEXP>(Rcpp::wrap(chunksizes)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void write_matrix_h5(const std::string& filename, const std::string& groupname, const std::string& dataname, SEXP data, const bool doTranspose = false, const Rcpp::IntegerVector offsets = Rcpp::IntegerVector::create(0,0), const Rcpp::IntegerVector chunksizes = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_write_matrix_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_write_matrix_h5 p_write_matrix_h5 = NULL;
        if (p_write_matrix_h5 == NULL) {
            validateSignature("void(*write_matrix_h5)(const std::string&,const std::string&,const std::string&,SEXP,const bool,const Rcpp::IntegerVector,const Rcpp::IntegerVector)");
            p_write_matrix_h5 = (Ptr_write_matrix_h5)R_GetCCallable("EigenH5", "_EigenH5_write_matrix_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_write_matrix_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)), Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(doTranspose)), Shield<SEXP>(Rcpp::wrap(offsets)), Shield<SEXP>(Rcpp::wrap(chunksizes)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline bool write_df_h5(Rcpp::DataFrame& df, const std::string groupname, const std::string outfile, Rcpp::IntegerVector deflate_level = Rcpp::IntegerVector::create(4)) {
        typedef SEXP(*Ptr_write_df_h5)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_write_df_h5 p_write_df_h5 = NULL;
        if (p_write_df_h5 == NULL) {
            validateSignature("bool(*write_df_h5)(Rcpp::DataFrame&,const std::string,const std::string,Rcpp::IntegerVector)");
            p_write_df_h5 = (Ptr_write_df_h5)R_GetCCallable("EigenH5", "_EigenH5_write_df_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_write_df_h5(Shield<SEXP>(Rcpp::wrap(df)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(outfile)), Shield<SEXP>(Rcpp::wrap(deflate_level)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline Rcpp::StringVector get_objs_h5(Rcpp::CharacterVector h5filepath, Rcpp::CharacterVector groupname = Rcpp::CharacterVector::create("/")) {
        typedef SEXP(*Ptr_get_objs_h5)(SEXP,SEXP);
        static Ptr_get_objs_h5 p_get_objs_h5 = NULL;
        if (p_get_objs_h5 == NULL) {
            validateSignature("Rcpp::StringVector(*get_objs_h5)(Rcpp::CharacterVector,Rcpp::CharacterVector)");
            p_get_objs_h5 = (Ptr_get_objs_h5)R_GetCCallable("EigenH5", "_EigenH5_get_objs_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_get_objs_h5(Shield<SEXP>(Rcpp::wrap(h5filepath)), Shield<SEXP>(Rcpp::wrap(groupname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::StringVector >(rcpp_result_gen);
    }

    inline Rcpp::List read_l_h5(const std::string h5filepath, const std::string groupname, Rcpp::CharacterVector subcols = Rcpp::CharacterVector::create(), Rcpp::IntegerVector offset = Rcpp::IntegerVector::create(), Rcpp::IntegerVector chunksize = Rcpp::IntegerVector::create(), Rcpp::IntegerVector filtervec = Rcpp::IntegerVector::create()) {
        typedef SEXP(*Ptr_read_l_h5)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_read_l_h5 p_read_l_h5 = NULL;
        if (p_read_l_h5 == NULL) {
            validateSignature("Rcpp::List(*read_l_h5)(const std::string,const std::string,Rcpp::CharacterVector,Rcpp::IntegerVector,Rcpp::IntegerVector,Rcpp::IntegerVector)");
            p_read_l_h5 = (Ptr_read_l_h5)R_GetCCallable("EigenH5", "_EigenH5_read_l_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_l_h5(Shield<SEXP>(Rcpp::wrap(h5filepath)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(subcols)), Shield<SEXP>(Rcpp::wrap(offset)), Shield<SEXP>(Rcpp::wrap(chunksize)), Shield<SEXP>(Rcpp::wrap(filtervec)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::List >(rcpp_result_gen);
    }

    inline Rcpp::ListOf<Rcpp::IntegerVector> intersect_snpinfo_h5(std::vector<std::string> h5files) {
        typedef SEXP(*Ptr_intersect_snpinfo_h5)(SEXP);
        static Ptr_intersect_snpinfo_h5 p_intersect_snpinfo_h5 = NULL;
        if (p_intersect_snpinfo_h5 == NULL) {
            validateSignature("Rcpp::ListOf<Rcpp::IntegerVector>(*intersect_snpinfo_h5)(std::vector<std::string>)");
            p_intersect_snpinfo_h5 = (Ptr_intersect_snpinfo_h5)R_GetCCallable("EigenH5", "_EigenH5_intersect_snpinfo_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_intersect_snpinfo_h5(Shield<SEXP>(Rcpp::wrap(h5files)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::ListOf<Rcpp::IntegerVector> >(rcpp_result_gen);
    }

}

#endif // RCPP_EigenH5_RCPPEXPORTS_H_GEN_
