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

    inline std::string openFileHandleRead(const std::string filepath) {
        typedef SEXP(*Ptr_openFileHandleRead)(SEXP);
        static Ptr_openFileHandleRead p_openFileHandleRead = NULL;
        if (p_openFileHandleRead == NULL) {
            validateSignature("std::string(*openFileHandleRead)(const std::string)");
            p_openFileHandleRead = (Ptr_openFileHandleRead)R_GetCCallable("EigenH5", "_EigenH5_openFileHandleRead");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_openFileHandleRead(Shield<SEXP>(Rcpp::wrap(filepath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::string >(rcpp_result_gen);
    }

    inline size_t closeFileHandle(const std::string fh) {
        typedef SEXP(*Ptr_closeFileHandle)(SEXP);
        static Ptr_closeFileHandle p_closeFileHandle = NULL;
        if (p_closeFileHandle == NULL) {
            validateSignature("size_t(*closeFileHandle)(const std::string)");
            p_closeFileHandle = (Ptr_closeFileHandle)R_GetCCallable("EigenH5", "_EigenH5_closeFileHandle");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_closeFileHandle(Shield<SEXP>(Rcpp::wrap(fh)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<size_t >(rcpp_result_gen);
    }

    inline bool has_blosc() {
        typedef SEXP(*Ptr_has_blosc)();
        static Ptr_has_blosc p_has_blosc = NULL;
        if (p_has_blosc == NULL) {
            validateSignature("bool(*has_blosc)()");
            p_has_blosc = (Ptr_has_blosc)R_GetCCallable("EigenH5", "_EigenH5_has_blosc");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_has_blosc();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool has_lzf() {
        typedef SEXP(*Ptr_has_lzf)();
        static Ptr_has_lzf p_has_lzf = NULL;
        if (p_has_lzf == NULL) {
            validateSignature("bool(*has_lzf)()");
            p_has_lzf = (Ptr_has_lzf)R_GetCCallable("EigenH5", "_EigenH5_has_lzf");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_has_lzf();
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
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
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
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
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline int len(RObject x) {
        typedef SEXP(*Ptr_len)(SEXP);
        static Ptr_len p_len = NULL;
        if (p_len == NULL) {
            validateSignature("int(*len)(RObject)");
            p_len = (Ptr_len)R_GetCCallable("EigenH5", "_EigenH5_len");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_len(Shield<SEXP>(Rcpp::wrap(x)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int >(rcpp_result_gen);
    }

    inline Rcpp::List permutation_order(const Rcpp::List options, Rcpp::IntegerVector dims) {
        typedef SEXP(*Ptr_permutation_order)(SEXP,SEXP);
        static Ptr_permutation_order p_permutation_order = NULL;
        if (p_permutation_order == NULL) {
            validateSignature("Rcpp::List(*permutation_order)(const Rcpp::List,Rcpp::IntegerVector)");
            p_permutation_order = (Ptr_permutation_order)R_GetCCallable("EigenH5", "_EigenH5_permutation_order");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_permutation_order(Shield<SEXP>(Rcpp::wrap(options)), Shield<SEXP>(Rcpp::wrap(dims)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::List >(rcpp_result_gen);
    }

    inline SEXP read_vector(std::string filename, std::string datapath, Rcpp::List options) {
        typedef SEXP(*Ptr_read_vector)(SEXP,SEXP,SEXP);
        static Ptr_read_vector p_read_vector = NULL;
        if (p_read_vector == NULL) {
            validateSignature("SEXP(*read_vector)(std::string,std::string,Rcpp::List)");
            p_read_vector = (Ptr_read_vector)R_GetCCallable("EigenH5", "_EigenH5_read_vector");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_vector(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)), Shield<SEXP>(Rcpp::wrap(options)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP read_matrix(std::string filename, std::string datapath, const Rcpp::List options) {
        typedef SEXP(*Ptr_read_matrix)(SEXP,SEXP,SEXP);
        static Ptr_read_matrix p_read_matrix = NULL;
        if (p_read_matrix == NULL) {
            validateSignature("SEXP(*read_matrix)(std::string,std::string,const Rcpp::List)");
            p_read_matrix = (Ptr_read_matrix)R_GetCCallable("EigenH5", "_EigenH5_read_matrix");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_matrix(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)), Shield<SEXP>(Rcpp::wrap(options)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline bool update_matrix(RObject data, const std::string filename, std::string datapath, const Rcpp::List& options) {
        typedef SEXP(*Ptr_update_matrix)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_update_matrix p_update_matrix = NULL;
        if (p_update_matrix == NULL) {
            validateSignature("bool(*update_matrix)(RObject,const std::string,std::string,const Rcpp::List&)");
            p_update_matrix = (Ptr_update_matrix)R_GetCCallable("EigenH5", "_EigenH5_update_matrix");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_update_matrix(Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)), Shield<SEXP>(Rcpp::wrap(options)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool update_vector(RObject data, std::string filename, std::string datapath, Rcpp::List options) {
        typedef SEXP(*Ptr_update_vector)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_update_vector p_update_vector = NULL;
        if (p_update_vector == NULL) {
            validateSignature("bool(*update_vector)(RObject,std::string,std::string,Rcpp::List)");
            p_update_vector = (Ptr_update_vector)R_GetCCallable("EigenH5", "_EigenH5_update_vector");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_update_vector(Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)), Shield<SEXP>(Rcpp::wrap(options)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool write_attribute_h5(const RObject& data, const std::string& filename, std::string datapath) {
        typedef SEXP(*Ptr_write_attribute_h5)(SEXP,SEXP,SEXP);
        static Ptr_write_attribute_h5 p_write_attribute_h5 = NULL;
        if (p_write_attribute_h5 == NULL) {
            validateSignature("bool(*write_attribute_h5)(const RObject&,const std::string&,std::string)");
            p_write_attribute_h5 = (Ptr_write_attribute_h5)R_GetCCallable("EigenH5", "_EigenH5_write_attribute_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_write_attribute_h5(Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline SEXP read_R_attribute_h5(const std::string& filename, std::string datapath) {
        typedef SEXP(*Ptr_read_R_attribute_h5)(SEXP,SEXP);
        static Ptr_read_R_attribute_h5 p_read_R_attribute_h5 = NULL;
        if (p_read_R_attribute_h5 == NULL) {
            validateSignature("SEXP(*read_R_attribute_h5)(const std::string&,std::string)");
            p_read_R_attribute_h5 = (Ptr_read_R_attribute_h5)R_GetCCallable("EigenH5", "_EigenH5_read_R_attribute_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_R_attribute_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline SEXP read_attribute_h5(const std::string& filename, std::string datapath) {
        typedef SEXP(*Ptr_read_attribute_h5)(SEXP,SEXP);
        static Ptr_read_attribute_h5 p_read_attribute_h5 = NULL;
        if (p_read_attribute_h5 == NULL) {
            validateSignature("SEXP(*read_attribute_h5)(const std::string&,std::string)");
            p_read_attribute_h5 = (Ptr_read_attribute_h5)R_GetCCallable("EigenH5", "_EigenH5_read_attribute_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_read_attribute_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline bool create_dataset_h5(const std::string& filename, std::string datapath, const RObject& data, Rcpp::List options) {
        typedef SEXP(*Ptr_create_dataset_h5)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_create_dataset_h5 p_create_dataset_h5 = NULL;
        if (p_create_dataset_h5 == NULL) {
            validateSignature("bool(*create_dataset_h5)(const std::string&,std::string,const RObject&,Rcpp::List)");
            p_create_dataset_h5 = (Ptr_create_dataset_h5)R_GetCCallable("EigenH5", "_EigenH5_create_dataset_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_create_dataset_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)), Shield<SEXP>(Rcpp::wrap(data)), Shield<SEXP>(Rcpp::wrap(options)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline void link_objects_h5(Rcpp::StringVector filename_from, const std::string filename_to, Rcpp::StringVector datapath_from, Rcpp::StringVector datapath_to) {
        typedef SEXP(*Ptr_link_objects_h5)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_link_objects_h5 p_link_objects_h5 = NULL;
        if (p_link_objects_h5 == NULL) {
            validateSignature("void(*link_objects_h5)(Rcpp::StringVector,const std::string,Rcpp::StringVector,Rcpp::StringVector)");
            p_link_objects_h5 = (Ptr_link_objects_h5)R_GetCCallable("EigenH5", "_EigenH5_link_objects_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_link_objects_h5(Shield<SEXP>(Rcpp::wrap(filename_from)), Shield<SEXP>(Rcpp::wrap(filename_to)), Shield<SEXP>(Rcpp::wrap(datapath_from)), Shield<SEXP>(Rcpp::wrap(datapath_to)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void create_file_h5(const std::string filename) {
        typedef SEXP(*Ptr_create_file_h5)(SEXP);
        static Ptr_create_file_h5 p_create_file_h5 = NULL;
        if (p_create_file_h5 == NULL) {
            validateSignature("void(*create_file_h5)(const std::string)");
            p_create_file_h5 = (Ptr_create_file_h5)R_GetCCallable("EigenH5", "_EigenH5_create_file_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_create_file_h5(Shield<SEXP>(Rcpp::wrap(filename)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline Rcpp::IntegerVector dataset_chunks(const std::string filename, const std::string datapath) {
        typedef SEXP(*Ptr_dataset_chunks)(SEXP,SEXP);
        static Ptr_dataset_chunks p_dataset_chunks = NULL;
        if (p_dataset_chunks == NULL) {
            validateSignature("Rcpp::IntegerVector(*dataset_chunks)(const std::string,const std::string)");
            p_dataset_chunks = (Ptr_dataset_chunks)R_GetCCallable("EigenH5", "_EigenH5_dataset_chunks");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_dataset_chunks(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::IntegerVector >(rcpp_result_gen);
    }

    inline void extend_dataset(const std::string filename, const std::string datapath, Rcpp::IntegerVector newdims) {
        typedef SEXP(*Ptr_extend_dataset)(SEXP,SEXP,SEXP);
        static Ptr_extend_dataset p_extend_dataset = NULL;
        if (p_extend_dataset == NULL) {
            validateSignature("void(*extend_dataset)(const std::string,const std::string,Rcpp::IntegerVector)");
            p_extend_dataset = (Ptr_extend_dataset)R_GetCCallable("EigenH5", "_EigenH5_extend_dataset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_extend_dataset(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)), Shield<SEXP>(Rcpp::wrap(newdims)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline void extend_dataset_by(const std::string filename, const std::string datapath, Rcpp::IntegerVector newdims) {
        typedef SEXP(*Ptr_extend_dataset_by)(SEXP,SEXP,SEXP);
        static Ptr_extend_dataset_by p_extend_dataset_by = NULL;
        if (p_extend_dataset_by == NULL) {
            validateSignature("void(*extend_dataset_by)(const std::string,const std::string,Rcpp::IntegerVector)");
            p_extend_dataset_by = (Ptr_extend_dataset_by)R_GetCCallable("EigenH5", "_EigenH5_extend_dataset_by");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_extend_dataset_by(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)), Shield<SEXP>(Rcpp::wrap(newdims)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

    inline Rcpp::List get_datset_filter(const std::string filename, const std::string datapath) {
        typedef SEXP(*Ptr_get_datset_filter)(SEXP,SEXP);
        static Ptr_get_datset_filter p_get_datset_filter = NULL;
        if (p_get_datset_filter == NULL) {
            validateSignature("Rcpp::List(*get_datset_filter)(const std::string,const std::string)");
            p_get_datset_filter = (Ptr_get_datset_filter)R_GetCCallable("EigenH5", "_EigenH5_get_datset_filter");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_get_datset_filter(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::List >(rcpp_result_gen);
    }

    inline Rcpp::IntegerVector guess_chunks(const std::vector<int> dimsize) {
        typedef SEXP(*Ptr_guess_chunks)(SEXP);
        static Ptr_guess_chunks p_guess_chunks = NULL;
        if (p_guess_chunks == NULL) {
            validateSignature("Rcpp::IntegerVector(*guess_chunks)(const std::vector<int>)");
            p_guess_chunks = (Ptr_guess_chunks)R_GetCCallable("EigenH5", "_EigenH5_guess_chunks");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_guess_chunks(Shield<SEXP>(Rcpp::wrap(dimsize)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::IntegerVector >(rcpp_result_gen);
    }

    inline bool exists_h5(const std::string filename, const std::string groupname = "/", const std::string dataname = "") {
        typedef SEXP(*Ptr_exists_h5)(SEXP,SEXP,SEXP);
        static Ptr_exists_h5 p_exists_h5 = NULL;
        if (p_exists_h5 == NULL) {
            validateSignature("bool(*exists_h5)(const std::string,const std::string,const std::string)");
            p_exists_h5 = (Ptr_exists_h5)R_GetCCallable("EigenH5", "_EigenH5_exists_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_exists_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool isObject(const std::string filename, std::string dataname) {
        typedef SEXP(*Ptr_isObject)(SEXP,SEXP);
        static Ptr_isObject p_isObject = NULL;
        if (p_isObject == NULL) {
            validateSignature("bool(*isObject)(const std::string,std::string)");
            p_isObject = (Ptr_isObject)R_GetCCallable("EigenH5", "_EigenH5_isObject");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_isObject(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline int ArrayTypeSize(const std::string filename, std::string dataname) {
        typedef SEXP(*Ptr_ArrayTypeSize)(SEXP,SEXP);
        static Ptr_ArrayTypeSize p_ArrayTypeSize = NULL;
        if (p_ArrayTypeSize == NULL) {
            validateSignature("int(*ArrayTypeSize)(const std::string,std::string)");
            p_ArrayTypeSize = (Ptr_ArrayTypeSize)R_GetCCallable("EigenH5", "_EigenH5_ArrayTypeSize");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ArrayTypeSize(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<int >(rcpp_result_gen);
    }

    inline bool isDataSet(const std::string filename, std::string dataname) {
        typedef SEXP(*Ptr_isDataSet)(SEXP,SEXP);
        static Ptr_isDataSet p_isDataSet = NULL;
        if (p_isDataSet == NULL) {
            validateSignature("bool(*isDataSet)(const std::string,std::string)");
            p_isDataSet = (Ptr_isDataSet)R_GetCCallable("EigenH5", "_EigenH5_isDataSet");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_isDataSet(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool isGroup(const std::string filename, std::string dataname) {
        typedef SEXP(*Ptr_isGroup)(SEXP,SEXP);
        static Ptr_isGroup p_isGroup = NULL;
        if (p_isGroup == NULL) {
            validateSignature("bool(*isGroup)(const std::string,std::string)");
            p_isGroup = (Ptr_isGroup)R_GetCCallable("EigenH5", "_EigenH5_isGroup");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_isGroup(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(dataname)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline Rcpp::StringVector ls_h5_exp(const std::string filename, Rcpp::CharacterVector groupname = Rcpp::CharacterVector::create("/"), bool full_names = false) {
        typedef SEXP(*Ptr_ls_h5_exp)(SEXP,SEXP,SEXP);
        static Ptr_ls_h5_exp p_ls_h5_exp = NULL;
        if (p_ls_h5_exp == NULL) {
            validateSignature("Rcpp::StringVector(*ls_h5_exp)(const std::string,Rcpp::CharacterVector,bool)");
            p_ls_h5_exp = (Ptr_ls_h5_exp)R_GetCCallable("EigenH5", "_EigenH5_ls_h5_exp");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ls_h5_exp(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(groupname)), Shield<SEXP>(Rcpp::wrap(full_names)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::StringVector >(rcpp_result_gen);
    }

    inline Rcpp::StringVector typeof_h5(const std::string filename, const std::string datapath) {
        typedef SEXP(*Ptr_typeof_h5)(SEXP,SEXP);
        static Ptr_typeof_h5 p_typeof_h5 = NULL;
        if (p_typeof_h5 == NULL) {
            validateSignature("Rcpp::StringVector(*typeof_h5)(const std::string,const std::string)");
            p_typeof_h5 = (Ptr_typeof_h5)R_GetCCallable("EigenH5", "_EigenH5_typeof_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_typeof_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::StringVector >(rcpp_result_gen);
    }

    inline Rcpp::DataFrame info_h5(const Rcpp::StringVector filename, Rcpp::StringVector datapaths) {
        typedef SEXP(*Ptr_info_h5)(SEXP,SEXP);
        static Ptr_info_h5 p_info_h5 = NULL;
        if (p_info_h5 == NULL) {
            validateSignature("Rcpp::DataFrame(*info_h5)(const Rcpp::StringVector,Rcpp::StringVector)");
            p_info_h5 = (Ptr_info_h5)R_GetCCallable("EigenH5", "_EigenH5_info_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_info_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapaths)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::DataFrame >(rcpp_result_gen);
    }

    inline Rcpp::DataFrame file_acc_ct(const std::string filename) {
        typedef SEXP(*Ptr_file_acc_ct)(SEXP);
        static Ptr_file_acc_ct p_file_acc_ct = NULL;
        if (p_file_acc_ct == NULL) {
            validateSignature("Rcpp::DataFrame(*file_acc_ct)(const std::string)");
            p_file_acc_ct = (Ptr_file_acc_ct)R_GetCCallable("EigenH5", "_EigenH5_file_acc_ct");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_file_acc_ct(Shield<SEXP>(Rcpp::wrap(filename)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::DataFrame >(rcpp_result_gen);
    }

    inline Rcpp::IntegerVector dim_h5(const std::string& filename, const std::string datapath) {
        typedef SEXP(*Ptr_dim_h5)(SEXP,SEXP);
        static Ptr_dim_h5 p_dim_h5 = NULL;
        if (p_dim_h5 == NULL) {
            validateSignature("Rcpp::IntegerVector(*dim_h5)(const std::string&,const std::string)");
            p_dim_h5 = (Ptr_dim_h5)R_GetCCallable("EigenH5", "_EigenH5_dim_h5");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_dim_h5(Shield<SEXP>(Rcpp::wrap(filename)), Shield<SEXP>(Rcpp::wrap(datapath)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Rcpp::IntegerVector >(rcpp_result_gen);
    }

    inline void concat_mats(const std::string newfile, const std::string newpath, Rcpp::List selections, int margin = 0) {
        typedef SEXP(*Ptr_concat_mats)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_concat_mats p_concat_mats = NULL;
        if (p_concat_mats == NULL) {
            validateSignature("void(*concat_mats)(const std::string,const std::string,Rcpp::List,int)");
            p_concat_mats = (Ptr_concat_mats)R_GetCCallable("EigenH5", "_EigenH5_concat_mats");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_concat_mats(Shield<SEXP>(Rcpp::wrap(newfile)), Shield<SEXP>(Rcpp::wrap(newpath)), Shield<SEXP>(Rcpp::wrap(selections)), Shield<SEXP>(Rcpp::wrap(margin)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
    }

}

#endif // RCPP_EigenH5_RCPPEXPORTS_H_GEN_
