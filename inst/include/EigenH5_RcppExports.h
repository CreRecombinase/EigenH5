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

}

#endif // RCPP_EigenH5_RCPPEXPORTS_H_GEN_
