// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// sma
Rcpp::NumericVector sma(Rcpp::NumericVector x, int window_size, bool recentre, String miss);
RcppExport SEXP _filters_sma(SEXP xSEXP, SEXP window_sizeSEXP, SEXP recentreSEXP, SEXP missSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    Rcpp::traits::input_parameter< bool >::type recentre(recentreSEXP);
    Rcpp::traits::input_parameter< String >::type miss(missSEXP);
    rcpp_result_gen = Rcpp::wrap(sma(x, window_size, recentre, miss));
    return rcpp_result_gen;
END_RCPP
}
// ema
Rcpp::NumericVector ema(Rcpp::NumericVector x, double alpha, String miss);
RcppExport SEXP _filters_ema(SEXP xSEXP, SEXP alphaSEXP, SEXP missSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< String >::type miss(missSEXP);
    rcpp_result_gen = Rcpp::wrap(ema(x, alpha, miss));
    return rcpp_result_gen;
END_RCPP
}
// hampel
Rcpp::NumericVector hampel(Rcpp::NumericVector x, int window_length, double a, double k, bool recentre);
RcppExport SEXP _filters_hampel(SEXP xSEXP, SEXP window_lengthSEXP, SEXP aSEXP, SEXP kSEXP, SEXP recentreSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type window_length(window_lengthSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type recentre(recentreSEXP);
    rcpp_result_gen = Rcpp::wrap(hampel(x, window_length, a, k, recentre));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_filters_sma", (DL_FUNC) &_filters_sma, 4},
    {"_filters_ema", (DL_FUNC) &_filters_ema, 3},
    {"_filters_hampel", (DL_FUNC) &_filters_hampel, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_filters(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}