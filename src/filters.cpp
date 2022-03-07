#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// TODO docstrings
// [[Rcpp::export]]
Rcpp::NumericVector sma(Rcpp::NumericVector x, int window_size, bool recentre=true, String miss="single") {
  if (! (miss == "single" || miss == "all" || miss == "none")) {
      Rcerr << "Error: miss must be one of 'single', 'all', or 'none'\n";
      Rcpp::NumericVector ret(1);
      ret[0] = NA_REAL;
      return ret;
  }
  
  int n = x.length();
  int shift = recentre ? floor(window_size / 2) : 0;
  Rcpp::NumericVector s(n);
  if (n > 1) {
    for (int i = shift; i < n; i++) {
      if (i < (window_size - 1)) {
        s[i-shift] = NA_REAL;
      } else {
          // If current point is NA and miss == 'single' then set NA
        if (miss == "single" && ISNAN(x[i-shift])) {
            s[i-shift] = NA_REAL;
        } else {
            if (miss == "all") {
                s[i-shift] = mean(x[Rcpp::Range(i-window_size+1, i)]);
            } else {
                s[i-shift] = mean(na_omit(x[Rcpp::Range(i-window_size+1, i)]));
            }
        }
      }
    }
    // Add in right hand NAs
    for (int j = (n - shift); j < n; j++) {
      s[j] = NA_REAL;
    }
  }
  return s;
}

// TODO docstrings
// [[Rcpp::export]]
Rcpp::NumericVector ema(Rcpp::NumericVector x, double alpha, String miss="reset") {
  if (! (miss == "reset" || miss == "carry" || miss == "carry_interpolate")) {
      Rcerr << "Error: miss must be one of 'reset', 'carry', or 'carry_interpolate'\n";
      Rcpp::NumericVector ret(1);
      ret[0] = NA_REAL;
      return ret;
  }
    
  int n = x.length();
  Rcpp::NumericVector s(n);
  s[0] = x[0];
  double prev_state = s[0];
  if (n > 1) {
    for (int i = 1; i < n; i++) {
      if (ISNAN(x[i])) {
          if (miss == "carry_interpolate") {
              s[i] = prev_state;
          } else {
              s[i] = NA_REAL;
          }
      } else {
          if (ISNAN(x[i-1]) && miss == "reset") {
              s[i] = x[i];
          } else {
              s[i] = alpha * x[i] + (1 - alpha) * prev_state;
          }
          
          prev_state = s[i];
      }
    }
  }
  return s;
}

// TODO docstrings
// [[Rcpp::export]]
Rcpp::NumericVector hampel(Rcpp::NumericVector x, int window_size, double a=3.0, double k=1.4826, bool recentre=true, String miss="single") {
  if (! (miss == "single" || miss == "all" || miss == "none")) {
      Rcerr << "Error: miss must be one of 'single', 'all', or 'none'\n";
      Rcpp::NumericVector ret(1);
      ret[0] = NA_REAL;
      return ret;
  }
  
  int n = x.length();
  int shift = recentre ? floor(window_size / 2) : 0;
  double xbar;
  double mad;
  Rcpp::NumericVector window(window_size);
  Rcpp::NumericVector abs_diff(window_size);
  Rcpp::NumericVector s(n);
  if (n > 1) {
    for (int i = shift; i < n; i++) {
      if (i < (window_size - 1)) {
        s[i-shift] = NA_REAL;
      } else {
          // If current point is NA and miss == 'single' then set NA
        if (miss == "single" && ISNAN(x[i-shift])) {
            s[i-shift] = NA_REAL;
        } else {
            if (miss == "all") {
                window = x[Rcpp::Range(i-window_size+1, i)];
                xbar = median(window);
                if (ISNAN(xbar)) {
                    s[i-shift] = xbar;
                } else {
                    abs_diff = abs(window-xbar);
                    mad = k * median(abs_diff);
                    if (abs(x[i-shift] - xbar) > a * mad) {
                      s[i-shift] = xbar;
                    } else {
                      s[i-shift] = x[i-shift];
                    }
                }
            } else {
                window = x[Rcpp::Range(i-window_size+1, i)];
                xbar = median(na_omit(window));
                abs_diff = abs(window-xbar);
                mad = k * median(na_omit(abs_diff));
                if (abs(x[i-shift] - xbar) > a * mad) {
                  s[i-shift] = xbar;
                } else {
                  s[i-shift] = x[i-shift];
                }
                
                
                
            }
        }
      }
      }
    // Add in right hand NAs
    for (int j = (n - shift); j < n; j++) {
      s[j] = NA_REAL;
    }
  }
  return s;
}