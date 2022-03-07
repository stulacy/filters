#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

//' Simple Moving Average
//' 
//' Smoothes a series by means of a windowed mean.
//' If x is an input series and y is the output filtered series,
//' then:
//' \deqn{y[i] = \frac{\sigma_{j=i - \text{window_size} + 1}^{i} x[i]}{\text{window_size}}}{y[i] = mean(x[(i-window_size+1) : i])}
//' 
//' @param x Input time-series as a vector.
//' @param window_size Window size as an integer
//' @param recentre Whether to recentre the window around the middle point in the window so as to avoid
//'   adding lag. I.e. for an odd window_length y[i] = mean(x[(i - floor(window_length/2)) : (i + floor(window_length/2))])
//'   For an even window length the output is located more closely to earlier points, i.e.
//'   y[i] = mean(x[(i - window_length/2 + 1) : (i + window_length/2)])
//' @param miss How to handle missing values. 'single' only returns NA for y[i] if x[i] = NA.
//'   'all' returns NA for y[i] if any value in the window is missing, and 'none' omits NAs from the window,
//'   so will only return NA for y[i] if all windowed values are also NA.
//' @return A filtered time-series the same length as x.
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

//' Exponential Moving Average
//' 
//' Smoothes a series by means of an exponential moving average.
//' If x is an input series and y is the output filtered series,
//' then:
//' \deqn{y[i] = \alpha x[i] + (1-\alpha)y[i-1]}{y[i] = alpha * x[i] + (1-alpha) * y[i-1]}
//' 
//' @param x Input time-series as a vector.
//' @param alpha Smoothing parameter, lower values have a greater smoothing effect.
//' @param miss How to handle missing data. 
//' When 'reset' is used and x[i] is NA then y[i] is also NA.
//' On the next non-NA value, j, then the rolling average is reset 
//' by using the current input, i.e.
//' x[j] = alpha * x[j] + (1-alpha) * x[j]
//' With 'carry' then the last non-NA value is carried over and used instead, i.e.
//' if x = [1, NA, 3] then y[3] = alpha * 3 + (1-alpha) * y[1]
//' 'carry_interpolate' is the same as 'carry' except that the previous non-NA 
//' value is also output from the filter.
//' I.e. with x = [1, NA, 3], then under 'carry' y[2] = NA but under 'carry_interpolate'
//' y[2] = 1
//' @return A filtered time-series the same length as x.
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

//' Hampel Filter for outlier removal
//' 
//' Smoothes a series by removing outliers.
//' Outliers are detected by being more than a standard deviations from the
//' median absolute deviation.
//' Non-outliers aren't affected.
//' The a parameter scales the standard deviations to have the CDF of a normal
//' distribution.
//' 
//' @param x Input time-series as a vector.
//' @param window_size Window size as an integer
//' @param recentre Whether to recentre the window around the middle point in the window so as to avoid
//'   adding lag. I.e. for an odd window_length y[i] = mean(x[(i - floor(window_length/2)) : (i + floor(window_length/2))])
//'   For an even window length the output is located more closely to earlier points, i.e.
//'   y[i] = mean(x[(i - window_length/2 + 1) : (i + window_length/2)])
//' @param a Threshold number of standard deviations from the median absolute deviation to 
//' classify outliers as.
//' @param k Tuning parameter to convert standard deviations to a normal CDF.
//' @param miss How to handle missing values. 'single' only returns NA for y[i] if x[i] = NA.
//'   'all' returns NA for y[i] if any value in the window is missing, and 'none' omits NAs from the window,
//'   so will only return NA for y[i] if all windowed values are also NA.
//' @return A filtered time-series the same length as x.
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