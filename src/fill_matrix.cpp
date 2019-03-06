#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector fill3dArray(NumericVector x, NumericVector y, NumericVector z,
                        NumericVector value, NumericVector dims) {
  int outLen = dims[0] * dims[1] * dims[2];
  int inLen = x.length();
  NumericVector arr(outLen);
  int index;
  for (int i = 0; i < inLen; i ++) {
    index = (x[i] - 1) * dims[1] * dims[2] + (y[i] - 1) * dims[2] + (z[i] - 1);
    arr[index] = value[i];
  }
  
  return arr;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
