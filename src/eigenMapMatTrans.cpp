#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]

SEXP eigenMapMatTrans(const Eigen::Map<Eigen::MatrixXd> A, int n_cores) {
  Eigen::setNbThreads(n_cores);
  Eigen::MatrixXd A_t;
  A_t.noalias() = A.transpose();
  return Rcpp::wrap(A_t);
}
