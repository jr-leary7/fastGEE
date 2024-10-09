updateAlphaExchangeable <- function(YY = NULL, 
                                    mu = NULL, 
                                    phi = NULL,
                                    std.err = NULL,
                                    p = NULL, 
                                    block.diag = NULL, 
                                    included = NULL,
                                    included.len = NULL, 
                                    sqrt.W = NULL, 
                                    use.p.correct = TRUE, 
                                    n.cores = 1L) {
  # check inputs 
  if (is.null(YY) || is.null(mu) || is.null(phi) || is.null(std.err) || is.null(p) || is.null(block.diag) || is.null(included) || is.null(indcluded.len) || is.null(sqrt.W)) { stop("Arguments to updateAlphaExchangeable() must be non-NULL.") }
  # compute new alpha 
  W <- sqrtW^2
  temp_prod <- eigenMapMatMult(A = std.error, B = included, n_cores = n.cores)
  temp_prod_2 <- eigenMapMatMult(A = temp_prod, B = sqrt.W, n_cores = n.cores)
  working_resid <- Matrix::Diagonal(x = YY - mu)
  Resid <- eigenMapMatMult(A = temp_prod_2, B = working_resid, n_cores = n.cores)
  denom <- phi * (sum(included.len * (included.len - 1)) / 2 - use.p.correct * p)
  temp_prod_3 <- eigenMapMatMult(A = std.err, B = working_resid, n_cores = n.cores)
  temp_prod_4 <- eigenMapMatMult(A = temp_prod_3, B = included, n_cores = n.cores)
  temp_prod_5 <- eigenMapMatMult(A = temp_prod_4, B = block.diag, n_cores = n.cores)
  temp_prod_6 <- eigenMapMatMult(A = temp_prod_5, B = W, n_cores = n.cores)
  temp_prod_7 <- eigenMapMatMult(A = temp_prod_6, B = working_resid, n_cores = n.cores)
  block.diag <- eigenMapMatMult(A = temp_prod_7, B = std.err, n_cores = n.cores)
  alpha <- sum(Matrix::triu(block.diag, k = 1))
  alpha_new <- alpha / denom
  return(alpha_new)
}
