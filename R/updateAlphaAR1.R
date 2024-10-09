updateAlphaAR1 <- function(YY = NULL, 
                           mu = NULL, 
                           phi = NULL, 
                           std.err = NULL, 
                           p = NULL,
                           included = NULL, 
                           sqrt.W = NULL, 
                           block.diag = NULL, 
                           use.p.correct = TRUE, 
                           n.cores = 1L) {
  # check inputs 
  if (is.null(YY) || is.null(mu) || is.null(phi) || is.null(std.err) || is.null(p) || is.null(block.diag) || is.null(included) || is.null(sqrt.W)) { stop("Arguments to updateAlphaAR1() must be non-NULL.") }
  # compute new alpha 
  working_resid <- Matrix::Diagonal(x = YY - mu)
  temp_prod <- eigenMapMatMult(A = std.err, B = included, n_cores = n.cores)
  temp_prod_2 <- eigenMapMatMult(A = temp_prod, B = sqrt.W, n_cores = n.cores)
  Resid <- eigenMapMatMult(A = temp_prod_2, B = working_resid, n_cores = n.cores)
  temp_prod_3 <- eigenMapMatMult(A = included, B = block.diag, n_cores = n.cores)
  temp_prod_4 <- eigenMapMatMult(A = temp_prod_3, B = included, n_cores = n.cores)
  denom <- phi * (sum(Matrix::band(Matrix::triu(temp_prod_4, k = 1), k1 = 1, k2 = 1)) - use.p.correct * p)
  temp_prod_5 <- eigenMapMatMult(A = Resid, B = block.diag, n_cores = 1)
  temp_prod_6 <- eigenMapMatMult(A = temp_prod_5, B = Resid, n_cores = n.cores)
  num <- sum(Matrix::band(Matrix::triu(temp_prod_6, k = 1), k1 = 1, k2 = 1))
  alpha <- num/denom
  return(as.numeric(alpha))
}
