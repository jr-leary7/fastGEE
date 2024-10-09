getAlphaInvExchangeable <- function(alpha.new = NULL, 
                                    diag.vec = NULL, 
                                    block.diag = NULL, 
                                    n.cores = 1L) {
  # check inputs 
  if (is.null(alpha.new) || is.null(diag.vec) || is.null(block.diag)) { stop("Arguments to getAlphaInvExchangeable() must be non-NULL.") }
  # compute inverse 
  diag_temp <- Matrix::Diagonal(x = (-alpha.new / ((1 - alpha.new) * (1 + (diag.vec - 1) * alpha.new))))
  temp_prod <- eigenMapMatMult(A = block.diag, B = diag_temp, n_cores = n.cores)
  temp_prod <- temp_prod + Matrix::Diagonal(x = ((1 + (diag.vec - 2) * alpha.new) / ((1 - alpha.new) * (1 + (diag.vec - 1) * alpha.new)) + alpha.new / ((1 - alpha.new) * (1 + (diag.vec - 1) * alpha.new))))
  temp_prod <- as(temp_prod, "SymmetricMatrix")
}
