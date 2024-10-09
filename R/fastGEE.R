#' Fit GEEs quickly. 
#' 
#' @name fastGEE
#' @author Jack R. Leary
#' @description This is the main fitting function of the package. 
#' @param formula An object of class \code{\link[stats]{formula}} specifying the model to be fitted. Defaults to NULL. 
#' @param data An object of class \code{\link[base]{data.frame} containing the response variable, covariates, and ID. Defaults to NULL. 
#' @param id A character specifying the name of the ID variable. Defaults to NULL. 
#' @param family The family of the model to be fitted. Defaults to \code{\link[stats]{gaussian}} with the identity link. 
#' @param cor.structure A character specifying the desired correlation structure. Defaults to "independence". 
#' @param n.cores (Optional) An integer specifying the number of cores to be used in Eigen matrix operations. Defaults to 1. 
#' @param epsilon (Optional) A double specifying the desired tolerance. Defaults to 1e-5. 
#' @param max.iter (Optional) An integer specifying the maximum number of iterations. Defaults to 20. 
#' @importFrom stats gaussian 
#' @export

fastGEE <- function(formula = NULL, 
                    data = NULL, 
                    id = NULL, 
                    family = stats::gaussian(link = "identity"), 
                    cor.structure = "independence",
                    n.cores = 1L, 
                    epsilon = 1e-5, 
                    max.iter = 20L) {
  # check inputs
  if (is.null(formula) || is.null(data) || is.null(id)) { stop("Please provide all necessary arguments to fastGEE().") }
  if (!inherits(formula, "formula")) { stop("Please provide a properly-formatted formula.") }
  
}
