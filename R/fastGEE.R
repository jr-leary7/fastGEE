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
#' @param n.cores (Optional) An integer specifying the number of cores to be used in \code{Eigen} matrix operations. Defaults to 1. 
#' @param epsilon (Optional) A double specifying the desired tolerance. Defaults to 1e-5. 
#' @param max.iter (Optional) An integer specifying the maximum number of iterations. Defaults to 20. 
#' @importFrom stats gaussian model.frame model.response model.matrix 
#' @importFrom rlang sym
#' @importFrom dplyr mutate 
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
  if (!inherits(data, "data.frame")) {
    data <- try({ as.data.frame(data) }, silent = TRUE)
  }
  if (inherits(data, "try-error")) { stop("Argument data must be coercable to an object of class data.frame.") }
  if (!inherits(id, "character")) { stop("Argument id must be of class character.") }
  if (!id %in% colnames(data)) { stop("Argument id must be one of the columns of data.") }
  if (is.unsorted(data[[id]])) { stop("Argument data must be sorted by id.") }
  if (!inherits(family, "family")) { stop("Argument family must be of class family.") }
  if (!inherits(cor.structure, "character")) { stop("Argument cor.structure must be of class character.") }
  cor.structure <- tolower(cor.structure)
  if (!cor.structure %in% c("independence", "exchangeable", "ar1", "unstructured")) { stop("Please provide a valid correlation structure.") }
  # create model matrix from formula 
  model_frame <- stats::model.frame(formula, data)
  y <- stats::model.response(model_frame)
  X <- stats::model.matrix(attr(model_frame, "terms"), data = model_frame)
  # make sure ID is formatted correctly 
  id_sym <- rlang::sym(id)
  if (is.factor(data[[id]])) {
    data <- dplyr::mutate(data, !!id_sym := as.integer(!!id_sym))
  } else if (is.character(data[[id]])) {
    data <- dplyr::mutate(data, !!id_sym := as.integer(as.factor(!!id_sym)))
  }
  
}
