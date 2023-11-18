#' @keywords internal 
"_PACKAGE"

#' The package provides a list LAD which can be used as method parameter to rpart.
#'
#' According to continuous regressors, this implementation directly follows the description in the 1984 book by
#' Breiman, Friedman, Olshen and Stone, but the mentioned "updating" procedure for median computation has been replaced
#' by an sort-and-search approach, which efficiently allows to compute all weighted medians for potential splits in
#' \eqn{O(n \log n)}. Computation of the LAD however is \eqn{O(n^2)} complex.
#'
#' According to discrete regressors, the algorithm makes use of the (rpart-specific) simplification heuristic which
#' takes just those splits into account, which are in line with the ordering of the categories with respect to their
#' medians.
#'
#' @import Rcpp
#' @import rpart
#' @useDynLib rpart.LAD
NULL