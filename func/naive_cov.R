# ##############################################################################
#' Computing the naive covariance without bias correction
#'
#' @param x the data vector.
#' @param y the data vector.
#' @return the naive covariance, a biased estimator
# ---------------------------------------------------------------------------- #
naive.cov <- function(x,y){
  mean((x - mean(x))*(y - mean(y)))
}


