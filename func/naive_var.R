# ##############################################################################
#' Computing the naive variance without bias correction
#'
#' @param x the data vector.
#' @return the naive variance, a biased estimator
# ---------------------------------------------------------------------------- #
naive.var <- function(x){
  diff.to.mean <- x - mean(x)
  squared.diff <- diff.to.mean^2
  output <- mean(squared.diff)
  output
}


