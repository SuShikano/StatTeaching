# ##############################################################################
#' Generating multiple datasets under different assumptions and set-ups
#'
#' @param srm.out output of lm() with a simple regression model
#' @return robust standard error
# ---------------------------------------------------------------------------- #

robust.se.srm <- function(srm.out){
  
  this.X <- srm.out$model[,2]
  this.resid <- srm.out$residuals
  
  robust.var  <- sum(((this.X - mean(this.X))^2)*(this.resid^2))/((sum((this.X - mean(this.X))^2))^2)
  
  sqrt(robust.var)
}
