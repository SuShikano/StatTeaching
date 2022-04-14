# ##############################################################################
#' Computing confidence intervals of sample mean
#'
#' @param x the data vector.
#' @param perc The percentage of the confidence interval. Default is 95.
#' @return A list including all parameter values and 
#'           generated values (lower.b and upper.b) 
# ---------------------------------------------------------------------------- #

ci.sample.mean <- function(x=NULL,
                            perc=95
){
  sample.mean <- mean(x)
  standard.err <- sqrt(var(x)/length(x))
  
  bounds.prob <- c(100-perc)/2
  bounds.prob <- c(bounds.prob,100-bounds.prob)/100
  
  bounds.t <- qt(bounds.prob,df=length(x)-1)
  
  lower.b <- sample.mean + bounds.t[1]*standard.err
  upper.b <- sample.mean + bounds.t[2]*standard.err
  
  list(x=x,perc=perc,
       lower.b=lower.b,upper.b=upper.b,
       bounds.prob=bounds.prob,
       bounds.t = bounds.t)
  
}
  