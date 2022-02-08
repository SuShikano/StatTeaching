# ##############################################################################
#' Generating multiple data
#'
#' @param sample.size
#' @param n.sim
#' @param n.iv
#' @param x.mu
#' @param x.Sigma
#' @param random.seed 
#' @param para
#' @param err.dist Specifies the distribution form of the errors. Possible values are 
#' "normal" (default) and "uniform".
#' @param err.disp 
#' @return 
# ---------------------------------------------------------------------------- #

data.generation <- function(sample.size=100,
                            n.sim=1000,
                            n.iv=2,
                            x.mu = c(2,5),
                            x.Sigma = diag(c(3,5)),
                            random.seed = NULL,
                            para = c(10,4,-5),
                            err.dist = "normal",
                            err.disp = 25
                            
){
  
  if (length(x.mu)!=n.iv ) stop("Check the length of x.mu and n.iv.")
  if (!is.matrix(x.Sigma)) stop("x.Sigma has to be a matrix.")
  if (nrow(x.Sigma)!=n.iv| ncol(x.Sigma)!=n.iv ) stop("Check the dimensionality of x.Sigma and the length of n.iv.")
  if (length(para) != (n.iv+1)) stop("Check the length of para and n.iv.")
  
  library(MASS)
  if (!is.null(random.seed)) set.seed(1234)  
  generated.data <- vector(mode = "list", length = n.sim)
  
  for (i.sim in 1:n.sim){
    X <- mvrnorm(sample.size, mu=x.mu, Sigma=x.Sigma) 
    colnames(X) <- paste0("X",1:n.iv)
    if (err.dist=="normal") error <- rnorm(sample.size,0,err.disp)
    if (err.dist=="uniform") error <- runif(sample.size,err.disp*(-.5),err.disp*(.5))
    
    y.hat <- c(cbind(1,X)%*% para)
    y <- y.hat + error
    
    this.dat <- as.data.frame(cbind(y,X,error))
    
    generated.data[[i.sim]]  <- this.dat
  }
  
  out.list <- list(sample.size=sample.size,
                   n.sim=n.sim,
                   n.iv=n.iv,
                   x.mu = x.mu,
                   x.Sigma = x.Sigma,
                   random.seed = random.seed,
                   para = para,
                   err.dist = err.dist,
                   err.disp = err.disp,
                   generated.data = generated.data)
}







