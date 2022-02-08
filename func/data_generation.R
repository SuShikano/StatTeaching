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
#' @param err.dist
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
                            err.disp = 30
                            
){
  
  if (length(x.mu)!=n.iv ) stop("Check the length of x.mu and n.iv")
  if (nrow(x.Sigma)!=n.iv| ncol(x.Sigma)!=n.iv ) stop("Check the dimensionality of x.Sigma and the length of n.iv")
  if (length(para) != (n.iv+1)) stop("Check the length of para and n.iv")
  
  library(MASS)
  if (!is.null(random.seed)) set.seed(1234)  
  generated.data <- vector(mode = "list", length = n.sim)
  
  for (i.sim in 1:n.sim){
    X <- mvrnorm(sample.size, mu=c(50,200), Sigma=covmat) 
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
                   err.var = err.var,
                   generated.data = generated.data)
}







temp <- data.generation()





summary(lm(y ~ X1 + X2,data=temp$generated.data[[2]]))






