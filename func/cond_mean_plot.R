# ##############################################################################
#' Plot the conditional mean in a joint distribution
#'
#' @param x the data vector.
#' @param y the data vector.
#' @param perc The percentage of the confidence interval. Default is 95.
#' @return A list including all parameter values and 
#'           generated values (lower.b and upper.b) 
# ---------------------------------------------------------------------------- #

cond.mean.plot <- function(x=NULL,
                           y=NULL,
                           perc=95,
                           xlab="X",
                           ylab="Y",
                           num.X=25
){

source("https://raw.githubusercontent.com/SuShikano/StatTeaching/main/func/ci_sample_mean.R")  
  
y.range <- range(y,na.rm=T)
x.range <- range(x,na.rm=T)

plot(y ~ x,xlab=xlab,ylab=ylab,
     xlim=x.range,ylim=y.range)

x.values <- seq(min(x,na.rm=T),max(x,na.rm=T),length=num.X)
x.interval <- x.values[2] -x.values[1]

conditional.mean <- lower.b <- upper.b <- rep(NA,length(x.values))
for (i in 1:length(conditional.mean)){
  
  selected.y <- y[(x> (x.values[i]-x.interval)) & 
                  (x <(x.values[i]+x.interval)) ]
  conditional.mean[i] <- mean(selected.y)
  this.ci <- ci.sample.mean(selected.y)
  lower.b[i] <- this.ci$lower.b
  upper.b[i] <- this.ci$upper.b
}

par(new=T)
plot(conditional.mean ~ x.values,ann=F,xlab="",ylab="",
     axes=F,
     xlim=x.range,ylim=y.range,
     col="red",pch=19,type="b")
abline(h=0,lty=2,lwd=3)
for (i in 1:length(conditional.mean)){
  lines(rep(x.values[i],2),c(upper.b[i],lower.b[i]),col="red",lwd=2)
}

}