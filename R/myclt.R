#' myclt Function
#'
#' @description
#' This function will display a distribution of the sum of uniforms
#'
#'
#' @param n the base value used for sum of uniforms
#' @param iter the number of iterations the function will perform
#' @param a the left endmost point
#' @param b the right endmost point
#'
#' @return the result of myclt which is a distribution of the sum of uniforms
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics curve hist polygon text
#' @importFrom stats dnorm pnorm qnorm runif
#'
#' @export
#'
#' @examples
#' w=myclt(n=50,iter=10000,a=5,b=10) # D

myclt=function(n,iter,a=0,b=5){
  x<-1
  y=runif(n*iter,a,b) # A
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # B
  sm=apply(data,2,sum) # C
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
}
