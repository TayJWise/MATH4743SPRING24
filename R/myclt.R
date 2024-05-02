#' myclt Function
#'
#' @param n a value
#' @param iter a value
#' @param a a value
#' @param b a value
#'
#' @return the result of myclt
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
