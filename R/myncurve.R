#' myncurve
#'
#' @description
#' Provides a visualization of a normal distribution based on input mean, sigma, and a
#'
#' @param mu the mean
#' @param sigma the standard deviation
#' @param a calculates up to x=a
#'
#' @return a normal distribution based on inputs
#' @export
#'
#' @examples
#' myncurve(mu=10, sigma=4, a=25)
myncurve = function(mu, sigma, a){


  x<-1
  x<-a
  # Y~N(4,2), P(1<=Y<5)

  mean <- mu
  sd <- sigma

  curve(dnorm(x,mean=mu,sd=sigma),xlim=c(mean-(3*sd),mean+(3*sd)))

  # Find the area between x=10 and 25

  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mean,a,length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean,sd)

  # Fill in the polygon with the given vertices
  polygon(c(mean=mu,xcurve,a),c(0,ycurve,0),col="pink")

  # Put in the text with the appropriate area

  # Area
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,2)

  # Click to paste the text onto the graph
  text(x=a, y=dnorm(a,mu,sigma)*(1/2), paste("Area = ", prob, sep=""))

}
