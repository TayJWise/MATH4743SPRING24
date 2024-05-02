#' Project1 ntickets Function
#'
#' @description
#' This function is for project1 to calculate the statistics for
#' the number of tickets sold on a flight based on the provided inputs
#'
#' @param N number of seats on a flight
#' @param gamma probability that a plane actually is overbooked
#' @param p probability of success that a person shows up
#'
#' @importFrom graphics abline segments par
#' @importFrom stats pbinom quantile uniroot
#'
#' @return two graphs and listed values of nc, nd, gamma, N, and p
#' @export
#'
#' @examples
#' ntickets(N=400,gamma = 0.02, p = 0.95)

ntickets <- function(N, gamma, p) {

  # Sequence so we can get values n
  n <- seq(N, floor(N+N/10), by=1)

  # ind <- which.min(abs(n)) is how to find min index within all of n

  # Discrete approximation, made with function so it's easier to see later
  # Also made up here because I keep having to change things in it
  discrete <- function(n)
  {
    # Found by subtracting gamma and pbinom based on inputs
    return(1-gamma-pbinom(q=N,size=n,prob=p))
  }

  # Finding the min index for discrete out of n
  indD <- which.min(abs(discrete(n)))

  # nd is at the indD index within the sequenced n values
  nd <- n[indD]

  # Normal approximation based on Central Limit Theorem given input n
  continuous <- function(n)
  {
    mu=n*p
    sigma=sqrt((n*p*(1-p)))
    result = 1-gamma-pnorm(q=N,mean=mu,sd=sigma)
    return(result)
  }

  # min index for continuous
  indC <- which.min(abs(continuous(n)))

  # Calculations for Normal Approximation
  # mu=np
  mu <- N*p
  # sigma=sqrt(npq)
  sigma <- sqrt(N*p*(1 - p))
  # Find z
  z <- qnorm(1-gamma)

  nc <- n[indC]
  nc <- N*2 - (mu + sigma*z)

  # Getting the y values for abline for both nd and nc
  ynd <- discrete(nd)
  ync <- continuous(nc)

  # Printing out nd, nc, N, p, and gamma
  print(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))

  # Plot 2 graphs with 2 rows and 1 column so that they are above each other
  # on the html
  #par(mfrow = c(2, 1))

  # Objective plot for discrete
  plot(n, discrete(n), type = 'b', col = 'blue', pch=19,
       xlab = 'n', ylab = 'Objective',
       main = 'Objective vs n Discrete')

  # Horizontal and Vertical lines (intersecting) for discrete
  abline(v = nd, col = 'red')
  abline(h = ynd, col = 'red')

  # Objective plot for continuous (Normal Approximation)
  plot(n, continuous(n), type = 'l', col = 'black',
       xlab = 'n', ylab = 'Objective',
       main = 'Objective vs n Continuous')

  # Horizontal and Vertical lines (intersecting) for discrete
  abline(v = nc, col = 'blue')
  abline(h = ync, col = 'blue')
}
