#' mybin function for table
#'
#' @description
#' Displays a table of mybin based on the user inputting iteration, size of n, and probability
#'
#'
#' @param iter number of iterations
#' @param n a size inputted by user
#' @param p a probability
#'
#' @return table of mybin
#' @export
#'
#' @examples
#' mybinny(iter=1, n=10, p=0.7)
#'
mybinny=function(iter, n, p){
  # makes a matrix for sample
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)

  # succ function that goes until iterations number
  succ=c()
  for( i in 1:iter){
    # changes each index in matrix based on input values with n and p
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    # stores this sum of the new value in the index into succ
    succ[i]=sum(sam.mat[,i])
  }

  # makes a table of the succ values
  succ.tab=table(factor(succ,levels=0:n))
  # displays this succ table values
  succ.tab
  # divides the table by number of iterations
  succ.tab/iter
}
