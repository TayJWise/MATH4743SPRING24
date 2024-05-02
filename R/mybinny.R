#' mybin function for table
#'
#' @param iter a value
#' @param n a value
#' @param p a value
#'
#' @return table of mybin
#' @export
#'
#' @examples
#' mybinny(iter=1, n=10, p=0.7)
#'
mybinny=function(iter, n, p){
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)

  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    succ[i]=sum(sam.mat[,i])
  }

  succ.tab=table(factor(succ,levels=0:n))
  succ.tab
  succ.tab/iter
}
