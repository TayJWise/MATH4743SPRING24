#' mysample
#'
#' @param n,iter,time numeric values
#'
#' @return distributions
#' @export
#'
#' @examples
#' mysample()
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #release the table
    Sys.sleep(time)
  }
}
