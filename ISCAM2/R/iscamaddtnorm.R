#' iscamaddtnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays a t density function with df degrees of freedom 
#' and a normal density function.
#' @param x a vector of numeric values.
#' @param df degrees of freedom
#' @keywords t
#' @export
#' @examples
#' iscamaddtnorm(x, 3)


iscamaddtnorm <- function(x, df){
  hist(x, freq=FALSE, ylim=c(0,0.5))
  min = min(x)
  max = max(x)
  myseq = seq(min, max, .001)
  lines(myseq, dt(myseq, df), col=2)
  lines(myseq, dnorm(myseq, 0, 1), col=3)
  
  legend("topleft", inset=.05, legend=c("t", "Normal"),
         col=c(2, 3), lty=1, cex=0.8)
}
