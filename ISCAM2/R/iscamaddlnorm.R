#' iscamaddlnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays a log normal density function.
#' @param x a vector of numeric values.
#' @keywords lognormal
#' @export
#' @examples
#' iscamaddlnorm(x)

iscamaddlnorm <- function(x){
  hist(x, freq=FALSE, xlab = deparse(substitute(x)), ylim=c(0,1))
  min = 0
  max = max(x)
  myseq = seq(min, max, .001)
  lines(myseq, dlnorm(myseq, mean(log(x)), sd(log(x))))
}
