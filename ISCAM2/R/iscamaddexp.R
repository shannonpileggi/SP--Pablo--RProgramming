#' iscamaddexp Function
#'
#' This function creates a histogram of the inputted variable and overlays an exponential density function with lambda = 1/mean.
#' @param x a vector of numeric values.
#' @keywords exponential
#' @export
#' @examples
#' iscamaddexp(x)


iscamaddexp <- function(x){
  hist(x, freq=FALSE, xlab = deparse(substitute(x)))
  min = 0
  max = max(x)
  myseq = seq(min, max, .001)
  lines(myseq, dexp(myseq, 1/mean(x)))
}
