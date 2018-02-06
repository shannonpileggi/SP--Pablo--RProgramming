#' iscamaddnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays a normal density function.
#' @param x a vector of numeric values.
#' @keywords normal
#' @export
#' @examples
#' iscamaddnorm(x)


iscamaddnorm <- function(x){
  hist(x, freq=FALSE)
  min = min(x)
  max = max(x)
  myseq = seq(min, max, .001)
  lines(myseq, dnorm(myseq, mean(x), sd(x)))
}
