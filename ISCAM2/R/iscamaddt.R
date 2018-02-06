#' iscamaddt Function
#'
#' This function creates a histogram of the inputted variable and overlays a t density function with df degrees of freedom.
#' @param x a vector of numeric values.
#' @param df degrees of freedom
#' @keywords t
#' @export
#' @examples
#' iscamaddt(x, 3)

iscamaddt <- function(x, df){
  hist(x, freq=FALSE)
  min = min(x)
  max = max(x)
  myseq = seq(min, max, .001)
  lines(myseq, dt(myseq, df))
}
