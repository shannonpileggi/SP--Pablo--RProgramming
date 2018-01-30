#' AddExp Function
#'
#' This function creates a histogram of the inputted variable and overlays an exponential density function with lambda = 1/mean.
#' @param x a vector of numeric values.
#' @keywords exponential
#' @export
#' @examples
#' AddExp()


AddExp <- function(x){
  Description = "iscamaddexp(x) \n This function creates a histogram of the inputted
  variable \n and overlays an exponential density function with lambda = 1/mean."
  
  if(as.character(x[1])=="?") stop(Description)
  
  hist(x, freq=FALSE, xlab = deparse(substitute(x))) #what is deparse(substitute)?
  min <- 0
  max <- max(x)
  myseq <- seq(min, max, .001)
  lines(myseq, dexp(myseq, 1/mean(x))) #is this the best way to do this?
}

