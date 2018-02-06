#' AddExp Function
#'
#' This function creates a histogram of the inputted variable and overlays an exponential density function with lambda = 1/mean.
#' @param x a vector of numeric values.
#' @keywords exponential
#' @export
#' @examples
#' AddExp()


AddExp <- function(x){
  h <- hist(x, plot=F) #what is deparse(substitute)?
  h$density <- h$counts/sum(h$counts)*100
  
  plot(h, freq=FALSE, xlab = deparse(substitute(x)), ylab="Percent", col="gray")
  min <- 0
  max <- max(x)
  myseq <- seq(min, max, .001)
  lines(myseq, dexp(myseq, 1/mean(x))) #is this the best way to do this?
}
