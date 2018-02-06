AddLNorm <- function(x){
  #' AddLNorm Function
  #'
  #' This function creates a histogram of the inputted variable and overlays an normal density function.
  #' @param x a vector of numeric values.
  #' @keywords normal
  #' @export
  #' @examples
  #' AddLNorm()
  
  h <- hist(x, plot=FALSE)
  plot(h, freq=FALSE, xlab = deparse(substitute(x)), ylab="Percent", col="gray")
  min = 0
  max = max(x)
  myseq = seq(min, max, .001)
  lines(myseq, dlnorm(myseq, mean(log(x)), sd(log(x))))
}
