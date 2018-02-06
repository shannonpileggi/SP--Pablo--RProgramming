#' iscamchisqprob Function
#'
#' This function calculates the upper tail probability for the chi-square distribution
#' @param xval x value
#' @param df degrees of freedom
#' @keywords chi square
#' @export
#' @examples
#' iscamchisqprob(10, 4)

iscamchisqprob <- function (xval, df){
  minx = 0
  maxx = max(20, xval, df)
  thisx = seq(minx, maxx, 0.001)
  xlabel = "chi-square values"
  plot(thisx, dchisq(thisx, df), col = 3, xlim = c(minx, maxx), 
       type = "l", xlab = xlabel, ylab = "density")
  abline(h = 0, col = "gray")
  probseq = seq(min(xval, maxx), maxx, 0.001)
  chisqprob = pchisq(xval, df, lower.tail = FALSE)
  showprob = format(chisqprob, digits = 4)
  polygon(c(min(maxx, xval), probseq, maxx), c(0, dchisq(probseq, 
                                                         df), 0), col = "red", border = "red")
  text(maxx, dchisq(0, df), labels = paste("P(X>=", xval, 
                                           ") \n =", showprob), col = "red", pos = 2)
  newtitle = substitute(paste("Chi-Square(", df == x3, ")"), 
                        list(x3 = df))
  title(newtitle)
  cat(c("probability:", showprob), "\n")
}
