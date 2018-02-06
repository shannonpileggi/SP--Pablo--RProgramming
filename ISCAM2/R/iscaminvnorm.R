#' iscaminvnorm Function
#'
#' This function calculates the normal quantile of a specified probability. 
#' @param prob1 desired probability
#' @param mean default = 0
#' @param sd default = 1
#' @param direction Specify whether you want this area to be "above", "below", or "outside" or "between"
#' @keywords inverse normal probability
#' @export
#' @examples
#' iscaminvnorm(.55, 15, 2, direction = "above")

iscaminvnorm <- function (prob1, mean = 0, sd = 1, direction) 
{
  min = mean - 4 * sd
  max = mean + 4 * sd
  thisx = seq(min, max, 0.001)
  plot(thisx, dnorm(thisx, mean, sd), xlab = "X=variable", 
       ylab = "density", type = "l")
  newtitle = paste("Normal (mean =", mean, ", SD = ", sd, 
                   ")")
  title(newtitle)
  abline(h = 0, col = "gray")
  if (direction == "below") {
    answer = signif(qnorm(prob1, mean, sd, TRUE), 4)
    thisrange = seq(min, answer, 0.001)
    polygon(c(thisrange, answer, 0), c(dnorm(thisrange, 
                                             mean, sd), 0, 0), col = "blue")
    text((min + answer)/2, dnorm(answer, mean, sd)/2, labels = prob1, 
         pos = 2, col = "blue")
    text(answer, 0, labels = paste("X<", answer), col = "red")
    cat("the observation with", prob1, "probability below is", 
        answer, "\n")
  }
  else if (direction == "above") {
    answer = signif(qnorm(prob1, mean, sd, FALSE), 4)
    thisrange = seq(answer, max, 0.001)
    polygon(c(answer, thisrange, max), c(0, dnorm(thisrange, 
                                                  mean, sd), 0), col = "pink")
    text((answer + max)/2, (dnorm(answer, mean, sd)/2), 
         labels = prob1, pos = 4, col = "blue")
    text(answer, 0, labels = paste("X>", answer), col = "red")
    cat("The observation with", prob1, "probability above is", 
        answer, "\n")
  }
  else if (direction == "between") {
    answer1 = signif(qnorm((1 - prob1)/2, mean, sd, TRUE), 
                     4)
    answer2 = mean + (mean - answer1)
    thisrange = seq(answer1, answer2, 0.001)
    polygon(c(answer1, thisrange, answer2), c(0, dnorm(thisrange, 
                                                       mean, sd), 0), col = "pink")
    text(mean, dnorm(mean, mean, sd)/2, labels = prob1, 
         col = "blue")
    text(answer1, 0, labels = paste("X>", answer1), col = "red")
    text(answer2, 0, labels = paste("X<", answer2), co = "red")
    cat("There is", prob1, "probability between", answer1, 
        "and", answer2, "\n")
  }
  else if (direction == "outside") {
    answer1 = signif(qnorm(prob1/2, mean, sd, TRUE), 4)
    answer2 = mean + (mean - answer1)
    thisrange1 = seq(min, answer1, 0.001)
    thisrange2 = seq(answer2, max, 0.001)
    polygon(c(min, thisrange1, answer1), c(0, dnorm(thisrange1, 
                                                    mean, sd), 0), col = "pink")
    polygon(c(answer2, thisrange2, max), c(0, dnorm(thisrange2, 
                                                    mean, sd), 0), col = "pink")
    text(answer1, dnorm(answer1, mean, sd)/2, labels = prob1/2, 
         col = "blue", pos = 2)
    text(answer2, dnorm(answer2, mean, sd)/2, labels = prob1/2, 
         col = "blue", pos = 4)
    text(answer1, 0, labels = paste("X<", answer1), col = "red")
    text(answer2, 0, labels = paste("X>", answer2), col = "red")
    cat("There is", prob1, "probability outside", answer1, 
        "and", answer2, "\n")
  }
}
