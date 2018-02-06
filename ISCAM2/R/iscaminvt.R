#' iscaminvt Function
#'
#' This function calculates the t quantile of a specified probability.
#' @param prob1 desired probability
#' @param df degrees of freedom
#' @param direction Specify whether you want this area to be "above", "below", or "outside" or "between"
#' @keywords inverse t probability
#' @export
#' @examples
#' iscaminvt(.55, 5, direction = "outside")

iscaminvt <- function(prob1, df, direction){
  min=-4
  max=4
  thisx = seq(min,max, .001)
  plot(thisx, dt(thisx, df), xlab="t-values", ylab="density",  type="l")
  newtitle=paste("t (df =", df, ")"); title(newtitle)
  
  abline(h=0, col="gray")
  if (direction=="below") {
    answer=signif(qt(prob1, df, lower.tail=TRUE),4)
    thisrange=seq(min,answer,.001)
    #should we use min instead of zero?
    polygon(c(thisrange,answer,0), c(dt(thisrange,df), 0, 0), col="blue")
    text((min+answer)/2, dt(answer, df)/2, labels=prob1, pos=2, col="blue")
    text(answer, 0, labels=paste("T<", answer), col="red")
    cat("the observation with", prob1, "probability below is", answer, "\n")
  }
  else if (direction=="above") {
    answer=signif(qt(prob1, df,  lower.tail=FALSE), 4)
    thisrange=seq(answer,max, .001)
    polygon(c(answer,thisrange,max), c(0, dt(thisrange,df), 0), col="pink")
    text((answer+max)/2, (dt(answer, df)/2), labels=prob1, pos=4, col="blue")
    text(answer, 0, labels=paste("T>", answer), col="red")
    cat("The observation with", prob1, "probability above is", answer, "\n")
  }
  else if (direction=="between"){
    answer1=signif(qt((1-prob1)/2, df, lower.tail=TRUE), 4)
    answer2=0+(0-answer1)
    thisrange=seq(answer1, answer2, .001)
    polygon(c(answer1, thisrange, answer2), c(0, dt(thisrange, df), 0), col="pink")
    text(0, (dt(.5, df)/2), labels=prob1, col="blue")
    text(answer1, 0, labels=paste("T>", answer1), col="red")
    text(answer2, 0, labels=paste("T<", answer2), co="red")
    cat("There is", prob1, "probability between", answer1, "and", answer2, "\n")
  }
  else if (direction=="outside"){
    answer1=signif(qt(prob1/2, df, lower.tail=TRUE), 4)
    answer2=0+(0-answer1)
    thisrange1=seq(min, answer1, .001)
    thisrange2=seq(answer2, max, .001)
    polygon(c(min, thisrange1, answer1), c(0, dt(thisrange1, df), 0), col="pink")
    polygon(c(answer2, thisrange2, max), c(0, dt(thisrange2,df), 0), col="pink")
    text(answer1, dt(answer1, df)/2, labels=prob1/2, col="blue", pos=2)
    text(answer2, dt(answer2, df)/2, labels=prob1/2, col="blue", pos=4) 
    text(answer1, 0, labels=paste("T<", answer1), col="red")
    text(answer2, 0, labels=paste("T>", answer2), col="red")
    cat("There is", prob1, "probability outside", answer1, "and", answer2, "\n")
  }
  return(c(answer1, answer2))
}
