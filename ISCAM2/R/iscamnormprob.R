#' iscamnormprob Function
#'
#' This function calculates tail probability for the normal distribution. 
#' @param xval x value
#' @param mean default mean = 0
#' @param sd default standard deviation = 1
#' @param direction a String for finding the probability above "above" or below "below" the inputted value. If "outside" or "between" are specified, a second larger observation needs to be given at the end.  
#' @param label It is highly recommended that you indicate a label for the horizontal axis, with the quotation marks e.g., "sample proportions"
#' @param xval2 used if direction is specified as "outside" or "between"
#' @param digits number of tick marks on x axis
#' @keywords normal tail probability
#' @export
#' @examples
#' iscamnormprob(12, 16, 1.5, direction="above", label = "sample proportions")
#' iscamnormprob(2, 5, 1.5, direction="outside", label = "sample proportions", xval2 = 15)

iscamnormprob <- function(xval, mean=0, sd=1, direction, label = NULL, xval2=NULL, digits = 4){
  
  if(is.null(xval2)) xval2=abs(xval)
  if(xval2 < xval) {temp=xval; xval=xval2; xval2=temp}
  xvallabel=format(xval, digits=digits)
  xval2label=format(xval2,digits=digits)
  options("scipen"=100, "digits"=digits)
  
  minx=min(mean-4*sd, xval-.15*abs(xval))
  maxx=max(mean+4*sd, xval2+.15*xval2)
  
  thisx = seq(minx,maxx, .001)
  xlabel="x-variable"
  if(!is.null(label)) xlabel=label
  
  plot(thisx, dnorm(thisx, mean, sd), col=3, xlim=c(minx, maxx), type="l", xlab=xlabel, ylab="density")
  abline(h=0, col="gray")
  
  if (direction=="below") {
    probseq=seq(minx,max(minx, xval),.001)
    normprob=pnorm(xval, mean, sd)
    showprob=format(normprob, digits=digits)
    polygon(c(probseq,max(minx,xval),0), c(dnorm(probseq, mean, sd), 0, 0), col="red", border="red")
    text(minx, dnorm(mean, mean, sd)/2, labels=paste("P(X<=", xvallabel, ") \n =",showprob), col="red", pos=4)
  }
  else  if (direction=="above"){
    probseq=seq(min(xval, maxx),maxx,.001)
    normprob=pnorm(xval, mean, sd, lower.tail=FALSE)
    showprob=format(normprob, digits=digits)
    polygon(c(min(maxx,xval), probseq, 1), c(0, dnorm(probseq, mean, sd), 0), col="red", border="red")
    text(maxx, dnorm(mean, mean, sd)/2, labels=paste("P(X>=", xvallabel, ") \n =", showprob), col="red", pos=2)
  }
  else if (direction=="between"){
    if(is.null(xval2)) stop("You need to specify a second observation value.")
    probseq=seq(xval, xval2,.001)
    normprob=pnorm(xval2, mean, sd)-pnorm(xval, mean, sd)
    showprob=format(normprob, digits=digits)
    polygon(c(xval, probseq, xval2), c(0, dnorm(probseq, mean, sd), 0), col="red", border="red")
    text(minx, dnorm(mean, mean, sd)/2, labels=paste("P(", xvallabel, "<= X<=", xval2label, ") \n =", showprob), col="red", pos=4)
  }
  else if (direction=="outside"){
    if(is.null(xval2)) stop("You need to specify a second observation value.")
    probseq1=seq(minx, xval, .001)
    probseq2=seq(xval2, maxx, .001)
    normprob=1-(pnorm(xval2, mean, sd) -pnorm(xval, mean, sd))
    showprob=format(normprob, digits=digits)
    polygon(c(minx, probseq1, xval), c(0, dnorm(probseq1, mean, sd), 0), col="red", border="red")
    polygon(c(xval2, probseq2, maxx), c(0, dnorm(probseq2, mean, sd), 0), col="red", border="red")	
    text(minx, dnorm(mean, mean, sd)/2, labels=paste("P(X <=", xvallabel, ") and \n P(X>=", xval2label, ")  =", showprob), col="red", pos=4)
  }
  else stop("Use \"above\", \"below\", \"between\", or \"outside\" as the direction.")
  
  newtitle=substitute(paste("Normal(", mean==x3, ",  ", SD==x4, ")" ), list(x3=mean, x4=signif(sd,4)))
  title(newtitle)
  cat(c("probability:",  showprob),  "\n")
}
