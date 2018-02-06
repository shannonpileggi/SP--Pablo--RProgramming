#' iscamtprob Function
#'
#' This function calculates tail probability for the t distribution.
#' @param xval x value
#' @param df degrees of freedom
#' @param direction a string for finding the probability above ("above") or below ("below") the inputted value 
#' @param xval2 second observation necessary if "outside" or "between" is specified as the direction
#' @keywords t probability
#' @export
#' @examples
#' iscamtprob(4, 5, "above")

iscamtprob <- function(xval, df, direction, xval2=NULL){
  minx=min(-5, -1*abs(xval)-.5)
  maxx=max(5, abs(xval)+.5)
  thisx= seq(minx, maxx, .001)
  xlabel="t-values"
  plot(thisx, dt(thisx, df), col=3, xlim=c(minx, maxx), type="l", xlab=xlabel, ylab="density")
  abline(h=0, col="gray")
  
  if (direction=="below") {
    probseq=seq(minx,max(minx, xval),.001)
    tprob=pt(xval, df)
    showprob=format(tprob, digits=4)
    polygon(c(probseq,max(minx,xval),0), c(dt(probseq,df), 0, 0), col="red", border="red")
    text(minx, dt(0, df)/2, labels=paste("P(X<=", xval, ") \n =",showprob), col="red", pos=4)
  }
  else  if (direction=="above"){
    probseq=seq(min(xval, maxx),maxx,.001)
    tprob=pt(xval, df, lower.tail=FALSE)
    showprob=format(tprob, digits=4)
    polygon(c(min(maxx,xval), probseq, 1), c(0, dt(probseq,df), 0), col="red", border="red")
    text(maxx, dt(0, df)/2, labels=paste("P(X>=", xval, ") \n =", showprob), col="red", pos=2)
  }
  else if (direction=="between"){
    if(is.null(xval2)) stop("You need to specify a second observation value.")
    if(xval2 < xval) {temp=xval; xval=xval2; xval2=temp}
    probseq=seq(xval, xval2,.001)
    tprob=pt(xval2, df)-pt(xval, df)
    showprob=format(tprob, digits=4)
    polygon(c(xval, probseq, xval2), c(0, dt(probseq, df), 0), col="red", border="red")
    text(minx, dt(0, df)/2, labels=paste("P(", xval, "<= X<=", xval2, ") \n =", showprob), col="red", pos=4)
  }
  else if (direction=="outside"){
    maxx = max(maxx, xval2) 
    if(is.null(xval2)) stop("You need to specify a second observation value.")
    if(xval2 < xval) {temp=xval; xval=xval2; xval2=temp}
    probseq1=seq(minx, xval, .001)
    probseq2=seq(xval2, maxx, .001)
    tprob=1-(pt(xval2, df) -pt(xval, df))
    showprob=format(tprob, digits=4)
    polygon(c(minx, probseq1, xval), c(0, dt(probseq1, df), 0), col="red", border="red")
    polygon(c(xval2, probseq2, maxx), c(0, dt(probseq2, df), 0), col="red", border="red")
    text(-2, dt(0, df)/2, labels=paste("P(X <=", xval, ") and \n P(X>=", xval2, ") \n =", showprob), col="red", pos=2)
  }
  else stop("Use \"above\", \"below\", \"between\", or \"outside\" as the direction.")
  
  newtitle=substitute(paste("t(", df==x3, ")" ), list(x3=df))
  title(newtitle)
  cat(c("probability:",  showprob),  "\n")
}
