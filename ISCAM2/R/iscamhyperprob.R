#' iscamhyperprob Function
#'
#' This function calculates tail probabilities from the hypergeometric distribution.
#' @param k number of successes of interest (integer) or difference in conditional proportions
#' @param total total number of observations in the study
#' @param succ overall number of successes
#' @param n number of observations in group A
#' @param lower.tail a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#' @keywords hypergeometric probability
#' @export
#' @examples
#' iscamhyperprob(26, 52, 2, 5, lower.tail = FALSE)

iscamhyperprob <- function(k, total, succ, n, lower.tail){
  if (k<1 & k>0) {k=round((k*(total-n)*n+succ*n)/total)}
  
  fail = total-succ
  thisx = max(0, n-fail):min(n,succ)
  plot(thisx, dhyper(thisx, succ, fail, n), xlab="Number of Successes", ylab="Probability",  type="h")
  abline(h=0, col="gray")
  
  if (lower.tail) {
    this.prob=phyper(k, succ, fail, n)
    showprob=format(this.prob, digits=4)
    lines(0:k, dhyper(0:k, succ, fail, n), col="red", type="h")
    text(k, dhyper(k, succ, fail, n), labels=paste("P(X<=",k, ")\n = ", showprob), pos=2, col="red")
    cat("Probability", k, "and below =", this.prob, "\n")
    
  }
  if (!lower.tail){
    this.prob=1-phyper(k-1, succ, fail, n)
    showprob=format(this.prob, digits=4)
    lines(k:n, dhyper(k:n, succ, fail,n), col="red", type="h")
    #text(k, dhyper(k, succ, fail, n), labels=showprob, pos=4, col="red")
    text(k, dhyper(k, succ, fail, n), labels=paste("P(X>=",k, ")\n = ", showprob), pos=2, col="red")
    cat("Probability", k, "and above =", this.prob, "\n")
    
  }
  newtitle=substitute(paste("Hypergeometric (", N==x1,", ", M==x2, ",",  n==x3, ")" ), list(x1=total, x2=succ, x3=n));   title(newtitle)
  return(this.prob)
}
