#' iscaminvbinom Function
#'
#' This function calculates the binomial quantile of a specified probability. The integer that achieves at most the stated probability will be returned.
#' @param alpha alpha level
#' @param n sample size
#' @param prob despired probability of success
#' @param lower.tail upper tail (FALSE) or lower tail (TRUE)
#' @keywords inverse binomial
#' @export
#' @examples
#' iscaminvbinom(.05, 15, .20, lower.tail = FALSE)

iscaminvbinom <- function(alpha, n, prob, lower.tail){
  thisx = 0:n
  minx=max(0, n*prob-4*sqrt(prob*(1-prob)*n))
  maxx=min(n, n*prob+4*sqrt(prob*(1-prob)*n))
  myy=dbinom(floor(n*prob), n, prob)/2
  plot(thisx, dbinom(thisx, size=n, prob), xlab="X=Number of Successes", ylab="Probability",  type="h", xlim=c(minx, maxx))
  newtitle=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=prob));   title(newtitle)
  myy=dbinom(floor(n*prob), n, prob)/2
  
  abline(h=0, col="gray")
  if (lower.tail) {
    answer=qbinom(alpha, n, prob, lower.tail)-1
    actualprob=format(pbinom(answer, n, prob, lower.tail),digits=4)
    lines(0:answer, dbinom(0:answer, size=n, prob), col="red", type="h")
    text(x=minx, y=myy, labels=paste("P(X <=", answer, ")=", actualprob, "\n P(X <=", answer+1, ")=", format(pbinom(answer+1, n, prob, lower.tail), digits=4)),  col="red")
    #text(answer, dbinom(answer, size=n, prob), labels=actualprob, pos=2, col="red")
    text(answer, 0, labels=paste("X=", answer), col="red")
    cat("The observation with at most", alpha, "probability at or below is", answer, "\n")
  }
  if (!lower.tail) {
    answer=qbinom(alpha, n, prob, lower.tail)+1
    actualprob=format(pbinom(answer-1, n, prob, lower.tail),digits=4)
    lines(answer:n, dbinom(answer:n, size=n, prob), col="red", type="h")
    myx=(maxx+n*prob)/2
    text(x=myx, y=myy, labels=paste("P(X >=", answer, ")=", actualprob, "\n P(X >=", answer-1, ")=", format(pbinom(answer-2, n, prob, lower.tail), digits=4)),  col="red")
    #text(answer, dbinom(answer, size=n, prob), labels=actualprob, pos=2, col="red")
    text(answer, 0, labels=paste("X=", answer), col="red")
    cat("The observation with at most", alpha, "probability at or above is", answer, "\n")
    
  }
  return(answer)
}
