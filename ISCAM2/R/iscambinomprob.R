#' iscambinomprob Function
#'
#' This function calculates tail probabilities from the binomial distribution.  
#' @param k number of successes of interest (must be an integer)
#' @param n number of trials (zero or more)
#' @param prob probability of success on each trial
#' @param lower.tail logical; a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive) 
#' @keywords binomial
#' @export
#' @examples
#' iscambinomprob(20, 30, 0.5, lower.tail = TRUE)
#' iscambinomprob(10, 55, 0.10, lower.tail = FALSE)

iscambinomprob <- function(k, n, prob, lower.tail){
  
  thisx = 0:n
  minx=max(0, n*prob-4*sqrt(prob*(1-prob)*n))
  maxx=min(n, n*prob+4*sqrt(prob*(1-prob)*n))
  myy=dbinom(floor(n*prob), n, prob)/2
  plot(thisx, dbinom(thisx, size=n, prob), xlab="Number of Successes", ylab="Probability",  type="h", xlim=c(minx, maxx))
  abline(h=0, col="gray")
  
  if (lower.tail) {
    this.prob=pbinom(k, n, prob)
    showprob=format(this.prob, digits=4)
    lines(0:k, dbinom(0:k, size=n, prob), col="red", type="h")
    text((minx+n*prob)/2, myy, labels=paste("P(X<=", k, ")\n =",showprob),pos=3, col="red")
    cat("Probability", k, "and below =", this.prob, "\n")
  }
  if (!lower.tail){
    this.prob=1-pbinom(k-1, n, prob)
    showprob=format(this.prob, digits=4)
    lines(k:n, dbinom(k:n, size=n, prob), col="red", type="h")
    text((maxx+n*prob)/2, myy, labels=paste("P(X>=",  k, ")\n =", showprob),pos=3, col="red")
    cat("Probability", k, "and above =", this.prob, "\n")
  }
  newtitle=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=prob));   title(newtitle)
  return(this.prob)
}
