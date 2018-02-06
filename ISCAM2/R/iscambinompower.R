#' iscambinompower Function
#'
#' This function determines the rejection region corresponding to the level of 
#' significance and the first probability and shows the second distribution shading 
#' its corresponding region.  
#' @param LOS level of significance
#' @param n number of trials (zero or more)
#' @param prob1 probability of success on each trial
#' @param alternative allows you to specify whether you want to find the probability 
#' "less" or "greater" or a symmetric "two.sided" probability 
#' @param prob2 NULL or a second probability
#' @keywords binomial power
#' @export
#' @examples
#' iscambinompower(.05, 30, 0.5, alternative = "above")
#' iscambinompower(.10, 55, 0.10, alternative = "below")
#' iscambinompower(.05, 144, 0.40, alternative = "two.sided")

iscambinompower <- function(LOS, n, prob1, alternative, prob2=NULL){
  
  thisx = 0:max(n)
  minx=max(0, min(n*prob1-4*sqrt(prob1*(1-prob1)*n), n*prob2-4*sqrt(prob2*(1-prob2)*n)))
  maxx=min(n, max(n*prob1+4*sqrt(prob1*(1-prob1)*n), n*prob2+4*sqrt(prob2*(1-prob2)*n)))
  myy1=dbinom(floor(n*prob1), n, prob1)/2
  par(mfrow=c(2,1))
  plot(thisx, dbinom(thisx, size=n, prob1), xlab="X=Number of Successes", ylab="Probability",  type="h", xlim=c(minx, maxx))
    abline(h=0, col="gray")
  
  if (alternative=="less") {
    rr=qbinom(LOS, n, prob1)-1
    this.prob1=pbinom(rr, n, prob1)
    showprob1=format(this.prob1, digits=4)
    lines(0:rr, dbinom(0:rr, size=n, prob1), col="red", type="h")
    text((minx+n*prob1)/2, myy1, labels=paste("P(X<=", rr, ")\n =", showprob1), pos=3, col="red")
    cat("Probability", rr, "and below =", this.prob1, "\n")
  }
  else if (alternative=="greater"){
    rr=qbinom(LOS, n, prob1, FALSE)+1
    this.prob1=1-pbinom(rr-1, n, prob1)
    showprob1=format(this.prob1, digits=4)
    lines(rr:n, dbinom(rr:n, size=n, prob1), col="red", type="h")
    text((maxx+n*prob1)/2, myy1, labels=paste("P(X>=",  rr, ")\n =", showprob1),pos=3, col="red")
    cat("Probability", rr, "and above =", this.prob1, "\n")
  }
  else if (alternative=="two.sided"){
    lowerrr=qbinom(LOS/2, n, prob1)-1
    upperrr=qbinom(LOS/2, n, prob1, FALSE)+1
    lowerprob1=pbinom(lowerrr, n, prob1)
    upperprob1=pbinom(upperrr-1, n, prob1, FALSE)
    showlowerprob1=format(lowerprob1, digits=4); showupperprob1=format(upperprob1, digits=4)
    showprob1=format(lowerprob1+upperprob1, digits=4)
    lines(0:lowerrr, dbinom(0:lowerrr, size=n, prob1), col="red", type="h")
    lines(upperrr:n, dbinom(upperrr:n, size=n, prob1), col="red", type="h")
    text((maxx+n*prob1)/2, myy1, labels=
           paste("P(X<=",  lowerrr, ")+P(X>=",upperrr, ")\n =", showlowerprob1, "+", showupperprob1, "\n =", showprob1),pos=3, col="red")
    cat("Probability in rejection region", showprob1, "\n")
  }
  else stop("Check input for alternative")
  
  newtitle=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=prob1));   title(newtitle)
  
  
  if (!is.null(prob2)){
    plot(thisx, dbinom(thisx, size=n, prob2), xlab="X=Number of Successes", ylab="Probability", type="h", xlim=c(minx, maxx))
    abline(h=0, col="gray")
    myy2=dbinom(floor(n*prob2), n, prob2)/2
    if (alternative=="less") {
      this.prob2=pbinom(rr, n, prob2)
      showprob2=format(this.prob2, digits=4)
      lines(0:rr, dbinom(0:rr, size=n, prob2), col="red", type="h")
      text((minx+n*prob2)/2, myy2, labels=paste("P(X<=", rr, ")\n =", showprob2), pos=3, col="red")
      cat("Probability", rr, "and below =", this.prob2, "\n")
    }
    else if (alternative=="greater"){
      this.prob2=1-pbinom(rr-1, n, prob2)
      showprob2=format(this.prob2, digits=4)
      lines(rr:n, dbinom(rr:n, size=n, prob2), col="red", type="h")
      text((maxx+n*prob2)/2, myy2, labels=paste("P(X>=",  rr, ")\n =", showprob2),pos=3, col="red")
      cat("Probability", rr, "and above =", this.prob2, "\n")
    }
    else if (alternative=="two.sided"){
      this.prob2=pbinom(lowerrr, n, prob2)+pbinom(upperrr-1, n, prob2, FALSE)
      showprob2=format(this.prob2, digits=4)
      lines(0:lowerrr, dbinom(0:lowerrr, size=n, prob2), col="red", type="h")
      lines(upperrr:n, dbinom(upperrr:n, size=n, prob2), col="red", type="h")
      text((maxx+n*prob2)/2, myy1, labels=paste("P(X<=",  lowerrr, ")+P(X>=",upperrr, ")\n =", showprob2),pos=3, col="red")
      cat("Probability in rejection region", this.prob2, "\n")
    }
    newtitle=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=prob2));   title(newtitle)
    
  }
  par(mfrow=c(1,1))
  
}
