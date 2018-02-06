#' iscambinomnorm Function
#'
#' This function illustrates the normal approximation to the binomial. 
#' @param k number of successes of interest
#' @param n number of trials (zero or more)
#' @param prob probability of success on each trial
#' @param direction allows you to specify whether you want to find the probability 
#' "above" or "below" k or a symmetric "two.sided" probability 
#' @keywords binomial normal
#' @export
#' @examples
#' iscambinomnorm(20, 30, 0.5, direction = "above")
#' iscambinomnorm(10, 55, 0.10, direction = "below")
#' iscambinomnorm(33, 144, 0.40, direction = "two.sided")

iscambinomnorm <- function(k, n, prob, direction){
  Description = "iscambinomnorm(k, n, prob, direction) \n This function illustrates the normal approximation to the binomial.\n  k is the number of successes of interest, n and prob are the number of trials and success probability \n direction allows you to specify whether you want to find the probability \"above\" or \"below\" k \n or a symmetric \"two.sided\" probability "
  
  if(as.character(k)=="?") stop(Description)
  
  thisx = 0:n
  phat=thisx/n
  minx=max(0, n*prob-4*sqrt(prob*(1-prob)*n))
  maxx=min(n, n*prob+4*sqrt(prob*(1-prob)*n))
  myy=dbinom(floor(n*prob), n, prob)/2
  plot(thisx, dbinom(thisx, size=n, prob), xlab="X=Number of Successes (Proportion)", ylab="Probability",  type="h", xlim=c(minx, maxx))
  abline(h=0, col="gray")
  axis(side=1,at = thisx, labels=signif(phat,2), padj=1.2, tick=FALSE, col.axis="blue")
  normmean=n*prob
  normsd=sqrt(n*prob*(1-prob))
  normseq=seq(0,n,.001)
  lines(normseq, dnorm(normseq, normmean, normsd), col=3)
  if (direction == "below") {
    probseq=seq(0,k,.001)
    phatseq=probseq/n
    withcorrect=seq(0,k+.5, .001)
    this.prob=pbinom(k, n, prob)
    normprob=pnorm(k, normmean, normsd)
    normprob2=pnorm(k+.5, normmean, normsd)
    showprob=format(this.prob, digits=4)
    showprob2=format(normprob, digits=4)
    showprob3=format(normprob2, digits=4)
    polygon(c(withcorrect,k+.5,0), c(dnorm(withcorrect, normmean, normsd), 0, 0), col=6)
    polygon(c(probseq,k,0), c(dnorm(probseq, normmean, normsd), 0, 0), col=5, border="red")
    lines(0:k, dbinom(0:k, size=n, prob), col="red", type="h")
    #lines(phatseq, dnorm(probseq,normmean,normsd))
    text((minx+normmean)/2, myy, labels=paste("P(X<=", k, ")=", showprob), col="red")
    
  }
  else if (direction == "above"){
    this.prob=1-pbinom(k-1, n, prob)
    probseq=seq(k,n,.001)
    withcorrect=seq(k-.5,n, .001)  
    normprob=pnorm(k, normmean, normsd, lower.tail=FALSE)
    normprob2=pnorm(k-.5, normmean, normsd, lower.tail=FALSE)
    showprob=format(this.prob, digits=4)
    showprob2=format(normprob, digits=4)
    showprob3=format(normprob2, digits=4)
    polygon(c(k-.5, withcorrect,n), c(0, dnorm(withcorrect, normmean, normsd), 0), col=6)
    polygon(c(k, probseq,n), c(0, dnorm(probseq, normmean, normsd), 0), col=5, border="red")
    lines(k:n, dbinom(k:n, size=n, prob), col="red", type="h")
    text((maxx+normmean)/2, myy, labels=paste("P(X>=", k, ")=", showprob), col="red")
  }
  else if (direction == "two.sided"){
    if(k < normmean){
      k1=k
      k2=floor(min(normmean-k+normmean, n))
      newvalue=dbinom(k2, size=n, prob)
      if (newvalue <= dbinom(k1, size=n, prob)+.00001) {k2=k2}
      else {k2=k2+1}
    }
    else {
      k1=floor(min(normmean-(k-normmean), n))
      k2=k
      newvalue=dbinom(k1, size=n, prob)
      if (newvalue <= dbinom(k, size=n, prob) +.00001){k1=k1}
      else{k1=k1-1}
      
      
    }
    this.prob=pbinom(k1, n, prob) + pbinom(k2-1, n, prob, lower.tail=FALSE)
    normprob=pnorm(k1, normmean,normsd) + pnorm(k2, normmean, normsd, lower.tail=FALSE)
    normprob2=pnorm(k1+.5, normmean, normsd) + pnorm(k2-.5, normmean, normsd, lower.tail=FALSE)
    showprob=format(this.prob, digits=4)
    showprob2=format(normprob, digits=4)
    showprob3=format(normprob2, digits=4)
    probseq1=seq(0,k1, .001); probseq2=seq(k2, n, .001)
    withcorrect=seq(0, k1+.5, .001); withcorrect2=seq(k2-.5, n, .001)
    polygon(c(withcorrect,k1+.5,0), c(dnorm(withcorrect, normmean, normsd), 0, 0), col=6)
    polygon(c(probseq1,k1,0), c(dnorm(probseq1, normmean, normsd), 0, 0), col=5, border="red")
    polygon(c(k2-.5, withcorrect2,n), c(0, dnorm(withcorrect2, normmean, normsd), 0), col=6)
    polygon(c(k2, probseq2,n), c(0, dnorm(probseq2, normmean, normsd), 0), col=5, border="red")
    lines(0:k1, dbinom(0:k1, size=n, prob), col="red", type="h")
    lines(k2:n, dbinom(k2:n, size=n, prob), col="red", type="h")
    text((maxx+normmean)/2, myy, labels=paste("P(X<=",k1, ")+ P(X>=", k2, ")=", showprob), col="red")
  }
  newtitle=substitute(paste("Binomial (", n==x1,", ", pi==x2, "), Normal(", mean==x3, ",  ", SD==x4, ")" ), list(x1=n, x2=prob, x3=prob, x4=signif(normsd/n,4)))
  title(newtitle)
  full=c(c(" binomial:", showprob), c("\n normal approx:",  showprob2), c("\n normal approx with continuity:", showprob3))
  cat(full, "\n")
}
