#' iscamnormpower Function
#'
#' This function determines the rejection region corresponding to the level of significance and the first probability and shows the second distribution shading its corresponding region.
#' @param LOS level of significance
#' @param n sample size
#' @param prob1 probability of success 
#' @param alternative can be "less", "greater", or "two.sided" 
#' @param prob2 second probability of success
#' @keywords normal power
#' @export
#' @examples
#' iscamnormpower(.05, 20, .5, "less", .7)

iscamnormpower <- function(LOS, n, prob1, alternative, prob2){
  minx=max(0, min(prob1-4*sqrt(prob1*(1-prob1)/n), prob2-4*sqrt(prob2*(1-prob2)/n)))
  maxx=min(n, max(prob1+4*sqrt(prob1*(1-prob1)/n), prob2+4*sqrt(prob2*(1-prob2)/n)))
  mean = prob1
  std=sqrt(prob1*(1-prob1)/n)
  myy1=dnorm(mean, mean, std)/2
  par(mfrow=c(2,1))
  drawseq=seq(minx, maxx, .001)
  plot(drawseq, dnorm(drawseq, mean, std), type="l", xlab="Probability of Success", ylab="Density")
  
  if (alternative=="less") {
    rr = qnorm(LOS, mean, std)
    this.prob1=pnorm(rr, mean, std)
    showprob1=format(this.prob1, digits=4)
    drawseq=seq(minx, rr, .001)
    polygon(c(drawseq, rr, minx), c(dnorm(drawseq, mean, std), 0, 0), col="red")
    rr=format(rr, digits=4)
    text((minx+prob1)/2, myy1, labels=paste("P(p-hat<=", rr, ")\n =", showprob1), pos=3, col="red")
    cat("Probability", rr, "and below =", this.prob1, "\n")
  }
  else if (alternative=="greater"){
    rr=qnorm(LOS, mean, std, FALSE)
    this.prob1=1-pnorm(rr, mean, std)
    
    showprob1=format(this.prob1, digits=4)
    drawseq=seq(rr, maxx, .001)
    polygon(c(drawseq, maxx, rr), c(dnorm(drawseq, mean, std), 0, 0), col="red")
    rr = format(rr, digits=4)
    text((maxx+n*prob1)/2, myy1, labels=paste("P(p-hat>=",  rr, ")\n =", showprob1),pos=3, col="red")
    cat("Probability", rr, "and above =", this.prob1, "\n")
  }
  else if (alternative=="two.sided"){
    lowerrr=qnorm(LOS/2, mean, std)
    upperrr=qnorm(LOS/2, mean, std, FALSE)
    lowerprob1=pnorm(lowerrr,mean, std)
    upperprob1=pnorm(upperrr, mean, std, FALSE)
    showlowerprob1=format(lowerprob1, digits=4); showupperprob1=format(upperprob1, digits=4)
    showprob1=format(lowerprob1+upperprob1, digits=4)
    drawseq=seq(minx, lowerrr, .001)
    polygon(c(drawseq, lowerrr, minx), c(dnorm(drawseq, mean, std), 0, 0), col="red")
    drawseq=seq(upperrr, maxx, .001)
    polygon(c(drawseq, maxx, upperrr), c(dnorm(drawseq,mean, std), 0, 0), col="red")
    showupperrr = format(upperrr, digits=4); showlowerrr=format(lowerrr, digits=4)
    text((maxx+prob1)/2, myy1, labels=
           paste("P(p-hat<=",  showlowerrr, ")+P(p-hat>=",showupperrr, ")\n =", showlowerprob1, "+", showupperprob1, "\n =", showprob1),pos=3, col="red")
    cat("Probability in rejection region", showprob1, "\n")
  }
  else stop("Check input for alternative")
  
  newtitle=substitute(paste("Normal (", mu==x1,", ", sigma==x2, ")", ), list(x1=mean, x2=std));   title(newtitle)
  
  
  if (!is.null(prob2)){
    
    mean2= prob2
    std2=sqrt(prob2*(1-prob2)/n)
    drawseq=seq(minx, maxx, .001)
    plot(drawseq, dnorm(drawseq, mean2, std2), type="l", xlab="Probability of Success", ylab="Density")
    
    if (alternative=="less") {
      rr = qnorm(LOS, mean, std)
      this.prob2=pnorm(rr, mean2, std2)
      showprob2=format(this.prob2, digits=4)
      drawseq=seq(minx, rr, .001)
      polygon(c(drawseq, rr, minx), c(dnorm(drawseq, mean2, std2), 0, 0), col="red")
      rr=format(rr, digits=4)
      text((minx+prob2)/2, myy1, labels=paste("P(p-hat<=", rr, ")\n =", showprob2), pos=3, col="red")
      cat("Probability", rr, "and below =", this.prob2, "\n")
    }
    else if (alternative=="greater"){
      rr=qnorm(LOS, mean, std, FALSE)
      this.prob2=1-pnorm(rr, mean2, std2)
      showprob2=format(this.prob2, digits=4)
      drawseq=seq(rr, maxx, .001)
      polygon(c(drawseq, maxx, rr), c(dnorm(drawseq, mean2, std2), 0, 0), col="red")
      rr = format(rr, digits=4)
      text((maxx+n*prob1)/2, myy1, labels=paste("P(p-hat>=",  rr, ")\n =", showprob2),pos=3, col="red")
      cat("Probability", rr, "and above =", this.prob2, "\n")
    }
    else if (alternative=="two.sided"){
      lowerrr=qnorm(LOS/2, mean, std)
      upperrr=qnorm(LOS/2, mean, std, FALSE)
      lowerprob2=pnorm(lowerrr,mean2, std2)
      upperprob2=pnorm(upperrr, mean2, std2, FALSE)
      showlowerprob2=format(lowerprob2, digits=4); showupperprob2=format(upperprob2, digits=4)
      showprob2=format(lowerprob2+upperprob2, digits=4)
      drawseq=seq(minx, lowerrr, .001)
      polygon(c(drawseq, lowerrr, minx), c(dnorm(drawseq, mean2, std2), 0, 0), col="red")
      drawseq=seq(upperrr, maxx, .001)
      polygon(c(drawseq, maxx, upperrr), c(dnorm(drawseq,mean2, std2), 0, 0), col="red")
      showupperrr = format(upperrr, digits=4); showlowerrr=format(lowerrr, digits=4)
      text((maxx+prob2)/2, myy1, labels=
             paste("P(p-hat<=",  showlowerrr, ")+P(p-hat>=",showupperrr, ")\n =", showlowerprob2, "+", showupperprob2, "\n =", showprob2),pos=3, col="red")
      cat("Probability in rejection region", showprob2, "\n")
    }
    
    
    newtitle=substitute(paste("Normal (", mu==x1,", ", sigma==x2, ")", ), list(x1=mean2, x2=std2));   title(newtitle)
    
  }
  par(mfrow=c(1,1))
  
}
