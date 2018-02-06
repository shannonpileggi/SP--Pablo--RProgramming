#' iscamonepropztest Function
#'
#' This function calculates a one proportion z-test and/or interval.
#' @param observed number of success or sample proportion (assumed if value is less than 1)
#' @param n sample size
#' @param hypothesized hypothesized probability of success
#' @param alternative form of alternative hypothesis "less", "greater", or "two.sided" 
#' @param conf.level confidence level(s) for a two-sided confidence interval
#' @keywords one proportion z test
#' @export
#' @examples
#' iscamonepropztest(12, 15, .70, "greater")

iscamonepropztest <- function(observed, n, hypothesized=NULL, alternative="two.sided", conf.level=NULL){
  if (observed<1) {observed=round(n*observed)}
  myout=prop.test(observed, n, hypothesized, alternative, correct=FALSE)
  cat("\n", "One Proportion z test\n", sep="","\n")
  statistic=signif(observed/n, 4)
  cat(paste("Data: observed successes = ", observed, ", sample size = ", n, ", sample proportion = ", statistic, "\n\n", sep=""))
  zvalue=NULL; pvalue=NULL; 
  if (!is.null(hypothesized)){
    cat(paste("Null hypothesis       : pi =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided="<>", not.equal="<>")
    cat(paste("Alternative hypothesis: pi", altname, hypothesized, sep=" "),"\n")
    zvalue = (statistic-hypothesized)/sqrt(hypothesized*(1-hypothesized)/n)
    cat("z-statistic:", signif(zvalue,4), "\n")
    pvalue=signif(myout$p.value, 4)
    cat("p-value:",pvalue, "\n")
    SD= sqrt(hypothesized*(1-hypothesized)/n)
    min=min(hypothesized-4*SD, hypothesized-abs(zvalue)*SD-.001)
    max= max(hypothesized +4*SD, hypothesized+abs(zvalue)*SD+.001)
    x=seq(min,max,.001)
    plot(x,dnorm(x, hypothesized,SD), xlab="<- sample proportions ->", ylab="Density", type="l", ylim=c(0, dnorm(hypothesized, hypothesized, SD)))
    zseq=c(hypothesized-3*SD, hypothesized -2*SD, hypothesized -SD, hypothesized, hypothesized +SD, hypothesized +2*SD, hypothesized +3*SD)
    axis(side=1,at = zseq, labels=c("z=-3", "z=-2", "z=-1", "z=0", "z=1", "z=2", "z=3"), padj=1.2, tick=FALSE, col.axis="blue")
    abline(h=0, col="black")
    if(alternative=="less"){
      drawseq=seq(min, statistic, .001)
      polygon(c(drawseq, statistic, min), c(dnorm(drawseq, hypothesized, SD), 0, 0), col="red")
      text(min, dnorm(hypothesized, hypothesized,SD)/2, labels=paste("z-statistic:", signif(zvalue,4)), pos=4, col="blue")
      text(min, dnorm(hypothesized, hypothesized, SD)/2.2, labels=paste("p-value:", pvalue), pos=4,  col="red")
    }
    else if(alternative=="greater"){
      drawseq=seq(statistic, max, .001)
      polygon(c(statistic , drawseq, max), c(0, dnorm(drawseq, hypothesized, SD), 0), col="red")
      text(max, dnorm(hypothesized, hypothesized,SD)/2, labels=paste("z-statistic:", signif(zvalue,4)), pos=2, col="blue")
      text(max, dnorm(hypothesized, hypothesized, SD)/2.2, labels=paste("p-value:", pvalue), pos=2,  col="red")
    }	
    else if(alternative=="two.sided" || alternative=="not.equal"){
      if(statistic < hypothesized){
        drawseq1=seq(min, statistic, .001)
        drawseq2=seq(hypothesized+(hypothesized-statistic), max, .001)
      }
      else {
        drawseq1=seq(min, hypothesized-(statistic-hypothesized), .001)
        drawseq2=seq(statistic, max, .001)
      }
      polygon(c(min, drawseq1, drawseq1[length(drawseq1)]), c(0, dnorm(drawseq1, hypothesized, SD), 0), col="red")
      polygon(c(drawseq2[1], drawseq2, max), c(0, dnorm(drawseq2, hypothesized, SD), 0), col="red")
      text(min, dnorm(hypothesized, hypothesized,SD)/2, labels=paste("z-statistic:", signif(zvalue,4)), pos=4, col="blue")
      text(min, dnorm(hypothesized, hypothesized, SD)/2.2, labels=paste("two-sided p-value:", pvalue), pos=4,  col="red")
      
    }
  }
  par(mar=c(4,.5,1.5,.5), mfrow=c(3,1))
  if (length(conf.level)>1) par(mar=c(4, 2, 1.5, 4), mfrow=c(length(conf.level),1))
  lower=0; upper=0
  if (!is.null(conf.level)){
    for (k in 1:length(conf.level)){
      if(conf.level[k] > 1) conf.level[k]=conf.level[k]/100
      myout=prop.test(observed, n, p=statistic, alternative="two.sided", conf.level[k], correct=FALSE)
      criticalvalue=qnorm((1-conf.level[k])/2)
      lower[k]=statistic+criticalvalue*sqrt(statistic*(1-statistic)/n)
      upper[k]=statistic-criticalvalue*sqrt(statistic*(1-statistic)/n)
      multconflevel=100*conf.level[k]
      cat(multconflevel, "% Confidence interval for pi: (", lower[k], ", ", upper[k], ") \n")
    }
    if (is.null(hypothesized)){
      SDphat=sqrt(statistic*(1-statistic)/n)
      min=statistic-4*SDphat
      max=statistic+4*SDphat
      CIseq=seq(min, max, .001)
      
      if (length(conf.level)==1){
        myxlab=substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(lower[1],4), x2=signif(SDphat,4)))
        plot(CIseq, dnorm(CIseq, lower[1], SDphat), type="l", xlab=" ")
        mtext("sample proportions", side=1, line=1.75, adj=.5, cex=.75)
        topseq=seq(statistic, max, .001)
        polygon(c(statistic, topseq,  max), c(0, dnorm(topseq, lower[1], SDphat), 0), col="red")
        title(myxlab)
        myxlab=substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(upper[1],4), x2=signif(SDphat, 4)))
        plot(seq(min,max, .001), dnorm(seq(min, max, .001), upper[1], SDphat), type="l", xlab=" ")
        mtext("sample proportions", side=1, line=1.75, adj=.5, cex=.75)
        bottomseq=seq(min, statistic, .001)
        polygon(c(min, bottomseq, statistic, statistic), c(0, dnorm(bottomseq, upper[1], SDphat), dnorm(statistic, upper[1], SDphat), 0), col="red")
        newtitle=substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(upper[1],4), x2=signif(SDphat, 4)))
        ;   title(newtitle)
      }
      for (k in 1:length(conf.level)){
        plot(c(min, statistic, max), c(1, 1,1), pch=c(".", "^", "."), ylab=" ", xlab="process probability", ylim=c(1,1))
        abline(v=statistic, col="gray")
        text(min*1.01, 1, labels=paste(conf.level[k]*100,"% CI:"))
        text(statistic, .9, labels=signif(statistic, 4))
        text(lower[k], 1, labels=signif(lower[k],4), pos=3)
        text(upper[k], 1, labels=signif(upper[k],4), pos=3)
        points(c(lower[k], upper[k]), c(1,1), pch=c("[", "]"))
        lines(c(lower[k], upper[k]), c(1,1))
      }
    }
  }
  par(mfrow=c(1,1))
  invisible(list("zvalue"=zvalue, "pvalue"=pvalue,"lower"=lower, "upper"=upper))
}
