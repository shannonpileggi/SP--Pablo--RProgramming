#' iscamtwopropztest Function
#'
#' This function calculates a two proportion z-test and/or interval. 
#' @param observed1 observed number of successes or sample proportions (assumed if value is less than one)
#' @param n1 sample size
#' @param observed2 observed number of successes (assumed if value is less than one)
#' @param n2 sample size
#' @param hypothesized hypothesized difference in probability of success/population proportions (default is zero)
#' @param alternative form of alternative hypothesis ("less", "greater", or "two.sided")
#' @param conf.level Input confidence level(s) for a two-sided confidence interval.
#' @param datatable Instead of entering the four individual numbers, you can pass in the two-way table of counts.  Be sure to label the datatable in this case.
#' @keywords two proportion z test
#' @export
#' @examples
#' iscamtwopropztest(6, 17, 12, 20, alternative = "less")

iscamtwopropztest <- function(observed1, n1, observed2, n2, hypothesized=0, alternative=NULL, conf.level=NULL, datatable=NULL){
    if(!is.null(datatable)) {
    observed1=datatable[1]
    n1=datatable[1]+datatable[2]
    observed2=datatable[3]
    n2=datatable[3]+ datatable[4]
  }
  pvalue = NULL; zvalue=NULL
  if (observed1<1) {observed1=round(n1*observed1)}
  if (observed2<1) {observed2=round(n2*observed2)}
  cat("\n", "Two Proportion z test\n", sep="","\n")
  statistic1=observed1/n1; statistic2=observed2/n2
  statistic=statistic1-statistic2
  cat(paste("Group1: observed successes = ", observed1, ", sample size = ", n1, ", sample proportion = ", signif(statistic1,4), "\n\n", sep=""))
  cat(paste("Group2: observed successes = ", observed2, ", sample size = ", n2, ", sample proportion = ", signif(statistic2,4), "\n\n", sep=""))
  if (!is.null(alternative)){
    cat(paste("Null hypothesis       : pi1-pi2 =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided="<>", not.equal="<>")
    cat(paste("Alternative hypothesis: pi1-pi2", altname, hypothesized, sep=" "),"\n")
    pooledphat=(observed1+observed2)/(n1+n2)
    zvalue = (statistic1-statistic2-hypothesized)/sqrt(pooledphat*(1-pooledphat)*(1/n1+1/n2))
    cat("z-statistic:", signif(zvalue,4), "\n")
    pvalue=2
    SD= sqrt(pooledphat*(1-pooledphat)*(1/n1+1/n2))
    min=min(hypothesized-4*SD, hypothesized-abs(zvalue)*SD-.001)
    max= max(hypothesized +4*SD, hypothesized+abs(zvalue)*SD+.001)
    x=seq(min,max,.001)
    plot(x,dnorm(x, hypothesized,SD), xlab="<- difference in sample proportions ->", ylab="Density", type="l", ylim=c(0, dnorm(hypothesized, hypothesized, SD)))
    zseq=c(hypothesized-3*SD, hypothesized -2*SD, hypothesized -SD, hypothesized, hypothesized +SD, hypothesized +2*SD, hypothesized +3*SD)
    axis(side=1,at = zseq, labels=c("z=-3", "z=-2", "z=-1", "z=0", "z=1", "z=2", "z=3"), padj=1.2, tick=FALSE, col.axis="blue")
    abline(h=0, col="black")
    newtitle=paste("Normal (mean=", hypothesized, ", SD=", format(SD, digits=2), ")"); title(newtitle)
    if(alternative=="less"){
      pvalue=signif(pnorm(zvalue),4)
      drawseq=seq(min, statistic, .001)
      polygon(c(drawseq, statistic, min), c(dnorm(drawseq, hypothesized, SD), 0, 0), col="red")
      text(min, dnorm(hypothesized, hypothesized,SD)/2, labels=paste("z-statistic:", signif(zvalue,4)), pos=4, col="blue")
      text(min, dnorm(hypothesized, hypothesized, SD)/2.2, labels=paste("p-value:", pvalue), pos=4,  col="red")
      
    }
    else if(alternative=="greater"){
      pvalue=signif(1-pnorm(zvalue),4)
      drawseq=seq(statistic, max, .001)
      polygon(c(statistic , drawseq, max), c(0, dnorm(drawseq, hypothesized, SD), 0), col="red")
      text(max, dnorm(hypothesized, hypothesized,SD)/2, labels=paste("z-statistic:", signif(zvalue,4)), pos=2, col="blue")
      text(max, dnorm(hypothesized, hypothesized, SD)/2.2, labels=paste("p-value:", pvalue), pos=2,  col="red")
      
    }	
    else if(alternative=="two.sided" || alternative=="not.equal"){
      pvalue=signif(2*pnorm(-1*abs(zvalue)),4)
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
  } #end have alternative
  lower=NULL; upper=NULL;
  if (!is.null(conf.level)){
    if(length(conf.level)>1) par(mar=c(4, 2, 1.5, 4), mfrow=c(length(conf.level),1))
    for(k in 1:length(conf.level)){
      if (conf.level[k]>1) conf.level[k] = conf.level[k]/100
      #myout=prop.test(observed, n, p=statistic, alternative="two.sided", conf.level, correct=FALSE)
      criticalvalue=qnorm((1-conf.level[k])/2)
      sephat=sqrt(statistic1*(1-statistic1)/n1 + statistic2*(1-statistic2)/n2)
      lower[k]=statistic+criticalvalue*sephat
      upper[k]=statistic-criticalvalue*sephat
      multconflevel=100*conf.level[k]
      cat(multconflevel, "% Confidence interval for pi1-pi2: (", lower[k], ", ", upper[k], ") \n")
    }
    if (is.null(alternative)){
      min=statistic-4*sephat
      max=statistic+4*sephat
      CIseq=seq(min, max, .001)
      if(length(conf.level)==1){
        par(mar=c(4,.5,1.5,.5), mfrow=c(3,1))
        myxlab=substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(lower[1],4), x2=signif(sephat,4)))
        plot(CIseq, dnorm(CIseq, lower[1], sephat), type="l", xlab=" ")
        mtext("difference in sample proportions", side=1, line=1.75, adj=.5, cex=.75)
        topseq=seq(statistic, max, .001)
        polygon(c(statistic, topseq,  max), c(0, dnorm(topseq, lower[1], sephat), 0), col="red")
        title(myxlab)
        myxlab=substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(upper[1],4), x2=signif(sephat, 4)))
        plot(seq(min,max, .001), dnorm(seq(min, max, .001), upper[1], sephat), type="l", xlab=" ")
        mtext("difference in sample proportions", side=1, line=1.75, adj=.5, cex=.75)
        bottomseq=seq(min, statistic, .001)
        polygon(c(min, bottomseq, statistic, statistic), c(0, dnorm(bottomseq, upper[1], sephat), dnorm(statistic, upper[1], sephat), 0), col="red")
        newtitle=substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(upper,4), x2=signif(sephat, 4))) 
        title(newtitle)
      } # end one interval
      for (k in 1:length(conf.level)){
        plot(c(min, statistic, max), c(1, 1,1), pch=c(".", "^", "."), ylab=" ", xlab="difference in process probabilities", ylim=c(1,1))
        abline(v=statistic, col="gray")
        text(min*1.01, 1, labels=paste(100*conf.level[k],"% CI:"))
        text(statistic, .9, labels=signif(statistic, 4))
        text(lower[k], 1, labels=signif(lower[k],4), pos=3)
        text(upper[k], 1, labels=signif(upper[k],4), pos=3)
        points(c(lower[k], upper[k]), c(1,1), pch=c("[", "]"))
        lines(c(lower[k], upper[k]), c(1,1))
      }
    } # end have alternative
  } # end have confidence level
  if(!is.null(alternative)) cat("p-value:",pvalue, "\n")
  par(mfrow=c(1,1))
  invisible(list("zvalue"=zvalue, "pvalue"=pvalue,"lower"=lower, "upper"=upper))
}
