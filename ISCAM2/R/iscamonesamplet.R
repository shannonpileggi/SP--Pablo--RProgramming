#' iscamonesamplet Function
#'
#' This function calculates a one sample t-test and/or interval from summary statistics.
#' @param xbar observed mean
#' @param sd standard deviation
#' @param n sample size
#' @param hypothesized hypothesized population mean
#' @param alternative form of alternative hypothesis ("less", "greater", or "two.sided")
#' @param conf.level Input confidence level(s) for a two-sided confidence interval.
#' @keywords one sample t test
#' @export
#' @examples
#' iscamonesamplet(3, 1.5, 20)

iscamonesamplet <- function(xbar, sd, n, hypothesized=0, alternative = NULL, conf.level =NULL){
  cat("\n", "One Sample t test\n", sep="","\n")
  statistic=xbar
  df = n-1
  se=sd/sqrt(n)
  tvalue=NULL; pvalue=NULL
  cat(paste("mean = ", xbar, ", sd = ", sd, ",  sample size = ", n, "\n", sep=""))
  if (!is.null(alternative)){
    cat(paste("Null hypothesis       : mu =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided="<>", not.equal="<>")
    cat(paste("Alternative hypothesis: mu", altname, hypothesized, sep=" "),"\n")
    
    tvalue = (statistic-hypothesized)/se
    cat("t-statistic:", signif(tvalue,4), "\n")
    min=min(-4, tvalue-.001); diffmin = min(hypothesized-4*se, hypothesized-abs(hypothesized-statistic)-.01)
    max=max(4, tvalue+.001); diffmax= max(hypothesized+4*se, hypothesized+abs(hypothesized-statistic)+.01)
    x=seq(min,max,.001)
    diffx=x*se+hypothesized
    plot(diffx,dt(x, df), xlab="<- sample means ->", ylab="Density", type="l", ylim=c(0, dt(0,df)))
    tseq=c(hypothesized-3*se, hypothesized -2*se, hypothesized - se, hypothesized, hypothesized +se, hypothesized +2*se, hypothesized +3*se)
    
    axis(side=1,at = tseq, labels=c("t=-3", "t=-2", "t=-1", "t=0", "t=1", "t=2", "t=3"), padj=1.2, tick=FALSE, col.axis="blue")
    abline(h=0, col="black")
    newtitle=paste("t (df=", df, ")"); title(newtitle)
    if(alternative=="less"){
      pvalue=pt(tvalue, df)
      drawseq=seq(diffmin, statistic, .001)
      polygon(c(drawseq, statistic, diffmin), c(dt((drawseq-hypothesized)/se, df), 0, 0), col="red")
      text(diffmin, dt(0, df)/2, labels=paste("t-statistic:", signif(tvalue,3)), pos=4, col="blue")
      text(diffmin, dt(0,df)/2.2, labels=paste("p-value:", signif(pvalue,4)), pos=4,  col="red")
      
    }
    else if(alternative=="greater"){
      pvalue=1-pt(tvalue, df)
      drawseq=seq(statistic, diffmax, .001)
      polygon(c(statistic, drawseq, diffmax), c(0, dt((drawseq-hypothesized)/se, df), 0), col="red")
      text(diffmax, dt(0, df)/2, labels=paste("t-statistic:", signif(tvalue,3)), pos=2, col="blue")
      text(diffmax, dt(0, df)/2.2, labels=paste("p-value:", signif(pvalue,4)), pos=2,  col="red")
      
    }	
    else if(alternative=="two.sided" || alternative=="not.equal"){
      pvalue=2*pt(-1*abs(tvalue), df)
      drawseq1=seq(diffmin, hypothesized-abs(hypothesized-statistic), .001)
      drawseq2=seq(hypothesized+ abs(hypothesized-statistic), diffmax, .001)
      polygon(c(diffmin, drawseq1, drawseq1[length(drawseq1)]), c(0, dt((drawseq1-hypothesized)/se, df), 0), col="red")
      polygon(c(drawseq2[1], drawseq2, diffmax), c(0, dt((drawseq2-hypothesized)/se, df), 0), col="red")
      text(diffmin, dt(0, df)/2, labels=paste("t-statistic:", signif(tvalue,4)), pos=4, col="blue")
      text(diffmin, dt(0, df)/2.2, labels=paste("two-sided p-value:", signif(pvalue,4)), pos=4,  col="red")
      
    }
  } # end test
  
  lower=NULL; upper=NULL
  if (!is.null(conf.level)){
    par(mar=c(4,.5,1.5,.5), mfrow=c(3,1))
    if(length(conf.level)>1)  par(mar=c(4, 2, 1.5, .4), mfrow=c(length(conf.level),1))
    for (k in 1:length(conf.level)){
      if (conf.level[k] > 1) conf.level[k] = conf.level[k]/100
      criticalvalue=qt((1-conf.level[k])/2, df)
      lower[k]=statistic+criticalvalue*se
      upper[k]=statistic-criticalvalue*se
      multconflevel=100*conf.level[k]
      cat(multconflevel, "% Confidence interval for mu: (", lower[k], ", ", upper[k], ") \n")
    }
    if (is.null(alternative)){
      min=statistic-4*se
      max=statistic+4*se
      CIseq=seq(min, max, .001)
      if(length(conf.level)==1){
        
        par(mar=c(4,.5,1.5,.5), mfrow=c(3,1))
        myxlab=substitute(paste(mean==x1), list(x1=signif(lower[1],4)))
        plot(CIseq, dnorm(CIseq, lower[1], se), type="l", xlab=" ")
        mtext("sample means", side=1, line=1.75, adj=.5, cex=.75)
        topseq=seq(statistic, max, .001)
        polygon(c(statistic, topseq,  max), c(0, dnorm(topseq, lower[1], se), 0), col="red")
        myxlab=substitute(paste("population mean",s==x1), list(x1=signif(lower[1],4)))
        title(myxlab)
        plot(seq(min,max, .001), dnorm(seq(min, max, .001), upper[1], se), type="l", xlab=" ")
        mtext("sample means", side=1, line=1.75, adj=.5, cex=.75)
        bottomseq=seq(min, statistic, .001)
        polygon(c(min, bottomseq, statistic, statistic), c(0, dnorm(bottomseq, upper[1], se), dnorm(statistic, upper[1], se), 0), col="red")
        newtitle=substitute(paste("population mean",s==x1), list(x1=signif(upper[1],4)))
        title(newtitle)
        #newtitle=substitute(paste("t (", df==x1, ")", ), list(x1=signif(upper[1],4), x2=signif(sephat, 4)));   title(newtitle)
      } #just one interval
      for (k in 1:length(conf.level)){
        plot(c(min, statistic, max), c(1, 1,1), pch=c(".", "^", "."), ylab=" ", xlab="population mean", ylim=c(1,1))
        abline(v=statistic, col="gray")
        text(min*1.01, 1, labels=paste(100*conf.level[k],"% CI:"))
        text(statistic, .9, labels=signif(statistic, 4))
        text(lower[k], 1, labels=signif(lower[k],4), pos=3)
        text(upper[k], 1, labels=signif(upper[k],4), pos=3)
        points(c(lower[k], upper[k]), c(1,1), pch=c("[", "]"))
        lines(c(lower[k], upper[k]), c(1,1))
      }
    }
  }
  if(!is.null(alternative)) cat("p-value:",pvalue, "\n")
  par(mfrow=c(1,1))
  invisible(list ("tvalue"=tvalue, "pvalue"=pvalue, "lower"=lower, "upper"=upper))
  
  
}
