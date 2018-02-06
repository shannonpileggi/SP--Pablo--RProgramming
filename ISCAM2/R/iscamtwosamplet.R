#' iscamtwosamplet Function
#'
#' This function calculates a two sample t-test and/or interval from summary data. 
#' @param x1 observed mean
#' @param sd1 observed standard deviation
#' @param n1 sample size
#' @param x2 observed mean
#' @param sd2 observed standard deviation
#' @param n2 sample size
#' @param hypothesized hypothesized difference in population means (default = 0)
#' @param alternative form of alternative ("less", "greater", or "two.sided") 
#' @param conf.level a confidence level for a two-sided confidence interval
#' @keywords two sample t test
#' @export
#' @examples
#' iscamtwosamplet(6, 1.7, 40, 8, 2, 60, alternative = "less")

iscamtwosamplet <- function(x1, sd1, n1, x2, sd2, n2, hypothesized=0, alternative = NULL, conf.level =0){
  cat("\n", "Two Sample t test\n", sep="","\n")
  statistic1=x1;   statistic2=x2
  statistic=statistic1-statistic2
  df= signif((sd1*sd1/n1 + sd2*sd2/n2)*( sd1*sd1/n1 + sd2*sd2/n2)/((sd1*sd1/n1)**2/(n1-1) + (sd2**2/n2)**2/(n2-1)), 4)
  unpooledsd=sqrt(sd1*sd1/n1+sd2*sd2/n2)
  
  cat(paste("Group1: mean = ", x1, ", sd = ", sd1, ",  sample size = ", n1, "\n", sep=""))
  cat(paste("Group2: mean = ", x2, ", sd = ", sd2, ",  sample size = ", n2, "\n", sep=""))
  cat(paste("diff:", x1-x2, "\n\n", sep=""))
  
  if (!is.null(alternative)){
    cat(paste("Null hypothesis       : mu1-mu2 =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided="<>", not.equal="<>")
    cat(paste("Alternative hypothesis: mu1-mu2", altname, hypothesized, sep=" "),"\n")
    
    tvalue = (statistic1-statistic2-hypothesized)/unpooledsd
    cat("t-statistic:", signif(tvalue,4), "\n")
    cat("df:", signif(df, 4), "\n")
    min=min(-4, tvalue-.001); diffmin = min(hypothesized-4*unpooledsd, min(hypothesized-4*unpooledsd, hypothesized-abs(hypothesized-statistic)-.001)
    )
    max=max(4, tvalue+.001); diffmax= max(hypothesized+4*unpooledsd, hypothesized+abs(hypothesized-statistic)+.001)
    
    x=seq(min,max,.001)
    diffx=x*unpooledsd+hypothesized
    plot(diffx,dt(x, df), xlab="<- difference in sample means ->", ylab="Density", type="l", ylim=c(0, dt(0,df)))
    tseq=c(hypothesized-3*unpooledsd, hypothesized -2*unpooledsd, hypothesized - unpooledsd, hypothesized, hypothesized +unpooledsd, hypothesized +2*unpooledsd, hypothesized +3*unpooledsd)
    
    axis(side=1,at = tseq, labels=c("t=-3", "t=-2", "t=-1", "t=0", "t=1", "t=2", "t=3"), padj=1.2, tick=FALSE, col.axis="blue")
    abline(h=0, col="black")
    newtitle=paste("t (df=", df, ")"); title(newtitle)
    if(alternative=="less"){
      pvalue=signif(pt(tvalue, df),4)
      tvalue=signif(tvalue,4)
      drawseq=seq(diffmin, statistic, .001)
      polygon(c(drawseq, statistic, diffmin), c(dt((drawseq-hypothesized)/unpooledsd, df), 0, 0), col="red")
      text(diffmin, dt(0, df)/2, labels=paste("t-statistic:", tvalue), pos=4, col="blue")
      text(diffmin, dt(0,df)/2.2, labels=paste("p-value:", pvalue), pos=4,  col="red")
      
    }
    else if(alternative=="greater"){
      pvalue=signif(1-pt(tvalue, df),4)
      tvalue=signif(tvalue,3)
      drawseq=seq(statistic, diffmax, .001)
      polygon(c(statistic, drawseq, diffmax), c(0, dt((drawseq-hypothesized)/unpooledsd, df), 0), col="red")
      text(diffmax, dt(0, df)/2, labels=paste("t-statistic:", tvalue), pos=2, col="blue")
      text(diffmax, dt(0, df)/2.2, labels=paste("p-value:", pvalue), pos=2,  col="red")
      
    }	
    else if(alternative=="two.sided" || alternative=="not.equal"){
      pvalue=signif(2*pt(-1*abs(tvalue), df),4)
      tvalue=signif(tvalue,4)
      drawseq1=seq(diffmin, hypothesized-abs(hypothesized-statistic), .001)
      drawseq2=seq(hypothesized+ abs(hypothesized-statistic), diffmax, .001)
      polygon(c(diffmin, drawseq1, drawseq1[length(drawseq1)]), c(0, dt((drawseq1-hypothesized)/unpooledsd, df), 0), col="red")
      polygon(c(drawseq2[1], drawseq2, diffmax), c(0, dt((drawseq2-hypothesized)/unpooledsd, df), 0), col="red")
      text(diffmin, dt(0, df)/2, labels=paste("t-statistic:", tvalue), pos=4, col="blue")
      text(diffmin, dt(0, df)/2.2, labels=paste("two-sided p-value:", pvalue), pos=4,  col="red")
      
    }
  }
  lower=NULL; upper=NULL;
  
  if (conf.level!=0){
    if(conf.level > 1) conf.level = conf.level/100
    criticalvalue=qt((1-conf.level)/2, df)
    lower=statistic+criticalvalue*unpooledsd
    upper=statistic-criticalvalue*unpooledsd
    multconflevel=100*conf.level
    cat(multconflevel, "% Confidence interval for mu1-mu2: (", lower, ", ", upper, ") \n")
    if (is.null(alternative)){
      min=statistic-4*unpooledsd
      max=statistic+4*unpooledsd
      CIseq=seq(min, max, .001)
      par(mar=c(4,.5,1.5,.5), mfrow=c(3,1))
      myxlab=substitute(paste(mean==x1), list(x1=signif(lower,4)))
      plot(CIseq, dnorm(CIseq, lower, unpooledsd), type="l", xlab=" ")
      mtext("difference in sample means", side=1, line=1.75, adj=.5, cex=.75)
      topseq=seq(statistic, max, .001)
      polygon(c(statistic, topseq,  max), c(0, dnorm(topseq, lower, unpooledsd), 0), col="red")
      myxlab=substitute(paste("difference in population mean",s==x1), list(x1=signif(lower,4)))
      title(myxlab)
      plot(seq(min,max, .001), dnorm(seq(min, max, .001), upper, unpooledsd), type="l", xlab=" ")
      mtext("difference in sample means", side=1, line=1.75, adj=.5, cex=.75)
      bottomseq=seq(min, statistic, .001)
      polygon(c(min, bottomseq, statistic, statistic), c(0, dnorm(bottomseq, upper, unpooledsd), dnorm(statistic, upper, unpooledsd), 0), col="red")
      newtitle=substitute(paste("difference in population mean",s==x1), list(x1=signif(upper,4)))
      title(newtitle)
      #newtitle=substitute(paste("t (", df==x1, ")", ), list(x1=signif(upper,4), x2=signif(sephat, 4)));   title(newtitle)
      
      plot(c(min, statistic, max), c(1, 1,1), pch=c(".", "^", "."), ylab=" ", xlab="difference in process means", ylim=c(1,1))
      abline(v=statistic, col="gray")
      text(min*1.01, 1, labels=paste(multconflevel,"% CI:"))
      text(statistic, .9, labels=signif(statistic, 4))
      text(lower, 1, labels=signif(lower,4), pos=3)
      text(upper, 1, labels=signif(upper,4), pos=3)
      points(c(lower, upper), c(1,1), pch=c("[", "]"))
      lines(c(lower, upper), c(1,1))
    }
  }
  if(!is.null(alternative)) {
    cat("p-value:",pvalue, "\n")
    invisible(list ("tvalue"=tvalue, "df"=df, "pvalue"=pvalue, "lower"=lower, "upper"=upper))
  }
  
}
