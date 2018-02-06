#' iscambinomtest Function
#'
#' This function performs an exact binomial test and graphs the binomial distribution and/or binomial confidence interval.
#' @param observed number of successes of interest or sample proportion (assumed if value less than one)
#' @param n sample size
#' @param hypothesized NULL or probability of success on each trial
#' @param alternative form of alternative "less", "greater", or "two.sided"
#' @param conf.level NULL or a confidence level (one or more values) for a two-sided confidence interval
#' @keywords binomial
#' @export
#' @examples
#' iscambinomtest(20, 35, alternative = "less")
#' iscambinomtest(10, 40, hypothesized = .50, alternative = "two.sided", conf.level = 0.90)

iscambinomtest <- function(observed, n, hypothesized=NULL, alternative, conf.level=NULL){
  if (observed<1) {observed=round(n*observed)}
  pvalue = NULL
  if (!is.null(hypothesized)){
    minx=max(0, n*hypothesized-4*sqrt(hypothesized *(1- hypothesized)*n))
    maxx=min(n, n*hypothesized +4*sqrt(hypothesized *(1- hypothesized)*n))
    myy=dbinom(floor(n*hypothesized), n, hypothesized)/2
   x=0:n
        plot(x, dbinom(x, size=n, prob=hypothesized), xlab="Number of Successes", ylab="Probability", type="h", xlim=c(minx, maxx))
  newtitle=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=hypothesized));   title(newtitle)
  
    if(alternative=="less"){
      pvalue=pbinom(observed, size=n, prob=hypothesized, TRUE)
      lines(0:observed, dbinom(0:observed, size=n, prob=hypothesized), col="red", type="h")
      text(minx, myy, labels=paste("p-value:", signif(pvalue,4)), pos=4, col="red")
    }
    else if(alternative=="greater"){
      value=observed-1
      pvalue=pbinom (value, size=n, prob=hypothesized, FALSE)
      lines(observed:n, dbinom(observed:n, size=n, prob=hypothesized), col="red", type="h")
      text(maxx, myy, labels=paste("p-value:", signif(pvalue,4)), pos=2, col="red")
    }
    else {
      pvalue=0
      firstvalue=dbinom(observed, size=n, prob=hypothesized)
      for(y in 0:n){
        newvalue=dbinom(y, size=n, prob=hypothesized)
        if (newvalue <= firstvalue+.00001){ 
          pvalue=pvalue+newvalue
          lines(y, newvalue, col="red", type="h")
        }		
      }
      text(minx, myy, labels=paste("two-sided p-value:\n", signif(pvalue,4)), pos=4, col="red")
    }
    pvalue=signif(pvalue, 5)
    abline(h=0, col="gray"); abline(v=0, col="gray")
  }
  cat("\n", "Exact Binomial Test\n", sep="","\n")
  statistic=signif(observed/n, 4)
  cat(paste("Data: observed successes = ", observed, ", sample size = ", n, ", sample proportion = ", statistic, "\n\n", sep=""))
  
  if (!is.null(hypothesized)){
    cat(paste("Null hypothesis       : pi =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided="<>")
    cat(paste("Alternative hypothesis: pi", altname, hypothesized, sep=" "),"\n")
    cat(paste("p-value:", pvalue, sep=" "), "\n")
  }
  p.L = function(x,alpha){
    if (x==0) 0
    else qbeta(alpha, x, n-x+1)
  }
  p.U = function(x,alpha){
    if (x==n) 1
    else qbeta(1-alpha, x+1, n-x)
  }
  CINT=0; multconflevel=0; lower1=NULL; upper1=NULL
  if(!is.null(conf.level)){
    for (k in 1:length(conf.level)){
      if(conf.level[k] > 1) conf.level[k]=conf.level[k]/100
      alpha = (1-conf.level[k])/2
      CINT = c(signif(p.L(observed, alpha),5), ",", signif(p.U(observed,alpha),5))
      multconflevel=100*conf.level[k]
      cat(multconflevel, "% Confidence interval for pi: (", CINT, ") \n")
      lower1[k]=as.numeric(CINT[1]); upper1[k]=as.numeric(CINT[3])
    }
  }
  par(mar=c(4,2,1.5,.5), mfrow=c(3,1))
  if (length(conf.level)>1)  par(mar=c(4, 2, 1.5, .4), mfrow=c(length(conf.level),1))
  
  if (is.null(hypothesized)){
    statistic=observed/n; 
    #lower=lower1[1]; upper=upper1[1]
    SDphat=sqrt(statistic*(1-statistic)/n)
    min=statistic-4*SDphat
    max=statistic+4*SDphat
    CIseq=seq(min, max, .01)
    minx=as.integer(max(0, min*n))
    maxx=as.integer(min(n, max*n))
    
    if(length(conf.level)==1){
      myxlab=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=signif(lower1[1],4)))
      plot(seq(minx, maxx), dbinom(seq(minx,maxx), size=n, prob= lower1[1]), xlab="  ", ylab=" ", type="h", xlim=c(minx, maxx))
      mtext("Number of successes", side=1, line=1.75, adj=.5, cex=.75)
      title(myxlab)
      lines(observed:n, dbinom(observed:n, size=n, prob=lower1[1]), col="red", type="h")
      
      myxlab=substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=signif(upper1[1],4)))
      plot(seq(minx,maxx), dbinom(seq(minx,maxx),size=n, prob=upper1[1]), xlab=" ", ylab=" ", type="h", xlim=c(minx,maxx))
      lines(0:observed, dbinom(0:observed, size=n, prob=upper1[1]), col="red", type="h")
      
      mtext("Number of successes", side=1, line=1.75, adj=.5, cex=.75)
      title(myxlab)
    } # end only one interval
    
    for (k in 1:length(conf.level)){
      plot(c(min, statistic, max), c(1, 1,1), pch=c(".", "^", "."), ylab=" ", xlab="process probability", ylim=c(1,1))
      abline(v=statistic, col="gray")
      text(min, 1, labels=paste(conf.level[k]*100,"% CI:"))
      text(statistic, .9, labels=signif(statistic, 4))
      text(lower1[k], 1, labels=signif(lower1[k],4), pos=3)
      text(upper1[k], 1, labels=signif(upper1[k],4), pos=3)
      points(c(lower1[k],upper1[k]), c(1,1), pch=c("[", "]"))
      lines(c(lower1[k], upper1[k]), c(1,1))
    } # end intervals loop
  } # end no hypothesized
  
  par(mfrow=c(1,1))
  invisible(list("pvalue"=pvalue,"lower"=lower1, "upper"=upper1))
  
}
