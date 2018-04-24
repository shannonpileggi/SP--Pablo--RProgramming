#' ISCAMOneSamplet Function
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
#' ISCAMOneSamplet(3, 1.5, 20)

ISCAMOneSamplet <- function(xbar, sd, n, hypothesized=0, alternative = NULL, conf.level =NULL){
  cat("\n", "One Sample t test\n", sep="","\n")
  statistic <- xbar
  df <- n-1
  se <- sd/sqrt(n)
  tvalue=NULL; pvalue=NULL
  cat(paste("mean = ", xbar, ", sd = ", sd, ",  sample size = ", n, "\n", sep=""))
  if (!is.null(alternative)){
    cat(paste("Null hypothesis       : mu =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided="<>", not.equal="<>")
    cat(paste("Alternative hypothesis: mu", altname, hypothesized, sep=" "),"\n")
    tvalue <- (statistic-hypothesized)/se
    cat("t-statistic:", signif(tvalue, 4), "\n")
    min <- min(-4, tvalue-.001); diffmin = min(hypothesized-4*se, hypothesized-abs(hypothesized-statistic)-.01)
    max <- max(4, tvalue+.001); diffmax= max(hypothesized+4*se, hypothesized+abs(hypothesized-statistic)+.01)
    x <- seq(min,max,.001)
    diffx <- x*se+hypothesized
    data <- data.frame(x = diffx, y = dt(x, df))
    if(alternative=="less"){
      pvalue <- pt(tvalue, df)
      plot1 <- ggplot(data, aes(x = x, y = y)) + 
        stat_function(fun = dt,  #drawing t density curve
                      args = list(df = df),
                      color = "dodgerblue") +
        stat_function(fun = dt,  #shading in t curve
                      args = list(df = df),
                      xlim = c(diffmin, statistic),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") 
    } else if (alternative == "greater"){
      pvalue <- 1-pt(tvalue, df)
      plot1 <- ggplot(data, aes(x = x, y = y)) + 
        stat_function(fun = dt,  #drawing t density curve
                      args = list(df = df),
                      color = "dodgerblue") +
        stat_function(fun = dt,  #shading in t curve
                      args = list(df = df),
                      xlim = c(statistic, diffmax),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") 
    } else if(alternative=="two.sided" || alternative=="not.equal"){
      pvalue <- 2*pt(-1*abs(tvalue), df)
      plot1 <- ggplot(data, aes(x = x, y = y)) + 
        stat_function(fun = dt,  #drawing t density curve
                      args = list(df = df),
                      color = "dodgerblue") +
        stat_function(fun = dt,  #shading in t curve
                      args = list(df = df),
                      xlim = c(diffmin, hypothesized-abs(hypothesized-statistic)),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") +
        stat_function(fun = dt,  #shading in t curve
                      args = list(df = df),
                      xlim = c(hypothesized+abs(hypothesized-statistic), diffmax),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4")
    }
  } 
  
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
  }
  print(plot1)
}