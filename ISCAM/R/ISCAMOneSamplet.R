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
#' ISCAMOneSamplet(70, 4, 25, hypothesized = 68, alternative = "greater")

ISCAMOneSamplet <- function(xbar, sd, n, hypothesized = 0, alternative = NULL, conf.level = NULL){
  cat("\n", "One Sample t test\n", sep = "", "\n") #output
  statistic <- xbar
  df <- n - 1
  maintitle <- paste("t (df = ", df, ")", sep = "")
  se <- sd/sqrt(n)
  tvalue <- NULL; pvalue <- NULL
  cat(paste("mean = ", xbar, ", sd = ", sd, ",  sample size = ", n, "\n", sep="")) #output
  if (!is.null(alternative)){
    cat(paste("Null hypothesis       : mu =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided = "\u2260", not.equal = "\u2260")
    cat(paste("Alternative hypothesis: mu", altname, hypothesized, sep=" "),"\n")
    tvalue <- (statistic-hypothesized)/se
    cat("t-statistic:", signif(tvalue, 4), "\n")
    subtitle1 <- paste("t-statistic:", tvalue, "\n")
    min <- min(-4, tvalue-.001); diffmin = min(hypothesized-4*se, hypothesized-abs(hypothesized-statistic)-.01)
    max <- max(4, tvalue+.001); diffmax = max(hypothesized+4*se, hypothesized+abs(hypothesized-statistic)+.01)
    x <- seq(min,max,.001)
    diffx <- x*se+hypothesized
    
    my_seq <- -3:3
    xticks <- hypothesized + my_seq*se
    perc <- c("t = -3", "t = -2", "t = -1", "t = 0", "t = 1", "t = 2", "t = 3")
    l <- paste(round(xticks, 2), perc, sep = "\n")
    
    data <- data.frame(x = diffx, y = dt(x, df))
    plot <- ggplot(data, aes(x = x, y = y)) + geom_line(color = "dodgerblue")
    
    if(alternative=="less"){
      pvalue <- pt(tvalue, df)
      plot1 <- plot +
        geom_ribbon(data = subset(data, x < statistic), 
                    aes(ymin = 0, ymax = y), fill = "dodgerblue4", alpha="0.5")
      subtitle2 <- paste("p-value:", round(pvalue, 4))
    } else if (alternative == "greater"){
      pvalue <- 1-pt(tvalue, df)
      plot1 <- plot +
        geom_ribbon(data = subset(data, x > statistic), 
                    aes(ymin = 0, ymax = y), fill = "dodgerblue4", alpha="0.5")
      subtitle2 <- paste("p-value:", round(pvalue, 4))
    } else if(alternative=="two.sided" || alternative=="not.equal"){
      pvalue <- 2*pt(-1*abs(tvalue), df)
      plot1 <- plot +
        geom_ribbon(data=subset(data, x < hypothesized-abs(hypothesized-statistic)), 
                    aes(ymin = 0, ymax = y), fill = "dodgerblue4", alpha="0.5") +
        geom_ribbon(data=subset(data, x > hypothesized+abs(hypothesized-statistic)), 
                    aes(ymin = 0, ymax = y), fill = "dodgerblue4", alpha="0.5")
      subtitle2 <- paste("two-sided p-value:", round(pvalue, 4))
    }
    finalplot <- plot1 + geom_segment(aes(x = diffmin, xend = diffmax, y = 0, yend = 0), color = "dodgerblue") +
      labs(title = maintitle,
           subtitle = paste(subtitle1, subtitle2),
           x = "Sample Means") +
      theme_bw(16, "serif") + 
      theme(plot.subtitle = element_text(color="dodgerblue")) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      scale_x_continuous(breaks = xticks, labels = l, lim = c(diffmin, diffmax))
    cat("p-value: ", pvalue)
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
  print(finalplot)
}