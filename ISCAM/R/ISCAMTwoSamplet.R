#' ISCAMTwoSamplet Function
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
#' ISCAMTwoSamplet(97.25, 3.65, 8, 87.25, 9.60, 12, alternative = "less")

ISCAMTwoSamplet <- function(x1, sd1, n1, x2, sd2, n2, hypothesized=0, alternative = NULL, conf.level =0){
  cat("\n", "Two Sample t test\n", sep="","\n")
  statistic1=x1;   statistic2=x2
  statistic=statistic1-statistic2
  df= signif((sd1*sd1/n1 + sd2*sd2/n2)*( sd1*sd1/n1 + sd2*sd2/n2)/((sd1*sd1/n1)**2/(n1-1) + (sd2**2/n2)**2/(n2-1)), 4)
  unpooledsd=sqrt(sd1*sd1/n1+sd2*sd2/n2)
  
  cat(paste("Group1: mean = ", x1, ", sd = ", sd1, ",  sample size = ", n1, "\n", sep=""))
  cat(paste("Group2: mean = ", x2, ", sd = ", sd2, ",  sample size = ", n2, "\n", sep=""))
  cat(paste("diff: ", x1-x2, "\n\n", sep=""))
  maintitle <- paste("t (df = ", df, ")", sep = "")
  
  if (!is.null(alternative)){
    cat(paste("Null hypothesis       : mu1-mu2 =", hypothesized, sep=" "), "\n")
    altname=switch(alternative, less="<", greater=">", two.sided="<>", not.equal="<>")
    cat(paste("Alternative hypothesis: mu1-mu2", altname, hypothesized, sep=" "),"\n")
    
    tvalue = (statistic1-statistic2-hypothesized)/unpooledsd
    subtitle1 <- paste("t-statistic:", signif(tvalue,4), "\n")
    cat("t-statistic:", signif(tvalue,4), "\n")
    cat("df:", signif(df, 4), "\n")
    min=min(-4, tvalue-.001); diffmin = min(hypothesized-4*unpooledsd, min(hypothesized-4*unpooledsd, hypothesized-abs(hypothesized-statistic)-.001)
    )
    max=max(4, tvalue+.001); diffmax= max(hypothesized+4*unpooledsd, hypothesized+abs(hypothesized-statistic)+.001)
    
    x=seq(min,max,.001)
    diffx=x*unpooledsd+hypothesized
    my_seq <- -3:3
    xticks <- hypothesized + my_seq*unpooledsd
    perc <- c("t = -3", "t = -2", "t = -1", "t = 0", "t = 1", "t = 2", "t = 3")
    l <- paste(round(xticks, 2), perc, sep = "\n")
    
    data <- data.frame(x = diffx, y = dt(x, df))
    plot <- ggplot(data, aes(x = x, y = y)) + geom_line(color = "dodgerblue")
    
    if(alternative=="less"){
      pvalue <- signif(pt(tvalue, df), 4)
      subtitle2 <- paste("p-value: ", pvalue)
      plot1 <- plot +
        geom_ribbon(data = subset(data, x < statistic), 
                    aes(ymin = 0, ymax = y), fill = "dodgerblue4", alpha="0.5")
      subtitle2 <- paste("p-value:", round(pvalue, 4))
    } else if (alternative == "greater"){
      pvalue <- signif(1-pt(tvalue, df), 4)
      subtitle2 <- paste("p-value: ", pvalue)
      plot1 <- plot +
        geom_ribbon(data = subset(data, x > statistic), 
                    aes(ymin = 0, ymax = y), fill = "dodgerblue4", alpha="0.5")
      subtitle2 <- paste("p-value:", round(pvalue, 4))
    } else if(alternative=="two.sided" || alternative=="not.equal"){
      pvalue <- signif(2*pt(-1*abs(tvalue), df), 4)
      
      subtitle2 <- paste("two-sided p-value: ", pvalue)
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
    print(finalplot)
    cat("p-value: ", pvalue, "\n")
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
      data <- data.frame(x = CIseq, y = dnorm(CIseq, lower[1], se))
      nicelower <- signif(lower, 4); niceupper <- signif(upper, 4)
      title2.1 <- substitute(paste("Difference in Population Mean",s==x1), list(x1=signif(lower,4)))
      plot2.1 <- ggplot(data, aes(x = x, y = y)) + 
        stat_function(fun = dnorm,  #drawing normal density curve
                      args = list(mean = lower[1], sd = unpooledsd),
                      color = "dodgerblue") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = lower[1], sd = unpooledsd),
                      xlim = c(statistic, max),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") +
        labs(x = "Difference in Sample Means",
             title = title2.1) +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        geom_segment(aes(x=min, xend=max, y=0, yend=0), color = "dodgerblue")
      title2.2 <- substitute(paste("Difference in Population Mean",s==x1), list(x1=signif(upper,4)))
      plot2.2 <- ggplot(data, aes(x = x, y = y)) + 
        stat_function(fun = dnorm,  #drawing normal density curve
                      args = list(mean = upper[1], sd = unpooledsd),
                      color = "dodgerblue") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = upper[1], sd = unpooledsd),
                      xlim = c(min, statistic),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") +
        labs(x = "Difference in Sample Means",
             title = title2.2) +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        geom_segment(aes(x=min, xend=max, y=0, yend=0), color = "dodgerblue")
      
      midpoint <- signif((upper[1] - lower[1])/2 + lower[1], 4)
      plot2_3 <- ggplot(data, aes(x = x, y = y)) + 
        geom_segment(aes(x = upper[1], y = 1, xend = lower[1], yend = 1), data = data) +
        geom_point(data = data, 
                   mapping = aes(x = upper[1], y = 1)) +
        geom_point(data = data, 
                   mapping = aes(x = lower[1], y = 1)) +
        geom_point(data = data, 
                   mapping = aes(x = midpoint, y = 1)) +
        geom_text(x = midpoint, y = 1.2, label = midpoint) +
        geom_text(x = lower[1] - 0.01, y = 1.2, label = nicelower) +
        geom_text(x = upper[1] + 0.01, y = 1.2, label = niceupper) +
        labs(x = "Difference in Process Means") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      grid.arrange(plot2.1, plot2.2, plot2_3, nrow = 3)
    }
  }
  if(!is.null(alternative)) {
    invisible(list ("tvalue"=tvalue, "df"=df, "pvalue"=pvalue, "lower"=lower, "upper"=upper))
  }
}