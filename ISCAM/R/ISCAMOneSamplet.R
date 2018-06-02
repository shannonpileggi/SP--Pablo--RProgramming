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
#' ISCAMOneSamplet(70, 4, 25, hypothesized = 68, conf.level = c(.90, .95))

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
    print(finalplot)
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
    if (is.null(alternative)){ #NO ALTERNATIVE --> THREE PLOTS ABOUT CONF.LEVEL
      min <- statistic - 4*se
      max <- statistic + 4*se
      CIseq <- seq(min, max, .001)
      if(length(conf.level) == 1){ #ONE CONFIDENCE INTERVAL
        data <- data.frame(x = CIseq, y = dnorm(CIseq, lower[1], se))
        nicelower <- signif(lower, 4); niceupper <- signif(upper, 4)
        title2.1 <- substitute(paste("population mean",s==x1), list(x1=signif(lower[1],4)))
        plot2.1 <- ggplot(data, aes(x = x, y = y)) + 
          stat_function(fun = dnorm,  #drawing normal density curve
                        args = list(mean = lower[1], sd = se),
                        color = "dodgerblue") +
          stat_function(fun = dnorm,  #shading in normal curve
                        args = list(mean = lower[1], sd = se),
                        xlim = c(statistic, max),
                        geom = "area", 
                        color = "dodgerblue4",
                        fill = "dodgerblue4") +
          labs(x = "Sample Means",
               title = title2.1) +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()) +
          geom_segment(aes(x=min, xend=max, y=0, yend=0), color = "dodgerblue")
        title2.2 <- substitute(paste("population mean",s==x1), list(x1=signif(upper[1],4)))
        plot2.2 <- ggplot(data, aes(x = x, y = y)) + 
          stat_function(fun = dnorm,  #drawing normal density curve
                        args = list(mean = upper[1], sd = se),
                        color = "dodgerblue") +
          stat_function(fun = dnorm,  #shading in normal curve
                        args = list(mean = upper[1], sd = se),
                        xlim = c(min, statistic),
                        geom = "area", 
                        color = "dodgerblue4",
                        fill = "dodgerblue4") +
          labs(x = "Sample Means",
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
          labs(x = "Population Mean") +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
        grid.arrange(plot2.1, plot2.2, plot2_3, nrow = 3)
      }
      plots <- list() #creating a list to hold the ggplots
      titles <- list() #creating a list to hold the plot titles
      multconflevel <- list() #creating a list to hold the different conf levels
      for (k in 1:length(conf.level)){
        multconflevel[[k]] <- 100*conf.level[k] #making conf levels whole numbers
        midpoint <- signif((upper[k] - lower[k])/2 + lower[k], 4) #finding midpoint of interval
        titles[[k]] <- paste(multconflevel[k], "% Confidence Interval", sep = "") #title of each plot
        plots[[k]] <- ggplot(data, aes(x = x, y = y)) + 
          geom_segment(aes(x = upper[k], y = 1, xend = lower[k], yend = 1), data = data) + #line segment representing conf interval
          geom_point(data = data, 
                     mapping = aes(x = upper[k], y = 1)) + #upper end point
          geom_point(data = data, 
                     mapping = aes(x = lower[k], y = 1)) + #lower end point
          geom_point(data = data, 
                     mapping = aes(x = midpoint, y = 1)) + #midpoint
          geom_text(x = midpoint, y = 1.2, label = midpoint) + #midpoint label
          geom_text(x = lower[k] - 0.01, y = 1.2, label = round(lower[k], 4)) + #lower point label
          geom_text(x = upper[k] + 0.01, y = 1.2, label = round(upper[k], 4)) + #upper point label
          labs(x = "Population Mean",
               title = titles[k]) +
          coord_cartesian(xlim=c(lower[k] - 0.01, upper[k] + 0.01)) + #setting x lims
          theme(axis.title.y=element_blank(), #removing y axis
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
      }
      do.call(grid.arrange, plots) #displaying graphs on one plot
    }
  }
}