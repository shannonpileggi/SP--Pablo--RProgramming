#' ISCAMOnePropZTest Function
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
#' ISCAMOnePropZTest(12, 15, .70, "less")
#' ISCAMOnePropZTest(12, 15, .6, "greater")
#' ISCAMOnePropZTest(12, 15, .5, "two.sided", 0.95)
#' ISCAMOnePropZTest(12, 15, conf.level = c(.9, .95))

ISCAMOnePropZTest <- function(observed, n, hypothesized=NULL, alternative="two.sided", conf.level=NULL){
  if (observed < 1) {observed <- round(n*observed)} #converting sample proportion to number of successes
  proptest <- prop.test(observed, n, hypothesized, alternative, correct = FALSE) #R's built in one prop z test
  
  cat("\n", "One Proportion z test\n", sep="","\n") #output
  statistic <- signif(observed/n, 4) #proportion of successes
  cat(paste("Data: observed successes = ", observed, ", sample size = ", n, ", sample proportion = ", statistic, "\n\n", sep=""))
  zvalue <- NULL; pvalue <- NULL; 
  
  if (!is.null(hypothesized)){
    zvalue <- (statistic-hypothesized)/sqrt(hypothesized*(1-hypothesized)/n)
    cat("z-statistic:", signif(zvalue, 4), "\n")
    subtitle1 <- paste("z-statistic:", signif(zvalue, 4), "\n")
    pvalue <- signif(proptest$p.value, 4)
    subtitle2 <- paste("p-value:", pvalue)
    cat("p-value:", pvalue, "\n")
    
    SD <- sqrt(hypothesized*(1-hypothesized)/n) #std error
    min <- min(hypothesized-4*SD, hypothesized-abs(zvalue)*SD-.001)
    max <- max(hypothesized +4*SD, hypothesized+abs(zvalue)*SD+.001)
    x <- seq(min, max, .001)
    
    my_seq <- -3:3 #setting up 2nd x axis
    xticks <- hypothesized + my_seq*SD
    perc <- c("z=-3", "z=-2", "z=-1", "z=0", "z=1", "z=2", "z=3")
    l <- paste(round(xticks, 2), perc, sep = "\n")
    
    data <- data.frame(x = x, y = dnorm(x, hypothesized, SD))
    plot <- ggplot(data, aes(x = x, y = y))
    if (alternative == "less"){
      plot1 <- plot + 
        stat_function(fun = dnorm,  #drawing normal density curve
                      args = list(mean = hypothesized, sd = SD),
                      color = "dodgerblue") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = hypothesized, sd = SD),
                      xlim = c(min, statistic),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") 
    } else if (alternative == "greater"){
      plot1 <- plot + 
        stat_function(fun = dnorm,  #drawing normal density curve
                      args = list(mean = hypothesized, sd = SD),
                      color = "dodgerblue") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = hypothesized, sd = SD),
                      xlim = c(statistic, max),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4")
    } else if (alternative == "two.sided"){
      subtitle2 <- paste("two-sided p-value:", pvalue)
      if(statistic < hypothesized){
        lowerstat <- statistic
        upperstat <- hypothesized + (hypothesized - statistic)
      } else {
        lowerstat <- hypothesized - (statistic - hypothesized)
        upperstat <- statistic                           
      }
      plot1 <- plot + 
        stat_function(fun = dnorm,  #drawing normal density curve
                      args = list(mean = hypothesized, sd = SD),
                      color = "dodgerblue") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = hypothesized, sd = SD),
                      xlim = c(min, lowerstat),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = hypothesized, sd = SD),
                      xlim = c(upperstat, max),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4")
    }
    mysubtitle <- paste(subtitle1, subtitle2)
    
    finalplot <- plot1 + 
      geom_segment(aes(x = min, xend = max, y = 0, yend = 0), color = "dodgerblue") +
      labs(x = "Sample Proportions",
           subtitle = mysubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle = element_text(color="#3366FF")) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      scale_x_continuous(breaks = xticks, labels = l, lim = c(min, max))
    
    
    print(finalplot)
  }
  
lower=0; upper=0
if (!is.null(conf.level)){
  for (k in 1:length(conf.level)){
    if(conf.level[k] > 1) conf.level[k]=conf.level[k]/100
    #myout=prop.test(observed, n, p=statistic, alternative="two.sided", conf.level[k], correct=FALSE)
    criticalvalue <- qnorm((1-conf.level[k])/2)
    lower[k] <- signif(statistic+criticalvalue*sqrt(statistic*(1-statistic)/n), 4)
    upper[k] <- signif(statistic-criticalvalue*sqrt(statistic*(1-statistic)/n), 4)
    multconflevel <- 100*conf.level[k]
    cat(multconflevel, "% Confidence interval for pi: (", lower[k], ", ", upper[k], ") \n", sep = "")
  }
}
  

  #making plots when hypothesized and alternative are not specified, but conf.level is specified
  if (is.null(hypothesized)){
    SDphat <- sqrt(statistic*(1-statistic)/n)
    min <- statistic-4*SDphat
    max <- statistic+4*SDphat
    CIseq <- seq(min, max, .001)
    
    if(length(conf.level) == 1){
      data <- data.frame(x = CIseq, y = dnorm(CIseq, lower[1], SDphat))
      nicelower <- round(lower, 4); niceupper <- round(upper, 4)
      title2.1 <- substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(lower[1],4), x2=signif(SDphat,4)))
      plot2.1 <- ggplot(data, aes(x = x, y = y)) + 
        stat_function(fun = dnorm,  #drawing normal density curve
                      args = list(mean = lower[1], sd = SDphat),
                      color = "dodgerblue") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = lower[1], sd = SDphat),
                      xlim = c(statistic, max),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") +
        labs(x = "Sample Proportions",
             title = title2.1) +
        theme(axis.title.y=element_blank(), #removing y axis
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        geom_segment(aes(x=min, xend=max, y=0, yend=0), color = "dodgerblue")
      title2.2 <- substitute(paste("Normal (", mean==x1,", ", SD==x2, ")", ), list(x1=signif(upper[1],4), x2=signif(SDphat, 4)))
      plot2.2 <- ggplot(data, aes(x = x, y = y)) + 
        stat_function(fun = dnorm,  #drawing normal density curve
                      args = list(mean = upper[1], sd = SDphat),
                      color = "dodgerblue") +
        stat_function(fun = dnorm,  #shading in normal curve
                      args = list(mean = upper[1], sd = SDphat),
                      xlim = c(min, statistic),
                      geom = "area", 
                      color = "dodgerblue4",
                      fill = "dodgerblue4") +
        labs(x = "Sample Proportions",
             title = title2.2) +
        theme(axis.title.y=element_blank(), #removing y axis
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
        geom_text(x = lower + 0.01, y = 1.2, label = nicelower) +
        geom_text(x = upper - 0.01, y = 1.2, label = niceupper) +
        labs(x = "Process Probability") +
        theme(axis.title.y=element_blank(), #removing y axis
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      grid.arrange(plot2.1, plot2.2, plot2_3, nrow = 3)
    } else if (length(conf.level) > 1){
      data <- data.frame(x = CIseq, y = dnorm(CIseq, lower[k], SDphat))
      
      plots <- list() #creating a list to hold the ggplots
      titles <- list() #creating a list to hold the plot titles
      multconflevel <- list() #creating a list to hold the different conf levels
      differences <- list()
      for (k in 1:length(conf.level)){
        if (max(conf.level) == conf.level[k]){ #finding widest CI to set xlims
          uplim <- upper[k]
          lowlim <- lower[k]
        }
        
        dfseg <- data.frame(V1=lower[k], V2=1, xend=upper[k], yend=1)
        
        
        xticks <- round(seq(lowlim, uplim, by = (uplim-lowlim)/5), 1)
        multconflevel[[k]] <- 100*conf.level[k] #making conf levels whole numbers
        midpoint <- signif((upper[k] - lower[k])/2 + lower[k], 4) #finding midpoint of interval
        titles[[k]] <- paste(multconflevel[k], "% Confidence Interval", sep = "") #title of each plot
        plots[[k]] <- ggplot(dfseg, aes(V1,V2)) +
          #ggplot(data, aes(x = x, y = y)) + 
          geom_segment(aes(x = upper[k], y = 1, xend = lower[k], yend = 1), data = data) + #line segment representing conf interval
          annotate("point", x = upper[k], y = 1) + #upper endpoint
          annotate("point", x = lower[k], y = 1) + #lower endpoint
          annotate("point", x = midpoint, y = 1) + #midpoint
          #geom_segment(x = upper[k], y = 1, xend = midpoint, yend = 1) +
          geom_text(x = midpoint, y = 1.2, label = midpoint) + #midpoint label
          geom_text(x = lower[k] - 0.01, y = 1.2, label = round(lower[k], 4)) + #lower point label
          geom_text(x = upper[k] + 0.01, y = 1.2, label = round(upper[k], 4)) + #upper point label
          labs(x = "Population Mean",
               title = titles[k]) +
          #scale_x_continuous(breaks = round(seq(lowlim, uplim, by = (uplim-lowlim)/5), 1)) +
          #lims(x = c(lowlim, uplim)) +
          #scale_x_continuous(limits=c(lowlim, uplim)) +
          #coord_cartesian(xlim=c(lowlim, uplim)) + #setting x lims
          theme(axis.title.y=element_blank(), #removing y axis
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
      }
      
      do.call(grid.arrange, plots) #displaying graphs on one plot
      
    }
  }
}