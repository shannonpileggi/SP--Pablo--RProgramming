#' ISCAMBinomTest Function
#'
#' This function performs an exact binomial test and graphs the binomial distribution and/or binomial confidence interval.
#' @param observed number of successes of interest or sample proportion (assumed if value less than one)
#' @param n sample size
#' @param hypothesized probability of success on each trial
#' @param alternative form of alternative "less", "greater", or "two.sided"
#' @param conf.level NULL or a confidence level (one or more values) for a two-sided confidence interval
#' @keywords binomial
#' @export
#' @examples
#' ISCAMBinomTest(20, 35, alternative = "less", hypothesized=0.5)
#' ISCAMBinomTest(10, 40, hypothesized = .50, alternative = "two.sided", conf.level = 0.90)


ISCAMBinomTest <- function(observed, n, hypothesized, alternative, conf.level = NULL){
  if (observed < 1) {observed <- round(n*observed)}
  myTitle <- substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=hypothesized)) #graph's main title
  thisx <- 0:n #range of data (number of trials) 
  df <- data.frame(x = thisx, y = dbinom(thisx, n, hypothesized)) #putting data into data frame
  
  binomtest <- binom.test(observed, n, hypothesized, alternative, conf.level)
  CI <- binomtest$conf.int
  pvalue <- binomtest$p.value
  showprob <- format(pvalue, digits = 4)
  
  if (alternative =="less"){
    mySubtitle <- paste("p-value =", showprob) #creating subtitle
    
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) + 
      geom_bar(stat = "identity",
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", #fills in part of histogram
               data = subset(df, x <= observed), 
               colour="black", 
               fill="#3366FF",
               alpha = .7) +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"))
  }
  else if (alternative =="greater"){
    mySubtitle <- paste("P-value =", showprob)
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) + 
      geom_bar(stat = "identity", 
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", 
               data = subset(df, x >= observed), 
               colour="black", 
               fill="#3366FF",
               alpha = .7)+
      labs(x = "Number of Successes",
           y = "Probability", 
           title = myTitle,
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"))
  }
  else if (alternative == "two.sided"){
    mySubtitle <- paste("Two-sided p-value =", showprob)
    phat <- observed/n
    phatdiff <- abs(phat - hypothesized)
    upper <- ifelse(phat > hypothesized, phat, hypothesized + phatdiff)*n
    lower <- ifelse(phat < hypothesized, phat, hypothesized - phatdiff)*n
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) +
      geom_bar(stat = "identity",
               col = "black",
               fill = "grey",
               alpha = .2) +
      geom_bar(stat = "identity",
               data = subset(df, x <= lower | x >= upper),
               colour="black",
               fill="#007f80",
               alpha = .7) +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle,
           subtitle = mySubtitle) +
      theme_bw(16, "serif") +
      theme(plot.subtitle=element_text(color="#007f80"))
  }
  print(plot1)
  cat("\n", "Exact Binomial Test\n", sep="","\n")
  statistic <- signif(observed/n, 4)
  cat(paste("Data: observed successes = ", observed, ", sample size = ", n, ", sample proportion = ", statistic, "\n\n", sep=""))
  cat(paste("Null hypothesis       : pi =", hypothesized, sep=" "), "\n")
  altname <- switch(alternative, less="<", greater=">", two.sided="\u2260")
  cat(paste("Alternative hypothesis: pi", altname, hypothesized, sep=" "),"\n")
  cat(paste("p-value:", showprob, sep=" "), "\n")
  
  cat(100*conf.level, "% Confidence interval for pi:", CI)
}