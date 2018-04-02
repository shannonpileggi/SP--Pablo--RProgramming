#' ISCAMBinomNorm Function
#'
#' This function illustrates the normal approximation to the binomial. 
#' @param k number of successes of interest
#' @param n number of trials (zero or more)
#' @param prob probability of success on each trial
#' @param direction allows you to specify whether you want to find the probability 
#' "above" or "below" k or a symmetric "two.sided" probability 
#' @keywords binomial normal
#' @export
#' @examples
#' ISCAMBinomNorm(20, 30, 0.5, direction = "above")
#' ISCAMBinomNorm(10, 55, 0.10, direction = "below")
#' ISCAMBinomNorm(33, 144, 0.40, direction = "two.sided")

ISCAMBinomNorm <- function(k, n, prob, direction){
  thisx = 0:n
  phat=thisx/n
  minx=max(0, n*prob-4*sqrt(prob*(1-prob)*n))
  maxx=min(n, n*prob+4*sqrt(prob*(1-prob)*n))
  normmean <- n*prob
  normsd <- sqrt(n*prob*(1-prob))
  myTitle <- "Title"
  #myTitle <- substitute(paste("Binomial (", n==x1,", ", pi==x2, "), Normal(", mean==x3, ",  ", SD==x4, ")" ), list(x1=n, x2=prob, x3=prob, x4=signif(normsd/n,4))) #graph's main title
  df <- data.frame(x = thisx, y = dbinom(thisx, n, prob)) #putting data into data frame
  
  if (direction =="below"){
    direction <- "less"
    binomtest <- binom.test(k, n, prob, direction) #setting up binom.test to get binom p-val
    pvalue <- binomtest$p.value
    showprob <- format(pvalue, digits = 4)
    mySubtitle <- paste("Binomial: Pr(X \u2264 ", k, ") = ", showprob, sep = "") #creating subtitle
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) + 
      geom_bar(stat = "identity",
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", #fills in part of histogram
               data = subset(df, x <= k), 
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
  else if (direction =="above"){
    direction <- "greater"
    binomtest <- binom.test(k, n, prob, direction) #setting up binom.test to get binom p-val
    pvalue <- binomtest$p.value
    showprob <- format(pvalue, digits = 4)
    mySubtitle <- paste("Binomial: Pr(X \u2265 ", k, ") = ", showprob, sep = "") #creating subtitle
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) + 
      geom_bar(stat = "identity", 
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", 
               data = subset(df, x >= k), 
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
  else if (direction == "two.sided"){
    binomtest <- binom.test(k, n, prob, direction) #setting up binom.test to get binom p-val
    pvalue <- binomtest$p.value
    showprob <- format(pvalue, digits = 4)
    mySubtitle <- paste("Two-sided p-value =", showprob)
    phat <- k/n
    phatdiff <- abs(phat - prob)
    upper <- ifelse(phat > prob, phat, prob + phatdiff)*n
    lower <- ifelse(phat < prob, phat, prob - phatdiff)*n
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
  finalplot <- plot1 + stat_function(geom = "line", 
                        fun = dnorm, 
                        args = list(mean = normmean, sd = normsd),
                        aes(colour = "Normal")) +
    scale_colour_manual("Distribution", values = "purple") +
    xlim(minx, maxx)
  print(finalplot)
}

