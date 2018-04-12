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
#' ISCAMBinomNorm(13, 44, 0.40, direction = "two.sided")

ISCAMBinomNorm <- function(k, n, prob, direction){
  thisx = 0:n
  phat=thisx/n
  minx <- floor(max(0, n*prob-4*sqrt(prob*(1-prob)*n)))
  maxx <- ceiling(min(n, n*prob+4*sqrt(prob*(1-prob)*n)))
  normmean <- n*prob
  normsd <- sqrt(n*prob*(1-prob))
  #myTitle <- substitute(paste("Binomial (", n==x1,", ", pi==x2, "), Normal(", mean==x3, ",  ", SD==x4, ")" ), list(x1=n, x2=prob, x3=prob, x4=signif(normsd/n,4))) #graph's main title
  df <- data.frame(x = thisx, y = dbinom(thisx, n, prob)) #putting data into data frame
  xticks <- seq(from = minx, to = maxx, by = round((maxx-minx)/7,0))
  perc <- round(xticks/n, 2)
  l <- paste(round(xticks, 1), perc, sep = "\n")
  
  breaks <- seq(minx,maxx, length.out = 6)
  
  
  if (direction =="below"){
    direction <- "less"
    binomtest <- binom.test(k, n, prob, direction) #setting up binom.test to get binom p-val
    pvalue <- binomtest$p.value
    showprob <- format(pvalue, digits = 4)
    normprob <- pnorm(k, normmean, normsd)
    normprob2 <- pnorm(k + .5, normmean, normsd)
    shownormprob <- format(normprob, digits=4)
    shownormprob2 <- format(normprob2, digits=4)
    myTitle <- paste("Pr(X \u2264 ", k, ") = ", sep="")
    subtitle2 <- paste("Normal Approximation:", shownormprob, "\n")
    subtitle3 <- paste("Normal Approximation with Continuity:", shownormprob2)
    
    mySubtitle <- paste("Binomial: ", showprob, "\n", subtitle2, subtitle3, sep = "") #creating subtitle
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) + 
      geom_bar(stat = "identity", #creating bars
               col = "black", 
               fill = "dimgrey",
               alpha = .2) + 
      
      stat_function(fun = dnorm, #shade in under normal curve
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(minx, k),
                    geom = "area", 
                    color = "dimgrey",
                    fill = "dimgrey",
                    alpha = 0.4) +
      stat_function(fun = dnorm, #shade in continuity correction
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(k, k + 0.5),
                    geom = "area", 
                    color = "orange",
                    fill = "orange",
                    alpha = 0.4) +
      geom_bar(stat = "identity", #fills in part of histogram
               data = subset(df, x <= k), 
               colour="black", 
               fill="#3366FF") +
      labs(x = "X = Number of Successes (Proportion)",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) +
      #scale_x_discrete(breaks = seq(minx, maxx, 5), lim = c(minx, maxx)) +
      theme_bw(14, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"), 
            plot.title = element_text(size=14))
    output3 <- paste("Pr(X \u2264 ", k, ") = ", "\n", "Binomial: ", showprob, "\n", subtitle2, subtitle3, sep = "") 
  }
  else if (direction =="above"){
    direction <- "greater"
    binomtest <- binom.test(k, n, prob, direction) #setting up binom.test to get binom p-val
    pvalue <- binomtest$p.value
    normprob <- pnorm(k, normmean, normsd, lower.tail=FALSE)
    normprob2 <- pnorm(k-.5, normmean, normsd, lower.tail=FALSE)
    shownormprob <- format(normprob, digits=4)
    shownormprob2 <- format(normprob2, digits=4)
    showprob <- format(pvalue, digits = 4)
    myTitle <- paste("Pr(X \u2265 ", k, ") = ", sep="")
    subtitle2 <- paste("Normal Approximation:", shownormprob, "\n")
    subtitle3 <- paste("Normal Approximation with Continuity:", shownormprob2)
    
    mySubtitle <- paste("Binomial: ", showprob, "\n", subtitle2, subtitle3, sep = "") #creating subtitle
    
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) + 
      geom_bar(stat = "identity", 
               col = "black", 
               fill = "dimgrey",
               alpha = .2) + 
      geom_bar(stat = "identity", 
               data = subset(df, x >= k), 
               colour="black", 
               fill="#3366FF",
               alpha = .7) +
      stat_function(fun = dnorm, #shade in under normal curve
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(k, maxx),
                    geom = "area", 
                    color = "dimgrey",
                    fill = "dimgrey",
                    alpha = 0.4) +
      stat_function(fun = dnorm, #shade in continuity correction
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(k - 0.5, k),
                    geom = "area", 
                    color = "orange",
                    fill = "orange",
                    alpha = 0.4) +
      labs(x = "Number of Successes",
           y = "Probability", 
           title = myTitle,
           subtitle = mySubtitle) + 
      theme_bw(14, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"),
            plot.title = element_text(size=14))
    output3 <- paste("Pr(X \u2265 ", k, ") = ", "\n", "Binomial: ", showprob, "\n", subtitle2, subtitle3, sep = "") 
  }
  else if (direction == "two.sided"){
    binomtest <- binom.test(k, n, prob, direction) #setting up binom.test to get binom p-val
    pvalue <- binomtest$p.value
    showprob <- format(pvalue, digits = 4)
    phat <- k/n
    phatdiff <- abs(phat - prob)
    upper <- ifelse(phat > prob, phat, prob + phatdiff)*n
    lower <- ifelse(phat < prob, phat, prob - phatdiff)*n
    
    if(k < normmean){
      k1 <- k
      k2 <- floor(min(normmean - k + normmean, n)) # ????
      newvalue <- dbinom(k2, size = n, prob) # ????
      if (newvalue <= dbinom(k1, size = n, prob) + .00001) {k2 = k2}
      else {k2 = k2 + 1}
    }
    else {
      k1 <- floor(min(normmean-(k-normmean), n))
      k2=k
      newvalue=dbinom(k1, size=n, prob)
      if (newvalue <= dbinom(k, size=n, prob) +.00001){k1 <- k1}
      else{k1 <- k1-1}
    }
    normprob <- pnorm(k1, normmean,normsd) + pnorm(k2, normmean, normsd, lower.tail=FALSE)
    normprob2 <- pnorm(k1 + .5, normmean, normsd) + pnorm(k2-.5, normmean, normsd, lower.tail=FALSE)
    shownormprob=format(normprob, digits=4)
    shownormprob2=format(normprob2, digits=4)
    myTitle <- paste("Pr(X \u2264 ", k1, ") + Pr(X \u2265 ", k2, ") = ", sep="")
    subtitle2 <- paste("Normal Approximation:", shownormprob, "\n")
    subtitle3 <- paste("Normal Approximation with Continuity:", shownormprob2)
    
    mySubtitle <- paste("Binomial: ", showprob, "\n", subtitle2, subtitle3, sep = "") #creating subtitle
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) +
      geom_bar(stat = "identity",
               col = "black",
               fill = "dimgrey",
               alpha = .2) +
      geom_bar(stat = "identity",
               data = subset(df, x <= k1 | x >= k2),
               colour="black",
               fill="#007f80",
               alpha = .7) +
      stat_function(fun = dnorm, #shade in under normal curve
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(minx, k1),
                    geom = "area", 
                    color = "dimgrey",
                    fill = "dimgrey",
                    alpha = 0.4) +
      stat_function(fun = dnorm, #shade in continuity correction
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(k1, k1 + 0.5),
                    geom = "area", 
                    color = "orange",
                    fill = "orange",
                    alpha = 0.4) +
      stat_function(fun = dnorm, #shade in under normal curve
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(k2, maxx),
                    geom = "area", 
                    color = "dimgrey",
                    fill = "dimgrey",
                    alpha = 0.4) +
      stat_function(fun = dnorm, #shade in continuity correction
                    args = list(mean = normmean, sd = normsd),
                    xlim = c(k2 - 0.5, k2),
                    geom = "area", 
                    color = "orange",
                    fill = "orange",
                    alpha = 0.4) +
      labs(x = "X = Number of Successes (Proportion)",
           y = "Probability",
           title = myTitle,
           subtitle = mySubtitle) +
      theme_bw(14, "serif") +
      theme(plot.subtitle=element_text(color="#3366FF"),
            plot.title = element_text(size=14))
    output3 <- paste("Pr(X \u2264 ", k1, ") + Pr(X \u2265 ", k2, ") = ", "\n", "Binomial: ", showprob, "\n", subtitle2, subtitle3, sep = "") 
  }
  finalplot <- plot1 + stat_function(geom = "line", 
                        fun = dnorm, 
                        args = list(mean = normmean, sd = normsd),
                        aes(colour = "Normal")) +
    scale_colour_manual("Distribution", values = "dimgrey") +
    theme(legend.position="none") + scale_x_continuous(breaks = xticks, labels = l, lim = c(minx, maxx))
  print(finalplot)
  
  output1 <- paste("Binomial (n = ", n, ", \u03C0 = ", prob, ")", sep="")
  output2 <- paste("Normal(mean = ", prob, ", SD = ", signif(normsd/n,4), ")", sep="")
  #output3 <- paste("Pr(X \u2264 ", k1, ") + Pr(X \u2265 ", k2, ") = ", "\n", "Binomial: ", showprob, "\n", subtitle2, subtitle3, sep = "") 
  cat(output1, "\n")
  cat(output2, "\n")
  cat(output3)
  
  #removes warnings
  #oldw <- getOption("warn")
  #options(warn = -1) 
}

