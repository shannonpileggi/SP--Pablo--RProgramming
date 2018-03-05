ISCAMBinomPower <- function(LOS, n, prob1, alternative, prob2=NULL){
  thisx = 0:n
  myTitle <- substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=prob1))
  if (alternative=="less") {
    rr=qbinom(LOS, n, prob1)-1
    this.prob1=pbinom(rr, n, prob1)
    showprob1=format(this.prob1, digits=4)
    mySubtitle <- paste("P(X \u2264", rr, ") =", showprob1)
    df <- data.frame(x = thisx, y = dbinom(thisx, n, prob1))
    plot1 <- ggplot(df, aes(x = x, y = y)) + 
      geom_bar(stat = "identity", 
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", #fills in part of histogram
               data = subset(df, x <= rr), 
               colour="black", 
               fill="red",
               alpha = .7) +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="red"))
  }
  else if (alternative=="greater"){
    rr=qbinom(LOS, n, prob1, FALSE)+1
    this.prob1=1-pbinom(rr-1, n, prob1)
    showprob1=format(this.prob1, digits=4)
    mySubtitle <- paste("P(X \u2265", rr, ") =", showprob1)
    df <- data.frame(x = thisx, y = dbinom(thisx, n, prob1))
    plot1 <- ggplot(df, aes(x = x, y = y)) + 
      geom_bar(stat = "identity", 
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", #fills in part of histogram
               data = subset(df, x >= rr), 
               colour="black", 
               fill="red",
               alpha = .7) +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="red"))
  }
  else if (alternative=="two.sided"){
    lowerrr=qbinom(LOS/2, n, prob1)-1
    upperrr=qbinom(LOS/2, n, prob1, FALSE)+1
    lowerprob1=pbinom(lowerrr, n, prob1)
    upperprob1=pbinom(upperrr-1, n, prob1, FALSE)
    showlowerprob1=format(lowerprob1, digits=4); showupperprob1=format(upperprob1, digits=4)
    showprob1=format(lowerprob1+upperprob1, digits=4)
    mySubtitle <- paste("P(X \u2264",  lowerrr, ")+P(X \u2265",upperrr, ") =", showprob1)
    df <- data.frame(x = thisx, y = dbinom(thisx, n, prob1))
    plot1 <- ggplot(df, aes(x = x, y = y)) + 
      geom_bar(stat = "identity", 
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", 
               data = subset(df, x <= lowerrr | x >= upperrr), 
               colour="black", 
               fill="red",
               alpha = .7) +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="red"))
  }
  else stop("Check input for alternative")
  if (!is.null(prob2)){
    myTitle2 <- substitute(paste("Binomial (", n==x1,", ", pi==x2, ")", ), list(x1=n, x2=prob2))
    if (alternative=="less") {
      this.prob2=pbinom(rr, n, prob2)
      showprob2=format(this.prob2, digits=4)
      mySubtitle2 <- paste("P(X \u2264", rr, ") =", showprob2)
      df <- data.frame(x = thisx, y = dbinom(thisx, n, prob2))
      plot2 <- ggplot(df, aes(x = x, y = y)) + 
        geom_bar(stat = "identity", 
                 col = "black", 
                 fill = "grey",
                 alpha = .2) + 
        geom_bar(stat = "identity", #fills in part of histogram
                 data = subset(df, x <= rr), 
                 colour="black", 
                 fill="red",
                 alpha = .7) +
        labs(x = "Number of Successess",
             y = "Probability",
             title = myTitle2, 
             subtitle = mySubtitle2) + 
        theme_bw(16, "serif") + 
        theme(plot.subtitle=element_text(color="red"))
    }
    else if (alternative=="greater"){
      this.prob2=1-pbinom(rr-1, n, prob2)
      showprob2=format(this.prob2, digits=4)
      mySubtitle2 <- paste("P(X \u2265", rr, ") =", showprob2)
      df <- data.frame(x = thisx, y = dbinom(thisx, n, prob2))
      plot2 <- ggplot(df, aes(x = x, y = y)) + 
        geom_bar(stat = "identity", 
                 col = "black", 
                 fill = "grey",
                 alpha = .2) + 
        geom_bar(stat = "identity", #fills in part of histogram
                 data = subset(df, x >= rr), 
                 colour="black", 
                 fill="red",
                 alpha = .7) +
        labs(x = "Number of Successess",
             y = "Probability",
             title = myTitle2, 
             subtitle = mySubtitle2) + 
        theme_bw(16, "serif") + 
        theme(plot.subtitle=element_text(color="red"))
    }
    else if (alternative=="two.sided"){
      this.prob2=pbinom(lowerrr, n, prob2)+pbinom(upperrr-1, n, prob2, FALSE)
      showprob2=format(this.prob2, digits=4)
      mySubtitle2 <- paste("P(X \u2264",  lowerrr, ")+P(X \u2265",upperrr, ") =", showprob2)
      df <- data.frame(x = thisx, y = dbinom(thisx, n, prob2))
      plot2 <- ggplot(df, aes(x = x, y = y)) + 
        geom_bar(stat = "identity", 
                 col = "black", 
                 fill = "grey",
                 alpha = .2) + 
        geom_bar(stat = "identity", 
                 data = subset(df, x <= lowerrr | x >= upperrr), 
                 colour="black", 
                 fill="red",
                 alpha = .7) +
        labs(x = "Number of Successess",
             y = "Probability",
             title = myTitle2, 
             subtitle = mySubtitle2) + 
        theme_bw(16, "serif") + 
        theme(plot.subtitle=element_text(color="red"))
    }
    else stop("Check input for alternative")
    grid.arrange(plot1, plot2, nrow=2)
  }
  if (is.null(prob2)){
    print(plot1)
  }  
}