#' iscam_hyperprob Function
#'
#' This function calculates tail probabilities from the hypergeometric distribution.
#' @param k number of successes of interest (integer) or difference in conditional proportions
#' @param total total number of observations in the study
#' @param succ overall number of successes
#' @param n number of observations in group A
#' @param lower.tail a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#' @keywords hypergeometric probability
#' @export
#' @examples
#' iscam_hyperprob(2, 52, 5, 13, lower.tail = FALSE)

iscam_hyperprob <- function(k, total, succ, n, lower.tail){
  if (k < 1 & k > 0) {k <- round((k*(total - n)*n + succ*n)/total)}
  fail <- total - succ
  thisx <- max(0, n - fail):min(n, succ)
  df <- data.frame(x = thisx, y = dhyper(thisx, succ, fail, n))
  myTitle <- paste("Hypergeometric (N = ", total, ", M = ", succ, ", n = ", n, ")", sep = "")
  if (lower.tail){
    this.prob <- phyper(k, succ, fail, n)
    showprob <- format(this.prob, digits=4)
    mySubtitle <- paste("P(X \u2264 ", k, ") = ", showprob, sep = "")
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
    cat("Probability", k, "and below =", this.prob, "\n")
  }
  if (!lower.tail){
    this.prob <- 1 - phyper(k-1, succ, fail, n)
    showprob <- format(this.prob, digits=4)
    mySubtitle <- paste("P(X \u2265 ", k, ") = ", showprob, sep = "")
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) + 
      geom_bar(stat = "identity",
               col = "black", 
               fill = "grey",
               alpha = .2) + 
      geom_bar(stat = "identity", #fills in part of histogram
               data = subset(df, x >= k), 
               colour="black", 
               fill="#3366FF",
               alpha = .7) +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"))
    cat("Probability", k, "and above =", this.prob, "\n")
  }
  print(plot1)
}