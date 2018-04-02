#' ISCAMInvNorm Function
#'
#' This function calculates the normal quantile of a specified probability. 
#' @param prob1 desired probability
#' @param mean default = 0
#' @param sd default = 1
#' @param direction Specify whether you want this area to be "above", "below", or "outside" or "between"
#' @keywords inverse normal probability
#' @export
#' @examples
#' ISCAMInvNorm(prob1 = 0.25, direction = "above")
#' ISCAMInvNorm(.55, 15, 2, direction = "below")
#' ISCAMInvNorm(.40, direction = "outside")
#' ISCAMInvNorm(0.5, direction = "between")

ISCAMInvNorm <- function(prob1, mean = 0, sd = 1, direction){
  min = mean - 4 * sd
  max = mean + 4 * sd
  mydf <- data.frame(x = c(min, max))
  upper <- qnorm(prob1, mean, sd, lower.tail = F)
  lower <- qnorm(prob1, mean, sd)
  upside <- signif(qnorm((1-prob1)/2, mean, sd, lower.tail=F), 4)
  downside <- signif(qnorm((1-prob1)/2, mean, sd, lower.tail=T), 4)
  outupper <- signif(qnorm(prob1/2, mean, sd, lower.tail=F), 4)
  outlower <- signif(qnorm(prob1/2, mean, sd, lower.tail=T), 4)
  mytitle <- paste("Normal (mean = ", mean, ", SD = ", sd, ")", sep="")
  
  
  if (direction=="above"){ 
    answer <- signif(upper, 4)
    mysubtitle <- paste("Pr(X >", answer, ") =", prob1)
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(x=min,xend=max,y=0,yend=0), color = "dodgerblue") +
      labs(x = "X = variable",
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.subtitle=element_text(color="#3366FF")) +
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = sd),
                    xlim = c(upper, max),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      #annotate(geom="text", x=3, y=0.09, label=mysubtitle,
      #         color="dodgerblue4", fontface =2) 
    cat("The observation with", prob1, "probability above is", 
        answer, "\n")
  }
  else if (direction == "below"){
    answer <- signif(lower, 4)
    mysubtitle <- paste("Pr(X < ", answer, ") = ", prob1, sep="")
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(x=min,xend=max,y=0,yend=0), color = "dodgerblue") +
      labs(x = "X = variable",
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.subtitle=element_text(color="#3366FF")) +
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = sd),
                    xlim = c(min, lower),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      #annotate(geom="text", x=-3, y=0.09, label=mysubtitle,
       #        color="dodgerblue4", fontface =2)
    cat("the observation with", prob1, "probability below is", answer, "\n")
  }
  else if (direction == "between"){
    mysubtitle <- paste("Pr(", downside, " < X < ", upside, ") = ", prob1, sep="")
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(x=min,xend=max,y=0,yend=0), color = "dodgerblue") +
      labs(x = "X = variable",
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.subtitle=element_text(color="#3366FF")) +
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = sd),
                    xlim = c(downside, upside),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      #annotate(geom="text", x=-2.5, y=0.3, label=mysubtitle,
       #        color="dodgerblue4", fontface =2)
    cat("There is", prob1, "probability between", downside, "and", upside, "\n")
  }
  else if (direction == "outside"){
    subtitle1 <- paste("Pr(X > ", outupper, ") = ", prob1/2, "\n", sep="")
    subtitle2 <- paste("Pr(X < ", outlower, ") = ", prob1/2, sep="")
    mysubtitle <- paste(subtitle1, subtitle2)
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(x=min,xend=max,y=0,yend=0), color = "dodgerblue") +
      labs(x = "X = variable",
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.subtitle=element_text(color="#3366FF")) +
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = sd),
                    xlim = c(min, outlower),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      stat_function(fun = dnorm, #Is there an easier way to combine these?
                    args = list(mean = mean, sd = sd),
                    xlim = c(outupper, max),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      #annotate(geom="text", x = 3, y=0.09, label=subtitle1,
       #        color="dodgerblue4", fontface =2) +
      #annotate(geom="text", x = -3, y=0.09, label=subtitle2,
       #        color="dodgerblue4", fontface =2)
    cat("There is", prob1, "probability outside", outlower, "and", outupper, "\n")
  }
  print(p)
}

