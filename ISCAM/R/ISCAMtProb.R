#' ISCAMtProb Function
#'
#' This function calculates tail probability for the t distribution.
#' @param xval x value
#' @param df degrees of freedom
#' @param direction a string for finding the probability above ("above") or below ("below") the inputted value 
#' @param xval2 second observation necessary if "outside" or "between" is specified as the direction
#' @keywords t probability
#' @export
#' @examples
#' ISCAMtProb(4, 5, "above")

ISCAMtProb <- function(xval, df, direction, xval2=NULL){
  minx <- min(-5, -1*abs(xval)-.5)
  maxx <- max(5, abs(xval)+.5)
  thisx <- seq(minx, maxx, .001)
  data <- data.frame(x = thisx, y = dt(xval, df))
  myTitle <- paste("t distribution with", df, "degrees of freedom")
  
  if (direction=="below"){
    tprob <- pt(xval, df)
    showprob <- format(tprob, digits=4)
    mySubtitle <- paste("Pr(X \u2264 ", xval, ") = ", showprob, sep = "")
    plot1 <- ggplot(data, aes(x = x, y = y, width = 0.25)) + 
      stat_function(fun = dt,  #drawing t density curve
                    args = list(df = df),
                    color = "dodgerblue") +
      stat_function(fun = dt,  #shading in t curve
                    args = list(df = df),
                    xlim = c(minx, xval),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle = element_text(color="#3366FF"))
  } 
  else if (direction == "above"){
    tprob <- pt(xval, df, lower.tail=FALSE)
    showprob <- format(tprob, digits=4)
    mySubtitle <- paste("Pr(X \u2265 ", xval, ") = ", showprob, sep = "")
    plot1 <- ggplot(data, aes(x = x, y = y, width = 0.25)) + 
      stat_function(fun = dt,  #drawing t density curve
                    args = list(df = df),
                    color = "dodgerblue") +
      stat_function(fun = dt,  #shading in t curve
                    args = list(df = df),
                    xlim = c(xval, maxx),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"))
  }
  else if (direction == "between"){
    if(is.null(xval2)) stop("You need to specify a second observation value.")
    if(xval2 < xval) {temp <- xval; xval <- xval2; xval2 <- temp}
    tprob <- pt(xval2, df) - pt(xval, df)
    showprob <- format(tprob, digits = 4)
    mySubtitle <- paste("Pr(", xval, " \u2264 X \u2264 ", xval2, ") = ", showprob, sep = "")
    plot1 <- ggplot(data, aes(x = x, y = y, width = 0.25)) + 
      stat_function(fun = dt,  #drawing t density curve
                    args = list(df = df),
                    color = "dodgerblue") +
      stat_function(fun = dt,  #shading in t curve
                    args = list(df = df),
                    xlim = c(xval, xval2),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"))
  }
  else if (direction == "outside") {
    maxx <- max(maxx, xval2) 
    thisx <- seq(minx, maxx, .001)
    data <- data.frame(x = thisx, y = dt(xval, df))
    if(is.null(xval2)) stop("You need to specify a second observation value.")
    if(xval2 < xval) {temp=xval; xval=xval2; xval2=temp}
    tprob <- 1 - (pt(xval2, df) - pt(xval, df))
    showprob <- format(tprob, digits=4)
    mySubtitle <- paste("Pr(X ", "\u2264 ", xval, ") and Pr(X ",  "\u2265 ", xval2, ") = ", showprob, sep = "")
    plot1 <- ggplot(data, aes(x = x, y = y, width = 0.25)) + 
      stat_function(fun = dt,  #drawing t density curve
                    args = list(df = df),
                    color = "dodgerblue") +
      stat_function(fun = dt,  #shading in t curve
                    args = list(df = df),
                    xlim = c(minx, xval),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      stat_function(fun = dt,  #shading in t curve
                    args = list(df = df),
                    xlim = c(xval2, maxx),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      labs(x = "Number of Successess",
           y = "Probability",
           title = myTitle, 
           subtitle = mySubtitle) + 
      theme_bw(16, "serif") + 
      theme(plot.subtitle=element_text(color="#3366FF"))
  } else stop("Use \"above\", \"below\", \"between\", or \"outside\" as the direction.")
  cat("Probability: ", showprob)
  plot1 + geom_segment(aes(x = minx, xend = maxx, y = 0, yend = 0), color = "dodgerblue")
}