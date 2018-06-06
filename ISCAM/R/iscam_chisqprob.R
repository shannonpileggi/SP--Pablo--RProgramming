#' iscam_chisqprob Function
#'
#' This function calculates the upper tail probability for the chi-square distribution
#' @param xval x value
#' @param df degrees of freedom
#' @keywords chi square
#' @export
#' @examples
#' iscam_chisqprob(10, 4)

iscam_chisqprob <- function(xval, df){
  maxx <- max(20, xval, df)
  mydf <- data.frame(x = c(0, maxx))
  chisqprob = pchisq(xval, df, lower.tail = FALSE)
  showprob = format(chisqprob, digits = 4)
  mytitle <- paste("Chi-Square Distribution with", df, "Degrees of Freedom")
  mysubtitle <- paste("P(X \u2265", xval, ") =", showprob)
  p <- ggplot(mydf, aes(x = x)) +
    stat_function(fun = dchisq, 
                args = list(df = df),
                color = "dodgerblue") +
    geom_segment(aes(x=0,xend=20,y=0,yend=0), color = "dodgerblue") +
    labs(x = "Chi-square values",
       y = "",
       title = mytitle,
       subtitle = mysubtitle) +
    theme_bw(12, "serif") + 
    theme(plot.subtitle=element_text(color="dodgerblue4"),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    stat_function(fun = dchisq,
                  args = list(df = df),
                  xlim = c(xval,maxx),
                  geom = "area", 
                  color = "dodgerblue4",
                  fill = "dodgerblue4") 
  print(p)
  cat(c("Chi-square probability:", showprob), "\n")
}