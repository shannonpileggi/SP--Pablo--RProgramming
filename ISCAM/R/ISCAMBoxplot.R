#' ISCAMBoxplot Function
#'
#' This function displays horizontal boxplot(s). 
#' Optional: A second, categorical variable can also be specified and values will be calculated separately for each group. 
#' @param x a continuous variable.
#' @param explanatory a categorical variable.
#' @param names used to specify the horizontal and vertical axis labels, respectively.
#' @keywords boxplot
#' @export
#' @examples
#' ISCAMBoxplot(chickwts$weight)
#' ISCAMBoxplot(chickwts$weight, names = "weight")
#' ISCAMBoxplot(chickwts$weight, chickwts$feed, names = c("weight", "feed"))


ISCAMBoxplot <- function(x, explanatory = NULL, names = NULL){
  df <- data.frame(x)
  if(is.null(explanatory)){
    ggplot(df, aes(x = "", y = x)) + 
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=2) +
      coord_flip() +
      labs(y = ifelse(is.null(names), deparse(substitute(x)), names))
  }
  else{
    ggplot(df, aes(x = explanatory, y = x, color = explanatory)) + 
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=2) +
      coord_flip() +
      labs(y = ifelse(is.null(names), deparse(substitute(x)), names[1]), 
           x = ifelse(is.null(names), deparse(substitute(explanatory)), names[2]), 
           col = ifelse(is.null(names), deparse(substitute(explanatory)), names[2]))
  }
}