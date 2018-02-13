#' ISCAMAddNorm Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' a normal density function.
#' @param x a vector of numeric values.
#' @keywords normal
#' @export
#' @examples
#' ISCAMAddNorm(x)

ISCAMAddNorm <- function(x){
  #lines(myseq, dnorm(myseq, mean(x), sd(x))) can i use this with ggplot?
  df <- data.frame(x)
  min = 0
  max = max(x)
  myseq = seq(min, max, .001)
  ggplot(df, aes(x)) +
    geom_histogram(aes(y = (..count..)/sum(..count..)), 
                   binwidth = 0.1, 
                   colour = "black", 
                   fill = "white") +
    scale_y_continuous(labels = percent) +
    stat_function(geom = "line", 
                  fun = dnorm, 
                  #args = list(x = myseq, mean = mean(x), sd = sd(x)), 
                  colour = "red") +
    labs(y = "Percent")
}
