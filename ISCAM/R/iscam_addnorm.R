#' iscam_addnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' a normal density function.
#' @param x a vector of numeric values.
#' @keywords normal
#' @export
#' @examples
#' iscam_addnorm(x)

iscam_addnorm <- function(x){
  df <- data.frame(x)
  min = 0
  max = max(x)
  myseq = seq(min, max, .001)
  ggplot(df, aes(x)) +
    geom_histogram(aes(y = (..density..)), 
                   binwidth = (max(x)-min(x))/20, 
                   colour = "black", 
                   fill = "white") +
    stat_function(geom = "line", 
                  fun = dnorm, 
                  args = list(mean = mean(x), sd = sd(x)), 
                  colour = "red") +
    labs(y = "Density", x = deparse(substitute(x)))
}
