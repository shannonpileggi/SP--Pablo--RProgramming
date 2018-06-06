#' iscam_addexp Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' an exponential density function with lambda = 1/mean.
#' @param x a vector of numeric values.
#' @keywords exponential
#' @export
#' @examples
#' iscam_addexp(x)

iscam_addexp <- function(x){
  df <- data.frame(x)
  ggplot(df, aes(x)) +
    geom_histogram(aes(y = (..density..)), 
    #geom_histogram(aes(y = (..count..)/sum(..count..)), if you want percents on y-axis
                   binwidth = (max(x)-min(x))/20, 
                   colour = "black", 
                   fill = "white") +
    #scale_y_continuous(labels = percent) +
    stat_function(geom = "line", 
                  fun = dexp, 
                  args = list(rate = 1/mean(x)), 
                  colour = "red") +
    labs(y = "Density", x = deparse(substitute(x)))
}