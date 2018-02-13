#' ISCAMAddt Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' a t density function with df degrees of freedom.
#' @param x a vector of numeric values.
#' @keywords t
#' @export
#' @examples
#' ISCAMAddt(x, 5)

ISCAMAddt <- function(x, df){
  data <- data.frame(x)
  min = 0
  max = max(x)
  myseq = seq(min, max, .001)
  ggplot(data, aes(x)) +
    geom_histogram(aes(y = (..count..)/sum(..count..)), 
                   binwidth = 0.1, 
                   colour = "black", 
                   fill = "white") +
    scale_y_continuous(labels = percent) +
    stat_function(geom = "line", 
                  fun = dt, 
                  args = list(x = myseq, df = df), 
                  colour = "red") +
    labs(y = "Percent")
}
