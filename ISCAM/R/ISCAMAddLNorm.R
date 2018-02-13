#' ISCAMAddLNorm Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' a log normal density function.
#' @param x a vector of numeric values.
#' @keywords lognormal
#' @export
#' @examples
#' ISCAMAddLNorm(x)

ISCAMAddLNorm <- function(x){
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
                  fun = dlnorm, 
                  #args = list(x = myseq, meanlog = mean(log(x)), sdlog = sd(log(x))), 
                  colour = "red") +
                  labs(y = "Percent")
}
