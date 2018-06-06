#' iscam_addlnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' a log normal density function.
#' @param x a vector of numeric values.
#' @keywords lognormal
#' @export
#' @examples
#' iscam_addlnorm(x)

iscam_addlnorm <- function(x){
  df <- data.frame(x)
  ggplot(df, aes(x)) +
  geom_histogram(aes(y = (..density..)), 
                   binwidth = 0.1, 
                   colour = "black", 
                   fill = "white") +
  stat_function(geom = "line", 
                  fun = dlnorm, 
                  args = list(meanlog = mean(log(x)), sdlog = sd(log(x))), 
                  colour = "red") +
                  labs(y = "Percent")
}
