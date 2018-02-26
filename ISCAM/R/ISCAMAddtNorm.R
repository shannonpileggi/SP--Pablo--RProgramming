#' ISCAMAddtNorm Function
#'
#' This function creates a histogram of the inputted variable and overlays a t density function with df degrees of freedom 
#' and a normal density function.
#' @param x a vector of numeric values.
#' @param df degrees of freedom
#' @keywords t
#' @export
#' @examples
#' ISCAMAddtNorm(x, 3)
 
ISCAMAddtNorm <- function(x, df){
    data <- data.frame(x)
    ggplot(data, aes(x)) +
      geom_histogram(aes(y = (..density..)), 
                     binwidth = (max(x)-min(x))/20, 
                     colour = "black", 
                     fill = "white") +
      stat_function(geom = "line", 
                    fun = dt, 
                    args = list(df = df), 
                    aes(colour = "t")) +
      stat_function(geom = "line", 
                    fun = dnorm, 
                    args = list(mean = mean(x), sd = sd(x)),
                    aes(colour = "Normal")) +
      labs(y = "Density", x = deparse(substitute(x))) +
      scale_colour_manual("Distribution", values = c("red", "green"))
}