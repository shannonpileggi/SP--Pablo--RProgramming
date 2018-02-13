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
                    args = list(df = df), 
                    aes(colour = "t")) +
      stat_function(geom = "line", 
                    fun = dnorm, 
                    #args = list(x = myseq, mean = mean(x), sd = sd(x)),
                    aes(colour = "Normal")) +
      labs(y = "Percent") +
      scale_colour_manual("Distribution", values = c("red", "green"))
}