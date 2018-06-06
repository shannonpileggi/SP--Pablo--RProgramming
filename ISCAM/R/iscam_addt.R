#' iscam_addt Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' a t density function with df degrees of freedom.
#' @param x a vector of numeric values.
#' @keywords t
#' @export
#' @examples
#' iscam_addt(x, 5)

iscam_addt <- function(x, df){
  data <- data.frame(x)
  ggplot(data, aes(x)) +
    geom_histogram(aes(y = (..density..)), 
                   binwidth = (max(x)-min(x))/20, 
                   colour = "black", 
                   fill = "white") +
    stat_function(geom = "line", 
                  fun = dt, 
                  args = list(df = df), 
                  colour = "red") +
    labs(y = "Density", x = deparse(substitute(x)))
}
