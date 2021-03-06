#' iscam_addt Function
#'
#' This function creates a histogram of the inputted variable and overlays 
#' a t density function with df degrees of freedom.
#' @param x a vector of numeric values.
#' @keywords t
#' @export
#' @examples
#' iscam_addt(x, 5)

iscam_addt <- function(x, df) {
  data <- data.frame(x) # Converting numeric vector to a data frame
  ggplot(data, aes(x)) +
    geom_histogram( # Histogram for inputted variable
      aes(y = (..density..)),
      binwidth = (max(x) - min(x)) / 20,
      colour = "black",
      fill = "white") +
    stat_function( # t density curve
      geom = "line",
      fun = dt, 
      args = list(df = df), # Specifying df
      colour = "red"
    ) +
    labs(y = "Density", x = deparse(substitute(x)))
}
