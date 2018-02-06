#' iscamdotplot Function
#'
#' This function creates horizontal dotplots.If no explanatory variable is specified, just one plot. If an explanatory variable is specified, parallel dotplots will be created.
#' @param response a numeric vector or a single list containing numeric vectors from which the dotplots are to be produced
#' @param explanatory grouping variable 
#' @param names used to specify the horizontal and vertical axis labels respectively 
#' @keywords dotplot
#' @export
#' @examples
#' iscamdotplot(x)
#' iscamdotplot(mtcars$mpg, explanatory = mtcars$vs, names = c("mpg", "vs"))

iscamdotplot <- function (response, explanatory = NULL, names = NULL){
  if (is.null(explanatory)) {
    stripchart(response, method = "stack", ylim = c(0, 1000), 
               xlab = names[1], ylab = names[2], pch = 16)
    abline(h = 0.98)
  }
  else {
    numCategories = length(table(explanatory))
    ymin = 0.5
    ymax = numCategories + 0.5
    stripchart(response ~ explanatory, vertical = FALSE, 
               method = "stack", ylim = c(ymin, ymax), xlab = names[1], 
               ylab = names[2], pch = 16)
    for (i in 1:numCategories) {
      abline(h = i)
    }
  }
}
