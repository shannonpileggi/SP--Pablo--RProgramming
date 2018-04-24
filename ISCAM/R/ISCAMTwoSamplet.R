#' ISCAMTwoSamplet Function
#'
#' This function calculates a two sample t-test and/or interval from summary data. 
#' @param x1 observed mean
#' @param sd1 observed standard deviation
#' @param n1 sample size
#' @param x2 observed mean
#' @param sd2 observed standard deviation
#' @param n2 sample size
#' @param hypothesized hypothesized difference in population means (default = 0)
#' @param alternative form of alternative ("less", "greater", or "two.sided") 
#' @param conf.level a confidence level for a two-sided confidence interval
#' @keywords two sample t test
#' @export
#' @examples
#' ISCAMTwoSamplet(6, 1.7, 40, 8, 2, 60, alternative = "less")

ISCAMTwoSamplet <- function(observed1, n1, observed2, n2, hypothesized=0, alternative=NULL, conf.level=NULL, datatable=NULL){
  
}