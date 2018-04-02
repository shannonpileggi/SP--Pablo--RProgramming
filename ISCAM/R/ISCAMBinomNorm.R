#' ISCAMBinomNorm Function
#'
#' This function illustrates the normal approximation to the binomial. 
#' @param k number of successes of interest
#' @param n number of trials (zero or more)
#' @param prob probability of success on each trial
#' @param direction allows you to specify whether you want to find the probability 
#' "above" or "below" k or a symmetric "two.sided" probability 
#' @keywords binomial normal
#' @export
#' @examples
#' ISCAMBinomNorm(20, 30, 0.5, direction = "above")
#' ISCAMBinomNorm(10, 55, 0.10, direction = "below")
#' ISCAMBinomNorm(33, 144, 0.40, direction = "two.sided")

ISCAMBinomNorm <- function(k, n, prob, direction){
  thisx = 0:n
  phat=thisx/n
  minx=max(0, n*prob-4*sqrt(prob*(1-prob)*n))
  maxx=min(n, n*prob+4*sqrt(prob*(1-prob)*n))
}