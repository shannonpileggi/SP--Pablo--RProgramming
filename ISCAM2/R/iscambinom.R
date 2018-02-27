#' iscambinom Function
#'
#' This function calculates either a tail probability or the inverse cumulative 
#' probability depending on whether k or prob is passed a question mark.
#' @param k number of successes
#' @param prob probability of interest
#' @param n number of trials (zero or more)
#' @param pi probability of success on each trial
#' @param lower.tail logical; if TRUE, probabilities are  P[X <= k], otherwise, P[X > k]
#' @keywords binomial
#' @export
#' @examples
#' iscambinom(?, .50, 20, lower.tail = TRUE)
#' iscambinom(10, ?, 20, lower.tail = TRUE)


iscambinom <- function(k, prob, n, pi, lower.tail){
  if(as.character(prob)=="?") iscambinomprob(k, n, pi, lower.tail)
  if(as.character(k)== "?") iscaminvbinom(prob, n, pi, lower.tail)
  }
