#' iscamhypernorm Function
#'
#' This function calculates tail probabilities from the hypergeometric distribution.
#' @param k number of successes of interest (integer) or difference in conditional proportions
#' @param total total number of observations in the study
#' @param succ overall number of successes
#' @param n number of observations in group A
#' @param lower.tail a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#' @keywords hypergeometric normal
#' @export
#' @examples
#' iscamhypernorm(26, 52, 2, 5, lower.tail = FALSE)

iscamhypernorm <- function (k, total, succ, n, lower.tail) 
{
  if (k < 1) 
    k = round((k * n * (total - n) + n * succ)/total)
  fail = total - succ
  thisx = max(0, n - fail):min(n, succ)
  normseq = seq(max(0, n - fail), min(n, succ), 0.001)
  plot(thisx, dhyper(thisx, succ, fail, n), xlab = "Number of Successes", 
       ylab = "Probability", type = "h")
  abline(h = 0, col = "gray")
  normmean = n * succ/total
  normsd = sqrt(n * succ/total * (total - n)/total * (total - 
                                                        succ)/(total - 1))
  minx = max(0, normmean - 4 * normsd)
  maxx = min(n, normmean + 4 * normsd)
  myy = dhyper(floor(normmean), succ, fail, n)/2
  if (lower.tail) {
    this.prob = phyper(k, succ, fail, n)
    showprob = format(this.prob, digits = 4)
    this.prob2 = pnorm(k, normmean, normsd)
    showprob2 = format(this.prob2, digits = 4)
    this.prob3 = pnorm(k + 0.5, normmean, normsd)
    showprob3 = format(this.prob3, digits = 4)
    withcorrect = seq(0, k + 0.5, 0.001)
    probseq = seq(0, k, 0.001)
    polygon(c(withcorrect, k + 0.5, 0), c(dnorm(withcorrect, 
                                                normmean, normsd), 0, 0), col = 6)
    polygon(c(probseq, k, 0), c(dnorm(probseq, normmean, 
                                      normsd), 0, 0), col = 5, border = "red")
    lines(normseq, dnorm(normseq, normmean, normsd))
    lines(0:k, dhyper(0:k, succ, fail, n), col = "red", 
          type = "h")
    text((minx + normmean)/2, myy, labels = paste("P(X<=", 
                                                  k, ")\n = ", showprob), col = "red")
  }
  if (!lower.tail) {
    this.prob = 1 - phyper(k - 1, succ, fail, n)
    showprob = format(this.prob, digits = 4)
    this.prob2 = pnorm(k, normmean, normsd, FALSE)
    showprob2 = format(this.prob2, digits = 4)
    this.prob3 = pnorm(k - 0.5, normmean, normsd, FALSE)
    showprob3 = format(this.prob3, digits = 4)
    withcorrect = seq(k - 0.5, n, 0.001)
    probseq = seq(k, n, 0.001)
    polygon(c(withcorrect, n, k - 0.5), c(dnorm(withcorrect, 
                                                normmean, normsd), 0, 0), col = 6)
    polygon(c(probseq, n, k), c(dnorm(probseq, normmean, 
                                      normsd), 0, 0), col = 5, border = "red")
    lines(normseq, dnorm(normseq, normmean, normsd))
    lines(k:n, dhyper(k:n, succ, fail, n), col = "red", 
          type = "h")
    text((maxx + normmean)/2, myy, labels = paste("P(X>=", 
                                                  k, ")\n = ", showprob), pos = 4, col = "red")
  }
  newtitle = substitute(paste("Hypergeometric (", N == x1, 
                              ", ", M == x2, ",", n == x3, "), Normal(", mean == x4, 
                              ",  ", SD == x5, ")"), list(x1 = total, x2 = succ, x3 = n, 
                                                          x4 = normmean, x5 = normsd))
  title(newtitle)
  full = c(c(" hypergeometric:", showprob), c("\n normal approx:", 
                                              showprob2), c("\n normal approx with continuity:", showprob3))
  cat(full, "\n")
}
