#' iscamsummary Function
#'
#' This function calculates the five number summary, mean, and standard deviation of the quantitative variable x.
#' @param x quantitative variable
#' @param explanatory optional, categorical grouping variable
#' @param digits number of tick marks on x axis
#' @keywords summary
#' @export
#' @examples
#' iscamsummary(y, x)

iscamsummary <- function(x, explanatory=NULL, digits = 3){
  
  if(is.null(explanatory)){
    qq = stats::quantile(x, na.rm=TRUE)
    qq = c(length(x), signif(c(qq[1L:5L], mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)),  digits))
    names(qq) = c("n", "Min ", "Q1", "Median", "Q3", "Max", "Mean", "SD")
    qq
  }
  else{
    mylabels=names(table(explanatory))
    lengths=tapply(x, explanatory, length)
    middle=tapply(x, explanatory, quantile)
    means = tapply(x, explanatory, mean)
    sds = tapply(x, explanatory, sd)
    
    grouparray=matrix(nrow=length(mylabels), ncol=8, dimnames=list(mylabels, c("n", "Min", "Q1", "Median", "Q3", "Max", "Mean", "SD")))
    
    for(i in 1:length(mylabels)){
      names(middle)[i]="group1"
      grouparray[i,]=c(lengths[i], signif(c(middle[i]$group1, means[i], sds[i]),digits))
    }
    
    data.frame(rbind(grouparray))
    
  }
}
