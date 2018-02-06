#' iscamboxplot Function
#'
#' This function displays horizontal boxplot(s) utilizing quartiles instead of hinges to match the summary statistics.
#' @param x a numeric vector or a single list containing numeric vectors form which the boxplots are to be produced
#' @param explanatory NULL or a second, categorical grouping variable 
#' @param names used to specify the horizontal and vertical axis labels respectively 
#' @keywords boxplot
#' @export
#' @examples
#' iscamboxplot(x)
#' iscamboxplot(mtcars$mpg, explanatory = mtcars$vs, names = c("mpg", "vs"))

iscamboxplot <- function(x, explanatory=NULL, names=NULL){
  if(is.null(names)) names=c(substitute(x), substitute(explanatory)) 
  
  if(is.null(explanatory)){
    qq = stats::quantile(x, na.rm=TRUE)
    bp=boxplot(x, plot=FALSE)
    bp$stats[2,1]=qq[2L]
    bp$stats[4,1]=qq[4L]
    bxp(bp, horizontal=TRUE)
    title(sub=names[1])
  }
  else{
    
    mylabels=names(table(explanatory))
    grouparray=matrix(nrow=length(mylabels), ncol=5)
    middle=tapply(x, explanatory, quantile)
    bp=boxplot(x~explanatory, plot=FALSE)
    for(i in 1:length(mylabels)){
      names(middle)[i]="group1"
      grouparray[i,]=(middle[i]$group1)
      bp$stats[2,i]=grouparray[i,2]
      bp$stats[4,i]=grouparray[i,4]
    }
    
    bxp(bp, horizontal=TRUE, xlab=names[1], ylab=names[2])
  }
}
