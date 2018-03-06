
ISCAMInvt <- function(prob1, df, direction){
  mydf <- data.frame(x = c(-4, 4))
  upper <- -qt(prob1, df)
  lower <- qt(prob1, df)
  upside <- -signif(qt((1-prob1)/2, df, lower.tail=TRUE), 4)
  downside <- -upside
  outupper <- -signif(qt(prob1/2, df, lower.tail=TRUE), 4)
  outlower <- -outupper

  if (direction=="above"){ 
  p <- ggplot(mydf, aes(x = x)) +
    stat_function(fun = dt, 
                  args = list(df = df),
                  color = "dodgerblue") +
    geom_segment(aes(x=-4,xend=4,y=0,yend=0), color = "dodgerblue") +
    labs(x = "t values",
         y = "density")+
         #title = mytitle,
         #subtitle = mysubtitle) +
    #theme_bw(12, "serif") + 
    #theme(plot.subtitle=element_text(color="dodgerblue4")) +
    stat_function(fun = dt,
                  args = list(df = df),
                  xlim = c(upper, 4),
                  geom = "area", 
                  color = "dodgerblue4",
                  fill = "dodgerblue4") 
  }
  else if (direction == "below"){
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dt, 
                    args = list(df = df),
                    color = "dodgerblue") +
      geom_segment(aes(x=-4,xend=4,y=0,yend=0), color = "dodgerblue") +
      labs(x = "t values",
           y = "density")+
      #title = mytitle,
      #subtitle = mysubtitle) +
      #theme_bw(12, "serif") + 
      #theme(plot.subtitle=element_text(color="dodgerblue4")) +
      stat_function(fun = dt,
                    args = list(df = df),
                    xlim = c(-4, lower),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") 
  }
  else if (direction == "between"){
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dt, 
                    args = list(df = df),
                    color = "dodgerblue") +
      geom_segment(aes(x=-4,xend=4,y=0,yend=0), color = "dodgerblue") +
      labs(x = "t values",
           y = "density")+
      #title = mytitle,
      #subtitle = mysubtitle) +
      #theme_bw(12, "serif") + 
      #theme(plot.subtitle=element_text(color="dodgerblue4")) +
      stat_function(fun = dt,
                    args = list(df = df),
                    xlim = c(downside, upside),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") 
  }
  else if (direction == "outside"){
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dt, 
                    args = list(df = df),
                    color = "dodgerblue") +
      geom_segment(aes(x=-4,xend=4,y=0,yend=0), color = "dodgerblue") +
      labs(x = "t values",
           y = "density")+
      #title = mytitle,
      #subtitle = mysubtitle) +
      #theme_bw(12, "serif") + 
      #theme(plot.subtitle=element_text(color="dodgerblue4")) +
      stat_function(fun = dt,
                    args = list(df = df),
                    xlim = c(-4, outlower),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") +
      stat_function(fun = dt, #Is there an easier way to combine these?
                    args = list(df = df),
                    xlim = c(outupper, 4),
                    geom = "area", 
                    color = "dodgerblue4",
                    fill = "dodgerblue4") 
  }
  print(p)
}

