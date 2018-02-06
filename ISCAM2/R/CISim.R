CISim <- function (number, iter) 
{
  newstat = 0
  for (k in 1:iter) {
    print(paste("k", k))
    for (i in 1:number) {
      if (group1outcomes[i] == 1) 
        group1outcomes[i] = 0
    }
    newoutcomes = c(group1outcomes, outcome[treatment == 
                                              "group2"])
    newoutcomes = sample(newoutcomes, 10)
    for (j in 1:5) {
      if (newoutcomes[j] == 0) {
        newoutcomes[j] = 1
        j = 5
      }
    }
    newstat[k] = mean(newoutcomes[newtreatment == "group1"]) - 
      mean(newoutcomes[newtreatment == "group2"])
  }
  return(newstat)
}
