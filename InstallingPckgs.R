install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("C:/Users/noell/Documents/SP--Pablo--RProgramming")
#create("ISCAM")
setwd("./ISCAM")
document()
build()
setwd("..")

install("ISCAM")
#making package for all existing functions
library(ISCAM)
library(ggplot2)
library(gridExtra)
library(skimr)
#library(scales)
load("C:/Users/noell/Desktop/ISCAM.RData")

# Creating Vignette
library(knitr)
devtools::use_vignette("my-vignette")
