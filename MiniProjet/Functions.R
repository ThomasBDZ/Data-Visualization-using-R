library(shinythemes)
require(RColorBrewer)
require(ggplot2)
library(gridExtra)
library(scatterplot3d)
library(cluster)
library(ggdendro)
library(reshape2)
library(ade4)
library(sp)
library(rgdal)
library(cartography)
library(dplyr)

load("data/forestfires.RData")

Histogram <- function(df, varquanti, nbins = 20, drawsummary = FALSE){
  caseNumber <- nrow(df)
  myPlot <- ggplot(df) + 
    geom_histogram(aes_string(x = varquanti), color = "white", fill = "grey30", bins = nbins) +
    scale_y_continuous(paste("Fréquence (n = ", caseNumber, ")", sep = "")) + theme_bw()
  
  if (isTRUE(drawsummary)){
    myPlot <- myPlot +  
      geom_vline(xintercept = mean(df[[varquanti]], na.rm = TRUE), color = "chocolate4") +
      geom_vline(xintercept = quantile(df[[varquanti]], probs = seq(0, 1, 0.25), na.rm = TRUE)[2:4], color = "chartreuse4")
  }
  return(myPlot)
}