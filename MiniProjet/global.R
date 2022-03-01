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

# plot histogram

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

# Draw mosaic plot ----

Mosaicplot <- function(df, varx, vary){
  mosaicPlot <- mosaicplot(table(df[, varx], df[, vary]), main = paste("Tri croisé : ", varx, " - ", vary, sep = ""))
  return(mosaicPlot)
}

# Draw scatter plot ----

ScatterPlot <- function(df, varx, vary){
  scatPlot <- ggplot(df) + 
    geom_point(aes_string(x = varx, y = vary), color = "grey60") + 
    geom_smooth(aes_string(x = varx, y = vary), method = "lm", se = FALSE, color = "chocolate") +
    theme_bw()
  
  return(scatPlot)
}


# Draw boxplot ----

Boxplot <- function(df, varx, vary, jit){
  nbLevels <- length(unique(df[, varx]))
  if(nbLevels == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (nbLevels > 2){
    colPal <- brewer.pal(n = nbLevels, name = "Set1")
  }
  
  if(jit == FALSE){
    boxPlot <- ggplot(df) + 
      geom_boxplot(aes_string(x = varx, y = vary, fill = varx), color = "grey20", show.legend = FALSE) +
      scale_fill_manual(values = colPal) +
      theme_bw()
  } else {
    boxPlot <- ggplot(df) + 
      geom_boxplot(aes_string(x = varx, y = vary, fill = varx), color = "grey20", show.legend = FALSE, outlier.size = NA) +
      geom_jitter(aes_string(x = varx, y = vary), width = 0.7, alpha = 0.4) +
      scale_fill_manual(values = colPal) +
      theme_bw()
  }
  return(boxPlot)
}


# Anova parameters (1 factor) ----

AnovaTab <- function(df, varx, vary){
  groupMean <- round(tapply(df[, vary], df[, varx], mean, na.rm = TRUE), digits = 2)
  groupMedian <- round(tapply(df[, vary], df[, varx], median, na.rm = TRUE), digits = 2)
  groupVar <- round(tapply(df[, vary], df[, varx], var, na.rm = TRUE), digits = 2)
  tabGroup <- data.frame(Modalité = names(groupMean), 
                         Moyenne = groupMean,
                         Médiane = groupMedian,
                         Variance = groupVar, 
                         stringsAsFactors = FALSE)
  tabAll <- data.frame(Modalité = "Ensemble", 
                       Moyenne = round(mean(df[, vary]), digits = 2), 
                       Médiane = round(median(df[, vary]), digits = 2), 
                       Variance = round(var(df[, vary]), digits = 2), 
                       stringsAsFactors = FALSE)
  
  tabVariance <- rbind(tabGroup, tabAll)
  
  return(tabVariance)
}


# Anova plot (1 factor) ----

AnovaPlot <- function(df, varx, vary){
  
  xLevels <- sort(unique(df[, varx]))
  df$ID <- df[, varx]
  df$VAR <- df[, vary]
  
  if(length(xLevels) == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (length(xLevels) > 2){
    colPal <- brewer.pal(n = length(xLevels), name = "Set1")
  }
  
  # jitter points
  set.seed(99)
  df$JIT <- as.numeric(as.factor(df[, varx])) + sample(x = seq(-0.3, 0.3, 0.01), size = nrow(df), replace = TRUE)
  
  # mean segments
  groupMean <- tapply(df[, vary], df[, varx], mean, na.rm = TRUE)
  avgSegment <- data_frame(ID = names(groupMean), 
                           XMIN = seq(1, length(groupMean), 1) - 0.4,  
                           XMAX = seq(1, length(groupMean), 1) + 0.4, 
                           YMIN = groupMean, 
                           YMAX = groupMean)
  
  # residuals segments
  df <- df %>% left_join(x = ., y = avgSegment[, c(1, 4)], by = "ID")
  
  aovPlot <- ggplot() +
    geom_hline(yintercept = mean(df$VAR, na.rm = TRUE), color = "grey60", size = 1, linetype = 2) +
    geom_segment(data = avgSegment, aes(x = XMIN, xend = XMAX, y = YMIN, yend = YMAX), color = "grey40", size = 2) +
    geom_segment(data = df, aes(x = JIT, xend = JIT, y = YMIN, yend = VAR), color = "grey40", alpha = 0.5) +
    geom_point(data = df, aes(JIT, VAR, color = ID), show.legend = FALSE) +
    scale_color_manual(values = colPal) +
    scale_x_continuous(name = varx, breaks = seq(1, length(groupMean), 1), labels = xLevels) +
    scale_y_continuous(name = vary) +
    theme_bw()
  
  return(aovPlot)
}

# correlation matrix ----

CorCompMat <- function(dudiobj, xaxis, yaxis){
  matCor <- round(cor(dudiobj$tab, use = "complete.obs", method = "pearson"), digits = 2)
  compCor <- dudiobj$co[, c(as.numeric(xaxis), as.numeric(yaxis))]
  finalMatCor <- cbind(compCor, matCor)
  return(finalMatCor)
} 

# plot dendrogram ----

PlotDendro <- function(classifobj){
  dendroPlot <- as.dendrogram(classifobj)
  dendroData <- dendro_data(dendroPlot, type = "rectangle")
  dendroGgplot <- ggplot(segment(dendroData)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_x_continuous("") + scale_y_continuous("") +
    theme_bw()
  
  return(dendroGgplot)
}

# plot inertia ----

PlotHeight <- function(classifobj){
  sortedHeight <- sort(classifobj$height, decreasing = TRUE)
  relHeigth <- sortedHeight / sum(sortedHeight) * 100
  tabHeight <- data.frame(NODES = factor(1:20),
                          INERTIE = relHeigth[1:20])
  
  heightPlot <- ggplot(tabHeight) +
    geom_bar(aes(x = NODES, y = INERTIE), fill = "grey30", stat = "identity") +
    scale_x_discrete("Nombre de classes") + scale_y_continuous("Niveau") +
    theme_bw()
  
  return(heightPlot)
}

# Compute linear model ----

ComputeRegression <- function(df, vardep, varindep, interact = FALSE){
  if(interact == FALSE){
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df)
    linModSumry <- summary(linMod)
  } else {
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "*")))), data = df)
    linModSumry <- summary(linMod)
  }
  coefReg <- round(linModSumry$coefficients, digits = 4)[, 1:2]
  rawR2 <- round(linModSumry$r.squared, digits = 2)
  adjR2 <- round(linModSumry$adj.r.squared, digits = 2)
  
  tabResid <- data.frame(ABSRESID = round(linModSumry$residuals, digits = 3), 
                         RELRESID = round(linModSumry$residuals / (df[, vardep] - linModSumry$residuals), digits = 3))
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination ajusté",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  
  return(list(TABCOEF = tabResults, TABRESID = tabResid, COEF = coefReg))
}

# plot profile ----

PlotProfile <- function(classifobj, nbclus){
  dfOri <- as.data.frame(classifobj$data, stringsAsFactors = FALSE)
  clusId <- cutree(classifobj, k = nbclus)
  dfOri$CLUS <- factor(clusId,
                       levels = 1:nbclus,
                       labels = paste("CLASSE", 1:nbclus))
  clusProfile <- aggregate(dfOri[, 1:ncol(dfOri)-1],
                           by = list(dfOri$CLUS),
                           mean)
  colnames(clusProfile)[1] <- "CLASSE"
  clusLong <- melt(clusProfile, id.vars = "CLASSE")
  
  profilePlot <- ggplot(clusLong) +
    geom_bar(aes(x = variable, y = value), fill = "grey30", position = "identity", stat = "identity") +
    scale_x_discrete("Variable") + scale_y_continuous("Valeur moyenne par classe") +
    facet_wrap(~ CLASSE) + coord_flip() + theme_bw()
  
  return(list(PROFILE = profilePlot, CLUSID = dfOri$CLUS))
}

# Draw barplot ----

Barplot <- function(df, varquali){
  caseNumber <- nrow(df)
  myPlot <- ggplot(df) + 
    geom_bar(aes_string(x = varquali), color = "grey30", fill = "grey30") +
    scale_x_discrete("Modalités") +  scale_y_continuous(paste("Fréquence (n = ", caseNumber, ")", sep = "")) + 
    theme_bw()
  return(myPlot)
}