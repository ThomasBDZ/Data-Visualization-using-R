# load packages ----

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

# load data ----

load("data/forestfires.RData")


# Draw histogram ----

Histogram <- function(df, varquanti, nbins = 15, drawsummary = FALSE){
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


# Draw barplot ----

Barplot <- function(df, varquali){
  caseNumber <- nrow(df)
  myPlot <- ggplot(df) + 
    geom_bar(aes_string(x = varquali), color = "grey30", fill = "grey30") +
    scale_x_discrete("Modalités") +  scale_y_continuous(paste("Fréquence (n = ", caseNumber, ")", sep = "")) + 
    theme_bw()
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
  tabGroup <- data.frame(Modalite = names(groupMean), 
                         Moyenne = groupMean,
                         Mediane = groupMedian,
                         Variance = groupVar, 
                         stringsAsFactors = FALSE)
  tabAll <- data.frame(Modalite = "Ensemble", 
                       Moyenne = round(mean(df[, vary]), digits = 2), 
                       Mediane = round(median(df[, vary]), digits = 2), 
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


# Draw boxplot 2 factors ----

Boxplot2 <- function(df, varx, vary, groupx){
  nbLevels <- length(unique(df[, groupx]))
  if(nbLevels == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (nbLevels > 2){
    colPal <- brewer.pal(n = nbLevels, name = "Set1")
  }
  
  boxPlot <- ggplot(df) + 
    geom_boxplot(aes_string(x = varx, y = vary, fill = groupx), color = "grey20", show.legend = TRUE, outlier.size = NA) +
    scale_fill_manual(values = colPal) +
    theme_bw()
  return(boxPlot)
}


# Anova plot (2 factors) ----

AnovaPlot2 <- function(df, varx, vary, groupx, interact = FALSE){
  
  xLevels <- sort(unique(df[, varx]))
  df$X <- df[, varx]
  df$GROUP <- df[, groupx]
  df$ID <- paste(df$X, df$GROUP, sep = "_")
  df$VAR <- df[, vary]
  
  nbLevels <- length(unique(df[, groupx]))
  if(nbLevels == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (nbLevels > 2){
    colPal <- brewer.pal(n = nbLevels, name = "Set1")
  }
  
  # jitter points
  set.seed(99)
  nbCut <- nbLevels / 2
  seqBreaks <- seq(-0.4, 0.4, 0.4/nbCut)
  df$PARTJIT <- sapply(as.numeric(as.factor(df[, groupx])), function(x) sample(seq(from = seqBreaks[x], to = seqBreaks[x + 1], by = 0.01), size = 1))
  df$JIT <- as.numeric(as.factor(df[, varx])) + df$PARTJIT
  
  # mean segments
  groupMean1 <- tapply(df[, vary], df[, varx], mean, na.rm = TRUE)
  groupMean2 <- tapply(df[, vary], df[, groupx], mean, na.rm = TRUE)
  devMean1 <- groupMean1 - mean(df[, vary], na.rm = TRUE) 
  devMean2 <- groupMean2 - mean(df[, vary], na.rm = TRUE) 
  
  combinNames <- expand.grid(MOD1 = names(groupMean1), MOD2 = names(groupMean2), stringsAsFactors = FALSE)
  combinValues <- expand.grid(DEVMEAN1 = devMean1, DEVMEAN2 = devMean2, stringsAsFactors = FALSE)
  combinValues$EXPECTED <- mean(df[, vary]) + combinValues$DEVMEAN1 + combinValues$DEVMEAN2
  
  if(interact == FALSE){
    avgSegment <- data_frame(ID = paste(combinNames$MOD1, combinNames$MOD2, sep = "_"),
                             XMIN = as.numeric(as.factor(combinNames$MOD1)) + seqBreaks[as.numeric(as.factor(combinNames$MOD2))],  
                             XMAX = as.numeric(as.factor(combinNames$MOD1)) + seqBreaks[as.numeric(as.factor(combinNames$MOD2)) + 1], 
                             YMIN = combinValues$EXPECTED, 
                             YMAX = combinValues$EXPECTED, 
                             GROUP = combinNames$MOD2)
  } else {
    groupMean <- stats::aggregate(df[, vary], list(df[, varx], df[, groupx]), mean, na.rm = TRUE)
    avgSegment <- data_frame(ID = paste(combinNames$MOD1, combinNames$MOD2, sep = "_"),
                             XMIN = as.numeric(as.factor(combinNames$MOD1)) + seqBreaks[as.numeric(as.factor(combinNames$MOD2))],  
                             XMAX = as.numeric(as.factor(combinNames$MOD1)) + seqBreaks[as.numeric(as.factor(combinNames$MOD2)) + 1], 
                             YMIN = groupMean$x, 
                             YMAX = groupMean$x, 
                             GROUP = groupMean$Group.2)
  }
  
  
  # residuals segments
  df <- df %>% left_join(x = ., y = avgSegment[, c(1, 4)], by = "ID")
  
  aovPlot <- ggplot() +
    geom_hline(yintercept = mean(df$VAR, na.rm = TRUE), color = "grey60", size = 1, linetype = 2) +
    geom_segment(data = df, aes(x = JIT, xend = JIT, y = YMIN, yend = VAR), color = "grey40", alpha = 0.5) +
    geom_segment(data = avgSegment, aes(x = XMIN, xend = XMAX, y = YMIN, yend = YMAX, color = GROUP), alpha = 0.8, size = 2) +
    geom_point(data = df, aes(JIT, VAR, color = GROUP), show.legend = FALSE) +
    scale_color_manual(values = colPal) +
    scale_x_continuous(name = varx, breaks = seq(1, length(xLevels), 1), labels = xLevels) +
    scale_y_continuous(name = vary) +
    theme_bw()
  
  return(aovPlot)
}



# Anova parameters (2 factors) ----

AnovaTab2 <- function(df, varx, vary, groupx){
  
  groupMean1 <- round(tapply(df[, vary], df[, varx], mean, na.rm = TRUE), digits = 2)
  groupMean2 <- round(tapply(df[, vary], df[, groupx], mean, na.rm = TRUE), digits = 2)
  groupMedian1 <- round(tapply(df[, vary], df[, varx], median, na.rm = TRUE), digits = 2)
  groupMedian2 <- round(tapply(df[, vary], df[, groupx], median, na.rm = TRUE), digits = 2)
  groupVar1 <- round(tapply(df[, vary], df[, varx], var, na.rm = TRUE), digits = 2)
  groupVar2 <- round(tapply(df[, vary], df[, groupx], var, na.rm = TRUE), digits = 2)
  tabGroup <- data.frame(Modalite = c(names(groupMean1), names(groupMean2)), 
                         Moyenne = c(groupMean1, groupMean2),
                         Mediane = c(groupMedian1, groupMedian2),
                         Variance = c(groupVar1, groupVar2), 
                         stringsAsFactors = FALSE)
  tabAll <- data.frame(Modalite = "Ensemble", 
                       Moyenne = round(mean(df[, vary]), digits = 2), 
                       Mediane = round(median(df[, vary]), digits = 2), 
                       Variance = round(var(df[, vary]), digits = 2), 
                       stringsAsFactors = FALSE)
  
  tabVariance <- rbind(tabGroup, tabAll)
  
  return(tabVariance)
}


# Anova parameters with interaction (2 factors) ----

AnovaTab2Interact <- function(df, varx, vary, groupx){
  
  groupMean <- stats::aggregate(df[, vary], list(df[, varx], df[, groupx]), mean, na.rm = TRUE)
  groupMedian <- stats::aggregate(df[, vary], list(df[, varx], df[, groupx]), median, na.rm = TRUE)
  groupVar <- stats::aggregate(df[, vary], list(df[, varx], df[, groupx]), var, na.rm = TRUE)
  
  tabGroup <- data.frame(Modalite = paste(groupMean[, 1], groupMean[, 2], sep = " : "), 
                         Moyenne = round(groupMean$x, digits = 2),
                         Mediane = round(groupMedian$x, digits = 2),
                         Variance = round(groupVar$x, digits = 2),
                         stringsAsFactors = FALSE)
  
  return(tabGroup)
}


# Draw scatter plot ----

ScatterPlotAncov <- function(df, varx, vary, groupx){
  nbLevels <- length(unique(df[, groupx]))
  if(nbLevels == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (nbLevels > 2){
    colPal <- brewer.pal(n = nbLevels, name = "Set1")
  }
  
  scatPlot <- ggplot(df) + 
    geom_point(aes_string(x = varx, y = vary,  color = groupx)) + 
    geom_smooth(aes_string(x = varx, y = vary, color = groupx), method = "lm", se = FALSE) +
    scale_color_manual(values = colPal) +
    theme_bw()
  
  return(scatPlot)
}

# Draw scatter plot 3D ----

ScatterPlot3D <- function(df, varx, vary, varz){
  scatPlot <- scatterplot3d(x = df[, varx], y = df[, vary], z = df[, varz], pch = 20, color = "#E41A1C", highlight.3d = FALSE, angle = 35,
                            xlab = varx, ylab = vary, zlab = varz)
  fitMod <- lm(formula = formula(eval(paste(varz, "~", varx, "+", vary, sep = " "))), data = df)
  scatPlot$plane3d(fitMod, lty.box = "solid")
  
  return(scatPlot)
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


# Compute linear model (multiple : n > 3) ----

ComputeRegressionMult <- function(df, vardep, varindep, interact = FALSE){
  if(interact == FALSE){
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df)
    linModSumry <- summary(linMod)
  } else {
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "*")))), data = df)
    linModSumry <- summary(linMod)
  }
  coefReg <- round(linModSumry$coefficients, digits = 2)[, 1:2]
  rawR2 <- round(linModSumry$r.squared, digits = 2)
  adjR2 <- round(linModSumry$adj.r.squared, digits = 2)
  akaCrit <- AIC(linMod)
  matCor <- round(cor(df[, c(vardep, varindep)], use = "complete.obs", method = "pearson"), digits = 3)
  tabResid <- data.frame(ABSRESID = round(linModSumry$residuals, digits = 3), 
                         RELRESID = round(linModSumry$residuals / (df[, vardep] - linModSumry$residuals), digits = 4))
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination ajusté",
                                       "Critère d'Akaike (AIC)",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, akaCrit, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  return(list(TABCOEF = tabResults, TABRESID = tabResid, MATCOR = matCor))
}



# Draw ANCOVA scatter plot without interaction ----

DrawRegressionLines <- function(df, varx, vary, groupx){
  # get bounding box by group
  dfSelec <- df[, c(groupx, varx, vary)]
  pointsBbox <- dfSelec %>% 
    group_by_(groupx) %>% 
    summarise_all(funs(min, max))
  colnames(pointsBbox) <- c("TYPE", "XMIN", "YMIN", "XMAX", "YMAX")
  
  # compute regression
  linMod <- lm(formula = formula(eval(paste(vary, "~", paste(varx, groupx, sep = "+")))), data = df)
  coefReg <- linMod$coefficients
  coefRegTab <- data.frame(TYPE = gsub(pattern = groupx, replacement = "", x = names(coefReg)),
                           INTERCEPT = linMod$coefficients)
  drawSegment <- left_join(x = pointsBbox, y = coefRegTab, by = "TYPE")
  drawSegment$INTERCEPT[is.na(drawSegment$INTERCEPT)] <- 0
  drawSegment$INTERCEPT <- drawSegment$INTERCEPT + coefReg[1]
  drawSegment$SLOPE <- coefReg[2]
  
  # draw plot
  nbLevels <- length(unique(df[, groupx]))
  if(nbLevels == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (nbLevels > 2){
    colPal <- brewer.pal(n = nbLevels, name = "Set1")
  }
  scattRegPlot <- ggplot() +
    geom_point(data = dfSelec, aes_string(x = varx, y = vary, color = groupx)) +
    geom_segment(data = drawSegment, aes(x = XMIN, 
                                         xend = XMAX,
                                         y = SLOPE * XMIN + INTERCEPT, 
                                         yend = SLOPE * XMAX + INTERCEPT), 
                 color = colPal, size = 1, lineend = "butt") +
    scale_color_manual(values = colPal) +
    theme_bw()
  
  return(scattRegPlot)
}


# Compute principal components analysis ----

ComputePrincipalComp <- function(df, varquanti, ident){
  # compute analysis
  selecVarQuanti <- df[, varquanti]
  row.names(selecVarQuanti) <- df[, ident]
  dudiObj <- dudi.pca(df = selecVarQuanti, center = TRUE, scale = TRUE, scannf = FALSE, nf = 4)
  
  return(dudiObj)
}


# Inertia decomposition ----

DecompInertia <- function(dudiobj){
  dimTab <- length(dudiobj$eig)
  summaryPca <- data.frame(
    EIG = dudiobj$eig,
    PCTVAR = 100 * dudiobj$eig / sum(dudiobj$eig),
    CUMPCTVAR = cumsum(100 * dudiobj$eig / sum(dudiobj$eig)),
    COMP = factor(x = seq(1, dimTab, 1),
                  levels = seq(1, dimTab, 1),
                  labels = paste("C", seq(1, dimTab, 1), sep = ""))
  )
  
  DecomPlot <- ggplot(summaryPca) + geom_bar(aes(x = COMP, y = PCTVAR), stat = "identity") + 
    scale_x_discrete("Composantes") +
    scale_y_continuous("Pourcentage de l'inertie totale (%)") +
    theme_bw()  
  
  return(DecomPlot)
}


# PCA circle ----

CorCircle <- function(dudiobj, xaxis, yaxis){
  dfCor <- data.frame(dudiobj$co, XORI = 0, YORI = 0, VARIABLE = row.names(dudiobj$co), stringsAsFactors = FALSE)
  oneCircle <- MakeCircle(coordx = 0, coordy = 0, rad = 1)
  corPlot <- ggplot() + 
    geom_vline(xintercept = 0, color = "grey50") + 
    geom_hline(yintercept = 0, color = "grey50") +
    geom_path(data = oneCircle, aes(x = XC, y = YC), color = "grey50") +
    geom_segment(data = dfCor, 
                 aes_string(x = "XORI", xend = colnames(dfCor)[xaxis], y = "YORI", yend = colnames(dfCor)[yaxis]), 
                 lineend = "round",
                 arrow = arrow(length = unit(0.01, "npc"))) +
    geom_label(data = dfCor, aes_string(x = colnames(dfCor)[xaxis], y = colnames(dfCor)[yaxis], label = "VARIABLE"), size = 3, hjust = "outward", vjust = "outward") +
    scale_x_continuous(name = paste("Composante", xaxis, sep = " "), limits = c(-1.2, 1.2)) +
    scale_y_continuous(name = paste("Composante", yaxis, sep = " "), limits = c(-1.2, 1.2)) +
    theme_bw() + coord_equal()
  
  return(corPlot)
}

# circle coordinates ----

MakeCircle <- function(coordx, coordy, rad = 1, npoints = 150){
  tc <- seq(0, 2 * pi, length.out = npoints)
  xc <- coordx + rad * cos(tc)
  yc <- coordy + rad * sin(tc)
  return(data.frame(XC = xc, YC = yc))
}


# correlation matrix ----

CorCompMat <- function(dudiobj, xaxis, yaxis){
  matCor <- round(cor(dudiobj$tab, use = "complete.obs", method = "pearson"), digits = 2)
  compCor <- dudiobj$co[, c(as.numeric(xaxis), as.numeric(yaxis))]
  finalMatCor <- cbind(compCor, matCor)
  return(finalMatCor)
} 

# contributions ----

ContribVarIndiv <- function(dudiobj){
  inertiaPca <- inertia.dudi(dudiobj, row.inertia = TRUE, col.inertia = TRUE)
  contribVar <- round(0.1 * inertiaPca$col.abs, digits = 0)
  contribInd <- data.frame(ID = row.names(inertiaPca$row.abs), round(0.1 * inertiaPca$row.abs, digits = 0), stringsAsFactors = FALSE)
  colnames(contribInd) <- c("ID", "Comp1", "Comp2", "Comp3", "Comp4")
  return(list(CTRVAR = contribVar, CTRIND = contribInd))
}

# individuals plot ----

PlotIndiv <- function(dudiobj, xaxis, yaxis, printlabel = FALSE){
  xString <- paste("Axis", xaxis, sep = "")
  yString <- paste("Axis", yaxis, sep = "")
  coordIndiv <- data.frame(ID = row.names(dudiobj$li),
                           dudiobj$li,
                           stringsAsFactors = FALSE)
  if(printlabel == FALSE){
    pcaIndivPlot <- ggplot(coordIndiv) +
      geom_hline(yintercept = 0, color = "grey50") + geom_vline(xintercept = 0, color = "grey50") +
      geom_point(aes_string(x = xString, y = yString), size = 0.6) +
      scale_x_continuous(name = paste("Composante", xaxis, sep = " ")) +
      scale_y_continuous(name = paste("Composante", yaxis, sep = " ")) +
      coord_equal() +
      theme_bw()
  } else {
    pcaIndivPlot <- ggplot(coordIndiv) +
      geom_hline(yintercept = 0, color = "grey50") + geom_vline(xintercept = 0, color = "grey50") +
      geom_text(aes_string(x = xString, y = yString, label = "ID"), size = 3) +
      scale_x_continuous(name = paste("Composante", xaxis, sep = " ")) +
      scale_y_continuous(name = paste("Composante", yaxis, sep = " ")) +
      coord_equal() +
      theme_bw()
  }
  
  return(pcaIndivPlot)
}


# HIERARCHICAL CLUSTERING ----

# compute classification ----

ComputeClassif <- function(df, varquanti, stand, method){
  classifObj <- agnes(x = df[, varquanti], diss = FALSE, metric = "euclidean", stand = stand, method = method)
  return(classifObj)
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


# Plot choropleth map

CartoVar <- function(spdf, df, idshape, idtab, varquanti, paltype, discret, nbcl){
  allPal <- c("blue.pal", "orange.pal", "red.pal", "brown.pal", "green.pal",
              "purple.pal", "pink.pal", "wine.pal", "grey.pal", "turquoise.pal", 
              "sand.pal", "taupe.pal", "kaki.pal" , "harmo.pal")
  if(paltype == "quanti"){
    colPal <- carto.pal(pal1 = allPal[sample(x = 1:14, size = 1)], n1 = nbcl, transparency = TRUE)
    choroLayer(spdf = spdf, 
               df = df,
               spdfid = idshape,
               dfid = idtab,
               var = varquanti,
               method = discret,
               nclass = nbcl,
               border = "grey",
               legend.values.rnd = 1,
               col = colPal,
               legend.title.cex = 1, legend.values.cex = 1)
  } else if (paltype == "diver") {
    if(nbcl %% 2 == 0){
      colPal <- carto.pal(pal1 = "orange.pal", n1 = nbcl/2, pal2 = "green.pal", n2 = nbcl / 2, transparency = TRUE)
    } else {
      colPal <- carto.pal(pal1 = "orange.pal", n1 = nbcl/2, pal2 = "green.pal", n2 = nbcl / 2, middle = TRUE, transparency = TRUE)
    }
    choroLayer(spdf = spdf, 
               df = df,
               spdfid = idshape,
               dfid = idtab,
               var = varquanti,
               method = discret,
               nclass = nbcl,
               border = "grey",
               legend.values.rnd = 1,
               col = colPal,
               legend.title.cex = 1, legend.values.cex = 1)
  } else if (paltype == "quali"){
    colPal <- carto.pal(pal1 = "multi.pal", n1 = nbcl, transparency = TRUE)
    typoLayer(spdf = spdf, 
              df = df,
              spdfid = idshape,
              dfid = idtab,
              var = varquanti,
              border = "grey",
              col = colPal,
              legend.title.cex = 1, legend.values.cex = 1)
  }
  
  
}



