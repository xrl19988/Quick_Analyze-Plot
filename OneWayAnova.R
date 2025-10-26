
# r packages####
pacman="pacman"
if (!pacman %in% rownames(installed.packages())) {
  install.packages(pacman)
}
library(pacman)
pacman::p_load(agricolae,ggplot2,rstatix,ggprism,reshape2,ggpubr,
               # introdataviz,
               palmerpenguins,tidyverse,openxlsx,car)

# OWA: One-Way ANOVA ####
OWA <- function(data, method = 'duncan',group,value){
  if ('facet' %in% colnames(data)) {
    facetgroup <- data.frame(unique(data$facet))
    colnames(facetgroup) <- "facetgroup"
    
    resulte <-  data.frame(group="",mean="",sd="",se="",label="",max="",facet = "")[-1,]
    for (i in c(1:length(unique(data$facet)))){
      facetgroup1 <- facetgroup[i,]
      datafacet <- data[data$facet == facetgroup1,]
      oneway <- aov(value~group, data = datafacet)
      anova(oneway)
      if (method == 'duncan') {
        result <- duncan.test(oneway, 'group')
      }
      if (method == 'lsd') {
        result <- LSD.test(oneway, 'group')
      }
      if (method == 'tukey') {
        result <- HSD.test(oneway, 'group')
      }
      mar<-result$groups
      rownamemar<-row.names(mar)
      newmar<-data.frame(rownamemar,mar[,1],mar$groups)
      sort<-newmar[order(newmar$rownamemar),]
      rowname<-row.names(result$means)
      mean<-result$means[,1]
      sd<-result$means[,2]
      se <- sd/sqrt(result$means[,3])
      max <- result$means[,ifelse(method == 'lsd',8,6)]
      marker<-sort$mar.groups
      facet <- rep(facetgroup1,length(rowname))
      result2<-data.frame(rowname,mean,sd,se,marker,max,facet)
      colnames(result2) <- c('group','mean','sd','se','label','max','facet')
      resulte <- rbind(resulte,result2)
    }
    return(resulte)
  } else{
    oneway <- aov(data[[value]] ~ data[[group]], data = data)
    anova(oneway)
    if (method == 'duncan') {
      result <- duncan.test(oneway, 'data[[group]]')
    }
    if (method == 'lsd') {
      result <- LSD.test(oneway, 'data[[group]]')
    }
    if (method == 'tukey') {
      result <- HSD.test(oneway, 'data[[group]]')
    }
    mar<-result$groups
    rownamemar<-row.names(mar)
    newmar<-data.frame(rownamemar,mar[,1],mar$groups)
    sort<-newmar[order(newmar$rownamemar),]
    rowname<-row.names(result$means)
    mean<-result$means[,1]
    sd<-result$means[,2]
    se <- sd/sqrt(result$means[,3])
    max <- result$means[,ifelse(method == 'lsd',8,6)]
    marker<-sort$mar.groups
    result2<-data.frame(rowname,mean,sd,se,marker,max)
    colnames(result2) <- c('group','mean','sd','se','label','max')
    return(result2)
  }
}





