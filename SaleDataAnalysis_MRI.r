library(dplyr)
library(tidyr)
library(ggplot2)


#cleandataToPlot <- read.csv("C:\\My_Materials\\iDX\\work-folder\\CleanData\\MySQL_CleanData08-Aug-2018-JuneJuly.csv")
cleandataToPlot <- read.csv("C:\\My_Materials\\iDX\\work-folder\\CleanData\\cleanSalesData.csv")

MRI_Data <- cleandataToPlot %>% filter(Category=='MRI')

MRI_PriceDistbn <- MRI_Data %>%
  dplyr::summarize(MeanPrice = mean(Price, na.rm=TRUE),
                   MinPrice = min(Price, na.rm=TRUE),
                   MaxPrice = max(Price, na.rm=TRUE),
                   MedianPrice = median(Price, na.rm=TRUE)) %>%
  select(MedianPrice, MeanPrice, MinPrice, MaxPrice)



summary_stats <- function(x) {
  mu <- mean(x)
  sigma1 <- mu-sd(x)
  sigma2 <- mu+sd(x)
  return(c(y=mu,ymin=sigma1,ymax=sigma2))
}


##################################################################################

modelStatPlotList <- list()
plot_summary_stats_for_MRI_models <- function(data){
  
  unique_model_names <- unique(data$Model)
  unique_model_names
  
  for (m in unique_model_names){
    model <- gsub('([[:punct:]])|\\s+','_',m)
    d <- data %>% filter(model==m)
    d_stat_summary <- summary_stats(d$Price)
    
    violinPlot <- ggplot(data=d, aes(x=model, y=Price)) + ggtitle(paste0(model, " Summary Statistics"))+
      geom_violin() + stat_summary(fun.data=summary_stats)
    
    boxPlot <- ggplot(data=d, aes(x=model, y=Price)) + ggtitle(paste0(model, " Summary Statistics"))+
      geom_crossbar(stat="summary", fun.y=summary_stats, fun.ymax=max, fun.ymin=min)
    #boxPlot
    model_stat_plot <- (list(summaryStats=d_stat_summary,vPlot=violinPlot,bPlot=boxPlot, modelName=m))
    
    
    modelStatPlotList[[m]] <- (model_stat_plot)
  }
  
  modelStatPlotList
  
}

MRI_Summary <- plot_summary_stats_for_MRI_models(MRI_Data)



plot_summary <- function(d){
  for (plot in d){
    p <- plot
    plot(p$m$bPlot)
  }
}


