library(ggplot2)
library(dplyr)
library(gridExtra)
library(kableExtra)
library(knitr)
library(EnvStats)
source('C:\\My_Materials\\iDX\\work-folder\\AnalysisScripts\\comparison.r')


print(length(MRI_Summary_sales))
print(length(MRI_Summary_scrape))

print(paste("No of rows in Sale data: ", nrow(saleData)))
print(paste("No of rows in Clean scraped data: ", nrow(scrapeData)))


stats_1 <- NULL
stats_2 <- NULL

for(i in 1:length(commonMRIModels)) {
  currModelName <- commonMRIModels[i]
  for(j in 1:length(MRI_Summary_sales)) {
    if(MRI_Summary_sales[[j]]$modelName == currModelName) {
      curMRISales <- MRI_Summary_sales[[j]]
    }
  }
  
  stats_1 <- cbind(stats_1, curMRISales$summaryStats)
  
  for(k in 1:length(MRI_Summary_scrape)) {
    if(MRI_Summary_scrape[[k]]$modelName == currModelName) {
      curMRIScrape <- MRI_Summary_scrape[[k]]
    }
  }
  
  stats_2 <- cbind(stats_2, curMRIScrape$summaryStats)  
  
  stats_sales <- as.data.frame(curMRISales$summaryStats)
  stats_scrape <- as.data.frame(curMRIScrape$summaryStats)
  
  colnames(stats_sales) <- c(curMRISales$modelName)
  rownames(stats_sales) <- c("Mean Price", "Standard Deviation", "Calculated Minimum Price", "Calculated Maximum Price", "Actual Minimum Price", "Actual Maximum Price")
  grid.arrange(curMRISales$bPlot, curMRISales$vPlot, ncol=2)
  

  colnames(stats_scrape) <- c(curMRIScrape$modelName)
  rownames(stats_scrape) <- c("Mean Price", "Standard Deviation", "Calculated Minimum Price", "Calculated Maximum Price", "Actual Minimum Price", "Actual Maximum Price")
  grid.arrange(curMRIScrape$bPlot, curMRIScrape$vPlot, ncol=2)
  
}

meanprice <- as.data.frame(rbind(stats_1[1,], stats_2[1,]))
meanprice <- as.data.frame(t(meanprice))

modelNames <- c("Signa HD 3.0T", "Selenia", "Symphony 1.5T", "Signa Excite 1.5T", "Harmony 1.0T",  "Intera 1.5T" )
colnames(meanprice) <- c("L3_Mean", "L1_Mean")

meanprice <- meanprice %>% mutate(L2_Upper = (L1_Mean - (0.1 * L1_Mean) - 34000))
meanprice <- meanprice %>% mutate(L2_Lower = (L3_Mean + (0.37 * L3_Mean)))
meanprice <- meanprice %>% mutate(L2_diff = L2_Upper - L2_Lower)
meanprice <- meanprice %>% mutate(L2_Mean = (L2_Upper + L2_Lower)/2)
meanprice <- meanprice %>% mutate(L1_L2_Delta = L1_Mean - L2_Mean)
meanprice <- meanprice %>% mutate(L1_L2_DiffPercent = round(((L1_Mean - L2_Mean)/L1_Mean) * 100, 2))
meanprice <- meanprice %>% mutate(L2_L3_Delta = L2_Mean - L3_Mean)
meanprice <- meanprice %>% mutate(L2_L3_DiffPercent = round(((L2_Mean - L3_Mean)/L2_Mean) * 100, 2))
meanprice <- meanprice %>% mutate(L1_L3_Delta = L1_Mean - L3_Mean)
meanprice <- meanprice %>% mutate(L1_L3_DiffPercent = round(((L1_Mean - L3_Mean)/L1_Mean) * 100, 2))

# meanprice$Avg_Percent_Change <- 'TBD'

# for(m in 1:nrow(meanprice)) {
#   meanprice$Avg_Percent_Change[m] <- round(geoMean(c(meanprice$L1_L2_DiffPercent[m], meanprice$L2_L3_DiffPercent[m])), 2)
# }

MRI_L1_L2_GeoMean <- geoMean(meanprice$L1_L2_DiffPercent)
MRI_L2_L3_GeoMean <- geoMean(meanprice$L2_L3_DiffPercent)
MRI_L1_L3_GeoMean <- geoMean(meanprice$L1_L3_DiffPercent)
CT_L1_L2_GeoMean <- 54.77
CT_L2_L3_GeoMean <- 51.46
CT_L1_L3_GeoMean <- 78.84

meanprice <- cbind(Model_Name = modelNames, meanprice)

#write.csv(meanprice, "C:\\My_Materials\\iDX\\work-folder\\CleanData\\MRI_Analysis.csv", row.names=FALSE)
lookupencode <- read_excel("C:\\My_Materials\\iDX\\work-folder\\LookupLibraries\\NamingConventionsEncode.xlsx")


newLookup <- merge(lookupencode, meanprice, by.x="System.Name", by.y="Model_Name", all.x = TRUE)

variables_to_keep <-   c("System.Name", "Modality.Name", "Variation 1", "Variation 2", "Variation 3", 
                         "System.OEM", "L1_L2_DiffPercent", "L2_L3_DiffPercent", "L1_L3_DiffPercent")

newLookup <- newLookup[variables_to_keep]


newLookup$L1_L2_DiffPercent[is.na(newLookup$L1_L2_DiffPercent) & newLookup$Modality.Name == "MRI"] <- MRI_L1_L2_GeoMean
newLookup$L2_L3_DiffPercent[is.na(newLookup$L2_L3_DiffPercent) & newLookup$Modality.Name == "MRI"] <- MRI_L2_L3_GeoMean
newLookup$L1_L3_DiffPercent[is.na(newLookup$L1_L3_DiffPercent) & newLookup$Modality.Name == "MRI"] <- MRI_L1_L3_GeoMean
newLookup$L1_L2_DiffPercent[is.na(newLookup$L1_L2_DiffPercent) & newLookup$Modality.Name == "CT"] <- CT_L1_L2_GeoMean
newLookup$L2_L3_DiffPercent[is.na(newLookup$L2_L3_DiffPercent) & newLookup$Modality.Name == "CT"] <- CT_L2_L3_GeoMean
newLookup$L1_L3_DiffPercent[is.na(newLookup$L1_L3_DiffPercent) & newLookup$Modality.Name == "CT"] <- CT_L1_L3_GeoMean

write.csv(newLookup, "C:\\My_Materials\\iDX\\work-folder\\LookupLibraries\\PricingLookup.csv", row.names=FALSE)