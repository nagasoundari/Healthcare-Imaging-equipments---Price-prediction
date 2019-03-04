library(readxl)
library(ggplot2)
library(kableExtra)

######################################### Plot results from clean data #############################################################

cleandataToPlot <- read.csv("./CleanData/MySQL_CleanData31-Jul-2018 12.00.csv")

################################################### MRI Machines #########################################################
#Select only the MRI Machines
MRIMachines <- cleandataToPlot %>% 
  filter(Category == "MRI") %>% 
  select(Category, Model, Title, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal,  
         YearManufactured, SourcePlatform, SourceURL)

CTMachines <- cleandataToPlot %>% 
  filter(Category == "CT") %>% 
  select(Category, Model, Title, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal,  
         YearManufactured, SourcePlatform, SourceURL)

UltraSoundMachines <- cleandataToPlot %>% 
  filter(Category == "Console" | Category == "Portable") %>% 
  select(Category, Model, Title, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal,  
         YearManufactured, SourcePlatform, SourceURL)


# converRate, VAT, Tariff, CurrencyID, ID, Make, SourceURL

#Select only the unique entries
MRIMachinesUnique <- MRIMachines[!duplicated(MRIMachines$SourceURL),]
CTMachinesUnique <- CTMachines[!duplicated(CTMachines$SourceURL),]
UltraSoundMachinesUnique <- UltraSoundMachines[!duplicated(UltraSoundMachines$SourceURL),]

##**************************************** Platformwise *********************************************##
MRIMachines_Platformwise <- MRIMachinesUnique %>% group_by(SourcePlatform) %>%
  summarise(entryCount = n(), machineCount = sum(Qty), Totalprice = sum(PriceConvertedUSD), 
            averagePrice = Totalprice/machineCount, minPrice = min(PriceConvertedUSD, na.rm = TRUE),
            maxPrice = max(PriceConvertedUSD, na.rm = TRUE))

##**************************************** ConditionWise ********************************************##
MRIMachines_Conditionwise <- MRIMachinesUnique %>% group_by(ConditionData) %>%
  summarise(entryCount = n(), machineCount = sum(Qty), Totalprice = sum(PriceConvertedUSD), 
            averagePrice = Totalprice/machineCount, minPrice = min(PriceConvertedUSD, na.rm = TRUE),
            maxPrice = max(PriceConvertedUSD, na.rm = TRUE))


##***************************************** OEMWise *************************************************##
MRIMachines_OEMwise <- MRIMachinesUnique %>% group_by(OEM) %>%
  summarise(entryCount = n(), machineCount = sum(Qty), Totalprice = sum(PriceConvertedUSD), 
            averagePrice = Totalprice/machineCount, minPrice = min(PriceConvertedUSD, na.rm = TRUE),
            maxPrice = max(PriceConvertedUSD, na.rm = TRUE))

##**************************************** Countrywise **********************************************##
MRIMachines_Countrywise <- MRIMachinesUnique %>% group_by(locationCountryFinal) %>%
  summarise(entryCount = n(), machineCount = sum(Qty), Totalprice = sum(PriceConvertedUSD), 
            averagePrice = Totalprice/machineCount, minPrice = min(PriceConvertedUSD, na.rm = TRUE),
            maxPrice = max(PriceConvertedUSD, na.rm = TRUE))



platformwiseMachineCount <- barplot(MRIMachines_Platformwise$machineCount, names.arg=MRIMachines_Platformwise$SourcePlatform, col="darkgreen", main = "Source Platform vs Machine Count",
        xlab = "Source Platform", cex.names=0.8, las = 2, ylab = "No. of Machines") 
platformwiseNetPrice <- barplot(MRIMachines_Platformwise$Totalprice, names.arg=MRIMachines_Platformwise$SourcePlatform, col="blue", main = "Source Platform vs Net Value",
        xlab = "Source Platform", cex.names=0.8, las = 2, ylab = "Total price of machines")


plot(MRIMachines_Platformwise$maxPrice, type = "o", xaxt = "n", col = 'red', 
     xlab = "Source Platform", ylab = "Maximum Price", main = "Source Platform vs Maximum Price")
#lines(MRIMachines_Platformwise$maxPrice, type = "o", col = "blue")
axis(1, at = 1:nrow(MRIMachines_Platformwise), labels = MRIMachines_Platformwise$SourcePlatform)



ggplot(MRIMachinesUnique, aes(x = SourcePlatform, y = Qty, fill = ConditionData)) + 
  geom_bar(stat="identity", position = "stack")

ggplot(MRIMachinesUnique, aes(x = SourcePlatform, y = Qty, fill = OEM)) + 
  geom_bar(stat="identity", position = "stack")

ggplot(cleandataToPlot, aes(x = SourcePlatform, y = Qty, fill = Category)) + 
  geom_bar(stat="identity", position = "stack")


#Filter only CT, MRI and Ultrasound devices

filteredDevices <- cleandataToPlot %>% filter(Category == "CT" | Category == "MRI")

ggplot(filteredDevices, aes(x = SourcePlatform, y = Qty, fill = Category)) + 
  geom_bar(stat="identity", position = "stack") + 
  scale_fill_manual("legend", values = c("CT" = "darkgreen", "MRI" = "royalblue3"))

