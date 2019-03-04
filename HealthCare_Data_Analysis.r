library(readxl)
library(ggplot2)
library(kableExtra)

######################################### Plot results from clean data #############################################################

cleandataToPlot <- read.csv("C:\\My_Materials\\iDX\\work-folder\\CleanData\\MySQL_CleanData08-Aug-2018-JuneJuly.csv")

# new <- cleandataToPlot %>% select(Category, Model, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal,  
#                            YearManufactured, SourcePlatform, SourceURL)
################################################### MRI Machines #########################################################
#Select only the MRI Machines
MRIMachines <- cleandataToPlot %>% 
  filter(Category == "MRI") %>% 
  select(Category, Model, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal,  
          YearManufactured, SourcePlatform, SourceURL)

# converRate, VAT, Tariff, CurrencyID, ID, Make, SourceURL

#Select only the unique entries
MRIMachinesUnique <- MRIMachines[!duplicated(MRIMachines$SourceURL),]

# new <- new[!duplicated(new$SourceURL),]

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



# ggplot(MRIMachines_Platformwise, aes(x=SourcePlatform, y=machineCount)) + geom_bar(stat=identity())
# 
# dev.off()
# 
# ggplot(MRIMachines_Platformwise, aes(x=SourcePlatform, y=machineCount, fill = Totalprice)) + 
#   geom_bar(stat = "identity", position = "dodge")




# colbind <- cbind(MRIMachines_Platformwise$machineCount, MRIMachines_Platformwise$Totalprice)
# rowbind <- rbind(MRIMachines_Platformwise$machineCount, MRIMachines_Platformwise$Totalprice)
# 
# barplot(rowbind, beside=T, col=c("darkblue","red"))
# barplot(counts, main="Car Distribution by Gears and VS",
#         xlab="Number of Gears", col=c("darkblue","red"))







##*************************** ConditionWise ****************************##

MRIMachines_alibaba <- MRIMachinesUnique %>% filter(SourcePlatform == "alibaba.com") %>% group_by(ConditionData) %>%
                      summarise(Count = n(), PriceSum = sum(PriceConvertedUSD), QtySum = sum(Qty), 
                      averagePrice = PriceSum/QtySum, minPrice = min(PriceConvertedUSD, na.rm = TRUE),
                      maxPrice = max(PriceConvertedUSD, na.rm = TRUE))
  
MRIMachines_medwow <- MRIMachinesUnique %>% filter(SourcePlatform == "medwow.com") %>% group_by(ConditionData) %>%
  summarise(Count = n(), PriceSum = sum(PriceConvertedUSD), QtySum = sum(Qty), 
            averagePrice = PriceSum/QtySum, minPrice = min(PriceConvertedUSD, na.rm = TRUE),
            maxPrice = max(PriceConvertedUSD, na.rm = TRUE))





MRIMachines_conditionwise <- MRIMachines %>% group_by(ConditionData) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n(), Average = PriceSum/Count) 

# kable(MRIMachines_conditionwise) %>%
#   kable_styling()




########################################## Category vs Price ##############################################################################
modalityfiltered <- cleandataToPlot %>% 
  filter(Category == "CT" | Category == "Console" | Category == "Portable" | Category == "MRI") %>% 
  select(Category, Model, PriceConvertedUSD)

equipmentsCount_Categorywise <- modalityfiltered %>% group_by(Category) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n()) 

equipmentsCount_Categorywise$Average <- equipmentsCount_Categorywise$PriceSum/equipmentsCount_Categorywise$Count


PriceModalityWise <- ggplot(data=equipmentsCount_Categorywise, aes(x = Category, y = Average)) +
  geom_bar(stat = "identity", fill = "orange") + 
  geom_text(aes(label=round(Average, digits = 0)), vjust=-0.3, size=3.5) +
  theme_minimal() + labs(title="Category vs Average Price",  x ="Category", y = "Avg Price") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



CountModalityWise <- ggplot(data=equipmentsCount_Categorywise, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "orange") + 
  geom_text(aes(label=Count), vjust=-0.3, size=3.5) +
  theme_minimal() + labs(title="Category vs Count",  x ="Category", y = "Count") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



################################################### CT Machines #########################################################

CTMachines <- cleandataToPlot %>% 
  filter(Category == "CT") %>% 
  select(Category, Model, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal)


CTMachines_conditionwise <- CTMachines %>% group_by(ConditionData) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n(), Average = PriceSum/Count) 

CTMachines_OEMwise <- CTMachines %>% group_by(OEM) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n(), Average = PriceSum/Count) 


################################################### Ultrasound Machines #########################################################
################  Console  ######################

UltraSoundMachinesConsole <- cleandataToPlot %>% 
  filter(Category == "Console") %>% 
  select(Category, Model, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal)


UltraSoundMachinesConsole_conditionwise <- UltraSoundMachinesConsole %>% group_by(ConditionData) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n(), Average = PriceSum/Count) 

UltraSoundMachinesConsole_OEMwise <- UltraSoundMachinesConsole %>% group_by(OEM) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n(), Average = PriceSum/Count) 


################  Portable  ######################

UltraSoundMachinesPortable <- cleandataToPlot %>% 
  filter(Category == "Portable") %>% 
  select(Category, Model, PriceConvertedUSD, ConditionData, Qty, OEM, locationCountryFinal)


UltraSoundMachinesPortable_conditionwise <- UltraSoundMachinesPortable %>% group_by(ConditionData) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n(), Average = PriceSum/Count) 

UltraSoundMachinesPortable_OEMwise <- UltraSoundMachinesPortable %>% group_by(OEM) %>%
  summarise(PriceSum = sum(PriceConvertedUSD), Count = n(), Average = PriceSum/Count)

########################################## Top 10 countries vs No of machines ##############################################################################
equipmentsCount_countrywise <- cleandataToPlot %>%
  group_by(locationCountryFinal) %>% 
  summarise(qtySum = sum(Qty)) %>% arrange(desc(qtySum)) 

# Create the chart (Countries on X axis, No of machines on Y axis)
File <- "./charts/CountriesvsMachines.png"
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

png(File, width = 800, height = 800, units = "px")

Top_10_countries_max_machines <- ggplot(data=head(equipmentsCount_countrywise, 15), aes(x = locationCountryFinal, y = qtySum)) +
  geom_bar(stat = "identity", fill = "Steelblue") + geom_text(aes(label=qtySum), vjust=-0.3, size=3.5)+
  theme_minimal() + labs(title="Top 15 countries vs No of machines",  x ="Countries", y = "No of Machines") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Top_10_countries_max_machines

# Save chart to the `charts/` folder
dev.off()


############################################ OEM's vs No. of machines ############################################################################
equipmentsCount_OEMwise <- cleandataToPlot %>%
  group_by(OEM) %>%
  summarise(qtySum = sum(Qty)) %>% arrange(desc(qtySum))

# # Create the chart (OEMs on X axis, No of machines on Y axis)
# File <- "./charts/OEMvsMachines.png"
# if (file.exists(File)) stop(File, " already exists")
# dir.create(dirname(File), showWarnings = FALSE)
# 
# png(File, width = 720, height = 720, units = "px")

OEM_Machine_count <- ggplot(data=equipmentsCount_OEMwise, aes(x = OEM, y = qtySum)) +
  geom_bar(stat = "identity", fill = "maroon4") + geom_text(aes(label=qtySum), vjust=-0.3, size=3.5)+
  theme_minimal() + labs(title="OEM's vs No. of machines",  x ="OEM", y = "No of Machines") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# OEM_Machine_count
# 
# # Save chart to the `charts/` folder
# dev.off()

############################################# Top 10 countries vs No of machines vs OEM #########################################################################

equipmentsCount_countryAndOEM <- cleandataToPlot %>%
  group_by(OEM, locationCountryFinal) %>% 
  summarise(qtySum = sum(Qty)) %>% arrange(desc(qtySum)) 

dfReduced <- subset(equipmentsCount_countryAndOEM, locationCountryFinal %in% head(equipmentsCount_countrywise$locationCountryFinal, 10))
#Top_10_countries_machines_OEMs_data <- subset(equipmentsCount_countryAndOEM, equipmentsCount_countryAndOEM$locationCountryFinal %in% equipmentsCount_countrywise$locationCountryFinal)

# Create the chart (Top 10 countries on X axis, No of machines on Y axis)
File <- "./charts/CountriesvsMachinesvsOEMs.png"
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

png(File, width = 1200, height = 800, units = "px")

Top_10_countries_machines_OEMs <- ggplot(data=dfReduced, aes(x = locationCountryFinal, y = qtySum, fill=OEM)) +
  geom_bar(stat = "identity") + geom_text(aes(label=qtySum), vjust=-0.3, size=3.5)+
  theme_minimal() + labs(title="Top 10 countries vs No of machines",  x ="Countries", y = "No of Machines") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Top_10_countries_machines_OEMs

# Save chart to the `charts/` folder
dev.off()

############################################### Source Platform wise #######################################################

equipmentsCount_sourcePlatformwise <- cleandataToPlot %>%
  group_by(SourcePlatform) %>% 
  summarise(qtySum = sum(Qty)) %>% arrange(desc(qtySum)) 

equipmentsPrice_sourcePlatformwise <- cleandataToPlot %>%
  group_by(SourcePlatform) %>% 
  summarise(PriceSum = sum(PriceConvertedUSD)) %>% arrange(desc(PriceSum)) 

# # Create the chart (Source platform on X axis, No of machines on Y axis)
# File <- "./charts/sourceplatformvsnoofmachines.png"
# if (file.exists(File)) stop(File, " already exists")
# dir.create(dirname(File), showWarnings = FALSE)
# 
# png(File, width = 1200, height = 800, units = "px")

plot1 <- ggplot(data=equipmentsCount_sourcePlatformwise, aes(x = SourcePlatform, y = qtySum)) +
  geom_bar(stat = "identity", fill = "maroon4") + geom_text(aes(label=qtySum), vjust = -0.3, size=3.5)+
  theme_minimal() + labs(title="Source Platform vs No. of machines",  x ="Source Platform", y = "No of Machines") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# # Save chart to the `charts/` folder
# dev.off()


# # Create the chart (Source platform on X axis, Value of machines on Y axis)
# File <- "./charts/sourceplatformvsvalofmachines.png"
# if (file.exists(File)) stop(File, " already exists")
# dir.create(dirname(File), showWarnings = FALSE)
# 
# png(File, width = 1200, height = 800, units = "px")

plot2 <- ggplot(data=equipmentsPrice_sourcePlatformwise, aes(x = SourcePlatform, y = PriceSum)) +
  geom_bar(stat = "identity", fill = "darkgreen") + 
  geom_text(aes(label=round(PriceSum, digits = 0)), vjust=-0.3, size=2.5)+
  theme_minimal() + labs(title="Source Platform vs Machine's value",  x ="Source Platform", y = "Machine's value") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# # Save chart to the `charts/` folder
# dev.off()

equipmentsPriceCount_sourcePlatformwise <- merge(equipmentsCount_sourcePlatformwise, equipmentsPrice_sourcePlatformwise, by = "SourcePlatform")
equipmentsPriceCount_sourcePlatformwise$AverageRate <- equipmentsPriceCount_sourcePlatformwise$PriceSum/equipmentsPriceCount_sourcePlatformwise$qtySum

plot3 <- ggplot(data=equipmentsPriceCount_sourcePlatformwise, aes(x = SourcePlatform, y = AverageRate)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title="Source Platform vs Average rate",  x ="Source Platform", y = "Average rate") +
  geom_text(aes(label=round(AverageRate, digits = 0)), vjust=-0.3, size=2.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
# counts <- table(equipmentsPriceCount_sourcePlatformwise$SourcePlatform, equipmentsPriceCount_sourcePlatformwise$PriceSum)
# barplot(counts, main="Distribution by Gears and VS",
#         xlab="Number of Gears", col=c("darkblue","red"),
#         legend = rownames(counts), beside=TRUE)
# ggplot(data=equipmentsPriceCount_sourcePlatformwise, aes(x = c(qtySum, PriceSum), y = SourcePlatform)) +
#   geom_bar(stat = "identity") 



############################ Correlation test ##########################################


findCorrelation <- function(columnToTest) {
  #print(columnToTest)
  corval <- cor.test(cleandataToPlot$PriceConvertedUSD, cleandataToPlot$columnToTest)
  print(corval)
}

findCorrelation(OEM)

columnsToCorrelate <- c(OEM, Category, ConditionData, CurrencyID, ModelYear, SellerNumberOfTransactions, 
                        SellerRating, SourcePlatform, Model, noOfViews, locationCountryFinal)

corval <- cor.test(cleandataToPlot$PriceConvertedUSD, cleandataToPlot$CurrencyID)



################################################# Outliers #######################################################################

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}



outlierKD(CTMachines, CTMachines$PriceConvertedUSD)


################################################################################################################
dataset <- read.csv("./RawData/RawData_07312018_june.csv", nrows=20000)

##Replace blank values with TBD
is.na(dataset) <- dataset =='TBD'

#Coalesce the columns that has similar values
dataset$CollectedPrice <- unlist(lapply(dataset$price, coalesce_column_values, dataset$reg_price))
dataset$CollectedOEM <- unlist(lapply(dataset$manufacturer, coalesce_column_values, dataset$brand))

##rename columns
colnames(dataset) <- c("X", "ID",	"CollectDTG",	"ConditionData",	"Description",	"Price",
                       "RegisteredPrice", "CurrencyID", "SourcePlatform",	"SourceURL",	"CollectedTitle",	
                       "Category", "LocationCity", "LocationCountry",	"LocationSpecific", "LocationState",
                       "SerialNumber",	"Brand", "CollectedModel", "Qty", "SellerAddress", "SellerName",
                       "ShipsTo",	"ItemUserViewsData",	"Make",	"Manufacturer", "YearManufactured",
                       "Notes",	"SellerNumberOfTransactions", "Type", "CollectedPrice", "CollectedOEM")

##Trim out non-data records from the dataset which are
##X, Price, RegisteredPrice, Brand,  Manufacturer

variables_to_keep <-   c("ID",	"CollectDTG",	"ConditionData",	"Description", "CurrencyID", 
                         "SourcePlatform", "SourceURL",	"CollectedTitle",	"Category", "LocationCity", 
                         "LocationCountry", "LocationSpecific", "LocationState", "SerialNumber",	
                         "CollectedModel", "Qty", "SellerAddress", "SellerName", "ShipsTo",	
                         "ItemUserViewsData", "Make", "YearManufactured", "Notes", "SellerNumberOfTransactions",	"Type", 
                         "CollectedPrice", "CollectedOEM")

dataset_new <- dataset[variables_to_keep]
mydftest <- dataset_new



system.time({
  outValue1 <- list()
  outValue2 <- list()
  getNoOfTransactionsBySeller <- function(inputData) {
    # for (i in 1:nrow(inputData)) {
      if((length(inputData) != 0)) {
        outValue1 <- extractFirstNumberInString(inputData)
        outValue2 <- str_extract(inputData, "(?i)(?<=past\\D)\\d+(?= months?)")
      }
      else {
        outValue1 <- NA
        outValue2 <- NA
      }
    #}
    return(list(outValue1, outValue2))
  }
  
  out <- lapply(temp$SellerNumberOfTransactions, getNoOfTransactionsBySeller)
  temp$noOfTransactions <- outValue1
  temp$transactionMonths <- outValue2
})

mydftest$Model[grepl(namingEncodeData$System.Name[i]|namingEncodeData$`Variation 1`[i]|
                       namingEncodeData$`Variation 2`[i]|namingEncodeData$`Variation 3`[i], 
                     mydftest$MOSD, ignore.case = TRUE)] <- namingEncodeData$System.Name[i]



  encodeModelNames <- function(namingEncodeData) {
    print(nrow(namingEncodeData))
    for(i in 1:nrow(namingEncodeData)) {
      namingencode_string <- paste('mydftest$Model[grepl("', namingEncodeData$System.Name[i])
      if(!is.na(namingEncodeData$`Variation 1`[i])) {
        namingencode_string <- paste(namingencode_string, "|", namingEncodeData$`Variation 1`[i])
      }
      if(!is.na(namingEncodeData$`Variation 2`[i])) {
        namingencode_string <- paste(namingencode_string, "|", namingEncodeData$`Variation 2`[i])
      }
      if(!is.na(namingEncodeData$`Variation 3`[i])) {
        namingencode_string <- paste(namingencode_string, "|", namingEncodeData$`Variation 3`[i])
      }
      namingencode_string <- paste(namingencode_string, '", mydftest$MOSD, ignore.case=TRUE)] <- "', namingEncodeData$System.Name[i], '"', sep = "")
      eval(parse(text=namingencode_string))
    }
    mydftest
  }
  
  
  encodeModelNames_sysName <- function(namingEncodeSystem) {
    for(i in 1:nrow(namingEncodeSystem)) {
      namingencode_string <- paste('mydftest$Model[grepl("', namingEncodeSystem$System.Name[i])
      if(!is.na(namingEncodeSystem$`Variation 1`[i])) {
        namingencode_string <- paste(namingencode_string, "|", namingEncodeSystem$`Variation 1`[i])
      }
      namingencode_string <- paste(namingencode_string, '", mydftest$MOSD, ignore.case=TRUE)] <- "', namingEncodeSystem$System.Name[i], '"', sep = "")
      eval(parse(text=namingencode_string))
    }
    mydftest
  }
  
  encodeModelNames_var1 <- function(namingEncodeSystem) {
    for(i in 1:nrow(namingEncodeSystem)) {
      if(!is.na(namingEncodeSystem$`Variation 1`[i])) {
        mydftest$Model[grepl(namingEncodeSystem$`Variation 1`[i], mydftest$MOSD, ignore.case = TRUE)] <- namingEncodeSystem$System.Name[i]
      }
    }
    mydftest
  }
  
  encodeModelNames_var2 <- function(namingEncodeSystem) {
    for(i in 1:nrow(namingEncodeSystem)) {
      if(!is.na(namingEncodeSystem$`Variation 2`[i])) {
        mydftest$Model[grepl(namingEncodeSystem$`Variation 2`[i], mydftest$MOSD, ignore.case = TRUE)] <- namingEncodeSystem$System.Name[i]
      }
    }
    mydftest
  }
  
  encodeModelNames_var3 <- function(namingEncodeSystem) {
    for(i in 1:nrow(namingEncodeSystem)) {
      if(!is.na(namingEncodeSystem$`Variation 3`[i])) {
        mydftest$Model[grepl(namingEncodeSystem$`Variation 3`[i], mydftest$MOSD, ignore.case = TRUE)] <- namingEncodeSystem$System.Name[i]
      }
    }
    mydftest
  }
  
  
  system.time({
    mydftest <- apply(lookupDF$NamingConventionsEncode$`System.Name`, 1, encodeModelNames_sysName)
  })

###################################################################################################

PreMOSD <- paste(mydftest$Category,mydftest$CollectedOEM,mydftest$CollectedModel,mydftest$CollectedTitle,mydftest$Description,mydftest$Notes)
  
#Assign it to the existing field
mydftest$MOSD <- PreMOSD
mydftest$Model <- 'TBD'


########################################## Top 10 countries vs No of machines - GE ##############################################################################
equipmentsCount_countrywise_GE <- cleandataToPlot %>% filter(OEM == "GE") %>%
  group_by(locationCountryFinal) %>% 
  summarise(qtySum = sum(Qty)) %>% arrange(desc(qtySum)) 

# Create the chart (Countries on X axis, No of machines on Y axis)
File <- "./charts/CountriesvsMachines-GE.png"
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)

png(File, width = 800, height = 800, units = "px")

Top_10_countries_max_machines_GE <- ggplot(data=head(equipmentsCount_countrywise_GE, 15), aes(x = locationCountryFinal, y = qtySum)) +
  geom_bar(stat = "identity", fill = "Darkgreen") + geom_text(aes(label=qtySum), vjust=-0.3, size=3.5)+
  theme_minimal() + labs(title="Top 15 countries vs No of machines - GE",  x ="Countries", y = "No of Machines") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Top_10_countries_max_machines_GE

# Save chart to the `charts/` folder
dev.off()

###############################################################################################################

View(cleandataToPlot)
cleandataToPlot_relevant <- cleandataToPlot %>% filter(Category == 'MRI' | Category == 'CT')

cleandataToPlot_Modelwise <- cleandataToPlot_relevant %>% group_by(Model) %>%  
  summarise(MeanPrice = mean(PriceConvertedUSD), StdDevPrice = sd(PriceConvertedUSD),
                MaxPrice = max(PriceConvertedUSD), MinPrice = min(PriceConvertedUSD)) %>%
select(Model, MeanPrice, MaxPrice, MinPrice, StdDevPrice) 

DataWithPricing <- merge(cleandataToPlot_relevant, cleandataToPlot_Modelwise, by = 'Model', all.x = TRUE)
DataWithPricing <- DataWithPricing %>% mutate(PriceCategory = if_else (PriceConvertedUSD > MaxPrice, 'High',
                                                    if_else(PriceConvertedUSD < MaxPrice & PriceConvertedUSD > MeanPrice, 'Above Mean',
                                                            if_else(PriceConvertedUSD > MinPrice & PriceConvertedUSD < MeanPrice, 'Below Mean', 'Low'))))


x <- DataWithPricing %>% group_by(Model, PriceCategory) %>% summarise(c = n())
y <- spread(x, PriceCategory, c)


write.csv(DataWithPricing, "C:\\My_Materials\\iDX\\work-folder\\CleanDataWithPricingCategory.csv")

###############################################################################################################

cleanData <- read.csv("C:\\My_Materials\\iDX\\work-folder\\CleanData\\MySQL_CleanData08-Aug-2018-JuneJuly.csv")
priceLookup <- read.csv("C:\\My_Materials\\iDX\\work-folder\\LookupLibraries\\PricingLookup.csv")
salesData <- read.csv("C:\\My_Materials\\iDX\\work-folder\\CleanData\\cleanSalesData.csv")

cleanData_relevant <- cleanData %>% filter(Category == 'MRI' | Category == 'CT')

salesData_relevant <- salesData %>% filter(Category == 'MRI' | Category == 'CT')
saleData_Modelwise <- salesData_relevant %>% group_by(Model) %>% 
  summarise(meanPrice = mean(Price), StdDevPrice = sd(Price), MaxPrice = max(Price), MinPrice = min(Price))


MRI_Machines <- salesData_relevant %>% filter(Category == 'MRI') 
MRI_MaxPrice <- max(MRI_Machines$Price)



scrapeSalesDataWthPricing <- merge(cleanData_relevant, saleData_Modelwise, by = 'Model', all.x = TRUE)


scrapeSalesDataWthPricing$meanPrice[is.na(scrapeSalesDataWthPricing$meanPrice) & scrapeSalesDataWthPricing$Category == 'MRI'] <-  MRI_MeanPrice
scrapeSalesDataWthPricing$MaxPrice[is.na(scrapeSalesDataWthPricing$MaxPrice) & scrapeSalesDataWthPricing$Category == 'MRI'] <-  MRI_MaxPrice
scrapeSalesDataWthPricing$MinPrice[is.na(scrapeSalesDataWthPricing$MinPrice) & scrapeSalesDataWthPricing$Category == 'MRI'] <-  MRI_MinPrice

MRI_MinPrice <- min(MRI_Machines$Price)
MRI_MeanPrice <- mean(MRI_Machines$Price)

CT_Machines <- salesData_relevant %>% filter(Category == 'CT') 
CT_MaxPrice <- max(CT_Machines$Price)
CT_MinPrice <- min(CT_Machines$Price)
CT_MeanPrice <- mean(CT_Machines$Price)
scrapeSalesDataWthPricing$meanPrice[is.na(scrapeSalesDataWthPricing$meanPrice) & scrapeSalesDataWthPricing$Category == 'CT'] <-  CT_MeanPrice
scrapeSalesDataWthPricing$MaxPrice[is.na(scrapeSalesDataWthPricing$MaxPrice) & scrapeSalesDataWthPricing$Category == 'CT'] <-  CT_MaxPrice
scrapeSalesDataWthPricing$MinPrice[is.na(scrapeSalesDataWthPricing$MinPrice) & scrapeSalesDataWthPricing$Category == 'CT'] <-  CT_MinPrice


scrapeSalesDataWthPricing <- scrapeSalesDataWthPricing %>% mutate(PriceCategory = if_else (PriceConvertedUSD > MaxPrice, 'High',
                                                                       if_else(PriceConvertedUSD < MaxPrice & PriceConvertedUSD > meanPrice, 'Above Mean',
                                                                               if_else(PriceConvertedUSD > MinPrice & PriceConvertedUSD < meanPrice, 'Below Mean', 'Low'))))


write.csv(scrapeSalesDataWthPricing, "C:\\My_Materials\\iDX\\work-folder\\scrapeDataPricingRangeOnSaleData.csv")
