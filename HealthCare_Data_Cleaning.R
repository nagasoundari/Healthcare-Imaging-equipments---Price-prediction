## R script name: MySQL MED PrepRemoveCleanSave with GeoRef.r
## Written by:   Aaron Tyler, April 2018
## Updated: Naga Soundari Balamurugan, Jayashree Raman, June 2018
##
## Description:   Script to open, aggregate, modify, parse, clean, adjust FX/VAT/Tariffs then
##                split and save data from numerous global sources.
#############################################################################################

## Get script start time
start_sess <- Sys.time()

source('./RScripts/library.R') #Source in the library.R file that has all the functions required

listOfPackagesRequired <- c('quantmod', 
                            'reshape2', 'lubridate',
                            'plyr', 'ggplot2', 
                            'stringr', 'dplyr',
                            'tidyr', 'readxl', 'RMySQL')

usepackages(listOfPackagesRequired)

## Get a list of csv files and lookup libraries in the datasets folder for medical		
file_list <- list.files(path = './RawData', pattern = '.csv')
lookup_file_list <- list.files(path = "./LookupLibraries")

#Create a dataset to read all the raw data 
dataset <- NULL
#create a list to store all the lookup libraries
lookupDF <- vector("list",length(lookup_file_list)) 

#Create a dataframe to read the lookup library names and extensions
lookupLibraries <- data.frame(names = 1:length(lookup_file_list), ext = 1:length(lookup_file_list))
for (i in 1:length(lookup_file_list)) {
  lookupLibraries$names[i] <- gsub("\\..*","",lookup_file_list[i])
  lookupLibraries$ext[i] <- gsub(".*\\.", "", lookup_file_list[i])
}


#Function call: to merge all the raw data files
dataset <- readRawDataFiles(file_list)

#Function call: to read all the lookup libraries
lookupDF <- readLookUpLibraries(lookupLibraries)

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
                       "Notes",	"SellerNumberOfTransactions", "Type", "SourceFilename", 
                       "CollectedPrice", "CollectedOEM")

##Trim out non-data records from the dataset which are
##X, Price, RegisteredPrice, Brand,  Manufacturer

variables_to_keep <-   c("ID",	"CollectDTG",	"ConditionData",	"Description", "CurrencyID", 
                         "SourcePlatform", "SourceURL",	"CollectedTitle",	"Category", "LocationCity", 
                         "LocationCountry", "LocationSpecific", "LocationState", "SerialNumber",	
                         "CollectedModel", "Qty", "SellerAddress", "SellerName", "ShipsTo",	
                         "ItemUserViewsData", "Make", "YearManufactured", "Notes", "SellerNumberOfTransactions",	"Type", 
                         "SourceFilename", "CollectedPrice", "CollectedOEM")

dataset_new <- dataset[variables_to_keep]


##Remove row where Model or Price is NA (columns #2 and 7, change if template for manual data changes)
##Using column name so that code is automatically taken care of without manual changes

df <- dataset_new[!is.na(dataset_new$CollectedPrice),]
#df <- dataset_new[!is.na(dataset_new$CollectedModel),]


#Remove the unneeded df
rm(variables_to_keep)
rm(dataset)
rm(file_list)
#rm(dataset_new)


##Remove NA columns
#df[sapply(df, function(x) all(is.na(x)))] <- NULL


##Get rid off duplicates first, take only unique entries
df <- unique(df[ ,1:length(df)])


##Create other columns to standardize setup
df$MOSD <- 'TBD'

##Impute known field data for applicable fields:
#For OEM, clear the cell for future use.  The inputs are the same in collected OEM
df$OEM <- 'TBD'

#Create a searchable combined field for Model OEM Searchable Data
#Create a combined df by pasting the data into it from numerous sources
PreMOSD <- paste(df$Category,df$CollectedOEM,df$CollectedModel,df$CollectedTitle,df$Description,df$Notes)

#Assign it to the existing field
df$MOSD <- PreMOSD

#Remove the unneeded df
rm(PreMOSD)

#Remove NULL from the field as this is not data and each field is represented elsewhere.
df$MOSD <- gsub("NULL", "", df$MOSD)

#Extracts only the numeric values in the field
df$Qty <- str_extract(df$Qty, "\\-*\\d+\\.*\\d*")

##Impute quantity available: set qty = 1 if not otherwise filled.
#For the quantity, if it is null, re-write as 1 (it can't be collected if there are less than 1)
df$Qty <- gsub("NULL|NA|N/A|null|na","1", df$Qty)

#Replace the rows with NA as 1
df$Qty[is.na(df$Qty)] <- '1'

#Trim out unnecessary spaces and characters
trimQty <- function (x) gsub("^\\s+|\\s+$|\\$|\\,|\\+", "", x)
df$Qty <- trimQty(df$Qty)

#Treat Qty as a numeric vice character field
df$Qty <- as.numeric(df$Qty)

#For the quantity, if it is zero, re-write as 1 (it can't be collected if there are none).
#Copy Qty to a new df to work on it
QtyMod <- df$Qty

#If it is numerically zero, replace it with the text NA (this will change the field back to character)
QtyMod[QtyMod==0] <- "NA"
#If it is NA, change it to 1
QtyMod <- gsub("NA", "1", QtyMod)

#Move it back to the Qty field in the original df whilst changing it back to numeric
df$Qty <- as.numeric(QtyMod)

#Remove the unneeded df
rm(QtyMod)

##Other Admin items related to table setup and operation
##make date column from a specific measure
df$Date <- as_date(df$CollectDTG, '%y%m%d')

##Change price field type from char to number
df$CollectedPrice <- gsub("[^0-9\\.]", "", df$CollectedPrice)
df$CollectedPrice <- as.numeric(df$CollectedPrice)
df <- subset(df, !is.na(CollectedPrice))

##Save a df for reference
df2 <- df

##Remove things with very low prices as they are probably not imaging equipment
##cut rows with a quantity of only one and a price of less than $20 
df <- df[grepl("1", df$Qty) & !df$CollectedPrice < 20, ] 

#save duplicate just in case you need it later
df3 <- df

#Paste the items in NonImaging list into a search field with a pike between items IOT facilitate multi-spectrum searching
ITI <- paste(lookupDF$NonImagingItems$NonImagingItems, collapse = "|")

##delete items that should not be in the title of desired data lines by using this multi-spectrum created field
#Example: df <- df[!grepl("TEMPERED GLASS SCREEN PROTECTOR|Mobile Phone Holder Stand|Charging Cable|TEMPERED GLASS FILM SCREEN PROTECTOR", df$ItemTitle,ignore.case=TRUE), ]
df <- df[!grepl(ITI, df$MOSD, ignore.case = TRUE), ]

#save duplicate just in case you need it later
df4 <- df

##Cleaning OEMs
#function call to clean the OEM: Replace OEM with correct spelling
df <- cleanOEM(OEM_list)

#Save Another Copy
df5 <- df

##Create the model column in dataset 
df$Model <- 'TBD'

#Function call: to encode the model names to model column
df <- encodeModelNames(lookupDF$NamingConventionsEncode)

#And save another copy
df6 <- df

#Function call: to encode the model numbers to model column using the partsnamingencode lookup library
df <- encodeModelNumbers(lookupDF$PartsNamingEncode)


#and save another DF
df7 <- df

#Sort out what category the listing is for (Parts, MRI, CT, etc)
#First empty the category field to TBD
df$Category <- 'TBD'

#Subsetting the namingencode to bind it with the modalitycompilation lookup library
#so that matching can be done for all models
variables_to_keep_category <-   c("System.Name", "Modality.Name", "System.OEM")
namingencode_category <- lookupDF$NamingConventionsEncode[variables_to_keep_category]
Categorization <- rbind(lookupDF$ModalityCompilation, namingencode_category)

#Find matches in the models between the dataframe and the modality file, Insert the modality from the modality file
#into the dataframe under category.
df$Category <- Categorization$Modality.Name[match(df$Model, Categorization$System.Name)]

#Then refine the OEMs using the same process.  This only applies to lines with known models
df$OEM <- Categorization$System.OEM[match(df$Model, Categorization$System.Name)]

#Remove the unneeded df
rm(variables_to_keep_category)
rm(namingencode_category)


######################################### FX ############################################
##before running the code, explore df, see what currencies you are dealing with
df$CurrencyID <- trim(df$CurrencyID)

#Convert Currencies to Standard Format
df$CurrencyID <- recode(df$CurrencyID, "Rs"="INR")
df$CurrencyID <- recode(df$CurrencyID, "Yen"="JPY")
df$CurrencyID <- recode(df$CurrencyID, "for"="USD")
df$CurrencyID <- recode(df$CurrencyID, "$"="USD")
df$CurrencyID <- recode(df$CurrencyID, "EURO"="EUR")
df$CurrencyID <- recode(df$CurrencyID, "???"="EUR")
df$CurrencyID <- recode(df$CurrencyID, "£"="GBP")
df$CurrencyID <- recode(df$CurrencyID, "R"="ZAR")
df$CurrencyID <- recode(df$CurrencyID, "JYP"="JPY")

df <- df %>% mutate(CurrencyID=ifelse(CurrencyID=="", "USD", CurrencyID))

#Remove irrelevant currencies
currList <- unique(df$CurrencyID)
currList <- currList[currList != ""] 

##create currency pair combinations i.e. EUR/GBP, USD/GBP 
currCombinations = paste0(union(unique(currList),"USD"),"USD", "=X")
currCombinations_noSep = paste0(union(unique(currList),"USD"),"USD")


##############################################################################
ReqdDate <- c()
for (i in 1:nrow(df)) {
  ReqdDate[i] <- strsplit(df$CollectDTG[i], " ")[[1]][1]
}
df <- data.frame(df, ReqdDate)


###################### Get List of All Exchange Rates  #####################
#################################################################

#Creates a list of different currencies from data
xtsobjlist <- vector("list", length(currCombinations_noSep))
  for (i in 1:length(currCombinations)){
    if(currCombinations[i] != 'USDUSD=X') {
      #TryCatch block to capture the invalid currency symbols
      xtsobjlist[[i]] <- tryCatch(getSymbols(currCombinations[i], auto.assign = FALSE), error=function(e) "Invalid Currency Combination")
    }
    names(xtsobjlist)[i] <- currCombinations_noSep[i]
  }

#Removes the USDUSD from the currency combination list(xtsobjlist) as the conversion rate is 1 for USD
xtsobjlist[['USDUSD']] <- NULL

#Create a list to capture the invalid currency combinations
invalidCurrList <- list()

#Loops through the xtsobjlist to split out the invalid currency combinations using the trycatch error
for (i in 1:length(xtsobjlist)){
  if(xtsobjlist[[i]]=='Invalid Currency Combination'){
      invalidCurrList[i] <- names(xtsobjlist[i])
    }
  }
  
#Removes the  invalid currency combinations from xtsobjlist by comparing it with invalidCurrList
xtsobjlist[which(names(xtsobjlist) %in% invalidCurrList)] <- NULL

#Repeat the same as currencies for currency ID as well
invalidCurrIDList <- gsub("USD", "", invalidCurrList) 
df_invalidCurrencies <- df %>% filter(CurrencyID %in% invalidCurrIDList)
df <- df[!(df$CurrencyID %in% df_invalidCurrencies$CurrencyID),]


converRate <- c()
for (i in 1:nrow(df)) {
  if(df$CurrencyID[i]!=''){
    converRate[i] <- getConversionRate(df$CurrencyID[i], df$ReqdDate[i])
  }
}

df <- data.frame(df, converRate)
df8 <- df

########################### Clean Out the ConditionData Column ########################################################

for (i in 1:length(deviceConditionList)) {
  len <- lapply(deviceConditionList[i], function(x) length(x))
  for(j in 1:len[[1]]) {
    df$ConditionData[grepl(deviceConditionList[[i]][[j]], df$ConditionData, ignore.case = T)] <- names(deviceConditionList[i])
  }
}

statusList <- c(names(deviceConditionList), "used")
shouldBecomeUnknown<-!(df$ConditionData %in% statusList)
df$ConditionData[shouldBecomeUnknown]<- "Unknown"


#################################################################################################

#Filter the dataset by removing Null and "" to avoid unnecessary iterations
#Selecting ID to use as common column while merging it with the actual dataset
temp <- df %>% filter(SellerNumberOfTransactions != "NULL"| SellerNumberOfTransactions=="") %>% select(ID, SellerNumberOfTransactions)

#Create a temporary column to store the number of transactions and months in which the transactions had happened

if(nrow(temp) != 0) {
  temp$noOfTransactions <- "TBD"
  temp$transactionMonths <- "TBD"
  
  
  #Function call: To get the no of transactions and the no of months in which the teansactions have happened
  temp <- getNoOfTransactionsBySeller(temp)
  
  #Kill SellerNumberOfTransactions in temp to avoid duplication
  temp$SellerNumberOfTransactions <- NULL
  #Merge the temp with actual dataset by common IDs
  df <- merge(df, temp, by = "ID", all.x = TRUE)
  
}



#Remove the unneeded off
rm(temp)


##Extract number of views from string - ItemUserViewsData
#Filter the dataset by removing Null and "" to avoid unnecessary iterations
#Selecting ID to use as common column while merging it with the actual dataset
temp <- df %>% filter(ItemUserViewsData != "NULL"| ItemUserViewsData=="") %>% select(ID, ItemUserViewsData)

#Create a temporary column to store the number of transactions and months in which the transactions had happened
if(nrow(temp) != 0) {
  temp$noOfViews <- "TBD"
}

  
#Function call: To get the number of views
temp <- getNoOfViews(temp)

#Kill ItemUserViewsData in temp to avoid duplication
temp$ItemUserViewsData <- NULL
#Merge the temp with actual dataset by common IDs
df <- merge(df, temp, by = "ID", all.x = TRUE)

#Remove the unneeded off
rm(temp)

#And yet another
df9 <- df

#######################################################################################################################
#Create new columns that are required  
df$locationCityFinal <-'TBD'
df$locationStateFinal <- 'TBD'
df$locationCountryFinal <- 'TBD'
df$locationISO2Final <- 'TBD'
df$latitude <- 'TBD'
df$longitude <- 'TBD'
df$GeoRef <- 'TBD'

#Function call to trim out special characters
df$LocationCity <- trim(df$LocationCity)
df$LocationCountry <- trim(df$LocationCountry)
df$LocationSpecific <- trim(df$LocationSpecific)

#Create a searchable combined field for Geographic Searchable Data
#Create a combined df by pasting the data into it from numerous sources
PreGeo <- paste(df$LocationSpecific, df$LocationCity, df$LocationCountry, sep = " ")
#Remove all the numbers in the concatenated string
PreGeo <- gsub('[0-9]+', '', PreGeo)

#Assign it to the existing field
df$GeoRef <- PreGeo

#Remove the unneeded df
rm(PreGeo)

#Remove NULL from the field as this is not data and each field is represented elsewhere.
df$GeoRef <- gsub("NULL|NA|unknown", "", df$GeoRef)

#Remove Special characters from the string
df$GeoRef <- str_replace_all(df$GeoRef, "[[:punct:]]", "")

#Remove white spaces from start and end of the string
df$GeoRef <- lapply(df$GeoRef, str_trim)

is.na(df$GeoRef) <- df$GeoRef == ''
df <- df[!is.na(df$GeoRef), ]


##################################################################################################
#Read the location lookup dataframe to a variable
LookupGeoref <- lookupDF$GeoRef_w_ISOCountryStateCityLL
#To change NA as string
LookupGeoref$ISO2[LookupGeoref$Country == "Namibia"] <- "NA"

################################## Country lookup mapping #######################################################################

#Function call
df <- mapCountryISO2(df)
df10 <- df


################################## Match United States ####################################################################################################### 

#Filter the data that do not have a country name and ISO2 code mapped
df_subset_country_TBD <- subset(df %>% filter(df$locationCountryFinal == "TBD"))
df_subset_country_mapped <- subset(df %>% filter(df$locationCountryFinal != "TBD"))

#Function call to map the rows whose georef is United States
df_subset_country_TBD <- matchUSinGeoRef(df_subset_country_TBD)

df <- rbind(df_subset_country_TBD, df_subset_country_mapped)
df11 <- df


################################## Match Korea ####################################################################################################### 

#Filter the data that do not have a country name and ISO2 code mapped
df_subset_country_TBD <- subset(df %>% filter(df$locationCountryFinal == "TBD"))
df_subset_country_mapped <- subset(df %>% filter(df$locationCountryFinal != "TBD"))

#Function call to map the rows whose georef is Korea
df_subset_country_TBD <- matchKoreainGeoRef(df_subset_country_TBD)

df <- rbind(df_subset_country_TBD, df_subset_country_mapped)
df12 <- df

################################## State lookup mapping ###################################################################################

#Filter the data that do not have a country name and ISO2 code mapped
df_subset_country_TBD <- subset(df %>% filter(df$locationCountryFinal == "TBD"))
df_subset_country_mapped <- subset(df %>% filter(df$locationCountryFinal != "TBD"))

#Function call
df_subset_country_TBD <- mapStateProvince(df_subset_country_TBD)
df <- rbind(df_subset_country_TBD, df_subset_country_mapped)
df13 <- df

######################################## Match ISO2 codes ####################################################################################################### 

#Filter the data that do not have a country name and ISO2 code mapped
df_subset_country_TBD <- subset(df %>% filter(df$locationCountryFinal == "TBD"))
df_subset_country_mapped <- subset(df %>% filter(df$locationCountryFinal != "TBD"))

df_subset_country_TBD$LocationCountry <- gsub("NULL|NA|unknown", "", df_subset_country_TBD$LocationCountry)

df_subset_country_TBD$LocationCountry <- lapply(df_subset_country_TBD$LocationCountry, str_trim)
is.na(df_subset_country_TBD$LocationCountry) <- df_subset_country_TBD$LocationCountry == ''
df_subset_country_TBD <- df_subset_country_TBD[!is.na(df_subset_country_TBD$LocationCountry), ]

#Remove all the numbers from th Locationcountry field
df_subset_country_TBD$LocationCountry <- gsub('[0-9]+', '', df_subset_country_TBD$LocationCountry)


#Split data into two subsets based the length of the Locationcountry column
#If length is 2, it can be directly compared, if not then the ISO2 code is to be extracted
df_subset_country_ISO <- subset(df_subset_country_TBD, nchar(df_subset_country_TBD$LocationCountry) == 2)
df_subset_country_NotISO <- subset(df_subset_country_TBD, nchar(df_subset_country_TBD$LocationCountry) != 2)

#Function call for the subset with ISOcode2 length as 2
df_subset_country_ISO <- mapISO2Code(df_subset_country_ISO)

#Extract only the ISO2 codes from the rows that has city/country name as well
df_subset_country_NotISO$LocationCountry <- lapply(df_subset_country_NotISO$LocationCountry, str_trim)
df_subset_country_NotISO$LocationCountry <- sub('.*,\\s*','',df_subset_country_NotISO$LocationCountry)



#again subset only if the length of the Locationcountry field is 2(it contains only the ISO2 code)
df_subset_country_NotISO_Len2 <- subset(df_subset_country_NotISO, nchar(df_subset_country_NotISO$LocationCountry) == 2)
df_subset_country_NotISO_LenLong <- subset(df_subset_country_NotISO, nchar(df_subset_country_NotISO$LocationCountry) != 2)
df_subset_country_NotISO_Len2 <- mapISO2Code(df_subset_country_NotISO_Len2)

#Bind the subsets to form the original dataframe
df_subset_country_NotISO <- rbind(df_subset_country_NotISO_Len2, df_subset_country_NotISO_LenLong)
df_subset_country_TBD <- rbind(df_subset_country_NotISO, df_subset_country_ISO)
df <- rbind(df_subset_country_TBD, df_subset_country_mapped)

df$GeoRef <- vapply(df$GeoRef, paste, collapse=", ", character(1L))
df$LocationCountry <- vapply(df$LocationCountry, paste, collapse=", ", character(1L))

df14 <- df

#################################  VAT Lookup Read Function ###########################
rownames(lookupDF$`2018Q1VAT`) <- lookupDF$`2018Q1VAT`$ISO2
VAT_2018 <- lookupDF$`2018Q1VAT`

df$VAT <- VAT_2018$VAT_Multiplier[match(df$locationISO2Final, VAT_2018$ISO2)]

###################################################################################################################

#################################  Tariff Lookup Read Function ####################################################
rownames(lookupDF$`2018_Tariffv2`) <- lookupDF$`2018_Tariffv2`$ISO2
Tariff_2018 <- lookupDF$`2018_Tariffv2`

df$Tariff <- Tariff_2018$TariffMultiplier[match(df$locationISO2Final, Tariff_2018$ISO2)]

####################################################################################################################

converAmt <- c()
for (i in 1:nrow(df)) {
  if(df$CurrencyID[i]!=''){
    converAmt[i] <- convertFX_removeTariff(df$CollectedPrice[i], df$converRate[i], df$VAT[i], df$Tariff[i])
  }
}

df$PriceConvertedUSD <- converAmt

#Save a final copy!
df15 <- df

####################################################################################################################

#copy the list of unique models to a df 
#df$Model[Reduce("|", lapply("ebay|bimedis", grepl, df$SourcePlatform))] <- df$CollectedTitle
#df$Model <- str_replace_all(df$Model, "For Sale: ", "")
 
# ActiveMod <- c(unique(df$Model))
# #Paste the unique models into a search set separated by a pike
# KeepMod <- paste(ActiveMod, collapse = "|")


##Split data into two parts:
#CleanDevices <- df[grepl(KeepMod, df$Model, ignore.case=TRUE) & !grepl ('TBD', df$Model, ignore.case = T), ]
CleanDevices <- df[!grepl ('TBD', df$Model, ignore.case = T), ]
CleanDevices <- CleanDevices[!is.na(CleanDevices$PriceConvertedUSD), ]


######################## Countrywise cities lookup mapping : Only For CleanDevices ###############################################################

# #Removes the special characters in LookupGeoref dataframe
# LookupGeoref$City <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", LookupGeoref$City)
# countrylookup <- LookupGeoref %>% distinct(Country, ISO2) %>% select(Country, ISO2)
# countrywiselookup <- list()
# countrywisedata <- list()
# 
# #Function call to create countrywise lookup
# countrywiselookup <- createCountryWiseLookup(countrylookup, LookupGeoref)
# 
# #Function call to create list of dataframes countrywise
# countrywisedata <- createCountryWiseData(countrylookup, CleanDevices)
# 
# #Function call to map city details
# countrywisedata <- mapCityData(countrywisedata, countrywiselookup)
# 
# #Unlists the dataframes into single dataframe
# CleanDevices <- ldply(countrywisedata, data.frame)

######################################################################################################################


#Save only the columns that are required for analysis
variables_to_drop <- c("CollectedTitle", "LocationCity", "LocationCountry", "LocationSpecific", 
                       "LocationState", "CollectedModel", "SellerAddress", "SellerName",
                       "Notes", "SourceFilename", "CollectedPrice", "CollectedOEM", "MOSD",
                        "SellerNumberOfTransactions")
#"GeoRef",

CleanDevices <- CleanDevices[ , !(names(CleanDevices) %in% variables_to_drop)]

#write.csv(CleanDevices, "./CleanData/MySQL_CleanData180509_002.csv", row.names = F)
filename <- paste0("./CleanData/MySQL_CleanData", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
write.csv(CleanDevices, filename, row.names=FALSE)


#write the df to a CSV for further research purposes
filename <- paste0("./CleanData/MySQL_ConsolidatedData", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
#Appending invalid currency data with consolidated data to check for exceptions
df <- rbind(df, df_invalidCurrencies)
write.csv(df, filename, row.names=FALSE)


#paint histogram to check prices 
hist(df$PriceConvertedUSD, c="blue",main="MySQL data")

##remove unused libraries and datasets if you won't use them again soon
#rm(ActiveMod)


## Print script diagnostics
end_sess <- Sys.time()
diff_sess <- as.numeric(difftime(end_sess, start_sess, units="sec"))
print(paste0('Cleaning data took ', round(diff_sess, 4), ' sec'))
print(paste0('Cleaning data end time ', end_sess))

##end of the code 


