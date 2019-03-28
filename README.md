# Healthcare-Imaging-equipments---Price-prediction
The data used for this analysis consists of list of imaging equipment(like CT scans, MRI machines etc.,) available for sale all over the world. The data set was very large and contained millions of rows with around 42 columns(features). The data is wrangled using R and then machine learning models were applied to predict the prices of imaging equipment. 


**Data:**
Details of imaging equipment listed over different websites for sales from around all over the world are scraped to store in a SQL server database. These data are scrapped on daily basis(over 400K rows each day) and contains abouve 40 features that includes Category, Model, Condition, Year Manufactured, Location, Quantity, OEM, Price, source website, source url etc., Few other features like VAT, Tariff, PriceConverted to USD are calculated based on the available details. 

**Data cleaning:**
The data is redundant as it is collected every day i.e., a listing can stay in a website for at least a week to month. Thus unique rows of data are filtered out and rows with N/A's for important features are removed. 
