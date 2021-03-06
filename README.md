# Healthcare-Imaging-equipments---Price-prediction
The data used for this analysis consists of list of imaging equipment(like CT scans, MRI machines etc.,) available for sale all over the world. The data set was very large and contained millions of rows with around 42 columns(features). The data is wrangled using R and then machine learning models were applied to predict the prices of imaging equipment. 


## Data:
Details of imaging equipment listed over different websites for sales from around all over the world are scraped to store in a SQL server database. These data are scrapped on daily basis(over 400K rows each day) and contains abouve 40 features that includes Category, Model, Condition, Year Manufactured, Location, Quantity, OEM, Price, source website, source url etc., Few other features like VAT, Tariff, PriceConverted to USD are calculated based on the available details. 

## Data cleaning:
The data is redundant as it is collected every day i.e., a listing can stay in a website for at least a week to month. Thus unique rows of data are filtered out and rows with N/A's for important features are removed. The data was very cumbersome with many mistakes and incomplete details. For example, the location column had city name, state and country. Most of the rows were complete whereas, other rows had either one or two location details. Thus a lookup file that countains all countries with its state and city was created. The data was compared against the lookup to sort out the accurate location details and three separate columns were created for city, state and country. There were also other issues like a city name can belong to more than one country and that was sorted out with a complex algorithm. Silmilarly, other columns like description, model name were also cleaned. New columns with details regarding VAT, Tariff were created based on the country using **getsymbols** library. Based on the country, a new column with price converted to USD was also created using **Quantmod** package(It gives the U.S dollar exchange rate for different currencies based on the date given as input).  

## Machine Learning models:
Few correlation and scatterplots were plotted using **ggplot** to know the outliers, average, range, minimum and maximum values of different features. Also, correlation rate between different features and price were calculated using **corrplot** and the ones with high correlation rate were filtered to built the models. **Single-variate and Multi-variate linear regression** models were built to predict the prices of CT and MRI maachines. In order to select the best features to fit in a model that predicts price of imaging equipment with accuracy, **forward and backward selection** methods were used. 

## Visualization:
Dashboards were built using Tableau to visualize price range of CT/MRI machines across different countries based on Model, Year manufactured, OEM. Also the sales listing of equipment over different platforms were visualized to identify the potential websites from which data could be scrapped efficiently in future. 

