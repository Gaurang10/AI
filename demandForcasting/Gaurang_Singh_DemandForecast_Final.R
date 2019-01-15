####################################################################################
#### Project Name : Demand Forecasting   		                                    ####
#### Author : Gaurang Singh                                                     ####
#### Environment Used : R studio                                                ####
####################################################################################

rm(list = ls(all.names = T))

## Importing library
library(lubridate)
library(plyr)
library(forecast)
library(randomForest)
library(rpart)
library(graphics)

## Reading data From csv files.
Product_data = read.csv("ProductsData.csv")
str(Product_data)
summary(Product_data)
sum(is.na(Product_data))
Holiday_data = read.csv("HolidayData.csv")
summary(Holiday_data)
Weatherdata <- read.csv("WeatherData.csv")
summary(Weatherdata)

##----------------- Data Preprocessing steps -----------------------------


## First we have converted date character column to date data-type. 
## Later created two function, First is to merger the product to date wise. 
## Second is to create weekwise time series.
## Converting date coulmn format.
Product_data$Date = as.Date(Product_data$Date, "%d-%m-%Y")
Product_data = subset(Product_data,select = -c(StoreID))
str(Product_data)
summary(Product_data)
head(Product_data)

Holiday_data$Date = as.Date(Holiday_data$Date, "%d-%m-%Y")
str(Holiday_data)
summary(Holiday_data)
head(Holiday_data)


Weatherdata$Date <- as.Date(Weatherdata$Date, "%d-%m-%Y")
str(Weatherdata)
summary(Weatherdata)
head(Weatherdata)

# 
# ## SAMPLE Code to extract month from Date column.
# Datedata <- Product_data$Date
# library(lubridate)
# datamonth = month(as.POSIXlt(Datedata,format = "%Y-%m-%d"))
# summary(datamonth)
# sum(datamonth==1)
# ###

## Sorting the data by ProductCode first and then Date
Product_data = Product_data[order(Product_data$ProductCode,Product_data$Date),]
head(Product_data)
Holiday_data = Holiday_data[order(Holiday_data$Date),]

length(Product_data$Date)
Product1 = Product_data[(Product_data$ProductCode==1001),]
Product2 = Product_data[(Product_data$ProductCode==1002),]
Product3 = Product_data[(Product_data$ProductCode==1003),]
Product4 = Product_data[(Product_data$ProductCode==1004),]
Product5 = Product_data[(Product_data$ProductCode==1005),]
Product6 = Product_data[(Product_data$ProductCode==1006),]
Product7 = Product_data[(Product_data$ProductCode==1007),]
Product8 = Product_data[(Product_data$ProductCode==1008),]
Product9 = Product_data[(Product_data$ProductCode==1009),]
Product10 = Product_data[(Product_data$ProductCode==1010),]
Product11 = Product_data[(Product_data$ProductCode==1011),]
Product12 = Product_data[(Product_data$ProductCode==1012),]
Product13 = Product_data[(Product_data$ProductCode==1013),]
Product14 = Product_data[(Product_data$ProductCode==1014),]
Product15 = Product_data[(Product_data$ProductCode==1015),]

summary(Product1)
summary(Product2)
summary(Product3)
summary(Product4)
summary(Product5)
summary(Product6)
summary(Product7) ## only 471 days data
summary(Product8)
summary(Product9)## 1082 days of data
summary(Product10)
summary(Product11)
summary(Product12)
summary(Product13)
summary(Product14)
summary(Product15)


## To check the unique data in the Product 1
length((Product1$Date))
length(unique(Product1$Date))
length((Holiday_data$Date))


## calculating weeknumber and yearnumber.

weeknumber = week(Holiday_data$Date)
length(weeknumber)
Holiday_data = cbind(Holiday_data,weeknumber)
yearnumber = year(Holiday_data$Date)
Holiday_data = cbind(Holiday_data,yearnumber)
Holiday_data =  Holiday_data[Holiday_data$yearnumber != 2013,] ## Removing 2013 data as our product data starts from 2014
summary(Holiday_data)
str(Holiday_data)
unique(Holiday_data$yearnumber)

Weatherdata_weeknum = week(Weatherdata$Date)
length(Weatherdata_weeknum)
yearnumber_1 = year(Weatherdata$Date)
Weatherdata = cbind(Weatherdata,Weatherdata_weeknum,yearnumber_1)

## Generating new features ##

## First aggregated HighSales data based on week and year. Calculated number of days which has highsales. 
HighSalesdata=data.frame(Holiday_data$HighSales,Holiday_data$weeknumber,Holiday_data$yearnumber)
colnames(HighSalesdata)=c("HighSales","weeknumber","yearnumber")
HighSalesdata = aggregate(HighSales ~ weeknumber + yearnumber, HighSalesdata , sum)

## Aggregated weather data week wise taking mean temparature.
Week_weather = data.frame(Weatherdata$Mean.TemperatureC,Weatherdata$Weatherdata_weeknum,Weatherdata$yearnumber_1)
colnames(Week_weather)=c("MeanTemperature","weeknum","yearnumber")
Week_weather = aggregate(MeanTemperature ~ weeknum +yearnumber,Week_weather, mean)
Week_weather = Week_weather[1:172,]


## Counting how many days in a week, temprature is more than average week temprature. 
AvrTemp_count <-0
count =0
for(i in 1:nrow(Week_weather)){
  count = 0
  for(j in 1:nrow(Weatherdata)){
    if(Week_weather$weeknum[i] == Weatherdata$Weatherdata_weeknum[j] & Week_weather$yearnumber[i]==Weatherdata$yearnumber_1[j])
    {
      if(Weatherdata$Mean.TemperatureC[j]>Week_weather$MeanTemperature[i])
        count =count+1
    }
  }
  AvrTemp_count[i]=count
}
Week_weather= cbind(Week_weather,AvrTemp_count)


## Creating features out of holiday data.
Holiday_data_mid = Holiday_data[,-c(1:3)]
Holiday_data_week = aggregate(. ~ weeknumber + yearnumber,Holiday_data_mid,sum)
Week_weather = Week_weather[1:171,] # remove last week(172th)
Week_weather = cbind(Week_weather,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
Week_weather = Week_weather[,-c(2)] # Removing the year column as no use.

mergeproduct <- function(Product){
  
  ## Aggregate the to add duplicate date value. 
  
  Product[duplicated(Product$Date),]
  Product = ddply(Product,.(Date),summarize,
                  Amount = sum(Amount),
                  Quantity = sum(Quantity),
                  UnitPrice = sum(UnitPrice)
  )
  Product[duplicated(Product$Date),]
  print(head(Product))
  print(summary(Product))
  
  
  ## To create the date Sequence 
  dateseq = seq(as.Date("2014-01-01"), as.Date("2017-03-26"), by="days")
  dateseq = as.data.frame(dateseq)
  colnames(dateseq) = "Date"
  summary(dateseq)
  
  
  ## Merging the data with date sequence. 
  mergeProd = merge(Product,dateseq,by="Date",all = TRUE)
  print(sum(is.na(mergeProd)))
  str(mergeProd)
  return(mergeProd)
}

## Aggregating data weekwise. 
weekagr <- function(mergeProd){
  day = ymd(mergeProd$Date)
  weeknumber = lubridate::week(day) 
  yr = lubridate::year(day)
  Prod_week =  data.frame("Quantity"=mergeProd$Quantity,weeknumber,yr)
  Prod_week = aggregate(Quantity ~ weeknumber + yr, Prod_week, sum)
  Prod_week = Prod_week[1:171,] ## Just passing 171 weeks as last week has only 1 day. 
  return(Prod_week)
}


## Building timeseries week-wise function
weekts <- function(mergeProd){
  
  day = ymd(mergeProd$Date)
  weeknumber = lubridate::week(day) 
  yr = lubridate::year(day)
  Prod_week =  data.frame("Quantity"=mergeProd$Quantity,weeknumber,yr)
  Prod_week = aggregate(Quantity ~ weeknumber + yr, Prod_week, sum)
  str(Prod_week)
  summary(Prod_week); 
  Prod_ts_week = ts(Prod_week$Quantity,frequency = 53,start = c(2014,1))
  print("Number of Na :")
  print(sum(is.na(Prod_ts_week)))
  print(head(Prod_ts_week))
  plot.ts(Prod_ts_week)
  return(Prod_ts_week)
  
}

## Function to test all the models. It returns the error matrix. 
fittingmodel <- function(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
{
  
  # Fitting models
  Model_HW = HoltWinters(TimeSeries_product1)
  Model_arima = arima(TimeSeries_product1, order = c(p,d,q)) 
  Model_arimaX <- arima(TimeSeries_product1,order = c(p,d,q), xreg = xreg) 
  Model_arima_auto = auto.arima(TimeSeries_product1)
  Model_arimaX_auto = auto.arima(TimeSeries_product1,xreg = xreg)
  Model_lm = lm(Quantity~.,data = mergeprod_1_lm_train)
  Model_rf = randomForest(Quantity~., data=mergeprod_1_lm_train, ntree=500)
  Model_rpart = rpart(Quantity ~ ., data = mergeprod_1_lm_train, control = rpart.control(cp = 0.0001))
  
  #Forecasting for last 2 time periods 
  library(forecast)
  preds_HW <- data.frame(forecast(Model_HW,h=2))$Point.Forecast 
  preds_arima <- data.frame(forecast.Arima(Model_arima, h=2))$Point.Forecast 
  preds_arimaX <- data.frame(forecast.Arima(Model_arimaX, h=2,xreg=xreg1))$Point.Forecast 
  preds_arima_auto <- data.frame(forecast.Arima(Model_arima_auto, h=2))$Point.Forecast 
  preds_arimax_auto <- data.frame(forecast.Arima(Model_arimaX_auto, h=2,xreg=xreg1))$Point.Forecast
  preds_lm = predict(Model_lm,mergeprod_1_lm_test[,-2]) ## Quantity is in 2nd position.
  preds_rf = predict(Model_rf,mergeprod_1_lm_test[,-2])
  preds_rpart = predict(Model_rpart,mergeprod_1_lm_test[,-2])
  
  library(DMwR) 
  
  ACC <- rbind.fill(  data.frame(t(regr.eval(trues = actuals, preds = preds_HW))),
                      data.frame(t(regr.eval(trues = actuals, preds = preds_arima))),
                      data.frame(t(regr.eval(trues = actuals, preds = preds_arimaX))),
                      data.frame(t(regr.eval(trues = actuals, preds = preds_arima_auto))),
                      data.frame(t(regr.eval(trues = actuals, preds = preds_arimax_auto))),
                      data.frame(t(regr.eval(trues = actuals, preds = preds_lm))),
                      data.frame(t(regr.eval(trues = actuals, preds = preds_rf))),
                      data.frame(t(regr.eval(trues = actuals, preds = preds_rpart)))
  )
  
  ACC <- round(ACC,2)
  ACC <- cbind(Type=c('Holt-Winter','Arima','ArimaX','Auto-Arima','Auto-Arimax','Regression','RandomForest','DecisionTree'),ACC)
  return(ACC)
}

## aggregated data by week wise and creating timeseries for each product. 
mergeprod1 <- mergeproduct(Product1)
product1_timeseries_week<-weekts(mergeprod1)

mergeprod2 <- mergeproduct(Product2)
prod2_ts_week<-weekts(mergeprod2)

mergeprod3 <- mergeproduct(Product3)
prod3_ts_week<-weekts(mergeprod3)

mergeprod4 <- mergeproduct(Product4)
prod4_ts_week<-weekts(mergeprod4)

mergeprod5 <- mergeproduct(Product5)
prod5_ts_week<-weekts(mergeprod5)

mergeprod6 <- mergeproduct(Product6)
prod6_ts_week<-weekts(mergeprod6)

mergeprod7 <- mergeproduct(Product7)
prod7_ts_week<-weekts(mergeprod7)

mergeprod8 <- mergeproduct(Product8)
prod8_ts_week<-weekts(mergeprod8)

mergeprod9 <- mergeproduct(Product9)
prod9_ts_week<-weekts(mergeprod9)

mergeprod10 <- mergeproduct(Product10)
prod10_ts_week<-weekts(mergeprod10)

mergeprod11 <- mergeproduct(Product11)
prod11_ts_week<-weekts(mergeprod11)

mergeprod12 <- mergeproduct(Product12)
prod12_ts_week<-weekts(mergeprod12)

mergeprod13 <- mergeproduct(Product13)
prod13_ts_week<-weekts(mergeprod13)

mergeprod14 <- mergeproduct(Product14)
prod14_ts_week<-weekts(mergeprod14)

mergeprod15 <- mergeproduct(Product15)
prod15_ts_week<-weekts(mergeprod15)


# ## Building time series  day-wise.
# 
# product1_timeseries = ts(mergeProd1$Quantity,frequency = 365,start = c(2014,1))
# sum(is.na(product1_timeseries))
# head(product1_timeseries)
# plot.ts(product1_timeseries)
# 
# ## Imputing data with LOCF.
# library(imputeTS)
# product1_timeseries = na.locf(product1_timeseries)
# sum(is.na(product1_timeseries))


# ## Building timeseries week-wise:
# library(lubridate)
# day = ymd(mergeProd1$Date)
# weeknumber = lubridate::week(day) # summary(weeknumber)
# yr = lubridate::year(day)
# Prod1_week =  data.frame("Quantity"=mergeProd1$Quantity,weeknumber,yr)
# Prod1_week = aggregate(Quantity ~ weeknumber + yr, Prod1_week, sum)
# str(Prod1_week)
# summary(Prod1_week); 
# product1_timeseries_week = ts(Prod1_week$Quantity,frequency = 53,start = c(2014,1))
# sum(is.na(product1_timeseries_week))
# head(product1_timeseries_week)
# plot.ts(product1_timeseries_week)



##----------------- Data Preprocessing ended ------------------------

## ---------------- Building models --------------------------------- 

## ----------------------------- ##
## Model Building for product 1  ## 
## ----------------------------- ##

# Study ACF, PACF plots to find auto and partial auto correlations manually and check if data has predominant trend and seasonal components 
Product1_ts = product1_timeseries_week
par(mfrow=c(1,2)) 
acf(Product1_ts)
pacf(Product1_ts)
     
# Use averaging models I.e. SMA/WMA/EMA for smoothing the data 
library(TTR) 
par(mfrow=c(1,1)) 
sma_Product1 = SMA(product1_timeseries_week,n=2) 
wma_Product1 = WMA(product1_timeseries_week,n=2) 
ema_Product1 = EMA(product1_timeseries_week,n=2) 
par(mfrow=c(1,1)) 
plot(product1_timeseries_week,col = "blue") 
lines(sma_Product1, col = "black") 
lines(wma_Product1, col = "red") 
lines(ema_Product1, col = "green")

# Errors using the averaging models
errorsma = mean(abs(product1_timeseries_week[2:171]-sma_Product1[2:171]))
errorwma = mean(abs(product1_timeseries_week[2:171]-wma_Product1[2:171]))
errorema = mean(abs(product1_timeseries_week[2:171]-ema_Product1[2:171]))

errorsma # 90.06315
errorwma # 60.0421
errorema # 55.75797

## All Model checking.
#rm(acc1)
mergeprod_1 = weekagr(mergeprod1)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)
#summary(mergeprod_1_lm)
#str(mergeprod_1_lm)
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 4 lag
p=4;d=1;q=1
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc1 = acc

## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 4 lag
p=4;d=1;q=1
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc1 = rbind(acc1,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 4 lag
p=4;d=1;q=1
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc1=rbind(acc1,acc)

## ----------------------------- ##
## Model Building for product 2  ## 
## ----------------------------- ##

# Study ACF, PACF plots to find auto and partial auto correlations manually and check if data has predominant trend and seasonal components 
par(mfrow=c(1,2)) 
acf(prod2_ts_week)
pacf(prod2_ts_week)


# Use averaging models I.e. SMA/WMA/EMA for smoothing the data 
library(TTR) 
par(mfrow=c(1,1)) 
sma_Product1 = SMA(prod2_ts_week,n=10) 
wma_Product1 = WMA(prod2_ts_week,n=10) 
ema_Product1 = EMA(prod2_ts_week,n=10) 
par(mfrow=c(1,1)) 
plot(prod2_ts_week,col = "blue") 
lines(sma_Product1, col = "black") 
lines(wma_Product1, col = "red") 
lines(ema_Product1, col = "green")

# Errors using the averaging models
errorsma = mean(abs(prod2_ts_week[10:171]-sma_Product1[10:171]))
errorwma = mean(abs(prod2_ts_week[10:171]-wma_Product1[10:171]))
errorema = mean(abs(prod2_ts_week[10:171]-ema_Product1[10:171]))

errorsma
errorwma
errorema

## All Model checking.
mergeprod_1 = weekagr(mergeprod2)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 3 lag
pacf(Quantity_stationary_ts) # 1 lag
p=1;d=1;q=3
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc

## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 3 lag
pacf(Quantity_stationary_ts) # 1 lag
p=1;d=1;q=3
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 3 lag
pacf(Quantity_stationary_ts) # 1 lag
p=1;d=1;q=3
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc


## ----------------------------- ##
## Model Building for product 3  ## 
## ----------------------------- ##



# Study ACF, PACF plots to find auto and partial auto correlations manually and check if data has predominant trend and seasonal components 
par(mfrow=c(1,2)) 
acf(prod3_ts_week)
pacf(prod3_ts_week)
plot(decompose(prod3_ts_week))

# Use averaging models I.e. SMA/WMA/EMA for smoothing the data 
library(TTR) 
par(mfrow=c(1,1)) 
sma_Product1 = SMA(prod3_ts_week,n=6) 
wma_Product1 = WMA(prod3_ts_week,n=6) 
ema_Product1 = EMA(prod3_ts_week,n=6) 
par(mfrow=c(1,1)) 
plot(prod3_ts_week,col = "blue") 
lines(sma_Product1, col = "black") 
lines(wma_Product1, col = "red") 
lines(ema_Product1, col = "green")

# Errors using the averaging models
errorsma = mean(abs(prod3_ts_week[6:171]-sma_Product1[6:171]))
errorwma = mean(abs(prod3_ts_week[6:171]-wma_Product1[6:171]))
errorema = mean(abs(prod3_ts_week[6:171]-ema_Product1[6:171]))

errorsma
errorwma
errorema

## All Model checking.
#rm(acc3)
mergeprod_1 = weekagr(mergeprod3)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=1
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc3 = acc

## checking prediction for last 168th and 169th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc3 = rbind(acc3,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=1
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc3 = rbind(acc3,acc)


## ----------------------------- ##
## Model Building for product  4 ## 
## ----------------------------- ##

## All Model checking.
rm(acc4)
mergeprod_1 = weekagr(mergeprod4)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
#Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
Quantity_stationary_ts = TimeSeries_product1
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 6 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=0;q=6
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc4 = acc
## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
#Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
Quantity_stationary_ts = TimeSeries_product1
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 6 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=0;q=6
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc4 = rbind(acc4,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
#Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
Quantity_stationary_ts = TimeSeries_product1
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 6 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=0;q=6
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc4 = rbind(acc4,acc)



## ---------------------------- ##
## Model Building for product 5 ## 
## ---------------------------- ##

# Study ACF, PACF plots to find auto and partial auto correlations manually and check if data has predominant trend and seasonal components 
par(mfrow=c(1,2)) 
acf(prod5_ts_week)
pacf(prod5_ts_week)
plot(decompose(prod5_ts_week))
plot(prod5_ts_week)

## All Model checking.
#rm(acc5)
mergeprod_1 = weekagr(mergeprod5)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 3 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=1;q=3
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc5 = acc

## checking prediction for last 168th and 169th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 3 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=1;q=3
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc5 = rbind(acc5,acc)


## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 3 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=3
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc5 = rbind(acc5,acc)



## ---------------------------- ##
## Model Building for product 6 ## 
## ---------------------------- ##

# Study ACF, PACF plots to find auto and partial auto correlations manually and check if data has predominant trend and seasonal components 
par(mfrow=c(1,2)) 
acf(prod6_ts_week)
pacf(prod6_ts_week)
plot(decompose(prod6_ts_week))
plot(prod6_ts_week)


# Use averaging models I.e. SMA/WMA/EMA for smoothing the data 
library(TTR) 
par(mfrow=c(1,1)) 
sma_Product1 = SMA(prod3_ts_week,n=11) 
wma_Product1 = WMA(prod3_ts_week,n=11) 
ema_Product1 = EMA(prod3_ts_week,n=11) 
par(mfrow=c(1,1)) 
plot(prod3_ts_week,col = "blue") 
lines(sma_Product1, col = "black") 
lines(wma_Product1, col = "red") 
lines(ema_Product1, col = "green")

# Errors using the averaging models
errorsma = mean(abs(prod3_ts_week[11:171]-sma_Product1[11:171]))
errorwma = mean(abs(prod3_ts_week[11:171]-wma_Product1[11:171]))
errorema = mean(abs(prod3_ts_week[11:171]-ema_Product1[11:171]))

errorsma
errorwma
errorema


## All Model checking.
#rm(acc6)
mergeprod_1 = weekagr(mergeprod6)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1,main="product 6",ylab="Timeseries")
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=1;q=1
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc6=acc

## checking prediction for last 168th and 169th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=1;q=1
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc6 = rbind(acc6,acc)
## checking prediction for last 166th and 167th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 3 lag
p=3;d=1;q=1
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc6 = rbind(acc6,acc)

## ---------------------------- ##
## Model Building for product 7 ## 
## ---------------------------- ##

# Study ACF, PACF plots to find auto and partial auto correlations manually and check if data has predominant trend and seasonal components 
par(mfrow=c(1,2)) 
acf(prod7_ts_week)
pacf(prod7_ts_week)
plot(decompose(prod7_ts_week))
plot(prod7_ts_week)


# Use averaging models I.e. SMA/WMA/EMA for smoothing the data 
library(TTR) 
par(mfrow=c(1,1)) 
sma_Product1 = SMA(prod3_ts_week,n=3) 
wma_Product1 = WMA(prod3_ts_week,n=3) 
ema_Product1 = EMA(prod3_ts_week,n=3) 
par(mfrow=c(1,1)) 
plot(prod3_ts_week,col = "blue") 
lines(sma_Product1, col = "black") 
lines(wma_Product1, col = "red") 
lines(ema_Product1, col = "green")

# Errors using the averaging models
errorsma = mean(abs(prod3_ts_week[3:71]-sma_Product1[3:71]))
errorwma = mean(abs(prod3_ts_week[3:71]-wma_Product1[3:71]))
errorema = mean(abs(prod3_ts_week[3:71]-ema_Product1[3:71]))

errorsma
errorwma
errorema

## Holt-Winter needs atleast 2 years data to predict.

## ARIMA Model. 
mergeprod_arima = weekagr(mergeprod7)
TimeSeries_arima = ts(mergeprod_arima$Quantity[1:69], frequency = 53,start = c(2014,1))
ndiffs(TimeSeries_arima)
timeseriesdiff1= diff(TimeSeries_arima, differences = 1)
plot.ts(timeseriesdiff1)
par(mfrow=c(1,2)) 
acf(timeseriesdiff1) 
pacf(timeseriesdiff1)


# Build an ARIMA model on first 169 time periods and forecast for the last 2 time periods; evaluate the error metrics 
quantity_arima <- arima(TimeSeries_arima, order = c(1,0,1)) 
quantity_arima_forecasts <- forecast.Arima(quantity_arima, h=2) 
preds <- data.frame(quantity_arima_forecasts)$Point.Forecast 
actuals <- mergeprod_arima$Quantity[70:71] 
#Error 
library(DMwR) 
acc7 = regr.eval(trues = actuals, preds = preds) 

# > regr.eval(trues = actuals, preds = preds)
# mae        mse       rmse       mape 
# 4.0683924 22.9837989  4.7941421  0.1827883 
# > preds
# [1] 28.46774 27.60453
# > actuals
# [1] 30 21


# Build an Auto-ARIMA model, forecast for the last 2 time periods, and evaluate the error
quantity_arima = auto.arima(TimeSeries_arima)
autoarima_forecast <- data.frame(forecast(quantity_arima,h=2))$Point.Forecast 
regr.eval(trues = actuals, preds = autoarima_forecast) 

# > regr.eval(trues = actuals, preds = autoarima_forecast)
# mae        mse       rmse       mape 
# 4.5000000 24.7554677  4.9754867  0.1973043 
# > actuals
# [1] 30 21
# > autoarima_forecast
# [1] 27.62261 27.62261


## 
TimeSeries_arima_1 = ts(mergeprod_arima$Quantity[1:67], frequency = 53,start = c(2014,1))
quantity_arima <- arima(TimeSeries_arima_1, order = c(2,0,1)) 
quantity_arima_forecasts <- forecast.Arima(quantity_arima, h=2) 
preds <- data.frame(quantity_arima_forecasts)$Point.Forecast 
actuals <- mergeprod_arima$Quantity[68:69] 
#Error 
library(DMwR) 
regr.eval(trues = actuals, preds = preds) 
# > regr.eval(trues = actuals, preds = preds)
# mae        mse       rmse       mape 
# 4.3198361 19.2935684  4.3924445  0.1386662 
# > preds
# [1] 26.88481 26.47552
# > actuals
# [1] 32 3
quantity_arima = auto.arima(TimeSeries_arima_1)
autoarima_forecast <- data.frame(forecast.Arima(quantity_arima,h=2))$Point.Forecast 
regr.eval(trues = actuals, preds = autoarima_forecast) 

# > regr.eval(trues = actuals, preds = autoarima_forecast)
# mae         mse        rmse        mape 
# 10.0747780 198.7469647  14.0977645   0.1763407 
# > actuals
# [1] 72.97 57.00
# > autoarima_forecast
# [1] 73.18345 76.93611

TimeSeries_arima_1 = ts(mergeprod_arima$Quantity[1:65], frequency = 53,start = c(2014,1))
quantity_arima <- arima(TimeSeries_arima_1, order = c(2,0,1)) 
quantity_arima_forecasts <- forecast.Arima(quantity_arima, h=2) 
preds <- data.frame(quantity_arima_forecasts)$Point.Forecast 
actuals <- mergeprod_arima$Quantity[66:67] 
#Error 
library(DMwR) 
regr.eval(trues = actuals, preds = preds) 

# > regr.eval(trues = actuals, preds = preds)
# mae        mse       rmse       mape 
# 9.1102717 94.9305712  9.7432321  0.5045191 
# > preds
# [1] 27.34422 27.56476
# > actuals
# [1] 33 15
quantity_arima = auto.arima(TimeSeries_arima_1)
autoarima_forecast <- data.frame(forecast.Arima(quantity_arima,h=2))$Point.Forecast 
regr.eval(trues = actuals, preds = autoarima_forecast)

# > regr.eval(trues = actuals, preds = autoarima_forecast)
# mae        mse       rmse       mape 
# 9.0000000 94.1780169  9.7045359  0.5023664 
# > autoarima_forecast
# [1] 27.63015 27.63015
# > actuals
# [1] 33 15



## ---------------------------- ##
## Model Building for product 8 ## 
## ---------------------------- ##

## All Model checking.
#rm(acc8)
mergeprod_1 = weekagr(mergeprod8)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
#Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
Quantity_stationary_ts = TimeSeries_product1
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 0 lag
pacf(Quantity_stationary_ts) # 0 lag
p=0;d=0;q=0
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc8 = acc

## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
#Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
Quantity_stationary_ts = TimeSeries_product1
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 0 lag
pacf(Quantity_stationary_ts) # 0 lag
p=0;d=0;q=0
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc8 =rbind(acc8,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
#Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
Quantity_stationary_ts = TimeSeries_product1
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 0 lag
pacf(Quantity_stationary_ts) # 0 lag
p=0;d=0;q=0
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc8= rbind(acc8,acc)


## ---------------------------- ##
## Model Building for product 9 ## 
## ---------------------------- ##

# Study ACF, PACF plots to find auto and partial auto correlations manually and check if data has predominant trend and seasonal components 
par(mfrow=c(1,2)) 
acf(prod9_ts_week)
pacf(prod9_ts_week)
plot(decompose(prod9_ts_week))
plot(prod9_ts_week)


# Use averaging models I.e. SMA/WMA/EMA for smoothing the data 
library(TTR) 
par(mfrow=c(1,1)) 
sma_Product1 = SMA(prod9_ts_week,n=2) 
wma_Product1 = WMA(prod9_ts_week,n=2) 
ema_Product1 = EMA(prod9_ts_week,n=2) 
par(mfrow=c(1,1)) 
plot(prod3_ts_week,col = "blue") 
lines(sma_Product1, col = "black") 
lines(wma_Product1, col = "red") 
lines(ema_Product1, col = "green")

# Errors using the averaging models
errorsma = mean(abs(prod3_ts_week[2:171]-sma_Product1[2:171]))
errorwma = mean(abs(prod3_ts_week[2:171]-wma_Product1[2:171]))
errorema = mean(abs(prod3_ts_week[2:171]-ema_Product1[2:171]))

errorsma
errorwma
errorema

## All Model checking.
mergeprod_1 = weekagr(mergeprod9)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 3 lag
p=1;d=1;q=3
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)

## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 3 lag
p=1;d=1;q=3
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)

## checking prediction for last 166th and 167th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 3 lag
p=1;d=1;q=3
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)



## ----------------------------- ##
## Model Building for product 10 ## 
## ----------------------------- ##


par(mfrow=c(1,2)) 
acf(prod10_ts_week)
pacf(prod10_ts_week)
plot(decompose(prod10_ts_week))
plot(prod10_ts_week)

## All Model checking.
mergeprod_1 = weekagr(mergeprod10)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1,main = "Product 10")
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 4 lag
p=4;d=1;q=1
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc10 =acc

## checking prediction for last 168th and 169th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 4 lag
p=4;d=1;q=1
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc10 = rbind(acc10,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 4 lag
p=4;d=1;q=1
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc10 = rbind(acc10,acc)


## ----------------------------- ##
## Model Building for product 11 ## 
## ----------------------------- ##

## All Model checking.
mergeprod_1 = weekagr(mergeprod11)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=1
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc11 = acc

## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=1
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc11 = rbind(acc11,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc11 = rbind(acc11,acc)

## ----------------------------- ##
## Model Building for product 12 ## 
## ----------------------------- ##


## All Model checking.
mergeprod_1 = weekagr(mergeprod11)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc12 = acc

## checking prediction for last 168th and 169th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc12 = rbind(acc12,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc12 = rbind(acc12,acc)


## ----------------------------- ##
## Model Building for product 13 ## 
## ----------------------------- ##

## All Model checking.
mergeprod_1 = weekagr(mergeprod13)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 5 lag
p=5;d=1;q=1
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc13 = acc

## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 5 lag
p=5;d=1;q=1
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc13 = rbind(acc13,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 4 lag
p=5;d=1;q=1
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc13 = rbind(acc13,acc)

## ----------------------------- ##
## Model Building for product 14 ## 
## ----------------------------- ##

## All Model checking.
mergeprod_1 = weekagr(mergeprod14)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)


TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 1 lag
p=1;d=1;q=1
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc14 = acc

## checking prediction for last 168th and 169th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 1 lag
p=1;d=1;q=1
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc14 =rbind(acc14,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 1 lag
pacf(Quantity_stationary_ts) # 1 lag
p=1;d=1;q=1
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc14 =rbind(acc14,acc)


## ---------------------------- ##
## Model Building for product 15 ## 
## ---------------------------- ##


## All Model checking.
mergeprod_1 = weekagr(mergeprod15)

##Processing for regression 
mergeprod_1_lm = cbind(mergeprod_1,Week_weather$MeanTemperature,Week_weather$AvrTemp_count,Holiday_data_week$WeekEnd,Holiday_data_week$Holiday,Holiday_data_week$LongVacation,Holiday_data_week$HighSales)
mergeprod_1_lm = mergeprod_1_lm[,-c(2)]
colnames(mergeprod_1_lm) =  c("weeknum","Quantity","MeanTemp","AvgTemCount","weekend","holiday","longVacation","highsales")
mergeprod_1_lm$weeknum = factor(mergeprod_1_lm$weeknum)

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:169], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[170:171] 
xreg = Week_weather[1:169,]
xreg1 = Week_weather[170:171,]
mergeprod_1_lm_train=mergeprod_1_lm[1:169,]
mergeprod_1_lm_test=mergeprod_1_lm[170:171,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc15 = acc


## checking prediction for last 168th and 169th week. 

TimeSeries_product1 = ts(mergeprod_1$Quantity[1:167], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[168:169] 
xreg = Week_weather[1:167,]
xreg1 = Week_weather[168:169,]
mergeprod_1_lm_train=mergeprod_1_lm[1:167,]
mergeprod_1_lm_test=mergeprod_1_lm[168:169,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc15 = rbind(acc15,acc)

## checking prediction for last 166th and 167th week. 
TimeSeries_product1 = ts(mergeprod_1$Quantity[1:165], frequency = 53,start = c(2014,1))
plot(TimeSeries_product1)
plot(decompose(TimeSeries_product1))
ndiffs(TimeSeries_product1)
Quantity_stationary_ts = diff(TimeSeries_product1,differences = 1) 
plot(Quantity_stationary_ts)
par(mfrow=c(1,2)) 
acf(Quantity_stationary_ts) # 2 lag
pacf(Quantity_stationary_ts) # 2 lag
p=2;d=1;q=2
actuals <- mergeprod_1$Quantity[166:167] 
xreg = Week_weather[1:165,]
xreg1 = Week_weather[166:167,]
mergeprod_1_lm_train=mergeprod_1_lm[1:165,]
mergeprod_1_lm_test=mergeprod_1_lm[166:167,]
## Fitting models
acc=fittingmodel(TimeSeries_product1,mergeprod_1_lm_train,mergeprod_1_lm_test,actuals,xreg,xreg1,p,d,q)
acc15 = rbind(acc15,acc)

## Visualization 
library(ggplot2)
ggplot(product1_timeseries_week,aes(x ,y))+geom_line(aes(color="Product 1"))+
  geom_line(data=prod2_ts_week,aes(color="Product 2"))+
  geom_line(data=prod3_ts_week,aes(color="Product 3"))+
  geom_line(data=prod4_ts_week,aes(color="Product 4"))+
  geom_line(data=prod5_ts_week,aes(color="Product 5"))+
  labs(color="Prodcts")


ts.plot(prod15_ts_week)
abc_test = cbind(product1_timeseries_week,prod2_ts_week,prod3_ts_week,prod4_ts_week,prod5_ts_week,prod6_ts_week)
ts.plot(abc_test,gpars= list(col=c('blue','red','dark green','black','orange','pink')))

