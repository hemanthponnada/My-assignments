# Business understanding:
#   
#   "Global Mart" is an online store super giant having worldwide operations.
# It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office.
# 


#--------Loading the required packages-------------------------
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)
library(plyr)
library(stats)
library(zoo)
library(sqldf)
library(dplyr)

#--------------Loading the dataset-----------------------------
dataset<-read.csv("Global Superstore.csv", stringsAsFactors = FALSE)

#--------------Finding the null values-----------------
sapply(dataset,function(x){sum(is.na(x))})

#Removing the Postal code as the postal code coloumn contains almost 40,000 null values. 
#And also removing the row id coloumn as it of no use.
dataset$Postal.Code<-NULL
dataset$Row.ID<-NULL

#Parsing the date coloumn 
dataset$Order.Date<-parse_date_time(x=dataset$Order.Date, orders = c("d-m-Y"))
dataset$Ship.Date<-parse_date_time(x=dataset$Ship.Date, orders = c("d-m-Y"))

min(dataset$Order.Date)
max(dataset$Order.Date)

#Arranging the months in a proper sequence
dataset$month<-sapply(dataset$Order.Date, function(x) length(seq(from= min(dataset$Order.Date), to=x, by='month')))

range(dataset$month)

unique(dataset$Segment)
unique(dataset$Market)
#The store caters to 7 different market segments and in 3 major categories
#The following table contains all the different values.
table(dataset$Segment, dataset$Market)


#Subsetting the market for US and different segments
US_CONSUMER<-subset(dataset,dataset$Segment=="Consumer" & dataset$Market=="US" )
US_CORPORATE<-subset(dataset, dataset$Segment=="Corporate" & dataset$Market=="US")
US_HOME_OFFICE<-subset(dataset, dataset$Segment=="Home Office" & dataset$Market=="US")

#Subsetting the market for Africa and differnet segments

AFRICA_CONSUMER<-subset(dataset,dataset$Segment=="Consumer" & dataset$Market=="Africa" )
AFRICA_CORPORATE<-subset(dataset, dataset$Segment=="Corporate" & dataset$Market=="Africa")
AFRICA_HOME_OFFICE<-subset(dataset, dataset$Segment=="Home Office" & dataset$Market=="Africa")


#Subsetting the market for Apac and differnet segments

APAC_CONSUMER<-subset(dataset,dataset$Segment=="Consumer" & dataset$Market=="APAC" )
APAC_CORPORATE<-subset(dataset, dataset$Segment=="Corporate" & dataset$Market=="APAC")
APAC_HOME_OFFICE<-subset(dataset, dataset$Segment=="Home Office" & dataset$Market=="APAC")

#Subsetting the market for Canada and differnet segments

CANADA_CONSUMER<-subset(dataset,dataset$Segment=="Consumer" & dataset$Market=="Canada" )
CANADA_CORPORATE<-subset(dataset, dataset$Segment=="Corporate" & dataset$Market=="Canada")
CANADA_HOME_OFFICE<-subset(dataset, dataset$Segment=="Home Office" & dataset$Market=="Canada")


#Subsetting the market for EMEA and differnet segments

EMEA_CONSUMER<-subset(dataset,dataset$Segment=="Consumer" & dataset$Market=="EMEA" )
EMEA_CORPORATE<-subset(dataset, dataset$Segment=="Corporate" & dataset$Market=="EMEA")
EMEA_HOME_OFFICE<-subset(dataset, dataset$Segment=="Home Office" & dataset$Market=="EMEA")


#Subsetting the market for EU and differnet segments

EU_CONSUMER<-subset(dataset,dataset$Segment=="Consumer" & dataset$Market=="EU" )
EU_CORPORATE<-subset(dataset, dataset$Segment=="Corporate" & dataset$Market=="EU")
EU_HOME_OFFICE<-subset(dataset, dataset$Segment=="Home Office" & dataset$Market=="EU")

#Subsetting the market for Latam and differnet segments

LATAM_CONSUMER<-subset(dataset,dataset$Segment=="Consumer" & dataset$Market=="LATAM" )
LATAM_CORPORATE<-subset(dataset, dataset$Segment=="Corporate" & dataset$Market=="LATAM")
LATAM_HOME_OFFICE<-subset(dataset, dataset$Segment=="Home Office" & dataset$Market=="LATAM")

#Aggregating the US data for total profit and CV

US_CONSUMER_aggr<-US_CONSUMER%>%
  group_by(US_CONSUMER$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="US_CONSUMER")

US_CORPORATE_aggr<-US_CORPORATE%>%
  group_by(US_CORPORATE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="US_CORPORATE")

US_HOMEOFFICE_aggr<-US_HOME_OFFICE%>%
  group_by(US_HOME_OFFICE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="US_HOMEOFFICE")


#Aggregating the africa data for total profit and CV

AFRICA_CONSUMER_aggr<-AFRICA_CONSUMER%>%
  group_by(AFRICA_CONSUMER$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="AFRICA_CONSUMER")

AFRICA_CORPORATE_aggr<-AFRICA_CORPORATE%>%
  group_by(AFRICA_CORPORATE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="AFRICA_CORPORATE")

AFRICA_HOMEOFFICE_aggr<-AFRICA_HOME_OFFICE%>%
  group_by(AFRICA_HOME_OFFICE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="AFRICA_HOMEOFFICE")

#Aggregating the APAC data for total profit and CV
APAC_CONSUMER_aggr<-APAC_CONSUMER%>%
  group_by(APAC_CONSUMER$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="APAC_CONSUMER")

APAC_CORPORATE_aggr<-APAC_CORPORATE%>%
  group_by(APAC_CORPORATE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="APAC_CORPORATE")

APAC_HOMEOFFICE_aggr<-APAC_HOME_OFFICE%>%
  group_by(APAC_HOME_OFFICE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="APAC_HOMEOFFICE")

#Aggregating the canada data for total profit and CV

CANADA_CONSUMER_aggr<-CANADA_CONSUMER%>%
  group_by(CANADA_CONSUMER$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="CANADA_CONSUMER")

CANADA_CORPORATE_aggr<-CANADA_CORPORATE%>%
  group_by(CANADA_CORPORATE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="CANADA_CORPORATE")

CANADA_HOMEOFFICE_aggr<-CANADA_HOME_OFFICE%>%
  group_by(CANADA_HOME_OFFICE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="CANADA_HOMEOFFICE")

#Aggregating the EMEA data for total profit and CV
EMEA_CONSUMER_aggr<-EMEA_CONSUMER%>%
  group_by(EMEA_CONSUMER$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="EMEA_CONSUMER")

EMEA_CORPORATE_aggr<-EMEA_CORPORATE%>%
  group_by(EMEA_CORPORATE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="EMEA_CORPORATE")

EMEA_HOMEOFFICE_aggr<-EMEA_HOME_OFFICE%>%
  group_by(EMEA_HOME_OFFICE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="EMEA_HOMEOFFICE")


#Aggregating the EU data for total profit and CV
EU_CONSUMER_aggr<-EU_CONSUMER%>%
  group_by(EU_CONSUMER$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="EU_CONSUMER")

EU_CORPORATE_aggr<-EU_CORPORATE%>%
  group_by(EU_CORPORATE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="EU_CORPORATE")

EU_HOMEOFFICE_aggr<-EU_HOME_OFFICE%>%
  group_by(EU_HOME_OFFICE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="EU_HOMEOFFICE")

#Aggregating the LATAM data for total profit and CV
LATAM_CONSUMER_aggr<-LATAM_CONSUMER%>%
  group_by(LATAM_CONSUMER$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="LATAM_CONSUMER")

LATAM_CORPORATE_aggr<-LATAM_CORPORATE%>%
  group_by(LATAM_CORPORATE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="LATAM_CORPORATE")

LATAM_HOMEOFFICE_aggr<-LATAM_HOME_OFFICE%>%
  group_by(LATAM_HOME_OFFICE$month )%>%
  summarise(monthly_profit=sum(Profit), monthly_sales=sum(Sales), monthly_quantity=sum(Quantity))%>%
  mutate(Net_profit=sum(monthly_profit), CV=sd(monthly_profit)/mean(monthly_profit), Market_Segment="LATAM_HOMEOFFICE")


#Combining all the dataframes for a combined dataset

retail_data_frame<-data.frame(cbind(US_CONSUMER_aggr$Net_profit[1], US_CONSUMER_aggr$CV[1], US_CONSUMER_aggr$Market_Segment[1]), stringsAsFactors = FALSE)

retail_data_frame<-rbind(retail_data_frame, c(US_CORPORATE_aggr$Net_profit[1], US_CORPORATE_aggr$CV[1], US_CORPORATE_aggr$Market_Segment[1]))

retail_data_frame<-rbind(retail_data_frame, c(US_HOMEOFFICE_aggr$Net_profit[1], US_HOMEOFFICE_aggr$CV[1], US_HOMEOFFICE_aggr$Market_Segment[1]))



retail_data_frame<-rbind(retail_data_frame,c(AFRICA_CONSUMER_aggr$Net_profit[1], AFRICA_CONSUMER_aggr$CV[1], AFRICA_CONSUMER_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(AFRICA_CORPORATE_aggr$Net_profit[1], AFRICA_CORPORATE_aggr$CV[1], AFRICA_CORPORATE_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(AFRICA_HOMEOFFICE_aggr$Net_profit[1], AFRICA_HOMEOFFICE_aggr$CV[1], AFRICA_HOMEOFFICE_aggr$Market_Segment[1]))



retail_data_frame<-rbind(retail_data_frame,c(APAC_CONSUMER_aggr$Net_profit[1], APAC_CONSUMER_aggr$CV[1], APAC_CONSUMER_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(APAC_CORPORATE_aggr$Net_profit[1], APAC_CORPORATE_aggr$CV[1], APAC_CORPORATE_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(APAC_HOMEOFFICE_aggr$Net_profit[1], APAC_HOMEOFFICE_aggr$CV[1], APAC_HOMEOFFICE_aggr$Market_Segment[1]))

retail_data_frame<-rbind(retail_data_frame,c(CANADA_CONSUMER_aggr$Net_profit[1], CANADA_CONSUMER_aggr$CV[1], CANADA_CONSUMER_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(CANADA_CORPORATE_aggr$Net_profit[1], CANADA_CORPORATE_aggr$CV[1], CANADA_CORPORATE_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(CANADA_HOMEOFFICE_aggr$Net_profit[1], CANADA_HOMEOFFICE_aggr$CV[1], CANADA_HOMEOFFICE_aggr$Market_Segment[1]))


retail_data_frame<-rbind(retail_data_frame,c(EMEA_CONSUMER_aggr$Net_profit[1], EMEA_CONSUMER_aggr$CV[1], EMEA_CONSUMER_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(EMEA_CORPORATE_aggr$Net_profit[1], EMEA_CORPORATE_aggr$CV[1], EMEA_CORPORATE_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(EMEA_HOMEOFFICE_aggr$Net_profit[1], EMEA_HOMEOFFICE_aggr$CV[1], EMEA_HOMEOFFICE_aggr$Market_Segment[1]))

retail_data_frame<-rbind(retail_data_frame,c(EU_CONSUMER_aggr$Net_profit[1], EU_CONSUMER_aggr$CV[1], EU_CONSUMER_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(EU_CORPORATE_aggr$Net_profit[1], EU_CORPORATE_aggr$CV[1], EU_CORPORATE_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(EU_HOMEOFFICE_aggr$Net_profit[1], EU_HOMEOFFICE_aggr$CV[1], EU_HOMEOFFICE_aggr$Market_Segment[1]))

retail_data_frame<-rbind(retail_data_frame,c(LATAM_CONSUMER_aggr$Net_profit[1], LATAM_CONSUMER_aggr$CV[1], LATAM_CONSUMER_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(LATAM_CORPORATE_aggr$Net_profit[1], LATAM_CORPORATE_aggr$CV[1], LATAM_CORPORATE_aggr$Market_Segment[1]))
retail_data_frame<-rbind(retail_data_frame, c(LATAM_HOMEOFFICE_aggr$Net_profit[1], LATAM_HOMEOFFICE_aggr$CV[1], LATAM_HOMEOFFICE_aggr$Market_Segment[1]))


colnames(retail_data_frame)<-c("Profit", "CV", "Market_Sector")

 retail_data_frame$Profit<-as.numeric(retail_data_frame$Profit)
 retail_data_frame$CV<-as.numeric(retail_data_frame$CV)

 arrange(retail_data_frame, desc(Profit), CV)
 
#The 2 most profitable (and consistent) segment from these 21 and forecast the sales and demand for these segments are
 
   # 222817.560 0.6321323     APAC_CONSUMER
   # 188687.707 0.6243052       EU_CONSUMER
 
 APAC_SALES<-data.frame(APAC_CONSUMER$month)
 APAC_SALES<-cbind(APAC_SALES, APAC_CONSUMER$Sales)
 colnames(APAC_SALES)<-c("month","Sales")
 APAC_SALES<-sqldf("select  month, sum(Sales) as Sales from APAC_SALES group by month")
 
 APAC_QUANTITY<-data.frame(APAC_CONSUMER$month)
 APAC_QUANTITY<-cbind(APAC_QUANTITY, APAC_CONSUMER$Quantity)
 colnames(APAC_QUANTITY)<-c("month","Quantity")
 
 APAC_QUANTITY<-sqldf("select  month, sum(Quantity) as Quantity from APAC_QUANTITY group by month")
 
 
####---------------TIME SERIES APAC SALES  ----------------------------------------------------
 
 #total_timeser <- ts(rawdata$Sales)
 APAC_SALES_ts<-ts(APAC_SALES$Sales)
 plot(APAC_SALES_ts)
 
 indata <- APAC_SALES[1:42,]
 timeser <- ts(indata$Sales)
 plot(timeser)
 
 w <-1
 smoothedseries <- stats::filter(timeser, 
                          filter=rep(1/(2*w+1),(2*w+1)), 
                          method='convolution', sides=2)
 
 #Smoothing left end of the time series
 
 diff <- smoothedseries[w+2] - smoothedseries[w+1]
 for (i in seq(w,1,-1)) {
   smoothedseries[i] <- smoothedseries[i+1] - diff
 }
 
 #Smoothing right end of the time series
 
 n <- length(timeser)
 diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
 for (i in seq(n-w+1, n)) {
   smoothedseries[i] <- smoothedseries[i-1] + diff
 }
 
 #Plot the smoothed time series
 
 timevals_in <- indata$month
 lines(smoothedseries, col="blue", lwd=2)
 
 
 #Building a model on the smoothed time series using classical decomposition
 #First, let's convert the time series to a dataframe
 
 smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
 colnames(smootheddf) <- c('month', 'sales')
 
 #Now, let's fit a multiplicative model with trend and seasonality to the data
 #Seasonality will be modeled using a sinusoid function
 
 lmfit <- lm(sales ~ sin(0.7*month) * poly(month,3) + cos(0.7*month) * poly(month,3)
             + month, data=smootheddf)
 global_pred <- predict(lmfit, month=timevals_in)
 summary(global_pred)
 lines(timevals_in, global_pred, col='red', lwd=2)
 
 
 #Now, let's look at the locally predictable series
 #We will model it as an ARMA series
 
 local_pred <- timeser-global_pred
 plot(local_pred, col='red', type = "l")
 acf(local_pred)
 acf(local_pred, type="partial")
 armafit <- auto.arima(local_pred)
 
 tsdiag(armafit)
 armafit
 
 #We'll check if the residual series is white noise
 
 resi <- local_pred-fitted(armafit)
 
 adf.test(resi,alternative = "stationary")
 kpss.test(resi)
 
 #Now, let's evaluate the model using MAPE
 #First, let's make a prediction for the last 6 months
 
 outdata <- APAC_SALES[43:48,]
 timevals_out <- outdata$month
 
 global_pred_out <- predict(lmfit,data.frame(month =timevals_out))
 
 fcast <- global_pred_out
 
 #Now, let's compare our prediction with the actual values, using MAPE
 
 MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
 MAPE_class_dec
 
 #Let's also plot the predictions along with original values, to
 #get a visual feel of the fit
 
 class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
 plot(APAC_SALES_ts, col = "black")
 lines(class_dec_pred, col = "red")
 
 #So, that was classical decomposition, now let's do an ARIMA fit
 
 autoarima <- auto.arima(timeser)
 autoarima
 tsdiag(autoarima)
 plot(autoarima$x, col="black")
 lines(fitted(autoarima), col="red")
 
 #Again, let's check if the residual series is white noise
 
 resi_auto_arima <- timeser - fitted(autoarima)
 
 adf.test(resi_auto_arima,alternative = "stationary")
 kpss.test(resi_auto_arima)
 
 #Also, let's evaluate the model using MAPE
 fcast_auto_arima <- predict(autoarima, n.ahead = 6)
 
 MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
 MAPE_auto_arima
 
 #Lastly, let's plot the predictions along with original values, to
 #get a visual feel of the fit
 
 auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
 plot(APAC_SALES_ts, col = "black")
 lines(auto_arima_pred, col = "red")
 
 sales=HoltWinters(APAC_SALES_ts,gamma = F) #Ideally we need to set values for alpha, beta and gamma,
 #but since we have level and trend component in our charts
 #we already knew that alpha and gamma will be true, hence we
 #didn't mention it in the function.
 plot(sales)
 sales
 rf=forecast:::forecast.HoltWinters(sales,h=6)# Forecasting for next 6 months
 rf
 plot(rf)
 
 
 #--------------Time Series for APAC QUANTITY--------------------------------

  APAC_QUANTITY_ts<-ts(APAC_QUANTITY$Quantity)
 plot(APAC_QUANTITY_ts)
 
 indata <- APAC_QUANTITY[1:42,]
 timeser <- ts(indata$Quantity)
 plot(timeser)
 
 w <-1
 smoothedseries <- stats::filter(timeser, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)
 
 #Smoothing left end of the time series
 
 diff <- smoothedseries[w+2] - smoothedseries[w+1]
 for (i in seq(w,1,-1)) {
   smoothedseries[i] <- smoothedseries[i+1] - diff
 }
 
 #Smoothing right end of the time series
 
 n <- length(timeser)
 diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
 for (i in seq(n-w+1, n)) {
   smoothedseries[i] <- smoothedseries[i-1] + diff
 }
 
 #Plot the smoothed time series
 
 timevals_in <- indata$month
 lines(smoothedseries, col="blue", lwd=2)
 
 
 #Building a model on the smoothed time series using classical decomposition
 #First, let's convert the time series to a dataframe
 
 smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
 colnames(smootheddf) <- c('month', 'Quantity')
 
 #Now, let's fit a multiplicative model with trend and seasonality to the data
 #Seasonality will be modeled using a sinusoid function
 
 lmfit <- lm(Quantity ~ sin(0.5*month) * poly(month,3) + cos(0.5*month) * poly(month,3)
             + month, data=smootheddf)
 global_pred <- predict(lmfit, month=timevals_in)
 summary(global_pred)
 lines(timevals_in, global_pred, col='red', lwd=2)
 
 
 #Now, let's look at the locally predictable series
 #We will model it as an ARMA series
 
 local_pred <- timeser-global_pred
 plot(local_pred, col='red', type = "l")
 acf(local_pred)
 acf(local_pred, type="partial")
 armafit <- auto.arima(local_pred)
 
 tsdiag(armafit)
 armafit
 
 #We'll check if the residual series is white noise
 
 resi <- local_pred-fitted(armafit)
 
 adf.test(resi,alternative = "stationary")
 kpss.test(resi)
 
 #Now, let's evaluate the model using MAPE
 #First, let's make a prediction for the last 6 months
 
 outdata <- APAC_QUANTITY[43:48,]
 timevals_out <- outdata$month
 
 global_pred_out <- predict(lmfit,data.frame(month =timevals_out))
 
 fcast <- global_pred_out
 
 #Now, let's compare our prediction with the actual values, using MAPE
 
 MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
 MAPE_class_dec
 
 #Let's also plot the predictions along with original values, to
 #get a visual feel of the fit
 
 class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
 plot(APAC_QUANTITY_ts, col = "black")
 lines(class_dec_pred, col = "red")
 
 #So, that was classical decomposition, now let's do an ARIMA fit
 
 autoarima <- auto.arima(timeser)
 autoarima
 tsdiag(autoarima)
 plot(autoarima$x, col="black")
 lines(fitted(autoarima), col="red")
 
 #Again, let's check if the residual series is white noise
 
 resi_auto_arima <- timeser - fitted(autoarima)
 
 adf.test(resi_auto_arima,alternative = "stationary")
 kpss.test(resi_auto_arima)
 
 #Also, let's evaluate the model using MAPE
 fcast_auto_arima <- predict(autoarima, n.ahead = 6)
 
 MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
 MAPE_auto_arima
 
 #Lastly, let's plot the predictions along with original values, to
 #get a visual feel of the fit
 
 auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
 plot(APAC_QUANTITY_ts, col = "black")
 lines(auto_arima_pred, col = "red")
 
 quantity=HoltWinters(APAC_QUANTITY_ts,gamma = F) #Ideally we need to set values for alpha, beta and gamma,
 #but since we have level and trend component in our charts
 #we already knew that alpha and gamma will be true, hence we
 #didn't mention it in the function.
 plot(quantity)
 quantity
 rf1=forecast:::forecast.HoltWinters(quantity,h=6)# Forecasting for next 6 months
 rf1
 plot(rf1)
 
 ########Modelling for EU SALES and EU QUANTITY------------------------
 
 EU_SALES<-data.frame(EU_CONSUMER$month)
 EU_SALES<-cbind(EU_SALES, EU_CONSUMER$Sales)
 colnames(EU_SALES)<-c("month","Sales")
 EU_SALES<-sqldf("select  month, sum(Sales) as Sales from EU_SALES group by month")
 
 EU_QUANTITY<-data.frame(EU_CONSUMER$month)
 EU_QUANTITY<-cbind(EU_QUANTITY, EU_CONSUMER$Quantity)
 colnames(EU_QUANTITY)<-c("month","Quantity")
 EU_QUANTITY<-sqldf("select  month, sum(Quantity) as Quantity from EU_QUANTITY group by month")
 
 
 #-----------------Time Series for EU_SALES----------------------------------
 
 #total_timeser <- ts(rawdata$Sales)
 EU_SALES_ts<-ts(EU_SALES$Sales)
 plot(EU_SALES_ts)
 
 indata <- EU_SALES[1:42,]
 timeser <- ts(indata$Sales)
 plot(timeser)
 
 w <-1
 smoothedseries <- stats::filter(timeser, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)
 
 #Smoothing left end of the time series
 
 diff <- smoothedseries[w+2] - smoothedseries[w+1]
 for (i in seq(w,1,-1)) {
   smoothedseries[i] <- smoothedseries[i+1] - diff
 }
 
 #Smoothing right end of the time series
 
 n <- length(timeser)
 diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
 for (i in seq(n-w+1, n)) {
   smoothedseries[i] <- smoothedseries[i-1] + diff
 }
 
 #Plot the smoothed time series
 
 timevals_in <- indata$month
 lines(smoothedseries, col="blue", lwd=2)
 
 
 #Building a model on the smoothed time series using classical decomposition
 #First, let's convert the time series to a dataframe
 
 smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
 colnames(smootheddf) <- c('month', 'sales')
 
 #Now, let's fit a multiplicative model with trend and seasonality to the data
 #Seasonality will be modeled using a sinusoid function
 
 lmfit <- lm(sales ~ sin(0.5*month) * poly(month,3) + cos(0.5*month) * poly(month,3)
             + month, data=smootheddf)
 global_pred <- predict(lmfit, month=timevals_in)
 summary(global_pred)
 lines(timevals_in, global_pred, col='red', lwd=2)
 
 
 #Now, let's look at the locally predictable series
 #We will model it as an ARMA series
 
 local_pred <- timeser-global_pred
 plot(local_pred, col='red', type = "l")
 acf(local_pred)
 acf(local_pred, type="partial")
 armafit <- auto.arima(local_pred)
 
 tsdiag(armafit)
 armafit
 
 #We'll check if the residual series is white noise
 
 resi <- local_pred-fitted(armafit)
 
 adf.test(resi,alternative = "stationary")
 kpss.test(resi)
 
 #Now, let's evaluate the model using MAPE
 #First, let's make a prediction for the last 6 months
 
 outdata <- EU_SALES[43:48,]
 timevals_out <- outdata$month
 
 global_pred_out <- predict(lmfit,data.frame(month =timevals_out))
 
 fcast <- global_pred_out
 
 #Now, let's compare our prediction with the actual values, using MAPE
 
 MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
 MAPE_class_dec
 
 #Let's also plot the predictions along with original values, to
 #get a visual feel of the fit
 
 class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
 plot(EU_SALES_ts, col = "black")
 lines(class_dec_pred, col = "red")
 
 #So, that was classical decomposition, now let's do an ARIMA fit
 
 autoarima <- auto.arima(timeser)
 autoarima
 tsdiag(autoarima)
 plot(autoarima$x, col="black")
 lines(fitted(autoarima), col="red")
 
 #Again, let's check if the residual series is white noise
 
 resi_auto_arima <- timeser - fitted(autoarima)
 
 adf.test(resi_auto_arima,alternative = "stationary")
 kpss.test(resi_auto_arima)
 
 #Also, let's evaluate the model using MAPE
 fcast_auto_arima <- predict(autoarima, n.ahead = 6)
 
 MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
 MAPE_auto_arima
 
 #Lastly, let's plot the predictions along with original values, to
 #get a visual feel of the fit
 
 auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
 plot(EU_SALES_ts, col = "black")
 lines(auto_arima_pred, col = "red")
 
 eu_sales=HoltWinters(EU_SALES_ts,gamma = F) #Ideally we need to set values for alpha, beta and gamma,
 #but since we have level and trend component in our charts
 #we already knew that alpha and gamma will be true, hence we
 #didn't mention it in the function.
 plot(eu_sales)
 eu_sales
 rf2=forecast:::forecast.HoltWinters(eu_sales,h=6)# Forecasting for next 6 months
 rf2
 plot(rf2)

 
 #-----------------Time Series for EU_QUANTITY----------------------------------
 
 #total_timeser <- ts(rawdata$Sales)
 EU_QUANTITY_ts<-ts(EU_QUANTITY$Quantity)
 plot(EU_QUANTITY_ts)
 
 indata <- EU_QUANTITY[1:42,]
 timeser <- ts(indata$Quantity)
 plot(timeser)
 
 w <-1
 smoothedseries <- stats::filter(timeser, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)
 
 #Smoothing left end of the time series
 
 diff <- smoothedseries[w+2] - smoothedseries[w+1]
 for (i in seq(w,1,-1)) {
   smoothedseries[i] <- smoothedseries[i+1] - diff
 }
 
 #Smoothing right end of the time series
 
 n <- length(timeser)
 diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
 for (i in seq(n-w+1, n)) {
   smoothedseries[i] <- smoothedseries[i-1] + diff
 }
 
 #Plot the smoothed time series
 
 timevals_in <- indata$month
 lines(smoothedseries, col="blue", lwd=2)
 
 
 #Building a model on the smoothed time series using classical decomposition
 #First, let's convert the time series to a dataframe
 
 smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
 colnames(smootheddf) <- c('month', 'Quantity')
 
 #Now, let's fit a multiplicative model with trend and seasonality to the data
 #Seasonality will be modeled using a sinusoid function
 
 lmfit <- lm(Quantity ~ sin(0.5*month) * poly(month,3) + cos(0.5*month) * poly(month,3)
             + month, data=smootheddf)
 global_pred <- predict(lmfit, month=timevals_in)
 summary(global_pred)
 lines(timevals_in, global_pred, col='red', lwd=2)
 
 
 #Now, let's look at the locally predictable series
 #We will model it as an ARMA series
 
 local_pred <- timeser-global_pred
 plot(local_pred, col='red', type = "l")
 acf(local_pred)
 acf(local_pred, type="partial")
 armafit <- auto.arima(local_pred)
 
 tsdiag(armafit)
 armafit
 
 #We'll check if the residual series is white noise
 
 resi <- local_pred-fitted(armafit)
 
 adf.test(resi,alternative = "stationary")
 kpss.test(resi)
 
 #Now, let's evaluate the model using MAPE
 #First, let's make a prediction for the last 6 months
 
 outdata <- EU_QUANTITY[43:48,]
 timevals_out <- outdata$month
 
 global_pred_out <- predict(lmfit,data.frame(month =timevals_out))
 
 fcast <- global_pred_out
 
 #Now, let's compare our prediction with the actual values, using MAPE
 
 MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
 MAPE_class_dec
 
 #Let's also plot the predictions along with original values, to
 #get a visual feel of the fit
 
 class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
 plot(EU_QUANTITY_ts, col = "black")
 lines(class_dec_pred, col = "red")
 
 #So, that was classical decomposition, now let's do an ARIMA fit
 
 autoarima <- auto.arima(timeser)
 autoarima
 tsdiag(autoarima)
 plot(autoarima$x, col="black")
 lines(fitted(autoarima), col="red")
 
 #Again, let's check if the residual series is white noise
 
 resi_auto_arima <- timeser - fitted(autoarima)
 
 adf.test(resi_auto_arima,alternative = "stationary")
 kpss.test(resi_auto_arima)
 
 #Also, let's evaluate the model using MAPE
 fcast_auto_arima <- predict(autoarima, n.ahead = 6)
 
 MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
 MAPE_auto_arima
 
 #Lastly, let's plot the predictions along with original values, to
 #get a visual feel of the fit
 
 auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
 plot(EU_QUANTITY_ts, col = "black")
 lines(auto_arima_pred, col = "red")
 
 eu_quantity=HoltWinters(EU_QUANTITY_ts,gamma = F) #Ideally we need to set values for alpha, beta and gamma,
 #but since we have level and trend component in our charts
 #we already knew that alpha and gamma will be true, hence we
 #didn't mention it in the function.
 plot(eu_quantity)
 eu_quantity
 rf3=forecast:::forecast.HoltWinters(eu_quantity,h=6)# Forecasting for next 6 months
 rf3
 plot(rf3)