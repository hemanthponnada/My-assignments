library(lubridate)
library(ggplot2)
library(gridExtra)
library(ggthemes)

#setting the current working directory
setwd( "C:/Users/saimohan/Documents/Data Science Course material UPGRAD/Uber CaseStudy")

#Reading the csv file from the current working directory
uber1<-read.csv("Uber Request Data.csv")


#--------------------------Data Cleaning-------------------------------

uber1$Request.timestamp<-parse_date_time(x=uber1$Request.timestamp, orders = c("d/m/Y H:M:S" , "d/m/Y H:M","d-m-Y H:M:S","d-m-Y H:M"))

uber1$Drop.timestamp <-parse_date_time(x=uber1$Drop.timestamp, orders = c("d/m/Y H:M:S" , "d/m/Y H:M","d-m-Y H:M:S","d-m-Y H:M"))

#Seperating Request Date and time from the Request Timestamp
uber1$Request.day<-format(as.POSIXct(uber1$Request.timestamp, format="%Y:%m:%d %H:%M:%S"),"%Y-%m-%d")

uber1$Request.Time<-format(as.POSIXct(uber1$Request.timestamp, format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")

#seperating Drop Date and time from the Drop Timestamp
uber1$Drop.day<-format(as.POSIXct(uber1$Drop.timestamp, format="%Y:%m:%d %H:%M:%S"),"%Y-%m-%d")

uber1$Drop.Time<-format(as.POSIXct(uber1$Drop.timestamp, format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")

#Calculating the trip time for each individual trip
uber1$Trip.time<-uber1$Drop.timestamp-uber1$Request.timestamp
uber1$Trip.time<-round(uber1$Trip.time)

#seperating the Hours from the Request time and Drop time
uber1$Request.hour = format(as.POSIXct(uber1$Request.Time,format="%H:%M:%S"),"%H")

uber1$Drop.hour= format(as.POSIXct(uber1$Drop.Time,format="%H:%M:%S"),"%H")

#-------------Trends for both Day wise and hourly plots---------------------

#Plots determining the day wise trends

#Day wise trends at the airport
ggplot(subset(uber1,uber1$Pickup.point=="Airport"), aes(x=Request.day, fill=Status))+geom_bar(stat = "count", position = position_dodge())+ggtitle("Day wise trends at the Airport")+xlab("Date")

#Day wise trends at the City
ggplot(subset(uber1,uber1$Pickup.point=="City"), aes(x=Request.day, fill=Status))+geom_bar(stat = "count", position = position_dodge())+ggtitle("Day wise trends at the City")+xlab("Date")

#Combined plot describing the day wise trends at the airport and city
ggplot(uber1,aes(Request.day))+geom_bar(fill="sky blue")+facet_wrap(~Pickup.point)+theme_get()

#Plots determining the hourly trends

#Hourly trends at the airport
ggplot(subset(uber1,uber1$Pickup.point=="Airport"), aes(x=Request.hour, fill=Status))+geom_bar(stat = "count",width = 1, position = position_dodge())+ggtitle("Hourly trends at the Airport")+xlab("Hours")

#Hourly trends at the City
ggplot(subset(uber1,uber1$Pickup.point=="City"), aes(x=Request.hour, fill=Status))+geom_bar(stat = "count",width = 1, position = position_dodge())+ggtitle("Hourly trends at City")+xlab("Hours")

#Combined Plot describing the hourly trends at the airport and city
ggplot(uber1,aes(Request.hour))+geom_bar(fill="light green")+facet_wrap(~Pickup.point)+theme_gray()

#Cancelled status at city
ggplot(subset(subset(uber1, uber1$Pickup.point=="City"),Status=="Cancelled" ),aes(x=Request.hour))+geom_bar(stat = "count",fill="green")+ggtitle("Number of cancelled at city")+geom_text(stat = "count",aes(label=..count..),vjust=-0.5)
#cancelled status at airport
ggplot(subset(subset(uber1, uber1$Pickup.point=="Airport"),Status=="Cancelled" ),aes(x=Request.hour))+geom_bar(stat = "count", fill="blue")+ggtitle("Number of cancelled at airport")+ geom_text(stat = "count", aes(label=..count..),vjust=-0.5)

#No cars available at city
ggplot(subset(subset(uber1, uber1$Pickup.point=="City"),Status=="No Cars Available" ),aes(x=Request.hour))+geom_bar(stat = "count", fill="green")+ggtitle("Number of cars not available at city")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5)
#NO cars available at Airport
ggplot(subset(subset(uber1, uber1$Pickup.point=="Airport"),Status=="No Cars Available" ),aes(x=Request.hour))+geom_bar(stat = "count",fill="blue")+ggtitle("Number of cars not available at the Airport")+geom_text(stat = "count", aes(label=..count..), vjust=-0.5)


#------------------Qunatifying and forecasting supply and demand gap-------------------------------------

#Demand at the airport

airport_demand<-ggplot(subset(uber1, uber1$Pickup.point=="Airport"), aes(Request.hour))+geom_bar(fill="blue")+ggtitle("Request Demand at the airport")+ylim(0,500) + scale_x_discrete(labels = abbreviate)

#Supply at the airport
airport_supply<-ggplot(subset(subset(uber1, uber1$Pickup.point=="Airport"),!is.na(Drop.hour)),aes(Drop.hour))+geom_bar(fill="green")+ggtitle("Request supply at the airport")+ylim(0,500)+ scale_x_discrete(labels = abbreviate)

grid.arrange(airport_demand,airport_supply,ncol=2)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = abbreviate)


#Demand at city
city_demand<-ggplot(subset(uber1, uber1$Pickup.point=="City"), aes(Request.hour))+geom_bar(fill="blue")+ggtitle("Request Demand at city")+theme(plot.title = element_text(hjust = 1))+ylim(0,400)

#Supply at city
city_supply<-ggplot(subset(subset(uber1, uber1$Pickup.point=="City"),!is.na(Drop.hour)),aes(Drop.hour))+geom_bar(fill="green")+ggtitle("Request supply at City")+theme(plot.title = element_text(hjust = 1))+ylim(0,400)

grid.arrange(city_demand,city_supply,ncol=2)

#-------------Finding out the gap between supply and demand at Airport and City----------------------------------

#Finding out Demand count at the airport
Airport_DC<-sqldf('select [Request.hour], count(*) No_Demand from uber1 where [Pickup.point]="Airport" group by [Request.hour]', method="name_class")

#Finding out Supply count at the airport
Airport_SC<-sqldf('select [Drop.hour], count(*) No_Supply from uber1 where [Pickup.point]="Airport" and Status="Trip Completed" group by [Drop.hour]', method="name_class")

Airport_Gap<-merge(Airport_DC,Airport_SC, by.x = "Request.hour",by.y = "Drop.hour")


Airport_Gap$Gap_No<-Airport_Gap$No_Demand-Airport_Gap$No_Supply

#Plotting the Demand and supply gap at the Airport
ggplot(Airport_Gap, aes(x=Request.hour,y=Gap_No))+geom_bar(stat = "identity",fill='steelblue')+ggtitle("Demand and Supply Gap at the Airport")+geom_text( aes(label=Gap_No), vjust=-0.5)+ xlab("Hours")+ylab("Gap") + theme_gray()

#Finding out Demand count at City
City_DC<-sqldf('select [Request.hour], count(*) No_Demand from uber1 where [Pickup.point]="City" group by [Request.hour]', method="name_class")

#Finding out Supply count at City
City_SC<-sqldf('select [Drop.hour], count(*) No_Supply from uber1 where [Pickup.point]="City" and Status="Trip Completed" group by [Drop.hour]', method="name_class")

City_Gap<-merge(City_DC,City_SC, by.x = "Request.hour",by.y = "Drop.hour")


City_Gap$Gap_No<-City_Gap$No_Demand-City_Gap$No_Supply

#Plotting the Demand and supply gap at City

ggplot(City_Gap, aes(x=Request.hour,y=Gap_No))+geom_bar(stat = "identity",fill='Green')+ggtitle("Demand and Supply Gap at City")+geom_text( aes(label=Gap_No), vjust=-0.5)+xlab("Hours")+ylab("Gap")+theme_gray()


write.csv(uber1,file = "uber1.csv")


#---------------------Recommendations based on the analysis--------------------------------------------------------------------------------------

#The two major problems are trip cancellations from city  and cars not being available at the airport.

#No. of cars not available were high at airport in the evenings and no. of cancellations from city are high in the mornings.

#To tackle this problem, if the drivers who are cancelling in the mornings from city were asked to complete trips from airport in the evenings.

#This would resolve the problem of no cars available at the airport in the evenings as this is the peak time at the airport. This may reduce the demand supply gap at the airport.

#To tackle the problem of cancellations in the mornings from city, if the drivers who have taken trips from airport in the evenings and late nights were asked to take trips in the mornings the next day may solve this problem. This may reduce the demand supply gap at the city.

#Also, it is advisable to increase the number of cabs and also compensate the drivers in their waiting time, so the number of cancellations decreases.


# The most problematic types of requests are from city to the airport.
#Beacause there are more number of cancellations from city to the airport.

#The demand supply gap is higher at the airport-city requests at the 6 PM time slot.




