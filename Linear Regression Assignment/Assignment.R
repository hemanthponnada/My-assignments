

#------Loading the required packages----------

library(tidyverse)
library(dplyr)
library(tidyr)
library(car)
library(MASS)

#------Importing the dataset--------------

dataset=read.csv("CarPrice_Assignment.csv")
summary(dataset)
str(dataset) 
View(dataset)

#------------------------Data cleaning-----------------------

#----Checking for missing values-----
sapply(dataset,function(x){sum(is.na(x))})
#There are no missing values


#-----Checking for duplicate values -------

distinct_data<-distinct(dataset)
nrow(dataset)
nrow(distinct_data)

#The values of nrow(dataset) and nrow(distinct_data) is the same. So there are no duplicate values.


#----------Seperating the Carname and model name from carname coloumn -------------

dataset<-separate(dataset, CarName, into = c("CarName","Model"), sep = "\\ ", extra = "merge", fill = "right")


#Removing the car model and the car id columns 

dataset$Model<-NULL
dataset$car_ID<-NULL

#Correcting the spellings of certain car names

unique(dataset$CarName)
dataset$CarName<-gsub("maxda", "mazda", dataset$CarName)
dataset$CarName<-gsub("Nissan", "nissan", dataset$CarName)
dataset$CarName<-gsub("porcshce", "porsche", dataset$CarName)
dataset$CarName<-gsub("toyouta", "toyota", dataset$CarName)
dataset$CarName<-gsub("vokswagen", "volkswagen", dataset$CarName)
dataset$CarName<-gsub("vw", "volkswagen", dataset$CarName)

#----------------Converting factors with two levels to numerical variables-------------------

#-------Fuel Type------------
unique(dataset$fueltype)

dataset$fueltype= factor(dataset$fueltype ,
                         levels = c('gas', 'diesel'),
                         labels = c(0,1))
dataset$fueltype<-as.numeric(dataset$fueltype)


#---------Aspiration----------
unique(dataset$aspiration)

dataset$aspiration = factor(dataset$aspiration ,
                         levels = c('std', 'turbo'),
                         labels = c(0,1))
dataset$aspiration<-as.numeric(dataset$aspiration)

#---------Door number-------------
unique(dataset$doornumber)

dataset$doornumber = factor(dataset$doornumber ,
                         levels = c('two', 'four'),
                         labels = c(2,4))
dataset$doornumber<-as.numeric(dataset$doornumber)

#--------Engine location----------------
unique(dataset$enginelocation)

dataset$enginelocation = factor(dataset$enginelocation, 
                                levels = c('front', 'rear'),
                                labels = c(0,1))
dataset$enginelocation<-as.numeric(dataset$enginelocation)

#--------------------------Dummy Variables creation-----------------------------------

#Dummy variable creation for car body
unique(dataset$carbody)
dummy_1 <- data.frame(model.matrix( ~carbody, data = dataset))
View(dummy_1)
dummy_1<-dummy_1[,-1]
dataset<-cbind(dataset[,-6],dummy_1)

#Dummy variable creation for drive wheel
unique(dataset$drivewheel)
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = dataset))
View(dummy_2)
dummy_2<-dummy_2[,-1]
dataset<-cbind(dataset[,-6], dummy_2)

#Dummy variable creation for Engine type
unique(dataset$enginetype)
dummy_3<-data.frame(model.matrix(~enginetype, data = dataset))
View(dummy_3)
dummy_3<-dummy_3[,-1]
dataset<-cbind(dataset[,-12], dummy_3)

#Dummy variable creation for cylinder number
unique(dataset$cylindernumber)
dummy_4<-data.frame(model.matrix(~cylindernumber, data = dataset))
View(dummy_4)
dummy_4<-dummy_4[,-1]
dataset<-cbind(dataset[,-12], dummy_4)

#Dummy variable creation for fuel system
unique(dataset$fuelsystem)
dummy_5<-data.frame(model.matrix(~fuelsystem, data = dataset))
View(dummy_5)
dummy_5<-dummy_5[,-1]
dataset<-cbind(dataset[,-13], dummy_5)


#Dummy variable creation for company name

unique(dataset$CarName)
dummy_6<-data.frame(model.matrix(~CarName, data = dataset))
View(dummy_6)
dummy_6<-dummy_6[,-1]
dataset<-cbind(dataset[,-2], dummy_6)


#------------------Seperating dataset into Train and test data --------------------------

#set the seed to 100 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(dataset), 0.7*nrow(dataset))

# Generating the train dataset
train = dataset[trainindices,]

#Generating the test dataset
test = dataset[-trainindices,]



#-------------------------Model Building---------------------------------

#Building the first model
model_1 <-lm(price~.,data=train)

summary(model_1)


step <- stepAIC(model_1, direction="both")

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Storing the last model equation of stepwise method into an object called model_2


#-------------------Model 2 creation----------------------

model_2<-lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
              enginesize + boreratio + stroke + horsepower + peakrpm + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
              enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
              fuelsystem2bbl + fuelsystemmpfi + CarNamebmw + CarNamebuick + 
              CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)


summary(model_2)

v_2m<-vif(model_2)

View(v_2m)
#Storing the VIF values in a variable so that it becomes easy to sort from decreasing to increasing when viewing the variables


#---------------------Model 3 creation----------------------

#In model2 the variables bore ratio, stroke, horse power, engine type ohc, cylinder number five, fuel system mpfi, car name saab all these 
  #variables have a high p value(>0.05) and also high VIF(>2)
#Removing bore ratio, stroke, horsepower, enginetype ohc, cylinder number five, fuelsystemmpfi, carnamesaab as they are highly insignificant

model_3<-lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
              enginesize + peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelrwd + enginetypedohcv + enginetypel + 
              enginetypeohcf + enginetyperotor + cylindernumberthree + 
              fuelsystem2bbl +  CarNamebmw + CarNamebuick + 
              CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNametoyota + CarNamevolkswagen, data = train)

summary(model_3)

v_3m<-vif(model_3)

View(v_3m)

#-----------------------Model 4 creation ------------------------

#In model 3 the variables curb weight, engine type l, cylinder number three, fuel system 2bbl have a high p value(>0.005)
#But the VIF value of cylinder number three is small(<2). So we are not removing that variable. 
#Removing the variables curbweight, enginetype1, fuelsystem2bbl

model_4<-lm(price ~ aspiration + enginelocation + carwidth + enginesize + peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelrwd + enginetypedohcv + enginetypeohcf + enginetyperotor + cylindernumberthree + 
              CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNametoyota + CarNamevolkswagen, data = train)


summary(model_4)

v_4m<-vif(model_4)

View(v_4m)



#--------------------Model 5 creation -----------------------------

#In model number 4, the variable cylinder number three has a very high p value(>0.05)

#Removing the variable cylinder number 3

model_5<-lm(price ~ aspiration + enginelocation + carwidth + enginesize + peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelrwd + enginetypedohcv + enginetypeohcf + enginetyperotor + 
              CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNametoyota + CarNamevolkswagen, data = train)


summary(model_5)

v_5m<-vif(model_5)

View(v_5m)

#-----------------Model 6 creation---------------------

#In model 5, all the variables have a signoficant p value(<0.05). But the variable carbodysedan has a very high VIF Value(>2)

#Removing the variable carbodysedan as it has high vif value 

model_6<-lm(price ~ aspiration + enginelocation + carwidth + enginesize + peakrpm + carbodyhardtop + carbodyhatchback +carbodywagon + 
              drivewheelrwd + enginetypedohcv + enginetypeohcf + enginetyperotor + 
              CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNametoyota + CarNamevolkswagen, data = train)


summary(model_6)

v_6m<-vif(model_6)

View(v_6m)


#-----------------------------Model 7 creation-------------------------------

#In model 6 the variables, carbodyhardtop, carbodyhatchback and carbody wagon have a very high p value(>0.05)
#Removing variables carbodyhardtop, carbody hatchback, carbody wagon as they have a very high p value

model_7<-lm(price ~ aspiration + enginelocation + carwidth + enginesize + peakrpm + 
              drivewheelrwd + enginetypedohcv + enginetypeohcf + enginetyperotor + 
              CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNametoyota + CarNamevolkswagen, data = train)


summary(model_7)

v_7m<-vif(model_7)

View(v_7m)

#----------------------------Model 8 creation---------------------------

#All the varriables in model 7 creation have a low p value(<0.05)
#But the variable engine size has a very high VIF value(>2). So we remove that variable. 
#Removing variable enginesize

model_8<-lm(price ~ aspiration + enginelocation + carwidth +peakrpm + 
              drivewheelrwd + enginetypedohcv + enginetypeohcf + enginetyperotor + 
              CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNametoyota + CarNamevolkswagen, data = train)


summary(model_8)

v_8m<-vif(model_8)

View(v_8m)

#---------------------Model 9 creation--------------------

#In Model 8 the variables aspiration, drivewheelrwd, enginetyperotor, caernamedodge, carnamenissan have a very high pvalue(>0.05)
#But the variable drivewheelrwd has only a high VIF value(>2)
#Rest of the variables have a very low VIF value(<2). So we are only removing the drivewheelrwd variable
#Removing drivewheelrwd variable

model_9<-lm(price ~ aspiration + enginelocation + carwidth +peakrpm + 
              enginetypedohcv + enginetypeohcf + enginetyperotor + 
              CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
              CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
              CarNametoyota + CarNamevolkswagen, data = train)


summary(model_9)

v_9m<-vif(model_9)

View(v_9m)

#---------------------Model 10 creation----------------------------

#In model 9 the variables aspiration and enginetyperotor have a very high pvalue(>0.05)
#So we remove these two variables.
#Removing aspiration, enginetyperotor variables

model_10<-lm(price ~  enginelocation + carwidth +peakrpm + 
               enginetypedohcv + enginetypeohcf + CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + CarNamejaguar + CarNamemazda + 
               CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
               CarNametoyota + CarNamevolkswagen, data = train)


summary(model_10)

v_10m<-vif(model_10)

View(v_10m)

#---------------------Model 11 creation----------------------------

#In model 10, the variables enginetypedohcv, carnamedodge have a very high p value(>0.05)

#Removing the variables enginetypedohcv, carnamedodge

model_11<-lm(price ~  enginelocation + carwidth +peakrpm + 
               enginetypeohcf + CarNamebmw + CarNamebuick +  CarNamehonda + CarNamejaguar + CarNamemazda + 
               CarNamemitsubishi + CarNamenissan + CarNameplymouth + CarNamerenault + 
               CarNametoyota + CarNamevolkswagen, data = train)


summary(model_11)

v_11m<-vif(model_11)

View(v_11m)

#---------------------Model 12 creation----------------------------
#In model 11 carnamenissan has a very high p value(>0.05)

#Removing the variable carnamenissan

model_12<-lm(price ~  enginelocation + carwidth +peakrpm + 
               enginetypeohcf + CarNamebmw + CarNamebuick +  CarNamehonda + CarNamejaguar + CarNamemazda + 
               CarNamemitsubishi + CarNameplymouth + CarNamerenault + 
               CarNametoyota + CarNamevolkswagen, data = train)


summary(model_12)

v_12m<-vif(model_12)

View(v_12m)


#---------------------Model 13 creation----------------------------

#In model 12 the variables enginetypeohcf, carnameplymouth, carnametoyota have a very high p value(>0.05)

#Removing the variables enginetypeohcf, carnameplymouth, carnametoyota

model_13<-lm(price ~  enginelocation + carwidth +peakrpm + 
               CarNamebmw + CarNamebuick +  CarNamehonda + CarNamejaguar + CarNamemazda + 
               CarNamemitsubishi +  CarNamerenault +CarNamevolkswagen, data = train)


summary(model_13)

v_13m<-vif(model_13)

View(v_13m)

#---------------------Model 14 creation----------------------------

#In model 13 carnamevolkswagen has a very high p value(>0.05)
#Removing the variable carnamevolkwagen

model_14<-lm(price ~  enginelocation + carwidth +peakrpm + 
               CarNamebmw + CarNamebuick +  CarNamehonda + CarNamejaguar + CarNamemazda + 
               CarNamemitsubishi +  CarNamerenault , data = train)


summary(model_14)

v_14m<-vif(model_14)

View(v_14m)

#---------------------Model 15 creation----------------------------

#In model 14 carnamehonda has a very high p value(>0.05)

#Removing the variable carnamehonda

model_15<-lm(price ~  enginelocation + carwidth +peakrpm + 
               CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
               CarNamemitsubishi +  CarNamerenault , data = train)


summary(model_15)

v_15m<-vif(model_15)

View(v_15m)

#---------------------Model 16 creation----------------------------

#In model 15 all the varibles have p values <0.05 and all the VIF values are <2
#To make the model more efficent we remove the varibles with * (>0.01) from the model 15
#Removing the variables CarNamemazda, CarNamemitsubishi, CarNamerenault

model_16<-lm(price ~  enginelocation + carwidth +peakrpm +CarNamebmw + CarNamebuick + CarNamejaguar  , data = train)


summary(model_16)

v_16m<-vif(model_16)

View(v_16m)


#---------------------Model 17 creation----------------------------

#In model 16 all the varibles have p values <0.05 and all the VIF values are <2
#To make the model more efficent we remove the varibles with ** (>0.001) from the model 16
#Removing the variables peakrpm

model_17<-lm(price ~  enginelocation + carwidth  +CarNamebmw + CarNamebuick + CarNamejaguar  , data = train)


summary(model_17)

v_17m<-vif(model_17)

View(v_17m)

#Now all the variables have perfect p values(with ***) and the VIF is <2 for all the variables

# predicting the results in test dataset
Predict_1 <- predict(model_17,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted prices. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared