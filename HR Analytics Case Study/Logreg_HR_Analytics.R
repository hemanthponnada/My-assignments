################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:
#A large company named XYZ, which employs around 4000 employees has attirtion rate around 15%
#This level of attrition is costing time,money and market reputation to company.
#Hence company is expecting from our HR analytics firm to understand what factors they should
#focus on in order to curb attirition

## AIM:

#To automate the prediction of probability of attrition

################################################################################

# Cleaning the Environment
rm(list=ls())
gc()

### Data Understanding

# HR Analytics Case Study - Probability of Employee Attrition

# Loading all the given datasets --------------------

empl_survey  <- read.csv("employee_survey_data.csv")
gen_data     <- read.csv("general_data.csv")
mangr_survey <- read.csv("manager_survey_data.csv")
in_time      <- read.csv("in_time.csv")
out_time     <- read.csv("out_time.csv")

# Common Dates available for In-Time and Out-Time datasets
length(setdiff(names(in_time),names(out_time)))
#0

# Each of the datasets have 4410 Ids (All datasets have unique Employee IDs)
length(unique(empl_survey$EmployeeID))
length(unique(gen_data$EmployeeID))
length(unique(mangr_survey$EmployeeID))
length(unique(in_time$X))
length(unique(out_time$X))

# Calculating average working hours ------------------
install.packages("janitor")
install.packages("lubridate")
library(janitor)
library(lubridate)

# Removing the holidays from in_time and out_time
in_time <- remove_empty(in_time, which = ("cols")) #Janitor package is used
out_time <- remove_empty(out_time, which = ("cols")) #Janitor package is used

# Converting the in_time and out_time to standard date formats
in_time[,2:250] <- lapply(in_time[,2:250], function(x) as_datetime(x))
out_time[,2:250] <- lapply(out_time[,2:250], function(x) as_datetime(x))

# Calculating the working hours 
working_hours <- out_time[,2:250]-in_time[,2:250]

# Binding the employee id with the particluar number of working hours
working_hours <- cbind(in_time$X, working_hours)

# Calculating average working hours for each employee using row means
working_hours[,2:250] <- lapply(working_hours[,2:250], function(x) as.numeric(x))
working_hours$Avg_working_hours <- rowMeans(working_hours[,2:250], na.rm = TRUE, dims = 1)
colnames(working_hours)[1]<-"EmployeeID"
working_hours <- working_hours[,c('EmployeeID','Avg_working_hours')]

# Removing Raw in_time & out_time dataset
rm(in_time,out_time)

# Combining datasets ---------------------

collate_data <- merge(empl_survey,gen_data,by="EmployeeID")
collate_data <- merge(collate_data,mangr_survey,by="EmployeeID")
collate_data <- merge(collate_data,working_hours,by="EmployeeID")

# Removing Employee/Manages survey, general data and working hours datasets
rm(empl_survey,mangr_survey,gen_data,working_hours)

# Data at Glance -------------------------

library(data.table)
summary_vars <- function(dt) {
  dset <- data.table(Variable=names(dt),
                  Class = sapply(dt, function(x) class(x)),
                  CountNA = sapply(dt, function(x) sum(is.na(x))),
                  CountBlank = sapply(dt, function(x) sum(as.character(x)=="")),
                  CountUnique = sapply(dt, function(x) length(unique(x)))
                  )
  return(dset)
}

dset <- summary_vars(collate_data)

# Zero VARIATION variables ------------------------

dset$Variable[which(dset$CountUnique==1)]
# Removing 'EmployeeCount', 'over18' & 'standard hours' (No Variation in the values) 
collate_data$Over18<-NULL
collate_data$StandardHours<-NULL
collate_data$EmployeeCount<-NULL

# MISSING values with count ---------------------

as.data.frame(paste(dset$Variable,dset$CountNA,sep="-")[which(dset$CountNA>0)])
# "EnvironmentSatisfaction-25" 
# "JobSatisfaction-20"         
# "WorkLifeBalance-38"         
# "NumCompaniesWorked-19" 
# "TotalWorkingYears-9" 

# Imputation using "MEDIAN" ---------------------

#Continuous variables having missing values ("NumCompaniesWorked" & "TotalWorkingYears") 
collate_data$NumCompaniesWorked[is.na(collate_data$NumCompaniesWorked)] <- median(collate_data$NumCompaniesWorked, na.rm=TRUE)
collate_data$TotalWorkingYears[is.na(collate_data$TotalWorkingYears)] <- median(collate_data$TotalWorkingYears, na.rm=TRUE)

# Imputation using "MICE"  ---------------------

# Discrete Categorical Variable having missing values 
# ("EnvironmentSatisfaction" , "JobSatisfaction" & "WorkLifeBalance")         

# (i) Subsetting Data for Imputation (Vars: empl_survey data)
Impute_data <- collate_data[,c("EmployeeID","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance")]
Impute_data[,2:4] <- lapply(Impute_data[,2:4],factor)

# (ii) Creating prediction matrix
pred <- 1 - diag(1, ncol(Impute_data))
pred 
dimnames(pred) <- list(names(Impute_data),names(Impute_data))
pred

# (iii) Excluding irrelevant variable as a predictor (i.e.; EmployeeID)
pred[, "EmployeeID"] = 0
pred 

# (iv) Loading Mice package & Creating Imputed values
install.packages("mice")
library(mice)
imp <- mice(Impute_data,m=1,predictorMatrix = pred, method = "polyreg",
            printFlag=FALSE, maxit = 1, seed=100)
# m - no. of imputed datasets , here its just 1
# method = polyreg(Bayesian polytomous regression) - For Factor Variables (>= 2 levels)

# (v) Completing the missing obsvn dataset using above Imputed values
Complete_data <- complete(imp,1)
#(no missing values after imputation)
sapply(Complete_data,function(x) sum(is.na(x)))

# (vi) Checking pre & post Imputation data distribution
prop.table(table(collate_data$EnvironmentSatisfaction,collate_data$JobSatisfaction),1)
prop.table(table(Complete_data$EnvironmentSatisfaction,Complete_data$JobSatisfaction),1)

prop.table(table(collate_data$EnvironmentSatisfaction,collate_data$WorkLifeBalance),1)
prop.table(table(Complete_data$EnvironmentSatisfaction,Complete_data$WorkLifeBalance),1)

prop.table(table(collate_data$JobSatisfaction,collate_data$WorkLifeBalance),1)
prop.table(table(Complete_data$JobSatisfaction,Complete_data$WorkLifeBalance),1)
# Not much of a change in distribution after imputation

# (vii) Joining the imputed variable dataset to the main dataset
collate_data1 <- merge(collate_data[,-c(2,3,4)],Complete_data,by="EmployeeID")

# (Overall NO missing values in final dataset)
sapply(collate_data1,function(x) sum(is.na(x)))

# Removing objects not required 
rm(Impute_data,pred,imp,Complete_data,collate_data)

# Recoding of level names for survey data ---------------

recode_vars <-  c("Education","JobLevel","StockOptionLevel","EnvironmentSatisfaction",
                  "JobInvolvement","JobSatisfaction","PerformanceRating","WorkLifeBalance")

collate_data1[recode_vars] <- lapply(collate_data1[recode_vars],factor)

# Checking level order (All in ascending order)
sapply(collate_data1[recode_vars],function(x) levels(x))

# Renaming the levels 
levels(collate_data1$Education) <- c('Below College','College','Bachelor','Master','Doctor')
levels(collate_data1$EnvironmentSatisfaction) <- c('Low','Medium','High','Very High')
levels(collate_data1$JobInvolvement) <- c('Low','Medium','High','Very High')
levels(collate_data1$JobSatisfaction) <- c('Low','Medium','High','Very High')
levels(collate_data1$PerformanceRating) <- c('Excellent','Outstanding') #ONLY 2 level of ratings
levels(collate_data1$WorkLifeBalance) <- c('Bad','Good','Better','Best')


# Graph Plotting to show factors affecting Attrition ----------------

# Barcharts for categorical features
library(ggplot2)
library(cowplot)
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(collate_data1, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,align="h")

plot_grid(ggplot(collate_data1, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,align="h")

plot_grid(ggplot(collate_data1, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,align="h")

plot_grid(ggplot(collate_data1, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(collate_data1, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,align="h")

# Histogram for Continuous features to show the trend of Attrition

#Monthl Income
ggplot(collate_data1, aes(MonthlyIncome))+ geom_histogram(data=subset(collate_data1,Attrition=="Yes"),binwidth = 10000,fill="purple",col="black")

#PercentSalaryHike
ggplot(collate_data1, aes(PercentSalaryHike))+ geom_histogram(data=subset(collate_data1,Attrition=="Yes"),binwidth = 1,col="black")

#TotalWorkingYears
ggplot(collate_data1, aes(TotalWorkingYears))+ geom_histogram(data=subset(collate_data1,Attrition=="Yes"),binwidth = 5,fill="green",col="black")

#TrainingTimesLastYear
ggplot(collate_data1, aes(TrainingTimesLastYear))+ geom_histogram(data=subset(collate_data1,Attrition=="Yes"),binwidth = 1,col="black")

#YearsAtCompany
ggplot(collate_data1, aes(YearsAtCompany))+ geom_histogram(data=subset(collate_data1,Attrition=="Yes"),binwidth = 5,col="black")

#YearsSinceLastPromotion
ggplot(collate_data1, aes(YearsSinceLastPromotion))+ geom_histogram(data=subset(collate_data1,Attrition=="Yes"),binwidth = 1,col="black")

#YearsWithCurrManager
ggplot(collate_data1, aes(YearsWithCurrManager))+ geom_histogram(data=subset(collate_data1,Attrition=="Yes"),binwidth = 1,col="black")


# Correlation Analysis between Continuous variables
library(GGally)
ggpairs(collate_data1[, c("Age","DistanceFromHome","MonthlyIncome", "NumCompaniesWorked", 
                          "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear",
                          "YearsAtCompany","YearsSinceLastPromotion",
                          "YearsWithCurrManager","Avg_working_hours")])

# Corel : YearsAtCompany & YearsWithCurrManager -- 0.77
# Corel : YearsAtCompany & YearsSinceLastPromotion -- 0.618
# Corel : YearsAtCompany & TotalWorkingYears -- 0.627
# Corel : Age & TotalWorkingYears -- 0.68


# Outlier Analysis in Continuous variables
quantile <- sapply(collate_data1[,c("Age","DistanceFromHome","MonthlyIncome", "NumCompaniesWorked", 
                        "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear",
                        "YearsAtCompany","YearsSinceLastPromotion",
                        "YearsWithCurrManager","Avg_working_hours")],
                         function(x) quantile(x,seq(0,1,.01),na.rm = T)) 
#NO outliers

# Data-driven & Business-driven metrics --------
collate_data1$overtime <- ifelse(collate_data1$Avg_working_hours>8,1,0)
collate_data1$Engagement <- collate_data1$YearsWithCurrManager/collate_data1$YearsAtCompany #high corel
collate_data1$Engagement[is.nan(collate_data1$Engagement)] <- 0

# Scaling the continuous variables
continuous_vars <- names(collate_data1[,c(2,6,13,14,15,17,18,19,20,21,24,29)])

# continuous_vars <-  c("Age","DistanceFromHome","MonthlyIncome", "NumCompaniesWorked",
#                       "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany",
#                       "YearsSinceLastPromotion","YearsWithCurrManager","Avg_working_hours","Engagement")

#collate_bck <- collate_data1

collate_data1[continuous_vars] <- sapply(collate_data1[continuous_vars],scale)

# Checking Attrition rate
Attrition_count <- length(which((collate_data1$Attrition=="Yes")))
Attrition_rate <- (Attrition_count/nrow(collate_data1))*100
Attrition_rate #16.122% of attrition

# Converting the target variable, PerformanceRating & Gender variable to  (1, 0) (Levels=2)

collate_data1$Gender <- as.character(collate_data1$Gender)
collate_data1$Gender<-ifelse(collate_data1$Gender=="Male", 1,0)

collate_data1$PerformanceRating <- as.character(collate_data1$PerformanceRating)
collate_data1$PerformanceRating <- ifelse(collate_data1$PerformanceRating=="Outstanding", 1,0)

collate_data1$Attrition <- as.character(collate_data1$Attrition)
collate_data1$Attrition <- ifelse(collate_data1$Attrition=="Yes",1,0)


# creating a dataframe of categorical features (Levels >2)

categorical_df <- collate_data1[,c(4,5,7,8,10,11,12,16,22,25,26,27)]

sapply(categorical_df,class)
#all factor

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(categorical_df, 
                            function(x) data.frame(model.matrix(~x-1,data =categorical_df))[,-1]))

collate_data_final <-cbind(collate_data1[,-c(4,5,7,8,10,11,12,16,22,25,26,27)], dummies)

# Removing Employee ID
collate_data_final$EmployeeID <- NULL

# Dividing Train & Test Data -------------------

# splitting the data between train and test
library(caTools)
set.seed(101)

indices = sample.split(collate_data_final$Attrition, SplitRatio = 0.7)

train = collate_data_final[indices,]
test = collate_data_final[!(indices),]

dim(train)
#3087   58
dim(test)
#1323   58

sum(train$Attrition)/nrow(train) #0.1613217
sum(test$Attrition)/nrow(test) #0.1609977

# Model Building ---------------------------------

# Running model keeping all the vars
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 
#AIC 2117.6

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
#AIC 2079.4

# Check Vif
library(car)
sort(vif(model_2))

#saveRDS(model_2,'Stepwise_Model2.rds')

formula(model_2)

# Removing 'BusinessTravel.xTravel_Rarely' (quite high Vif and medium significance)

model_3 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.xHigh + 
                 JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                 EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + WorkLifeBalance.xBetter + WorkLifeBalance.xBest,
                 data = train, family = "binomial")

summary(model_3)
#AIC 2088.1
sort(vif(model_3))

# Removing 'Department.xResearch...Development' (quite high Vif and medium significance)
model_4 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                   Department.xSales + EducationField.xOther + EducationField.xTechnical.Degree + 
                   JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.xHigh + 
                   JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                   EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                   JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                   WorkLifeBalance.xGood + WorkLifeBalance.xBetter + WorkLifeBalance.xBest,
                   data = train, family = "binomial")

summary(model_4)
#AIC 2100.9
sort(vif(model_4))

# Removing 'WorkLifeBalance.xBest' 
# (high Vif, medium significance,less obsvn,coefficient is almost equivalent to WorkLifeBalance.xGood,should be much more lesser ideally)
model_5 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                 Department.xSales + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.xHigh + 
                 JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                 EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + WorkLifeBalance.xBetter,
                 data = train, family = "binomial")

summary(model_5)
#AIC 2108.2
sort(vif(model_5))

# Removing JobInvolvement.x.High
model_6 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                 Department.xSales + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle +  
                 JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                 EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + WorkLifeBalance.xBetter,
                 data = train, family = "binomial")

summary(model_6)
#AIC: 2108.1
sort(vif(model_6))

formula(model_6)

# Removing Department.xSales
model_7 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle +  
                 JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                 EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + WorkLifeBalance.xBetter,
                 data = train, family = "binomial")

summary(model_7)
#AIC: 2108.7
sort(vif(model_7))

# Removing JobRole.xResearch.Director
model_8 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobRole.xManufacturing.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle +  
                 JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                 EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + WorkLifeBalance.xBetter,
                 data = train, family = "binomial")

summary(model_8)
#AIC: 2109.4
sort(vif(model_8))


# Remove 'EducationField.xTechnical.Degree'
model_9 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + JobLevel.x2 + JobRole.xManufacturing.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle +  
                 JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                 EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + WorkLifeBalance.xBetter,
                 data = train, family = "binomial")

summary(model_9)
#AIC: 2111.6
sort(vif(model_9))

# Remove 'JobLevel.x2'
model_10 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + JobRole.xManufacturing.Director + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle +  
                 JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                 EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                 JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                 WorkLifeBalance.xGood + WorkLifeBalance.xBetter,
                data = train, family = "binomial")

summary(model_10)
#AIC: 2113.4
sort(vif(model_10))

# Remove WorkLifeBalance.xGood'
model_11 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                  EducationField.xOther + JobRole.xManufacturing.Director + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle +  
                  JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                  EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter,
                  data = train, family = "binomial")

summary(model_11)
#AIC: 2115.5
sort(vif(model_11))

# Remove 'EducationField.xOther '
model_12 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle +  
                  JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                  EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter,
                  data = train, family = "binomial")

summary(model_12)
#AIC: 2119.7
sort(vif(model_12))

#Remove 'MaritalStatus.xMarried'
model_13 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                  EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xMedium + JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter,
                  data = train, family = "binomial")

summary(model_13)
#AIC: 2123.2
sort(vif(model_13))


#Remove 'JobSatisfaction.xMedium'
model_14 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                  EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xHigh + JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter,
                  data = train, family = "binomial")

summary(model_14)
#AIC: 2127.4
sort(vif(model_14))

#Remove 'JobSatisfaction.xHigh'
model_15 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.xVery.High + EnvironmentSatisfaction.xMedium + 
                  EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter,
                data = train, family = "binomial")

summary(model_15)
#AIC: 2129.2
sort(vif(model_15))

#Remove 'JobInvolvement.xVery.High'
model_16 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xMedium + 
                  EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter,
                  data = train, family = "binomial")

summary(model_16)
#AIC: 2133.3
sort(vif(model_16))

#Remove 'JobRole.xManufacturing.Director'
model_17 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + overtime + Engagement + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xMedium + 
                  EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xVery.High + 
                  JobSatisfaction.xVery.High + 
                  WorkLifeBalance.xBetter,
                data = train, family = "binomial")

summary(model_17)
#AIC: 2140.8
sort(vif(model_17))

# All significant variables 
final_model <- model_17

##################################################################

### Model Evaluation

# Predicted values-------

test$pred <- predict(object = final_model, newdata = test, type = "response")
train$pred <- predict(object = final_model, newdata = train, type = "response")

# AUC (Test/Train) ------
library(ROCR)
auc_test <- performance(prediction.obj = prediction(predictions = as.numeric(test$pred), 
                                               labels = as.numeric(test$Attrition)),"auc")@y.values[[1]]
auc_test
#0.8161062

auc_train <- performance(prediction.obj = prediction(predictions = as.numeric(train$pred),
                                                labels = as.numeric(train$Attrition)),"auc")@y.values[[1]]
auc_train
#0.815821

# Confusion Matrix --------
library(e1071)
library(caret)

# At prob=0.5
test_pred_attr <- factor(ifelse(test$pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attr,test_pred_attr)

# At prob=0.4
test_pred_attr <- factor(ifelse(test$pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf

#########################################################################################
# Let's Choose a better cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test$pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.0004733 to 0.9319049 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test$pred)

s = seq(.01,.93,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

cutoff
#0.1679798

# Let's choose a cutoff value of 0.1680 for final model

test_cutoff_attr <- factor(ifelse(test$pred >=0.1680, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
# 0.7256236 
sens
# 0.7276995 
spec
# 0.7252252 

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test <- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.4529248

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attr, test$pred, groups = 10)

