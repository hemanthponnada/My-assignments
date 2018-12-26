
#-------------------------------Business Understanding------------------------------
#A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
# Suppose that you have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices. 
# The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

#Installing the required packages and loading them 

install.packages("caret")
install.packages("kernlab")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("janitor")

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(janitor)

#------------------Loading the test and train data-----------------------------

dataset<-read.csv("mnist_train.csv", header=FALSE)
dataset_test<-read.csv("mnist_test.csv", header = FALSE)

#-------------------------Data Preparation-------------------------------

#Removing the coloumns and rows that are completely empty(if there are any)

dataset<- remove_empty(dataset, which = c("rows","cols"))#Janitor Package was used

#--------------Checking for missing values-----------------------

sapply(dataset,function(x){sum(is.na(x))})


#-----------Checking for duplcate values-----------------

distinct_data<-distinct(dataset)
nrow(dataset)
nrow(distinct_data)
#The values of nrow(dataset) and nrow(distinct_data) is the same. So there are no duplicate values.


#-----Converting into factors--------
dataset$V1 <-factor(dataset$V1)
dataset_test$V1<-factor(dataset_test$V1)


##---------Sampling the train dataset-----------
# It would take a lot of time for modeling on the full MNIST data, 
# So we sample the data and build the model which would make the computation faster.

set.seed(1)
dataset.indices<-sample(1:nrow(dataset),0.15*nrow(dataset))
dataset<-dataset[dataset.indices,]



# Split the data into train and test set
#The dataset that has been sampled was split into test and train dataset.
#The model that we are going to build is first tested on this test dataset and then on the actual test dataset.

set.seed(1)
train.indices = sample(1:nrow(dataset), 0.7*nrow(dataset))
train = dataset[train.indices, ]
test = dataset[-train.indices, ]



#-------------------------------Model Building---------------------------------------------

#------Using Linear Kernel-------
Model_linear <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#----confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$V1)

#Accuracy - 90%

#----PREDICTING ON THE REAL TEST DATA
Eval_linear_test<-predict(Model_linear, dataset_test)

#Confusion matrix for the real test data
confusionMatrix(Eval_linear_test, dataset_test$V1)

# The ACCURACY of this model is 91%

#----------Model building Using RBF Kernel

Model_RBF <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$V1)

#Accuracy - 95%

#Predicitng on the real test dataset
Eval_RBF_test<-predict(Model_RBF, dataset_test)

#Confusion matrix for the real test dataset
confusionMatrix(Eval_RBF_test,dataset_test$V1)

#The accuracy of this model is 95%

# Paramter : Cost C=1
#Hyperparameter : sigma = 1.64127655057413e-07 


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 


trainControl <- trainControl(method="cv", number=3)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(0.64,1.64,2.64), .C=c(1,2,3) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(V1~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

plot(fit.svm)



#Building the final model

Model_final<- ksvm(V1~ ., data = train, scale = FALSE, kernel = "rbfdot",C=3, kpar=list(sigma=2.63e-07))

#Predicting on the test dataset
Eval_final<- predict(Model_final, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_final,test$V1)

#Accuracy - 96%

#Predicting on the real test dataset
Eval_final_test<- predict(Model_final, dataset_test)

#Final confusion matrix on the test dataset - RBF Kernel
confusionMatrix(Eval_final_test, dataset_test$V1)

#Accuracy - 96%

#---------------Conclusion------------------


#Based on the above observations maximum accuracy can be achieved with:
#Final hyperparameters:
# C=3  
# sigma=2.63e-07

#Final Accuracy - 96%