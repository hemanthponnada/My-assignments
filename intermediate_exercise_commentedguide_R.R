# Look at structure of mtcars
str(mtcars)

# Store mtcars in a new dataframe called cars. 
cars <- mtcars

# Calculate each car's score in a new column named score 
# score = mpg.hp / wt
cars$score<-cars$mpg*cars$hp/cars$wt
length(cars)
# Store the scores in a vector s
s<-cars$score
length(s)
# Create a new vector named performance equal to the length of the vector s
# http://stackoverflow.com/questions/22104774/how-to-initialize-a-vector-with-fixed-length-in-r
performance<-vector(mode='character', length=length(s))
mean(score) 
# If score < mean(score), performance is 'average', else performance is 'good'
for(i in 1:nrow(cars)){
if(cars[i,"score"]<mean(score)){
  performance[i]<-"average"
}else{
  performance[i]<-"good"
}
}
cars
performance
# Add the performance vector as a column to cars
cars$performance<-performance
cars
# Convert the performance variable to factor type
cars$performance<-as.factor(performance)
# Summarise the cars df
summary(cars)
?which



