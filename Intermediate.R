
#Recalling Session 1 

#Download the bank dataset file

getwd()

#Set right Working Directory.


#Reading excel file, with StringsasFactors command as False.
bank <- read.csv("bank.csv", stringsAsFactors = F)

#Check structure and datatype of bank
str(bank)
class(bank)

#Converting an atrribute into Factor
bank$marital <- factor(bank$marital)
str(bank$marital)

#Subsetting specific columns from bank data frame
subset1 <- data.frame(bank$age, bank$salary, bank$y)
str(subset1)

#Accessing a column of bank data frame
bank[1, ]

###




##Relational Operators on vectors 

8 > c(2, 9, 6, 8, 10)

c(8, 5, 7, 1, 0) > c(2, 9, 6, 8, 0)

c(8, 5, 7, 1, 0) > c(2, 9, 6, 8)



#Logical Operators

#AND Operator &&

6 > 5 & 7 > 4
6 > 5 & 1 > 4

#OR Operator |
6 > 5 | 7 > 4
6 > 5 | 1 > 4

#NOT Operator !

!TRUE

7 != 6
7 != 7

c(8, 5, 7, 1, 0) - c(2, 9, 6, 8, 0)
A <- c(8, 5, 7, 1, 0)
A
sum(A>=4)
A_upvotes <- c(7, 3, 2, 8, 0, 4)
A_upvotes>=4
length(which(A_upvotes>=4))
###

#If-Else Statements

shopping_bill <- c(90,130,52,75,70,24,72,125,90,68,56,50, 85)
total <- sum(shopping_bill)
total


if (total >1000)
{
  print("You are out of cash!")
} else if (total < 900){
  print("Yay! You can buy some chocolates!")
} else {
  print("Your shopping is done, but No Chocolate!")
}
######




#Applying logical & conditional statements on data-set

person <- bank[20, ]
person

if(person$marital == "married")
{
  if(person$housing == "yes" | person$salary > 60000)
  {
    print("Issue credit card")
  }
  else
  {
    print("Sorry, not eligible for credit card")
  }
} else if(person$marital == "single")
{
  if(person$education == "tertiary" & person$salary > 40000)
  {
    print("Issue credit card")
  }
  else
  {
      print("Sorry, not eligible for credit card")
  }
}

#####


#Loops

print("Hello")
print("Hello")
print("Hello")
print("Hello")
print("Hello")

for(i in 1:5)
{
  print ("Hello")
}
nrow(bank)
# Applying for loop for the bank case study

person1 <- bank[1,]
person2 <- bank[2,]
# and so on

# Run the credit card check for each person object individually

for (i in 1:nrow(bank))
{  
       person <- bank[i, ]
       person
       
       if(person$marital == "married")
       {
         if(person$housing == "yes" | (!is.na(person$salary) & person$salary) > 60000)
         {
           bank[i,"my_decision"] <- "yes"
         }
         else
         {
           bank[i,"my_decision"] <- "no"
         }
       } else if(person$marital == "single")
       {
         if(person$education == "tertiary" & !is.na(person$salary) & person$salary > 40000)
         {
           bank[i,"my_decision"] <- "yes"
         }
         else
         {
           bank[i,"my_decision"] <- "no"
         }
       }
}
bank
#######

# Converting selecting columns to factor types simulateneosly

factor_columns <- c(2, 4, 5, 6, 7, 8, 10, 11, 12, 19, 20)

for (i in factor_columns) {
bank[, i] <- factor(bank[, i])}

str(bank)



#Functions

#Calling built-in functions

mean(bank$age)
sd(bank$age)

mean(bank$salary)
sd(bank$salary)

#Counting number of NA in bank dataframe
sum(is.na(bank))

#Counting number of NA in a bank column, say salary
sum(is.na(bank$salary))

#Row numbers which have bank$salary = NA
which(is.na(bank$salary))

#Calculating the mean and standard deviation of salary
mean(bank$age, na.rm = TRUE)
sd(bank$age, na.rm = TRUE)

#Search for mean function and look for attributes
?mean

#Calculating maximum age of the person in the data set
max(bank$age)

#Calculating the row number of person with maximum age
which.max(bank$age)
      
#Information about attributes for person with maximum age
bank[which.max(bank$age), ]

#Information about number of people - issued credit card
length(which(bank$y == "yes"))

length(which(bank$my_decision == "yes"))

#Information about number of married & single people length(which(bank$y == "yes" & bank$marital == "married"))
length(which(bank$y == "yes" & bank$marital == "single"))


##############

# Writing functions in R

credit_card_decision <- function(p)
{
  if(p$marital == "married")
  {
    if(p$housing == "yes" | (!is.na(p$salary) & p$salary) > 60000)
    {
      decision <- "yes"
    }
    else
    {
      decision <- "no"
    }
  } else if(p$marital == "single")
  {
    if(p$education == "tertiary" & !is.na(p$salary) & p$salary > 40000)
    {
      decision <- "yes"
    }
    else
    {
      decision <- "no"
    }
  } else
    {
    decision = "no"
  }
  return(decision)
}

for(i in 1:nrow(bank))
{
  person <- bank[i,] 
  bank[i,"my_decision"] <- credit_card_decision(person)
}
function_math <- function(x, y){
  z <- x + y
  p <- x * y
  q <- z / p
  return(c(z, p, q))
}
function_math(2, 3)

######

# APPLY FAMILY

# Used to apply a function over several values simulatenaouly 
# Saves us from writing a complicated FOR loop


#Converting into Factors using R loop
bank <- read.csv("bank.csv", stringsAsFactors = F)
str(bank)

for (i in 1:ncol(bank)) {
  bank[, i] <- factor(bank[, i])}
str(bank)

#Converting into Factors using sapply
bank <- read.csv("bank.csv", stringsAsFactors = F)
str(bank)

bank2 <- data.frame(sapply(bank, factor))
str(bank2)


#Applying sapply to a vector
marks <- c(23,12,15,30,40)

sapply(marks,function(x) x/40*100)

####