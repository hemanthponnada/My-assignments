library(dplyr)

#CHECK POINT 1
companies<-read.delim("companies.txt", header = TRUE, sep = "\t")
companies <- data.frame(lapply(companies, as.character), stringsAsFactors=FALSE)
rounds2<-read.csv("rounds2.csv", stringsAsFactors = FALSE)

rounds2$company_permalink<- tolower(rounds2$company_permalink)
companies$permalink<- tolower(companies$permalink)

#Number of unique companies in rounds2
length(unique(rounds2$company_permalink))

#Companies in round2 that are not present in companies
length(setdiff(rounds2$company_permalink, companies$name) )

#merging two data frames
master_frame<-merge(companies, rounds2, by.x = "permalink", by.y="company_permalink")


#CHECK POINT 2
#sqldf("select avg(raised_amount_usd) from master_frame where funding_round_type='venture'")
#Average investment amount of all funding types
summarise(group_by(master_frame,funding_round_type), mean(raised_amount_usd,na.rm = TRUE))
#Average investment amount for venture funding
summarise(filter(master_frame, funding_round_type=="venture"), mean(raised_amount_usd,na.rm = TRUE))
#Average investment amount for angel funding
summarise(filter(master_frame, funding_round_type=="angel"), mean(raised_amount_usd,na.rm = TRUE))
#Average investment amount for seed funding
summarise(filter(master_frame, funding_round_type=="seed"), mean(raised_amount_usd,na.rm = TRUE))
#Average investment amount for private equity funding
summarise(filter(master_frame, funding_round_type=="private_equity_type"), mean(raised_amount_usd,na.rm = TRUE))


#CHECK POINT 3 : COUNTRY ANALYSIS

library(plyr)

#1.Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)
head(arrange(master_frame,desc(raised_amount_usd)), n = 9)

#2.For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)

top9<-head(arrange(subset(master_frame,funding_round_type=="venture"), desc(raised_amount_usd)),n=9)


