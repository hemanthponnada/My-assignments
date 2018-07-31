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
length(setdiff(unique(rounds2$company_permalink), unique(companies$permalink)) )

#merging two data frames
master_frame<-merge(companies, rounds2, by.x = "permalink", by.y="company_permalink")


#Replacing NA values with numeric 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)]<-0
#CHECK POINT 2
#sqldf("select avg(raised_amount_usd) from master_frame where funding_round_type='venture'")
#Average investment amount of all funding types
summarise(group_by(master_frame,funding_round_type), mean(raised_amount_usd))
#Average investment amount for venture funding
summarise(filter(master_frame, funding_round_type=="venture"), mean(raised_amount_usd))
#Average investment amount for angel funding
summarise(filter(master_frame, funding_round_type=="angel"), mean(raised_amount_usd))
#Average investment amount for seed funding
summarise(filter(master_frame, funding_round_type=="seed"), mean(raised_amount_usd))
#Average investment amount for private equity funding
summarise(filter(master_frame, funding_round_type=="private_equity_type"), mean(raised_amount_usd))


#CHECK POINT 3 : COUNTRY ANALYSIS



#1.Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)

head(summarise(group_by(master_frame,country_code),sum(raised_amount_usd,na.rm = TRUE)),n=9)


#2.For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)


top9<-summarise(filter(group_by(master_frame,country_code),funding_round_type=="venture"), sum(raised_amount_usd))
colnames(top9)<-c("Country_code","Total_sum")
top9<-head(arrange(top9,desc(Total_sum)),n=9)

#CHECK POINT 4 : SECTOR ANALYSIS

#1.Extract the primary sector of each category list from the category_list column
library(tidyr)
master_frame<-separate(master_frame,category_list, into = "Primary_sector", sep="\\|", extra= "drop")

#Extracting the data of mapping file
mapping<-read.csv("mapping.csv", stringsAsFactors = FALSE)
#converting from wide fromat to long format
mapping<-gather(mapping,"main_sector","Value",2:10)
mapping<-mapping[(!mapping$Value==0),]
mapping<-mapping[,-3]


#CHECK POINT 5 : SECTOR ANALYSIS 2

mapping$category_list <- tolower(mapping$category_list)
master_frame$Primary_sector <- tolower(master_frame$Primary_sector)

master_frame_2<-merge(master_frame, mapping,by.x = "Primary_sector",by.y = "category_list")

d1_USA<-filter(master_frame_2, country_code=="USA", funding_round_type=="venture")
d2_GBR<-filter(master_frame_2, country_code=="GBR", funding_round_type=="venture")
d3_IND<-filter(master_frame_2, country_code=="IND", funding_round_type=="venture")














