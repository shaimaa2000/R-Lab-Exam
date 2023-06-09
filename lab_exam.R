#Get the current working directory.
getwd()


#Setup home directory in R in Files section.
setwd("F:/الفرقة الرابعة الترم الثاني/معمل تحليل البيانات الجينومية/امتحان العملي/labs")


#Read the dataset file that contains missing data.
sydney <- read.csv("G3_sydney_hobart_times.csv")


#View your dataset on R environment.
View(sydney)

#Get data frame dimensions.
dim(sydney)

#Number of rows.
nrow(sydney)


#Number of columns.
ncol(sydney)


#Column names.
colnames(sydney)

#Get first/last 6 rows.
head(sydney)
tail(sydney)


#get first/last N rows.
head(sydney, 25)
tail(sydney, 25)

#Computing new variable from existing one data.
sydney$start_finish <- sydney$fleet_start - sydney$fleet_finish



#Add NA in all missing data cells. Write the strings that will be converted to NA inside c().
sydney <- read.csv("G3_sydney_hobart_times.csv", na.strings = c(""))
sydney


#To get all locations of NA
complete.cases(sydney)


#Get all rows contain missing data.
sydney[ ! complete.cases(sydney), ]


#Get the data structure and summary to know the incorrect types for variables.
str(sydney)
summary(sydney)


#Removing text in numeric values.
sydney$Time<-gsub(" day", "", sydney$Time)
sydney$Time <- as.numeric(sydney$Time)



#get all row with missing data for specific variable.
missingTime <- sydney[is.na(sydney$Time), ]
missingTime <- sydney[! is.na(sydney$Time), ]
missingTime


#Re-coding means use different values for a variable.
sydney$newYear[sydney$Year < 2000] ="Before 2000"
sydney$newYear[sydney$Year >= 2000] = "After 2000"
sydney


#Re-code of code
sydney$newYear2[sydney$newYear == "Before 2000"] = "1"
sydney$newYear2[sydney$newYear == "After 2000"] = "2"



#Calculate the median of a variable with the use of na.rm.
median((sydney[ ,"Time"]), na.rm = T)

medB<- median(sydney[sydney$newYear == "Before 2000", "Time"], na.rm = T)
sydney[is.na(sydney$Time) & sydney$newYear == "Before 2000", "Time"] <- medB

medA<- median(sydney[sydney$newYear == "After 2000", "Time"], na.rm = T)
sydney[is.na(sydney$Time) & sydney$newYear == "After 2000", "Time"] <- medA


#Filter the dataset according to specific criteria.
filter1 <- sydney[sydney$newYear == "After 2000" , ]
filter1
filter2 <- sydney[sydney$time > 2 , c(1, 3, 7)]
filter2


#Using if-else statement.
sydney$Code.Time.less.than.3 <- as.factor(ifelse(sydney$Time < 3, "Yes", "No"))


#Sorting the dataset according to specific columns.
sortTime <- sydney[order(sydney$Time) , ]


#//////////////////////////////visualization//////////////////////////////////



#Uses of ggplot2 package.
library(ggplot2)


#Histogram chart 
figure1 <- ggplot(sydney, aes(Time))
figure1 + geom_histogram() + ggtitle("Time of sydney")

#Scatter plot between the fleet_start and fleet_finish
figure2<-ggplot(sydney , aes(x=fleet_start  , y= fleet_finish))
figure2 + geom_point() + ggtitle("The co_relation between the fleet_start and fleet_finish")


#Scatter plot between the fleet_start and start_finish
figure3<-ggplot(sydney , aes(x=fleet_start  , y= start_finish))
figure3 + geom_point() + ggtitle("The co_relation between the fleet_start and start_finish")


#Scatter plot between the fleet_finish and start_finish
figure4<-ggplot(sydney , aes(x=fleet_finish  , y= start_finish))
figure4 + geom_point() + ggtitle("The co_relation between the fleet_finish and start_finish")


#Bar chart 
figure5 <- ggplot(sydney, aes(x = Code.Time.less.than.3, fill= newYear))
figure5 + geom_bar()


