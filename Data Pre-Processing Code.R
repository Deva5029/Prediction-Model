#################################################################
#     --DEVAMSH KANDRAGUNTA AND RAJESH KODAGANTI                #
#     --CSUN ID : 203125890 AND 203136602                       #
#     --"DATA MINING FINAL PROJECT CODE(PRE-PROCESSING)"        #
#     --PROJECT NAME : PRE-OWNED CARS PRICING PREDICTION        #
#                                                               #
#################################################################     

#install pacakages
install.packages("psych")
install.packages("stringr")
install.packages("stats")

#loading libraries
library(stringr)
library(stats)
library(psych)
#Reading the two datasets

data1 <- read.csv("C:/Data Mining/German_cars.csv")
data2 <- read.csv("C:/Data Mining/US_cars.csv")

###Cleaning the data sets

#Removing the unnecessary columns from dataset 1.

data1$offerType = NULL

#Removing the unnecessary columns from dataset 2.

data2$vin = NULL 
data2$lot = NULL 
data2$condition = NULL 
data2$X = NULL
data2$country = NULL
data2$title_status = NULL
data2$color = NULL
data2$state = NULL

#Adding required columns to dataset 2.

data2$fuel <- NA
data2$hp <- NA
data2$gear <- NA

#Renaming the column name in dataset 1

colnames(data1)[colnames(data1) %in% c("make")] <- c("brand")

#Merging the both data sets
data5 <- rbind(data1, data2)

#Checking if there are any missing values

is.na(data5)
which(is.na(data5))
which(is.na(data5), arr.ind = TRUE)

#Finding the total number of missing values

sum(is.na(data5))

#Finding the positions of Missing Values

which(rowSums(is.na(data5))!=0)

#To find number of missing values in each column

colSums(is.na(data5))

###Pre-processing the dataset by dealing with missing values

#Finding mean for "hp"

mean_hp <- mean((data5$hp), na.rm = TRUE)
mean_hp

#Replacing NA's in "hp" column with mean 
data5$hp[is.na(data5$hp)] <- mean((data5$hp), na.rm = TRUE)
data5

#Rounding the values in hp

data5_round <- data.frame(lapply(data5, 
                        function(hp) if(is.numeric(hp)) round(hp, 1) else hp))

#Finding Mode for "fuel"

mode <- function(fuel) {
  unique_fuel <- unique(fuel)
  tabulate_fuel <- tabulate(match(fuel, unique_fuel))
  unique_fuel[tabulate_fuel == max(tabulate_fuel)]
}
mode(data5_round$fuel)

#Inserting Mode(fuel) value in "Na" for fuel column

data5_round$fuel[is.na(data5_round$fuel)] <- mode(data5_round$fuel)

#Finding mode for "Gear" 

mode <- function(gear) {
  unique_gear <- unique(gear)
  tabulate_gear <- tabulate(match(gear, unique_gear))
  unique_gear[tabulate_gear == max(tabulate_gear)]
}
mode(data5_round$gear)

#Inserting Mode(gear) value in "Na" for gear column

data5_round$gear[is.na(data5_round$gear)] <- mode(data5_round$gear)

#Finding the total number of missing values

sum(is.na(data5_round))

#Changing categorical values in gear column to numeric values
data5_round$gear <- str_replace(data5_round$gear, 'Manual', "0")
data5_round$gear <- str_replace(data5_round$gear, 'Automatic', "1")
data5_round$gear <- as.numeric(data5_round$gear)
data5_round$gear[is.na(data5_round$gear)]<-median(data5_round$gear,na.rm=TRUE)
table(data5_round$gear)

#Changing categorical values in fuel column to numeric values
data5_round$fuel <- str_replace(data5_round$fuel, 'Gasoline', "0")
data5_round$fuel <- str_replace(data5_round$fuel, 'Electric', "1")
data5_round$fuel <- str_replace(data5_round$fuel, 'Diesel', "2")
data5_round$fuel <- as.numeric(data5_round$fuel)
data5_round$fuel[is.na(data5_round$fuel)]<-median(data5_round$fuel,na.rm=TRUE)
table(data5_round$fuel)

na.exclude(data5_round$price)
sum(is.na(data5_round$price))

View(data5_round)

###Finding Correlation Coefficient

#Before pre-processing

plot(year ~ mileage, 
     data = data1, 
     main = "year vs mileage", 
     xlab = "mileage", 
     ylab = "year")
cor(data1$mileage, data1$year)

plot(year ~ price, 
     data = data1, 
     main = "price vs year", 
     xlab = "price", 
     ylab = "year")
cor(data1$year, data1$price)

#After pre-processing

plot(year ~ mileage, 
     data = data5_round, 
     main = "year vs mileage", 
     xlab = "mileage", 
     ylab = "year")
cor(data5_round$mileage, data5_round$year)

plot(year ~ price, 
     data = data5_round, 
     main = "price vs year", 
     xlab = "price", 
     ylab = "year")
cor(data5_round$year, data5_round$hp)

plot(data5_round, main = "Cars", col=c("blue","red","green","yellow","brown","pink","black","orange"))

####Correlation Analysis 

#A.) Mileage and Price

describe(data5_round$mileage)
describe(data5_round$price)
cor.test(data5_round$price, data5_round$mileage)

#B.) Price and hp

describe(data5_round$price)
describe(data5_round$hp)
cor.test(data5_round$price, data5_round$hp)

#C.) Price and year

describe(data5_round$price)
describe(data5_round$price)
cor.test(data5_round$price, data5_round$year)

#Changing memory size for allocating vector of size 3.7 GB
memory.limit(size=500000)


###Data Visualization

#boxplot for both Mileage and year

boxplot(year ~ mileage, data = data5_round,
        xlab = "mileage",
        ylab = "year",
        main = "mileage vs year")

#boxplot for both Mileage and price

boxplot(price ~ mileage, data = data5_round,
        xlab = "mileage",
        ylab = "price",
        main = "mileage vs price")

#boxplot for both hp and price

boxplot(price ~ hp, data = data5_round,
        xlab = "hp",
        ylab = "price",
        main = "hp vs price")

#boxplot for both year and price

boxplot(price ~ year, data = data5_round,
        xlab = "year",
        ylab = "price",
        main = "year vs price")


#scatter plot for price and year
plot(data5_round$year,data5_round$price,
     xlab = "year",
     ylab = "price",
     main="Scatter plot for price and year")

#scatter plot for mileage and price
plot(data5_round$mileage, data5_round$price,
     xlab = "mileage",
     ylab = "price",
     main="Scatter plot for mileage and price ")

#scatter plot for price and hp
plot(data5_round$hp,data5_round$price,
     xlab = "hp",
     ylab = "price",
     main="Scatter plot for price and hp")


#Q-Q plot for price 

qqnorm(data5_round$price, 
       xlab = "frequency",
       ylab = "price",
       main="QQ plot for price")
qqline(data5_round$price)

#Q-Q plot for mileage
qqnorm(data5_round$mileage,
       xlab = "frequency",
       ylab = "price",
       main="QQ plot for mileage with line")
qqline(data5_round$mileage)


#Histogram for mileage

hist(data5_round$mileage, 
     freq=F, 
     xlab = 'mileage', 
     ylab ='frequency')

hist(data5_round$mileage, 
     prob=T, 
     xlab = 'mileage', 
     ylab ='frequency')


#To export the file into system from studio
write.csv(data5_round,file = "finaldata.csv", row.names = FALSE)