# Load required libraries
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(readr)
library(corrplot)

# To import dataset
heartData <- read.csv("Heart_Attack.csv") #this method require absolute path 

#summarise the dataset using summary(): I got mean, median, maximum,min based on column
summary(heartData)

## dimention and structure of dataset using  str() and dim() : 
#str() : gives the type of dataset, column name with datatype and more.
str(heartData)
#gives o/p in m and n format where m is number of row and n is number of column
dim(heartData)

# head is giving o/p from the front data
head(heartData,8)
# tail is giving last data
tail(heartData,5)

#it gives the output of column names
colnames(heartData)
#it gives the output of dataset type
class(heartData)
#sapply method is used to get the column name with its data type
#6:- class variable is the column name of the heart attack dataset, which is the indicate that data is classified in diff categories such as a age sex chol of the user
#7:- datatype define the value-type of that class variable like cp is int, fbs is numeric, sex is integer
sapply(heartData,class)

#8:- change the type of column to factor
heartData$age <- as.factor(heartData$age)

#9:- it return sum of all missing value like if there are 50 field have missing value. then some is 50
sum(is.na(heartData))

#each colums with total missing value
#10:- it represent the total number of missing value for each column
colSums(is.na(heartData))

#11:- add 0 to the missing value so we get atleast 0 value
heartData[(is.na(heartData))]<-0

#output indicate that dataset does not have missing value
colSums(is.na(heartData))

# 12:- Rename sex column from 0 to male
# firstly I check if sex variable of dataset is 0 then it change it to male
#if the value is 1 then change it to the female. It is like a conditional operation
#heartData$sex[heartData$sex == 0] <-"male"
heartData$sex[heartData$sex == "0"] <- "male"
heartData$sex[heartData$sex == "1"] <- "female"

#13:-  i took the two variable of graph which is common x and y based on axis.
# i define the variables for cholesterol and age 
x <- heartData$chol
y <- heartData$age

#plot() is used to draw the scatter plots
#plot method is used to show the relation between age and cholesterol
plot(x,y)
#13(a) here I added the title of x and y axis as well as main paramter is used to give the title. Also, Add the colour.
plot(x, y,xlab="cholesterol ", ylab="age ",main = "Analysis of Age and Cholesterol", col="blue",col.lab ="blue",col.main = "blue")
#13(b) I added pch = 24 which is value for trigle also add colour red into trigle using bg parameter
plot(x, y,xlab="cholesterol ", ylab="age ",main = "Analysis of Age and Cholesterol",pch = 24,bg="red",col.lab ="blue",col.main = "blue")

#14: ggplot2 library is used to execute this method
#data is default dataset for plotting and then use mapping to get the value of cholesterol and age. ALso, shape is default value for square
ggplot(data = heartData,colour = 'red', aes(x = cp, y = age)) +geom_point(shape=15)

#15(a):- barplot is used to create the plot. Also I added colour and labels
barplot(table(y), main="Barplot of Age", xlab="age",col = "red")

#16:- create the histogram plots and I put the value of Chest pain.
chestpain <- heartData$cp
hist(chestpain)
#16(a) min() is usd to find minimum value and  max() is usd to find maximum value
min(chestpain)
max(chestpain) 
#16(b) I added breaks into histogram. Based on break, it takes group of cp
hist(chestpain,breaks=10) 
#16(c) I added title to the graph and added array of colour so it can take the colours based on value of chest pain. For first value it tales blue and for third value it takes green and then repleat it for every iteration
hist(chestpain,breaks=15, main = "Chest Pain disease",col = c("blue", "red", "green"))
#16(B):- Seq is used and it has a three parameter, minimum, maximum and
ggplot(heartData,breaks=20, aes(x = cp)) +geom_histogram(breaks = seq(min(chestpain), max(chestpain), 1), fill = "red") +labs(title = "Chest Pain disease")


#17:- to draw the boxplot, bloxplot(attribute) is used: this plot clearly gives the view of maximum and minimum, first Quartile, median and third Quartile
age<-heartData$age
boxplot(age, col = c("red"))

#18:- for the correlationn plot, all value must be numeric so firstly I did this char to numeric
heartData$sex[heartData$sex == "male"] <- 0
heartData$sex[heartData$sex == "female"] <- 1

# i can use this method but it return the list type of dataset and I need frame type of so I avoid this and use manual one
#heartData[2:14] <- lapply(heartData[1:14], as.numeric)

# chanaged the type of all class variable from others to numeric.
heartData$age <- as.numeric(heartData$age)
heartData$sex <- as.numeric(heartData$sex)
heartData$cp <- as.numeric(heartData$cp)
heartData$trtbps <- as.numeric(heartData$trtbps)
heartData$chol <- as.numeric(heartData$chol)
heartData$thalachh <- as.numeric(heartData$thalachh)
heartData$slp <- as.numeric(heartData$slp)
heartData$thall <- as.numeric(heartData$thall)
heartData$output <- as.numeric(heartData$output)
#check that all variables are numeric-type
sapply(heartData,class)
str(heartData)
#corrplot() is used to create the plot
corrplot(cor(heartData))









