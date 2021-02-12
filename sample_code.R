setwd("/Users/...R/")

D_A <- read.csv("/Users/..train.csv")

dim(D_A)              #Show the size of the table
data_size <- dim(D_A) #Create a variable with dimensions of the data table
data_size[2]          #Display the second element of data_size matrix
head(D_A)             #Show first few rows of the data
names(D_A)            #Display the variables in the data set

quantile(D_A$SalePrice)
boxplot(D_A$SalePrice)    #Use boxplot to identify outliers
#Create a variable taking value of 1 if above 3 standard deviations
outlier_1<-(D_A$SalePrice>(mean(D_A$SalePrice)+3*sd(D_A$SalePrice)))*1
D_A$SalePrice[which(outlier_1=='1')] #What are outlier prices?


