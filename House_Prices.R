# House Prices
 
# Load the libraries that you will use etc

options(repos="http://cran.us.r-project.org") # May need this line to avoid errors
loadmanylibs <- c('ggplot2','corrplot','gridExtra') 
install.packages(loadmanylibs) #This line will install ALL (!) packages at once
lapply(loadmanylibs, require, character.only = TRUE) 

# Load libraries and Data

# Set working directory where you keep the dataset 

setwd("/Users... R") 
all <- read.csv("train.csv")
dim(all)
str(all[,c(1:10, 81)]) #display first 10 variables and the response variable
all$Id <- NULL #remove IDs

# Prepare Data

# Use R code to keep only houses with more than 3 bedrooms:

all<-all[all$BedroomAbvGr < 3,]

# Advanced histogram

# Plot a histogram for houses cheaper than 400000 using any other color instead of "blue"

all$group = ifelse(all$SalePrice < 400001, "0-400000", ifelse(all$SalePrice < 500001, "400000+", "500000+"))
ggplot(data=all[!is.na(all$SalePrice), ], aes(x=SalePrice, fill = group)) +
        geom_histogram(binwidth = 10000) +
        scale_x_continuous(breaks= seq(0, 800000, by=100000)) +
        scale_fill_manual(values = c("0-400000" = "orange",
                                 "400000+" = "blue",
                                 "500000+" = "dark blue"))

# Correlation plot by strength of correlations

# Re-run code for positive correlations only using ellipse for lower triangular correlation matrix

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

# Sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

# Select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, lower="ellipse", upper = "circle", tl.col="black", tl.pos = "lt")

# Box plot of Sale price by overall quality
# Plot a modified boxplot for houses more expensive than 500000 using any other color instead of "blue"

datanew <- all[all$SalePrice > 500000,]

ggplot(data=datanew[!is.na(datanew$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
        geom_boxplot(colour = 'orange') + labs(x='Overall Quality') +
        scale_y_continuous(breaks= seq(0, 800000, by=100000))

# Sale price and number of houses sold by neighborhood 
# Plot a modified graph n1 (but not n2) without geom_label and with any other but red color of geom_hline

n1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
        geom_bar(stat='summary', fun.y = "median", fill='blue') +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks= seq(0, 800000, by=50000)) +
        geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
# Warning: Ignoring unknown parameters: fun.y

grid.arrange(n1) #Need package gridExtra

# Sale price by roof style
# Plot a modified graph tb2 (but not tb1) by replacing the total number of bath by RoofStyle

#all$RoofStyle <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)

tb2 <- ggplot(data=all, aes(x=as.factor(RoofStyle))) +
        geom_histogram(stat='count')

# Warning: Ignoring unknown parameters: binwidth, bins, pad

grid.arrange(tb2)

# Sale price by year remodelled
# Plot a modified graph with year built by year remodelled (YearRemodAdd) and without scale_y_continuous

ggplot(data=all[!is.na(all$SalePrice),], aes(x=YearRemodAdd, y=SalePrice))+
        geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) 
