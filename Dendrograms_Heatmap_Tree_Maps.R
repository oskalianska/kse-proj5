# Dendrograms, Heatmaps and Tree Maps

debt <- read.csv('/Users/../debt.csv')

# First, produce a scatter plot with Debt_to_GDP in 2003 on x-axis, Debt_to_GDP in 2007 on y-axis and country names as text labels

x <- debt$Debt_to_GDP_Ratio_2003
y <- debt$Debt_to_GDP_Ratio_2007

# Plot with main and axis titles

# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Debt-to-GDP ratio in EU",
     xlab = "Debt_to_GDP in 2003", ylab = "Debt_to_GDP in 2007",
     pch = 19, frame = FALSE)
     
# Add regression line
abline(lm(y ~ x, data = debt), col = "blue")

data = data.frame(debt[,2:7])
dt = dist(data, method = "euclidean") #Compute distance matrix
clust = hclust(dt) #Hierarcial cluster
plot(data$Debt_to_GDP_Ratio_2003, data$Debt_to_GDP_Ratio_2007) # generates a scatter plot
text(debt$Debt_to_GDP_Ratio_2003,debt$Debt_to_GDP_Ratio_2003, labels = row.names(debt), cex = 0.6) # applies labels to a scatter plot.

# Second, produce colored dendrogram with 3 clusters

plot(clust) # generates a dendogram plot

options(repos="https://cran.rstudio.com") # May need this line to avoid errors

# Elegant way to install missing packages - not re-installing it again
if(!require("dendroextras")) install.packages("dendroextras")

# Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
# logical.return = TRUE, : there is no package called 'dendroextras'

library(dendroextras)
#data(da)
par(mar = c(2,10,2,10), cex = 0.6) # set margins on 4 sides of the plot
clst1=colour_clusters(hclust(dist(data), "ave"),3,groupLabels=as.roman) #3 clusters
plot(clst1, main = "Dendrogram with 3 clusters", horiz = TRUE)

# to change the view from rectangle to Triangle#
plot(clst1, main = "Dendrogram with 3 clusters", horiz = TRUE, type = "triangle")

# Third, generate a heatmap with any color palette instead of green including Debt_to_GDP for all years

if(!require("pheatmap")) install.packages("pheatmap")

# Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
# logical.return = TRUE, : there is no package called 'pheatmap'

if(!require("RColorBrewer")) install.packages("RColorBrewer")
library("pheatmap")
library("RColorBrewer")

debt_to_gdp = as.matrix(scale(debt[,2:9])) #Scale to get comparable units
#debt_to_gdp = as.matrix(debt)
clst = hclust(dist(debt_to_gdp))

# Simple heatmap
pheatmap(debt_to_gdp, cluster_row= FALSE, cluster_col = FALSE, main ="Debt_to_GDP")

# Add color palette
heatcolor = brewer.pal(7,"Blues")
pheatmap(debt_to_gdp, cluster_row= FALSE, cluster_col = FALSE, color = heatcolor, main = "Debt_to_GDP", fontsize_number = 10)

