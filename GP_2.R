#GP Assignment 2 - Multidimensional Scaling
library(psych)
library(smacof)   # needed for the sim2diss function
library(MASS) 

#Data
nations <- read.delim(file.choose())
View(nations)

#Data Check 
summary(nations)
describe(nations) #no missing data, or unusual values 

#Similarities need to be converted into disimilarties 
nations_dis <- sim2diss(nations, method = 9) #highest value is 9
View(nations_dis)
?dist
nations_dist = dist(nations_dis) 

#Non-metric multidimensional scaling, subjective ratings
?isoMDS
nations_MDS <- isoMDS(nations_dist) #high stress
print(nations_MDS) #points
x = nations_MDS$points[,1]
y = nations_MDS$points[,2]

?plot
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", 
     xlim = range(x)*1.2, type = "n")
lines(x,y)
text(x, y, labels = colnames(nations), cex = 1)

