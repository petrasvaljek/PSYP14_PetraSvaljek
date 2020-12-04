#GP. Assignment 1 – The Covariance Matrix 
library(psych)




womenshealth <- read.delim(file.choose())
summary(womenshealth)
describe(womenshealth)
womenshealth <- womenshealth[complete.cases(womenshealth), ]
#X's
x1 <- womenshealth[,"timedrs"]
x2 <- womenshealth[,"attdrug"]
x3 <- womenshealth[,"atthouse"]
x4 <- womenshealth[,"income"]
x5 <- womenshealth[,"emplmnt"]
x6 <- womenshealth[,"mstatus"]
x7 <- womenshealth[,"race"]
x8 <- womenshealth[,"log10.ltimedrs"]

#Mean of X's
M1 = mean(x1)
M2 = mean(x2)
M3 = mean(x3)
M4 =mean(x4)
M5 = mean(x5)
M6 =mean(x6)
M7 = mean(x7)
M8 = mean(x8)

# D =(𝑥 − mean x)
D1 = x1 - M1
D2 = x2 - M2
D3 = x3 - M3
D4 = x4 - M4
D5 = x5 - M5
D6 = x6 - M6
D7 = x7 - M7
D8 = x8 - M8
D_mat = cbind(D1,D2,D3,D4,D5,D6,D7,D8)

# Covariance Matrix
S = (1/(436)) * crossprod(D_mat) #covariance matrix?
S
cov(x1,x2)
cov(D_mat)
diag(S) #the covariance of the variable with itself is simply its variance
#depand on scales of measurment 

#Correlation Matrix
D = 1/sqrt(diag(S))
cov2cor(S)

