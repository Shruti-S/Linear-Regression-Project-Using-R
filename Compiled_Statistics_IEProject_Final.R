rm(list=ls())

setwd("~/Desktop/UMASS/Classes/Junior Year Semester 2 Classes/Statistics 525/R_CodeFile_Stats525/IE_Project")
crimerate <- read.csv(file="crime.csv")

########################################################################################
#To figure out which X works better for the chosen Y (Crime) for this paper, 
#We do Pearsons Correlation for the whole data set:

Header_names <- names(crimerate)
increase <- 1
totalFinal_1 <- c()
#pearson: 
for (val in Header_names)
{
        mean_y <- mean(crimerate[[Header_names[16]]])
        mean_x1 <- mean(crimerate[[Header_names[increase]]])
        x1_difference <- crimerate[[Header_names[increase]]] - mean_x1
        y_difference <- crimerate[[Header_names[16]]] - mean_y
        upperFinal_1 <- sum(x1_difference * y_difference)
        lowerFinal_1 <- sqrt(sum(x1_difference^2)*sum(y_difference^2))
        totalFinal_1[increase] <- upperFinal_1/lowerFinal_1
        increase <- increase + 1
}

barplot(totalFinal_1, names.arg =  Header_names)



########################################################################
##Original Data Frame

summary(crimerate)
Y <- crimerate$Crime
X1 <- crimerate$Ed
X2 <- crimerate$NW
X3 <- crimerate$Po1
X4 <- crimerate$Prob
X2t <- log(X2)



########################################################################################
##Boxplot, Outliers and Descriptive Statistics With Original Data Frame

#X1 Boxplot, Outliers and Descriptive Statistics
boxplot(X1, horizontal=TRUE, xlab="Education Years", 
        main="Education Years Distribution")
axis(1, at=seq(8,13, by=0.5)) 
summary(X1)
sd(X1)
var(X1)
range(X1)

#X2 Boxplot, Outliers and Descriptive Statistics
boxplot(X2, horizontal=TRUE, xlab="Percentage of Non-whites", 
        main="Percentage of Non-White Distribution")
axis(1, at=seq(0,50, by=1)) 
summary(X2)
sd(X2)
var(X2)
range(X2)

#Transformed X2 (X2t) Boxplot, Outliers and Descriptive Statistics
boxplot(X2t, horizontal=TRUE, xlab="Percentage of Non-whites", 
        main="Percentage of Non-White Distribution")
axis(1, at=seq(0,50, by=1)) 
summary(X2t)
sd(X2t)
var(X2t)
range(X2t)

#no outliers in transformed X2t data

#X3 Boxplot, Outliers and Descriptive Statistics
boxplot(X3, horizontal=TRUE, xlab="Per Capita Expenditure on Police", 
        main="Per Capita Expenditure on Police Protection in 1960 Distribution")
axis(1, at=seq(4,18, by=1)) 
summary(X3)
sd(X3)
var(X3)
range(X3)

#X4 Boxplot, Outliers and Descriptive Statistics
boxplot(X4, horizontal=TRUE, xlab="Probability of Imprisonment(#Commitments/#Offenses)", 
        main="Probability of Imprisonment Distribution")
axis(1, at=seq(0,0.13, by=0.01)) 
summary(X4)
sd(X4)
var(X4)
range(X4)

outlierX4 <- which(X4 > 0.085 & Y[18] & Y[22] & Y[42])
outlierX4

#Y Boxplot, Outliers and Descriptive Statistics
boxplot(Y, horizontal=TRUE, xlab="Crime Rate", 
        main="Crime Rate Distribution")
axis(1, at=seq(0,2000, by=50)) 
summary(Y)
sd(Y)
var(Y)
range(Y)

outlierY <- which(
                  Y > 1650 & X1[4] & X2[4] & X3[4] & X4[4] 
                  & X1[11] & X2[11] & X3[11] & X4[11]
                  & X1[26] & X2[26] & X3[26] & X4[26]
                  )
outlierY



########################################################################################
##Scatterplots With Original Data Frame & Matrices

#X1 Scatter Plot
plot(X1, Y, xlab="Education Years", ylab="Crime Rate", 
     main="Crime Rate and Education Years")

#X2 Scatter Plot
plot(X2, Y, xlab="Percentage of Non-Whites", ylab="Crime Rate", 
     main="Crime Rate and Percentage of Non-Whites")

#Transformed X2 (X2t) Scatter Plot
plot(X2t, Y, xlab="Percentage of Non-Whites", ylab="Crime Rate", 
     main="Crime Rate and Percentage of Non-Whites")

#X3 Scatter Plot
plot(X3, Y, xlab="Per Capita Expenditure on Police", ylab="Crime Rate", 
     main="Crime Rate and Per Capita Expenditure on Police Protection in 1960")

#X4 Scatter Plot
plot(X4, Y, xlab="Probability of Imprisonment(#Commitments/#Offenses)", ylab="Crime Rate", 
     main="Crime Rate and Probability of Imprisonment")

#X1 and X2 transformed 
plot(X1, X2t, xlab="Education in Years", ylab="Percentage of Non-Whites", 
     main="Figure 9-Percentage of Non-Whites and Education in Years")
cor(X1, X2t, use="all.obs", method="pearson")
#there is a slight downward slope but no significant relationship betwee the two variables

#X1 and X3
plot(X1, X3, xlab="Education in Years", ylab="per capita expenditure on police protection", 
     main="Percapita expenditure on police protection and Education in Years")
abline(lm(crimerate$Po1~crimerate$Ed))
cor(X1, X3, use="all.obs", method="pearson")
#there is a weak positive corelation.

#X1 and X4
plot(X1, X4, xlab="Education in Years", ylab="Probability of imprisonment", 
     main="Probability of imprisonment and Education in Years")
cor(X1, X4, use="all.obs", method="pearson")
#NO significant relationship

#X2 transformed and X4
plot(X2t, X4, xlab="Percentage of non-whites", ylab="Probability of imprisonment", 
     main="Probability of imprisonment and Percentage of non-whites")
cor(X2t, X4, use="all.obs", method="pearson")
#no significant relationship

#X3 and transformed X2 
plot(X3, X2t, ylab="Percentage of non-whites", xlab="Per capita expenditure on police protection", 
     main="Figure 9-Per capita expenditure on police protection and Percentage of non-whites")
cor(X3, X2t, use="all.obs", method="pearson")
#no significant relationship

#X3 and X4
plot(crimerate$Po1, crimerate$Prob, xlab="Per capita expenditure on police protection", ylab="Probability of imprisonment", 
     main="Figure 11-Probability of imprisonment and Per capita expenditure on police protection")
cor(X3, X4, use="all.obs", method="pearson")
#as the percapita expenditure goes up the probability of imprisonment goes down

#Scatterplot Matrix with transformed X2
pairs(~Y+X1+X2t+X3+X4, data=crimerate)



########################################################################################
##Linear Regressions & Pearson Correlations For X1, X2, X2 Transformed,
#X3 and x4 With Original Data Frame

#X1 Linear Regression
Y_X1 <- lm(Y ~ X1)
summary(Y_X1)
plot(Y~X1, xlab="Education Years", ylab="Crime Rate", 
     main="Crime Rate and Education Years")
abline(Y_X1, col="blue")
cor(X1, Y, use="all.obs", method="pearson")

#X2 Linear Regression
Y_X2 <- lm(Y ~ X2)
summary(Y_X2)
plot(Y~X2, xlab="Percentage of Non-Whites", ylab="Crime Rate", 
     main="Crime Rate and Percentage of Non-Whites")
abline(Y_X2, col="blue")
cor(X2, Y, use="all.obs", method="pearson")

#Transformed X2 (X2t) Linear Regression
X2t <- log(X2)
Y_X2t <- lm(Y ~ X2t)
summary(Y_X2t)
plot(Y~X2t, xlab="Percentage of Non-Whites(Transformed)", ylab="Crime Rate", 
     main="Crime Rate and Percentage of Non-Whites(Transformed)")
abline(Y_X2t, col="blue")
cor(X2t, Y, use="all.obs", method="pearson")

#X3 Linear Regression
Y_X3 <- lm(Y ~ X3)
summary(Y_X3)
plot(Y~X3, xlab="Per Capita Expenditure on Police", ylab="Crime Rate", 
     main="Crime Rate and Per Capita Expenditure on Police Protection in 1960")
abline(Y_X3, col="blue")
cor(X3, Y, use="all.obs", method="pearson")

#X4 Linear Regression
Y_X4 <- lm(Y ~ X4)
summary(Y_X4)
plot(Y~X4, xlab="Probability of Imprisonment(#Commitments/#Offenses)", ylab="Crime Rate", 
     main="Crime Rate and Probability of Imprisonment")
abline(Y_X4, col="blue")
cor(X4, Y, use="all.obs", method="pearson")

#Multiple Regression-Model 1 
model1<-data.frame(Y,X1,X2t,X3,X4)
model1reg <- lm(Y~X1+X2t+X3+X4)
summary(model1reg)

#R-squared
summary(model1reg)$r.squared

#Correlation Matrix
cor(model1)



########################################################################################
##Residual, Residuals vs Fitted Values and QQ Plots With Original Data Frame

#X1 Residual & QQ Plots to Check
Resid_X1 <- Y_X1$residuals
plot(Resid_X1, main="Residual Plot of Crime vs Education Years")
abline(h=0, col="red")

qqnorm(Resid_X1)

#X2 Residual & QQ Plots to Check
Resid_X2 <- Y_X2$residuals
plot(Resid_X2, main="Residual Plot of Crime vs Percentage of Non-Whites")
abline(h=0, col="red")

qqnorm(Resid_X2)

#Transformed X2 (X2t) Residual Plot to Check
Resid_X2t <- Y_X2t$residuals
plot(Resid_X2t, main="Residual Plot of Crime vs Percentage of Non-Whites(Transformed)")
abline(h=0, col="red")

qqnorm(Resid_X2t)

#X3 Residual & QQ Plots to Check
Resid_X3 <- Y_X3$residuals
plot(Resid_X3, main="Residual Plot of Crime vs Per Capita Expenditure on Police Protection in 1960")
abline(h=0, col="red")

qqnorm(Resid_X3)

#X4 Residual & QQ Plots to Check
Resid_X4 <- Y_X4$residuals
plot(Resid_X4, main="Residual Plot of Crime vs Probability of Imprisonment")
abline(h=0, col="red")

qqnorm(Resid_X4)

#X1 Residuals against Fitted values Plot 
Y_X1_fit <- Y_X1$fitted.values
plot(Y_X1_fit, Resid_X1, 
     main="Residual Plot of Crime vs Education Years Fitted Values")
abline(h=0, col="red")

#X2 Residuals against Fitted values Plot 
Y_X2_fit <- Y_X2$fitted.values
plot(Y_X2_fit, Resid_X2, 
     main="Residual Plot of Crime vs Percentage of Non-Whites Fitted Values")
abline(h=0, col="red")

#X2t Residuals against Fitted values Plot 
Y_X2t_fit <- Y_X2t$fitted.values
plot(Y_X2t_fit, Resid_X2t, 
     main="Residual Plot of Crime vs Percentage of Non-Whites (Transformed) Fitted Values")
abline(h=0, col="red")

#X3 Residuals against Fitted values Plot 
Y_X3_fit <- Y_X3$fitted.values
plot(Y_X3_fit, Resid_X3, 
     main="Residual Plot of Crime vs Per Capita Expenditure on Police Protection in 1960 Fitted Values")
abline(h=0, col="red")

#X4 Residuals against Fitted values Plot 
Y_X4_fit <- Y_X4$fitted.values
plot(Y_X4_fit, Resid_X4, 
     main="Residual Plot of Crime vs Probability of Imprisonment Fitted Values")
abline(h=0, col="red")

#Multiple Regression Residuals against Fitted values Plot
model1resid <- model1reg$residuals
model1fit <- model1reg$fitted.values
plot(model1fit, model1resid, 
     main="Figure 16-Multiple Regression(Original Data Frame) Residual vs Fitted Values Plot")
abline(h=0, col="red")



############################################################################################################
##New Data Frame with Outliers Omitted

#Outliers omitted were X4: i=18, 22, 42, Y: i=4, 11, 26
outlier_crime <- rbind(crimerate[1:3, ], crimerate[5:10, ], 
                       crimerate[12:17, ], crimerate[19:21, ], 
                       crimerate[23:25, ], crimerate[27:41, ], crimerate[43:47, ])
summary(outlier_crime)
Y.o <- outlier_crime$Crime
X1.o <- outlier_crime$Ed
X2.o <- outlier_crime$NW
X3.o <- outlier_crime$Po1
X4.o <- outlier_crime$Prob
X2t.o <- log(X2.o)

#Scatterplot Matrix with transformed X2
pairs(~Y.o+X1.o+X2t.o+X3.o+X4.o, data=outlier_crime)



########################################################################################
##Linear Regressions & Pearson Correlations With New Data Frame
# marked with asterisk * are ones to include in final report

#X1.o Linear Regression *
Y.o_X1.o <- lm(Y.o ~ X1.o)
summary(Y.o_X1.o)
plot(Y.o~X1.o, xlab="Education Years", ylab="Crime Rate", 
     main="Crime Rate and Education Years")
abline(Y.o_X1.o, col="blue")
abline(Y_X1, col="red")
cor(X1.o, Y.o, use="all.obs", method="pearson")

#X2.o Linear Regression
Y.o_X2.o <- lm(Y.o ~ X2.o)
summary(Y.o_X2.o)
plot(Y.o~X2.o, xlab="Percentage of Non-Whites", ylab="Crime Rate", 
     main="Crime Rate and Percentage of Non-Whites")
abline(Y.o_X2.o, col="blue")
cor(X2.o, Y.o, use="all.obs", method="pearson")

#X2t.o Linear Regression *
Y.o_X2t.o <- lm(Y.o ~ X2t.o)
summary(Y.o_X2t.o)
plot(Y.o~X2t.o, xlab="Percentage of Non-Whites(Transformed)", ylab="Crime Rate", 
     main="Crime Rate and Percentage of Non-Whites(Transformed)")
abline(Y.o_X2t.o, col="blue")
abline(Y_X2t, col="red")
cor(X2t.o, Y.o, use="all.obs", method="pearson")

#X3.o Linear Regression *
Y.o_X3.o <- lm(Y.o ~ X3.o)
summary(Y.o_X3.o)
plot(Y.o~X3.o, xlab="Per Capita Expenditure on Police", ylab="Crime Rate", 
     main="Crime Rate and Per Capita Expenditure on Police Protection in 1960")
abline(Y.o_X3.o, col="blue")
abline(Y_X3, col="red")
cor(X3.o, Y.o, use="all.obs", method="pearson")

#X4.o Linear Regression *
Y.o_X4.o <- lm(Y.o ~ X4.o)
summary(Y.o_X4.o)
plot(Y.o~X4.o, xlab="Probability of Imprisonment(#Commitments/#Offenses)", ylab="Crime Rate", 
     main="Crime Rate and Probability of Imprisonment")
abline(Y.o_X4.o, col="blue")
abline(Y_X4, col="red")
cor(X4.o, Y.o, use="all.obs", method="pearson")

#Multiple Regression-Model 2 
model2 <- data.frame(Y.o, X1.o,X2t.o,X3.o,X4.o)
model2reg <- lm(Y.o~X1.o+X2t.o+X3.o+X4.o)
summary(model2reg)

#R-squared
summary(model2reg)$r.squared

#Correlation Matrix
cor(model2)



########################################################################################
##Residual, Residual vs Fitted Values and QQ Plots With New Data Frame

#X1.o Residual & QQ Plots to Check
Resid_X1.o <- Y.o_X1.o$residuals
plot(Resid_X1.o, main="Residual Plot of Crime vs Education Years")
abline(h=0, col="red")

qqnorm(Resid_X1.o)

#X2.o Residual & QQ Plots to Check
Resid_X2.o <- Y.o_X2.o$residuals
plot(Resid_X2, main="Residual Plot of Crime vs Percentage of Non-Whites")
abline(h=0, col="red")

qqnorm(Resid_X2.o)

#X2t.o Residual Plot to Check
Resid_X2t.o <- Y.o_X2t.o$residuals
plot(Resid_X2t.o, main="Residual Plot of Crime vs Percentage of Non-Whites(Transformed)")
abline(h=0, col="red")

qqnorm(Resid_X2t.o)

#X3.o Residual & QQ Plots to Check
Resid_X3.o <- Y.o_X3.o$residuals
plot(Resid_X3.o, main="Residual Plot of Crime vs Per Capita Expenditure on Police Protection in 1960")
abline(h=0, col="red")

qqnorm(Resid_X3.o)

#X4.o Residual & QQ Plots to Check
Resid_X4.o <- Y.o_X4.o$residuals
plot(Resid_X4.o, main="Residual Plot of Crime vs Probability of Imprisonment")
abline(h=0, col="red")

qqnorm(Resid_X4.o)

#X1.o Residuals against Fitted values Plot 
Y_X1_fit.o <- Y.o_X1.o$fitted.values
plot(Y_X1_fit.o, Resid_X1.o, 
     main="Residual Plot of Crime vs Education Years Fitted Values")
abline(h=0, col="red")

#X2.o Residuals against Fitted values Plot 
Y_X2_fit.o <- Y.o_X2.o$fitted.values
plot(Y_X2_fit.o, Resid_X2.o, 
     main="Residual Plot of Crime vs Percentage of Non-Whites Fitted Values")
abline(h=0, col="red") 

#X2t.o Residuals against Fitted values Plot 
Y_X2t_fit.o <- Y.o_X2t.o$fitted.values
plot(Y_X2t_fit.o, Resid_X2t.o, 
     main="Residual Plot of Crime vs Percentage of Non-Whites (Transformed) Fitted Values")
abline(h=0, col="red")

#X3.o Residuals against Fitted values Plot 
Y_X3_fit.o <- Y.o_X3.o$fitted.values
plot(Y_X3_fit.o, Resid_X3.o, 
     main="Residual Plot of Crime vs Per Capita Expenditure on Police Protection in 1960 Fitted Values")
abline(h=0, col="red")

#X4.o Residuals against Fitted values Plot 
Y_X4_fit.o <- Y.o_X4.o$fitted.values
plot(Y_X4_fit.o, Resid_X4.o, 
     main="Residual Plot of Crime vs Probability of Imprisonment Fitted Values")
abline(h=0, col="red")

#Multiple Regression(Model2) Residuals against Fitted values Plot
model2resid <- model2reg$residuals
model2fit <- model2reg$fitted.values
plot(model2fit, model2resid, 
     main="Figure 17-Multiple Regression(Outliers Omitted) Residual vs Fitted Values Plot")
abline(h=0, col="red")



########################################################################################
##Hypothesis Testing with New Data Frame
#lowest correlation: Y.o and X1.o
cor(model2)
summary(Y.o_X1.o) 
anova(model2reg)

#99%
alpha <- 0.01
n <- length(Y.o)
qf(1-alpha, 4, n-5)

#95%
alpha1 <- 0.05
n <- length(Y.o)
qf(1-alpha1, 4, n-5)





