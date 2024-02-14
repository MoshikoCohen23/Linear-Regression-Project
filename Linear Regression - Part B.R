library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
#library(readxl)
# library(knitr)
# library(rmarkdown)
library(simmer)
library(simmer.plot)
install.packages(lmtest)
install.packages("strucchange")
install.packages("lmtest")
library(strucchange)
library(lmtest)

filePath=choose.files() 
ImportData <-read.csv(filePath,header=TRUE)
Dataset <- na.omit(ImportData)

  
## 2

  ##2.1
  Cor <- cor(Dataset[ , 2:11])
  print(Cor [, 1])

  cor.test(Dataset$X5, Dataset$Y,alternative = "two.sided",method = "pearson")
  cor.test(Dataset$X6, Dataset$Y,alternative = "two.sided",method = "pearson")
  cor.test(Dataset$X7, Dataset$Y,alternative = "two.sided",method = "pearson")

  plot (main = "Life expectancy by continent ", x = as.factor(Dataset$X8), y= Dataset$Y,xlab = "Continent", ylab = "Life expectancy" )

  cor(Dataset$X1,Dataset$Y, method = "pearson")
  mod<-lm(Dataset$Y ~ Dataset$X1 + Dataset$X2 + Dataset$X3 + Dataset$X4 + Dataset$X5 + Dataset$X6 + Dataset$X7 + Dataset$X8 + Dataset$X9 )
  summary(mod)

  # #2.2
  subdata <- sqldf("select Y, X1,X2,X3, X4, X5, X6, X7
  from Dataset ")
  plot (main = "scatter plot", subdata)

  Dataset[13,10] = 3
  Dataset[29,10] = 3


  #  2.4
  mod1_8<-lm(Dataset$Y ~ Dataset$X1*as.factor(Dataset$X8))
  summary(mod1_8)
  plot(Dataset$X1[as.factor(Dataset$X8)==1] ,Dataset$Y[as.factor(Dataset$X8)==1], col="blue",xlim = c(0,100),ylim=c(0,100) ,xlab="Outdoor air pollution (%)",ylab="Life expectancy (year)", main = "Life expectancy by Outdoor air pollution")
  points(Dataset$X1[as.factor(Dataset$X8)==2] ,Dataset$Y[as.factor(Dataset$X8)==2], col="green")
  points(Dataset$X1[as.factor(Dataset$X8)==3] ,Dataset$Y[as.factor(Dataset$X8)==3], col="red")
  points(Dataset$X1[as.factor(Dataset$X8)==5] ,Dataset$Y[as.factor(Dataset$X8)==5], col="black")
  abline(a=(mod1_8$coefficients[1]), b=(mod1_8$coefficients[2]), col="blue")
  abline(a=(mod1_8$coefficients[1]+mod1_8$coefficients[3]), b=(mod1_8$coefficients[2]+mod1_8$coefficients[6]), col="green")
  abline(a=(mod1_8$coefficients[1]+mod1_8$coefficients[4]), b=(mod1_8$coefficients[2]+mod1_8$coefficients[7]), col="red")
  abline(a=(mod1_8$coefficients[1]+mod1_8$coefficients[5]), b=(mod1_8$coefficients[2]+mod1_8$coefficients[8]), col="black")

  mod2_8<-lm(Dataset$Y ~ Dataset$X2*as.factor(Dataset$X8))
  summary(mod2_8)
  plot(Dataset$X2[as.factor(Dataset$X8)==1] ,Dataset$Y[as.factor(Dataset$X8)==1], col="blue",xlim = c(0,7700000),ylim=c(0,100) ,xlab="HIV - Estimated number of people that have been infected",ylab="Life expectancy (year)", main = "Life expectancy by HIV infected")
  points(Dataset$X2[as.factor(Dataset$X8)==2] ,Dataset$Y[as.factor(Dataset$X8)==2], col="green")
  points(Dataset$X2[as.factor(Dataset$X8)==3] ,Dataset$Y[as.factor(Dataset$X8)==3], col="red")
  points(Dataset$X2[as.factor(Dataset$X8)==5] ,Dataset$Y[as.factor(Dataset$X8)==5], col="black")
  abline(a=(mod2_8$coefficients[1]), b=(mod2_8$coefficients[2]), col="blue")
  abline(a=(mod2_8$coefficients[1]+mod2_8$coefficients[3]), b=(mod2_8$coefficients[2]+mod2_8$coefficients[6]), col="green")
  abline(a=(mod2_8$coefficients[1]+mod2_8$coefficients[4]), b=(mod2_8$coefficients[2]+mod2_8$coefficients[7]), col="red")
  abline(a=(mod2_8$coefficients[1]+mod2_8$coefficients[5]), b=(mod2_8$coefficients[2]+mod2_8$coefficients[8]), col="black")

  mod3_8<-lm(Dataset$Y ~ Dataset$X3*as.factor(Dataset$X8))
  summary(mod3_8)
  plot(Dataset$X3[as.factor(Dataset$X8)==1] ,Dataset$Y[as.factor(Dataset$X8)==1], col="blue",xlim = c(0,82000),ylim=c(0,100) ,xlab="malaria - Estimated number of people that have been infected",ylab="Life expectancy (year)", main = "Life expectancy by malaria infected")
  points(Dataset$X3[as.factor(Dataset$X8)==2] ,Dataset$Y[as.factor(Dataset$X8)==2], col="green")
  points(Dataset$X3[as.factor(Dataset$X8)==3] ,Dataset$Y[as.factor(Dataset$X8)==3], col="red")
  points(Dataset$X3[as.factor(Dataset$X8)==5] ,Dataset$Y[as.factor(Dataset$X8)==5], col="black")
  abline(a=(mod3_8$coefficients[1]), b=(mod3_8$coefficients[2]), col="blue")
  abline(a=(mod3_8$coefficients[1]+mod3_8$coefficients[3]), b=(mod3_8$coefficients[2]+mod3_8$coefficients[6]), col="green")
  abline(a=(mod3_8$coefficients[1]+mod3_8$coefficients[4]), b=(mod3_8$coefficients[2]+mod3_8$coefficients[7]), col="red")
  abline(a=(mod3_8$coefficients[1]+mod3_8$coefficients[5]), b=(mod3_8$coefficients[2]+mod3_8$coefficients[8]), col="black")

  mod4_8<-lm(Dataset$Y ~ Dataset$X4*as.factor(Dataset$X8))
  summary(mod4_8)
  plot(Dataset$X4[as.factor(Dataset$X8)==1] ,Dataset$Y[as.factor(Dataset$X8)==1], col="blue",xlim = c(0,70000),ylim=c(0,100) ,xlab="Average income per person ($)",ylab="Life expectancy (year)", main = "Life expectancy by Average income per person")
  points(Dataset$X4[as.factor(Dataset$X8)==2] ,Dataset$Y[as.factor(Dataset$X8)==2], col="green")
  points(Dataset$X4[as.factor(Dataset$X8)==3] ,Dataset$Y[as.factor(Dataset$X8)==3], col="red")
  points(Dataset$X4[as.factor(Dataset$X8)==5] ,Dataset$Y[as.factor(Dataset$X8)==5], col="black")
  abline(a=(mod4_8$coefficients[1]), b=(mod4_8$coefficients[2]), col="blue")
  abline(a=(mod4_8$coefficients[1]+mod4_8$coefficients[3]), b=(mod4_8$coefficients[2]+mod4_8$coefficients[6]), col="green")
  abline(a=(mod4_8$coefficients[1]+mod4_8$coefficients[4]), b=(mod4_8$coefficients[2]+mod4_8$coefficients[7]), col="red")
  abline(a=(mod4_8$coefficients[1]+mod4_8$coefficients[5]), b=(mod4_8$coefficients[2]+mod4_8$coefficients[8]), col="black")

  mod5_8<-lm(Dataset$Y ~ Dataset$X5*as.factor(Dataset$X8))
  summary(mod5_8)
  plot(Dataset$X5[as.factor(Dataset$X8)==1] ,Dataset$Y[as.factor(Dataset$X8)==1], col="blue",xlim = c(0,35),ylim=c(0,100) ,xlab="Alcohol consumption per person (liters, year)",ylab="Life expectancy (year)", main = "Life expectancy by Alcohol consumption per person")
  points(Dataset$X5[as.factor(Dataset$X8)==2] ,Dataset$Y[as.factor(Dataset$X8)==2], col="green")
  points(Dataset$X5[as.factor(Dataset$X8)==3] ,Dataset$Y[as.factor(Dataset$X8)==3], col="red")
  points(Dataset$X5[as.factor(Dataset$X8)==5] ,Dataset$Y[as.factor(Dataset$X8)==5], col="black")
  abline(a=(mod5_8$coefficients[1]), b=(mod5_8$coefficients[2]), col="blue")
  abline(a=(mod5_8$coefficients[1]+mod5_8$coefficients[3]), b=(mod5_8$coefficients[2]+mod5_8$coefficients[6]), col="green")
  abline(a=(mod5_8$coefficients[1]+mod5_8$coefficients[4]), b=(mod5_8$coefficients[2]+mod5_8$coefficients[7]), col="red")
  abline(a=(mod5_8$coefficients[1]+mod5_8$coefficients[5]), b=(mod5_8$coefficients[2]+mod5_8$coefficients[8]), col="black")

  mod6_8<-lm(Dataset$Y ~ Dataset$X6*as.factor(Dataset$X8))
  summary(mod6_8)
  plot(Dataset$X6[as.factor(Dataset$X8)==1] ,Dataset$Y[as.factor(Dataset$X8)==1], col="blue",xlim = c(0,1300),ylim=c(0,100) ,xlab="density per square (km)",ylab="Life expectancy (year)", main = "Life expectancy by density")
  points(Dataset$X6[as.factor(Dataset$X8)==2] ,Dataset$Y[as.factor(Dataset$X8)==2], col="green")
  points(Dataset$X6[as.factor(Dataset$X8)==3] ,Dataset$Y[as.factor(Dataset$X8)==3], col="red")
  points(Dataset$X6[as.factor(Dataset$X8)==5] ,Dataset$Y[as.factor(Dataset$X8)==5], col="black")
  abline(a=(mod6_8$coefficients[1]), b=(mod6_8$coefficients[2]), col="blue")
  abline(a=(mod6_8$coefficients[1]+mod6_8$coefficients[3]), b=(mod6_8$coefficients[2]+mod6_8$coefficients[6]), col="green")
  abline(a=(mod6_8$coefficients[1]+mod6_8$coefficients[4]), b=(mod6_8$coefficients[2]+mod6_8$coefficients[7]), col="red")
  abline(a=(mod6_8$coefficients[1]+mod6_8$coefficients[5]), b=(mod6_8$coefficients[2]+mod6_8$coefficients[8]), col="black")
  
  mod7_8<-lm(Dataset$Y ~ Dataset$X7*as.factor(Dataset$X8))
  summary(mod7_8)
  plot(Dataset$X7[as.factor(Dataset$X8)==1] ,Dataset$Y[as.factor(Dataset$X8)==1], col="blue",xlim = c(0,40),ylim=c(0,100) ,xlab="Cigarette consumption (%)",ylab="Life expectancy (year)", main = "Life expectancy by Cigarette consumption")
  points(Dataset$X7[as.factor(Dataset$X8)==2] ,Dataset$Y[as.factor(Dataset$X8)==2], col="green")
  points(Dataset$X7[as.factor(Dataset$X8)==3] ,Dataset$Y[as.factor(Dataset$X8)==3], col="red")
  points(Dataset$X7[as.factor(Dataset$X8)==5] ,Dataset$Y[as.factor(Dataset$X8)==5], col="black")
  abline(a=(mod7_8$coefficients[1]), b=(mod7_8$coefficients[2]), col="blue")
  abline(a=(mod7_8$coefficients[1]+mod7_8$coefficients[3]), b=(mod7_8$coefficients[2]+mod7_8$coefficients[6]), col="green")
  abline(a=(mod7_8$coefficients[1]+mod7_8$coefficients[4]), b=(mod7_8$coefficients[2]+mod7_8$coefficients[7]), col="red")
  abline(a=(mod7_8$coefficients[1]+mod7_8$coefficients[5]), b=(mod7_8$coefficients[2]+mod7_8$coefficients[8]), col="black")

  mod1_9<-lm(Dataset$Y ~ Dataset$X1*as.factor(Dataset$X9))
  summary(mod1_9)
  plot(Dataset$X1[as.factor(Dataset$X9)==0] ,Dataset$Y[as.factor(Dataset$X9)==0], col="purple",xlim = c(0,100),ylim=c(0,100) ,xlab="Outdoor air pollution (%)",ylab="Life expectancy (year)", main = "Life expectancy by Outdoor air pollution")
  points(Dataset$X1[as.factor(Dataset$X9)==1] ,Dataset$Y[as.factor(Dataset$X9)==1], col="orange")
  #legend(980,50,legend = c(1,2,3,4),col=c("blue","green", "red", "black"),pch=c(1,1,1,1),bty="n")
  abline(a=(71.41534), b=(-0.06904), col="purple")
  abline(a=(73.12812+2.71944), b=(-0.06904+0.18643), col="orange")

  mod2_9<-lm(Dataset$Y ~ Dataset$X2*as.factor(Dataset$X9))
  summary(mod2_9)
  plot(Dataset$X2[as.factor(Dataset$X9)==0] ,Dataset$Y[as.factor(Dataset$X9)==0], col="purple",xlim = c(0,7700000),ylim=c(0,100) ,xlab="HIV infected",ylab="Life expectancy (year)", main = "Life expectancy by HIV infected")
  points(Dataset$X2[as.factor(Dataset$X9)==1] ,Dataset$Y[as.factor(Dataset$X9)==1], col="orange")
  #legend(980,50,legend = c(1,2,3,4),col=c("blue","green", "red", "black"),pch=c(1,1,1,1),bty="n")
  abline(a=(mod2_9$coefficients[1]), b=(mod2_9$coefficients[2]), col="purple")
  abline(a=(mod2_9$coefficients[1]+mod2_9$coefficients[3]), b=(mod2_9$coefficients[2]+mod2_9$coefficients[4]), col="orange")

  mod3_9<-lm(Dataset$Y ~ Dataset$X3*as.factor(Dataset$X9))
  summary(mod3_9)
  plot(Dataset$X3[as.factor(Dataset$X9)==0] ,Dataset$Y[as.factor(Dataset$X9)==0], col="purple",xlim = c(0,82000),ylim=c(0,100) ,xlab="Malaria infected",ylab="Life expectancy (year)", main = "Life expectancy by Malaria infected")
  points(Dataset$X3[as.factor(Dataset$X9)==1] ,Dataset$Y[as.factor(Dataset$X9)==1], col="orange")
  #legend(980,50,legend = c(1,2,3,4),col=c("blue","green", "red", "black"),pch=c(1,1,1,1),bty="n")
  abline(a=(mod3_9$coefficients[1]), b=(mod3_9$coefficients[2]), col="purple")
  abline(a=(mod3_9$coefficients[1]+mod3_9$coefficients[3]), b=(mod3_9$coefficients[2]+0), col="orange")

  mod4_9<-lm(Dataset$Y ~ Dataset$X4*as.factor(Dataset$X9))
  summary(mod4_9)
  plot(Dataset$X4[as.factor(Dataset$X9)==0] ,Dataset$Y[as.factor(Dataset$X9)==0], col="purple",xlim = c(0,82000),ylim=c(0,100) ,xlab="Average income per person ($)",ylab="Life expectancy (year)", main = "Life expectancy by Average income")
  points(Dataset$X4[as.factor(Dataset$X9)==1] ,Dataset$Y[as.factor(Dataset$X9)==1], col="orange")
  #legend(980,50,legend = c(1,2,3,4),col=c("blue","green", "red", "black"),pch=c(1,1,1,1),bty="n")
  abline(a=(mod4_9$coefficients[1]), b=(mod4_9$coefficients[2]), col="purple")
  abline(a=(mod4_9$coefficients[1]+mod4_9$coefficients[3]), b=(mod4_9$coefficients[2]+mod4_9$coefficients[4]), col="orange")

  mod5_9<-lm(Dataset$Y ~ Dataset$X5*as.factor(Dataset$X9))
  summary(mod5_9)
  plot(Dataset$X5[as.factor(Dataset$X9)==0] ,Dataset$Y[as.factor(Dataset$X9)==0], col="purple",xlim = c(0,35),ylim=c(0,100) ,xlab="Alcohol consumption per person (liters, year)",ylab="Life expectancy (year)", main = "Life expectancy by Alcohol consumption")
  points(Dataset$X5[as.factor(Dataset$X9)==1] ,Dataset$Y[as.factor(Dataset$X9)==1], col="orange")
  #legend(980,50,legend = c(1,2,3,4),col=c("blue","green", "red", "black"),pch=c(1,1,1,1),bty="n")
  abline(a=(mod5_9$coefficients[1]), b=(mod5_9$coefficients[2]), col="purple")
  abline(a=(mod5_9$coefficients[1]+mod5_9$coefficients[3]), b=(mod5_9$coefficients[2]+mod5_9$coefficients[4]), col="orange")

  mod6_9<-lm(Dataset$Y ~ Dataset$X6*as.factor(Dataset$X9))
  summary(mod6_9)
  plot(Dataset$X6[as.factor(Dataset$X9)==0] ,Dataset$Y[as.factor(Dataset$X9)==0], col="purple",xlim = c(0,1300),ylim=c(0,100) ,xlab="Density per square (km)",ylab="Life expectancy (year)", main = "Life expectancy by density")
  points(Dataset$X6[as.factor(Dataset$X9)==1] ,Dataset$Y[as.factor(Dataset$X9)==1], col="orange")
  #legend(980,50,legend = c(1,2,3,4),col=c("blue","green", "red", "black"),pch=c(1,1,1,1),bty="n")
  abline(a=(mod6_9$coefficients[1]), b=(mod6_9$coefficients[2]), col="purple")
  abline(a=(mod6_9$coefficients[1]+mod6_9$coefficients[3]), b=(mod6_9$coefficients[2]+mod6_9$coefficients[4]), col="orange")

  mod7_9<-lm(Dataset$Y ~ Dataset$X7*as.factor(Dataset$X9))
  summary(mod7_9)
  plot(Dataset$X7[as.factor(Dataset$X9)==0] ,Dataset$Y[as.factor(Dataset$X9)==0], col="purple",xlim = c(0,40),ylim=c(0,100) ,xlab="Cigarette consumption (%)",ylab="Life expectancy (year)", main = "Life expectancy by Cigarette consumption")
  points(Dataset$X7[as.factor(Dataset$X9)==1] ,Dataset$Y[as.factor(Dataset$X9)==1], col="orange")
  #legend(980,50,legend = c(1,2,3,4),col=c("blue","green", "red", "black"),pch=c(1,1,1,1),bty="n")
  abline(a=(mod7_9$coefficients[1]), b=(mod7_9$coefficients[2]), col="purple")
  abline(a=(mod7_9$coefficients[1]+mod7_9$coefficients[3]), b=(mod7_9$coefficients[2]+mod7_9$coefficients[4]), col="orange")


  # #### 3
  DatasetNew <- sqldf("select Y, X1, X2, X3, X4, X8, X9
                        from Dataset ")

  NewModel <- lm(DatasetNew$Y ~ DatasetNew$X1 + DatasetNew$X2 * factor(DatasetNew$X8) +
                   DatasetNew$X2 * factor(DatasetNew$X9)+  DatasetNew$X3 * factor(DatasetNew$X8)+
                    + DatasetNew$X4 )
  summary(NewModel)
 

  #3.1

  #Empty model
  Emp <- lm (Y ~ 1,data=DatasetNew)
  summary(Emp)
  
  #Full model
  Full <- lm (DatasetNew$Y ~ DatasetNew$X1 + DatasetNew$X2 * factor(DatasetNew$X8) +
                DatasetNew$X2 * factor(DatasetNew$X9)+  DatasetNew$X3 * factor(DatasetNew$X8)+
                DatasetNew$X3 + DatasetNew$X4)
  summary(Full)
 

  ### FORWARD ###
  fwd.model <-  step(Emp, direction='forward', scope= ~ DatasetNew$Y ~ DatasetNew$X1 + DatasetNew$X2 * factor(DatasetNew$X8) +
                       DatasetNew$X2 * factor(DatasetNew$X9)+  DatasetNew$X3 * factor(DatasetNew$X8)+
                       DatasetNew$X3 + DatasetNew$X4)
  summary(fwd.model)
 
  ###  BACKWARD ###
   bw.model <-  step(Full, direction='backward', scope= ~1)
  summary(bw.model)

  ### STEPWISE ###
  sw.model <-  step(Emp, direction='both', scope= ~ DatasetNew$Y ~ DatasetNew$X1 + DatasetNew$X2 * factor(DatasetNew$X8) +
                      DatasetNew$X2 * factor(DatasetNew$X9)+  DatasetNew$X3 * factor(DatasetNew$X8)+
                      DatasetNew$X3 + DatasetNew$X4)
  summary(sw.model)
  
  
  ######3.2 הנחות המודל
  FinalModel <- lm(  DatasetNew$Y ~ DatasetNew$X2 + factor(DatasetNew$X8) + 
  DatasetNew$X3 + DatasetNew$X4 + DatasetNew$X2:factor(DatasetNew$X8) + 
  factor(DatasetNew$X8):DatasetNew$X3)
  summary(FinalModel)
  
  ## הנחות מודל הרגרסיה
  # שלב א - תרשימי פיזור 
   Dataset$fitted <- fitted(FinalModel) #predicted value
   Dataset$residuals <- residuals(FinalModel) #residuals
   s.e_res <- sqrt(var(Dataset$residuals)) 
   Dataset$stan_residuals <- (residuals(FinalModel)/s.e_res)
   plot(Dataset$stan_residuals , xlab = "Normalized error", ylab = "Predicted value" , main = "Normalized error by Predicted value") # תרשים פיזור של השגיאה המתוקננת
   abline(a=0,b=0)
   
   qqnorm(Dataset$stan_residuals)
   abline(a=0, b=1)

   hist(Dataset$stan_residuals, xlab = "Normalized error" , main = "Histogram of normalized error")
   
   
   
  #שלב ב - מבחנים סטטיסים
   
  ## מבחני נורמליות
  ks.test(x= Dataset$stan_residuals, y="pnorm", alternative = "two.sided", exact = NULL)
  shapiro.test(Dataset$stan_residuals)  
  
  sctest(FinalModel,type="Chow")
  # מבחן שיויון שונויות
  gqtest(FinalModel,alternative = "two.sided",fraction=30, data = DatasetNew)

  
  #### 4. שיפור המודל 
  boxcox(FinalModel, lambda = seq(0,6))
  FinalModelTrans <- lm(  (DatasetNew$Y)^3 ~ DatasetNew$X2 + factor(DatasetNew$X8) + 
                       DatasetNew$X3 + DatasetNew$X4 + DatasetNew$X2:factor(DatasetNew$X8) + 
                       factor(DatasetNew$X8):DatasetNew$X3)
  summary(FinalModelTrans)
  
  
  Dataset$fitted2 <- fitted(FinalModel) #predicted value
  Dataset$residuals2 <- residuals(FinalModel) #residuals
  s.e_res2 <- sqrt(var(Dataset$residuals)) 
  Dataset$stan_residuals2 <- (residuals(FinalModel)/s.e_res2)
  plot(Dataset$stan_residuals2 , xlab = "Normalized error", ylab = "Predicted value" , main = "Normalized error by Predicted value") # תרשים פיזור של השגיאה המתוקננת
  abline(a=0,b=0)
  # מקיים הנחת נורמליות
  shapiro.test(Dataset$stan_residuals2)
  #  מקיים שיוין שונויות
  gqtest(FinalModelTrans,alternative = "two.sided",fraction=30, data = DatasetNew)
  # מקיים הנחת ליניאריות
  sctest(FinalModelTrans,type="Chow")
  
  
  
 