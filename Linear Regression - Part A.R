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
#install.packages("viridis")
library(viridis)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("hrbrthemes")
library(hrbrthemes)
#install.packages("maps")
#library(maps)


filePath=choose.files() 
ImportData <-read.csv(filePath,header=TRUE)
Dataset <- na.omit(ImportData)


## 4
subdata <- sqldf("select X1, X2, X3, X4, X5, X6, X7
                  from Dataset")
plot (subdata)
  
# 4_A
  plot (x = subdata$X2, y= subdata$X3,xlab = "HIV infected", ylab = "Malaria infected" )
  fit <- lm(subdata$X3 ~ subdata$X2, data = subdata)
  abline(fit)
  cor(subdata$X2, subdata$X3) %>% print()
# 4_B
  plot (x = subdata$X6, y= subdata$X3,xlab = "Density population", ylab = "Malaria infected" )
  fit <- lm(subdata$X3 ~ subdata$X6, data = subdata)
  abline(fit)
  cor(subdata$X6, subdata$X3) %>% print()
# 4_C
  plot (x = subdata$X5, y= subdata$X7,xlab = "Alcohol consumption", ylab = "Cigarette consumption" )
  fit <- lm(subdata$X7 ~ subdata$X5, data = subdata)
  abline(fit)
  cor(subdata$X5, subdata$X7) %>% print()
# 4_D
  plot (x = subdata$X6, y= subdata$X1,xlab = "Density population", ylab = "Outdoor air pollution" )
  fit <- lm(subdata$X1 ~ subdata$X6, data = subdata)
  abline(fit)
  cor(subdata$X6, subdata$X1) %>% print()
# 4_E
  plot (x = subdata$X4, y= subdata$X2,xlab = "Average income per person", ylab = "HIV infected" )
  fit <- lm(subdata$X2 ~ subdata$X4, data = subdata)
  abline(fit)
  cor(subdata$X4, subdata$X2) %>% print()

## 5

# X1
summary(Dataset$X1)
sd(Dataset$X1)
skewness(Dataset$X1)

# X2
summary(Dataset$X2)
sd(Dataset$X2)
skewness(Dataset$X2)

# X3
summary(Dataset$X3)
sd(Dataset$X3)
skewness(Dataset$X3)

# X4
summary(Dataset$X4)
sd(Dataset$X4)
skewness(Dataset$X4)

# X5
summary(Dataset$X5)
sd(Dataset$X5)
skewness(Dataset$X5)

# X6
summary(Dataset$X6)
sd(Dataset$X6)
skewness(Dataset$X6)

# X7
summary(Dataset$X7)
sd(Dataset$X7)
skewness(Dataset$X7)

# X8
summary(Dataset$X8)

# X9
summary(Dataset$X9)


## 6

bp1 <- boxplot(Dataset$X1, main = "Outdoor air pollution")

DatasetNew <- sqldf("select *
               from Dataset
                where X1 < 98 ")
bp1New <- boxplot(DatasetNew$X1, main = "New outdoor air pollution")

bp2 <- boxplot(DatasetNew$X2, main = "HIV - Estimated number of people that have been infected")
bp3 <- boxplot(DatasetNew$X3, main = "Malaria - Estimated number of people that have been infected")
bp4 <- boxplot(DatasetNew$X4, main = "Average income per person")
bp5 <- boxplot(DatasetNew$X5, main = "Alcohol consumption per person")
bp6 <- boxplot(DatasetNew$X6, main = "Density per square")
bp7 <- boxplot(DatasetNew$X7, main = "Cigarette consumption")


## 7
hist(DatasetNew$X1,prob=TRUE,ylim = c(0,0.04) , main="PDF - Outdoor air pollution",xlab = "Percent of air pollution" ,col="grey")
lines(density(DatasetNew$X1),col="blue",lwd=2)
plot.ecdf(DatasetNew$X1, main="CDF - Outdoor air pollution",xlab="Percent of air pollution", lwd=7)

hist(DatasetNew$X6,xlim = c(0,1400),ylim = c(0,0.01), prob=TRUE, main="PDF - Population density",xlab = "People per square km", breaks = 20,col="grey")
lines(density(DatasetNew$X6),col="blue",lwd=2)
plot.ecdf(DatasetNew$X6, main="CDF - Population density",xlab="People per square km", lwd=7)

hist(DatasetNew$X7,prob=TRUE,ylim = c(0,0.06), main="PDF - Cigarette consumption",xlab = "Percent of People" ,col="grey")
lines(density(DatasetNew$X7),col="blue",lwd=2)
plot.ecdf(DatasetNew$X7, main="CDF - Cigarette consumption",xlab="Percent of People", lwd=7)


## 8
# 8_A
    plot(x=DatasetNew$X8,y=DatasetNew$X3,xlab
         ="Continent",ylab="Malaria infected", main ="Malaria infected by continents" )
#8_B   
    DatasetNew$X9 <-ifelse(DatasetNew$X9==0,"blue","red")
    plot(x=DatasetNew$X8,y=DatasetNew$X4,xlab
         ="Continent",ylab="Average income per person ", main ="Average income by continents", col = DatasetNew$X9 )
    
    DatasetNew$X9 <-ifelse(DatasetNew$X9=="blue","0","1")
    
    Asia_income  <- sqldf ("select X4
                   from DatasetNew
                   where X8 == 1"  )
    Africa_income  <- sqldf ("select X4
                   from DatasetNew
                   where X8 == 2"  )
    Samerica_income  <- sqldf ("select X4
                   from DatasetNew
                   where X8 == 3"  )
    Europe_income  <- sqldf ("select X4
                   from DatasetNew
                   where X8 == 4"  )
    Camerica_income  <- sqldf ("select X4
                   from DatasetNew
                   where X8 == 5"  )
    
    V_Asia_income <- var(Asia_income)
    paste(V_Asia_income)
    V_Africa_income <- var(Africa_income)
    paste(V_Africa_income)
    V_Samerica_income <- var(Samerica_income)
    paste(V_Samerica_income)
    V_Europe_income <- var(Europe_income)
    paste(V_Europe_income)
    V_Camerica_income <- var(Camerica_income)
    paste(V_Camerica_income)
    
    
    A_Asia_income <- mean(Asia_income$X4)
    paste(A_Asia_income)
    A_Africa_income <- mean(Africa_income$X4)
    paste(A_Africa_income)
    A_Samerica_income <- mean(Samerica_income$X4)
    paste(A_Samerica_income)
    A_Europe_income <- mean(Europe_income$X4)
    paste(A_Europe_income)
    A_Camerica_income <- mean(Camerica_income$X4)
    paste(A_Camerica_income)
#8_C
    bubbleplot <- plot_ly(DatasetNew, x = ~DatasetNew$X4, y = ~DatasetNew$Y,
                          color =~DatasetNew$X8 , 
                          marker =
                            list(opacity = 0.7,
                                 sizemode = "diameter"))
    bubbleplot <- bubbleplot%>%layout(title = 'The connetion between income to life expectancy',
                                      xaxis = list(title = 'average income per person'),yaxis = list(title = 'life expectancy (year)'))
    bubbleplot
    cor(DatasetNew$X4, DatasetNew$Y) %>% print()
#8_D 
    ggplot(DatasetNew, aes(x = X5, y = X7)) +
      ggtitle("The conection between alcohol, ciggarette to life expectancy")+
      xlab("Alcohol consumption")  + ylab("Cigarette consumption")+
     # scale_x_continuous(limits = c(0,60),breaks = seq(0,60,by=10)) +
      geom_point(aes(color = as.factor(X8), size = Y), alpha = 0.5) +
      scale_color_manual(name = "continent" ,values = c("#4DB3E6", "#00FF50", "#C00000", "#FFA500", "#37004D")) +
      scale_size(name = "Life expectancy", range = c(1, 13)) + # Adjust the range of points size
      theme_set(theme_bw() +theme(legend.position = "bottom"))
#8_E
    ggplot(DatasetNew,   aes(x = factor(X8, levels = c ("1", "2", "3", "4", "5"), labels = c("Asia", "Africa and persian gulf ", "South america", "Europe", "Center america")), y = Y)) +
       geom_point(aes(color = as.factor(X8), size = Y), alpha = 0.5) +
      scale_color_manual(name = "continent" ,values = c("#4DB3E6", "#00FF50", "#C00000", "#FFA500", "#37004D")) +
      geom_boxplot() +
      labs(title = "Life expectancy by continent"  )+
      ylab("Life expectancy (year)") + 
      xlab("Continent") 
    
    data8 <- data.frame(
      continent=c(DatasetNew$X8),
      value=c(DatasetNew$Y)
    )

## 9
#9_A1
  table_continent<-sqldf("select X8 as continent, count(*) as count
               from DatasetNew
               group by X8")
  table_continent["precent"]<- table_continent$count / sum(table_continent$count)
  print (table_continent)
#9_A2
  table_OECD<-sqldf("select X9 as OECD, count(*) as count
               from DatasetNew
               group by X9")
  table_OECD["precent"]<- table_OECD$count / sum(table_OECD$count)
  print (table_OECD)
#9_B1
  PollutionByContinent <- cbind(Freq=table(cut(DatasetNew$X8,breaks = 
                         seq(0,5,1)),cut(DatasetNew$X1,breaks=seq(0,100,10))))
  PollutionByContinent_Percent <- prop.table(PollutionByContinent)
#9_B2
  CiggaretteByContinent <- cbind(Freq=table(cut(DatasetNew$X8,breaks = 
                                                 seq(0,5,1)),cut(DatasetNew$X7,breaks=seq(0,100,10))))
  CiggaretteByContinent_Percent <- prop.table(CiggaretteByContinent)
  
  
  
  
  