install.packages("tseries")
install.packages("TTR")
install.packages("TSA")
install.packages("forecast")
library(tseries)
library(TTR)
library(TSA)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)

## 3.Data Prepocessing
load("C:/Users/lenovo/Desktop/615 R/buoy/MR.RData")
buoy = data.frame(MR)
buoy_1 = buoy %>% filter(YYYY >= "1997") 
buoy_2 = buoy_1 %>% filter(hh == "12")
buoy_3 = buoy_2[,c(1,2,3,13)]
buoy_4 = buoy_3[buoy_3$ATMP<99.0,]
buoy_5 = buoy_4 %>% unite(date,YYYY,MM,DD,sep="-")

write.csv(buoy_4,file="C:/Users/lenovo/Desktop/615 R/buoy/Everyday.csv")
write.csv(buoy_5,file="C:/Users/lenovo/Desktop/615 R/buoy/Everyday_date.csv")

## 4.Data Analysis & Visualization
buoy=read.csv("Everyday.csv",header=TRUE)
Jan=0
for(i in 1997:2016){Jan[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="01"])}
Feb=0
for(i in 1997:2016){Feb[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="2"])}
Mar=0
for(i in 1997:2016){Mar[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="3"])}
Apr=0
for(i in 1997:2016){Apr[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="4"])}
May=0
for(i in 1997:2016){May[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="5"])}
Jun=0
for(i in 1997:2016){Jun[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="6"])}
Jul=0
for(i in 1997:2016){Jul[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="7"])}
Aug=0
for(i in 1997:2016){Aug[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="8"])}
Sep=0
for(i in 1997:2016){Sep[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="9"])}
Oct=0
for(i in 1997:2016){Oct[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="10"])}
Nov=0
for(i in 1997:2016){Nov[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="11"])}
Dec=0
for(i in 1997:2016){Dec[i-1996]=mean(buoy$ATMP[buoy$YYYY==i|buoy$MM=="12"])}

atmp=cbind.data.frame(Year=1997:2016,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
write.csv(atmp,file="C:/Users/lenovo/Desktop/615 R/buoy/atmp.csv")

par(mfrow=c(2,6))
ggplot(atmp,aes(x=X,y=Jan))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Jan),col="blue")
ggplot(atmp,aes(x=X,y=Feb))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Feb),col="blue")
ggplot(atmp,aes(x=X,y=Mar))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Mar),col="blue")
ggplot(atmp,aes(x=X,y=Apr))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Apr),col="blue")
ggplot(atmp,aes(x=X,y=May))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(May),col="blue")
ggplot(atmp,aes(x=X,y=Jun))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Jun),col="blue")
ggplot(atmp,aes(x=X,y=Jul))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Jul),col="blue")
ggplot(atmp,aes(x=X,y=Aug))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Aug),col="blue")
ggplot(atmp,aes(x=X,y=Sep))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Sep),col="blue")
ggplot(atmp,aes(x=X,y=Oct))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Oct),col="blue")
ggplot(atmp,aes(x=X,y=Nov))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Nov),col="blue")
ggplot(atmp,aes(x=X,y=Dec))+geom_point()+geom_smooth(method=lm,col="green")+geom_abline(slope=0,intercept=mean(Dec),col="blue")

## 5.Seasonal ARIMA
#Data Preparation
atmp=read.csv(file="atmp.csv",header=TRUE)
atmpt=t(atmp)
atmpt1=atmpt[-c(1,2),]
atmpv=as.vector(atmpt1)
atmpts=ts(atmpv,frequency=12)
tsdisplay(atmpts,xlab="Year",ylab="Air Temp")

write.csv(atmpv,file="C:/Users/lenovo/Desktop/615 R/buoy/atmpv.csv")


#White Noise Test & Stationary Test
Box.test(atmpts)
plot.ts(atmpts)
adf.test(atmpts)

#Fit the Time Series
fit1=auto.arima(atmpts)
print(fit1)

#Check the Residuals
tsdisplay(residuals(fit1))
checkresiduals(fit1)

#Predict Air Temperature of the Next Three Years
fore1=forecast(fit1,h=3*12)
plot(fore1)
lines(fore1$fitted,col="blue")
lines(atmp,col="yellow")

#Eliminate Seasonal Effects
atmpts_components=decompose(atmpts)
plot(atmpts_components)
atmpts_seasonallyadjusted=atmpts-atmpts_components$seasonal
plot(atmpts_seasonallyadjusted)

fit2=auto.arima(atmpts_seasonallyadjusted)
print(fit2)
fore2=forecast(fit2,h=3*12)
par(mfrow=c(2,1))
plot(fore1)
lines(fore1$fitted,col="blue")
lines(atmp,col="yellow")
plot(fore2)
lines(fore2$fitted,col="blue")
lines(atmp,col="yellow")

