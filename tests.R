rm(list = ls()) 

library(forecast)
library(ggplot2)

data= read.csv('TFP.csv')
#ano = (1950:2011)
t=c('isocode', 'year','rtfpna')
year = matrix(2012:2021, 10,1)
rtfpna = matrix(rep(0,10),10,1)
temp = as.matrix(cbind(year,rtfpna))
colnames(temp) = t



dataUSA =  data[1:62,1:3]
dataCAN = data[63:124,1:3]
dataMEX = data[125:186,1:3]



#p <- ggplot(data, aes(x = data$year, y = data$rtfpna , color = as.factor(data$isocode)) ) +
#  geom_point() +
#  geom_smooth(method = "loess")
#p

isocode = matrix(rep('USA'),10,1)
e=as.data.frame(forecast(dataUSA$rtfpna, h=10))[1]
e=cbind(isocode,year,e)
colnames(e) = t
dataUSA2 = rbind(as.matrix(dataUSA),as.matrix(e))

isocode = matrix(rep('CAN'),10,1)
c=as.data.frame(forecast(dataCAN$rtfpna, h=10))[1]
c=cbind(isocode,year,c)
colnames(c) = t
dataCAN2 = rbind(as.matrix(dataCAN),as.matrix(c))


isocode = matrix(rep('MEX'),10,1)
m=as.data.frame(forecast(dataMEX$rtfpna, h=10))[1]
m=cbind(isocode,year,m)
colnames(m) = t
dataMEX2 = rbind(as.matrix(dataMEX),as.matrix(m))

data2=as.data.frame(rbind(dataUSA2,dataCAN2,dataMEX2))###problemaAqui
rownames(data2) <- NULL
rownames(data2$isocode) <- NULL
rownames(data2$year) <- NULL
rownames(data2$rtfpna) <- NULL
colnames(data2) <- t



p <- ggplot(data2, aes(x = data2$year, y = data2$rtfpna , color = as.factor(data2$isocode)) ) +
  geom_point() +
  scale_y_discrete(breaks = c(0.6,0.8,1.0,1.2,1.4)) +
  scale_x_discrete (breaks =c(1940,1960,1980,2000,2020))
p



