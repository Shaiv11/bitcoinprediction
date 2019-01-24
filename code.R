library(xts)
library(anytime)
library(forecast)
library(dygraphs)

setwd("C:/Users/Shaival/Desktop")
train_data<-read.csv("bitcoin_price_Training - Training.csv")

head(train_data)

summary(train_data)

#Volume section contain NAs
train_data$Volume<-NULL
head(train_data)

#converting data into date wise format
tr<-xts(train_data[,-1], order.by = as.POSIXct(train_data$Date))
head(tr)

data_13<-tr[2:248,]
data_14<-tr[249:614,]
data_15<-tr[615:980,]
data_16<-tr[981:1346,]
data_17<-tr[1347:1556,]

plot(data_13[,'Close'], main = "Closing Price Trend in 2013", xlab="Closing Price", ylab="Date", xlim=c(60,3000))
plot(data_14[,'Close'], main = "Closing Price Trend in 2014", xlab="Closing Price", ylab="Date", xlim=c(60,3000))
plot(data_15[,'Close'], main = "Closing Price Trend in 2015", xlab="Closing Price", ylab="Date", xlim=c(60,3000))
plot(data_16[,'Close'], main = "Closing Price Trend in 2016", xlab="Closing Price", ylab="Date", xlim=c(60,3000))
plot(data_17[,'Close'], main = "Closing Price Trend in 2017", xlab="Closing Price", ylab="Date", xlim=c(60,3000))


#Dynamic Candlestick Chart to show the closing price trends for 2013-17
dygraph(tr) %>%
  dyCandlestick()

#Regression 

sample<- data.frame(close=train_data$Close, open=log(train_data$Open+1), high=log(train_data$High), low=log(train_data$Low+1), market=log(train_data$Market.Cap+1))
f<-step(lm(close~open+high+low+market, data = sample))

plot(fitted(f), sample$close, ylab = "Closing Price", xlab = "Predicted Price", color='r')

