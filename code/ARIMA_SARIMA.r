# -*- coding: utf-8 -*-

#라이브러리
library(data.table)
library(ggplot2)
install.packages("lmtest")
library(lmtest) #dw test
library(dplyr)
install.packages("psych")
library(psych)
install.packages("forecast")
library(forecast)
install.packages("TTR")
library(TTR)

data = read.csv(file="/content/data.csv", header=T)

#conform data
div_solar_data = data$div_solar
div_solar_data = div_solar_data[div_solar_data > 0]
data_pop_data = ts(as.numeric(div_solar_data), start=c(1), frequency = 24)

#install.packages("lubridate")
library(lubridate)

compoenets_data.ts = decompose(data_pop_data)

plot(compoenets_data.ts)

install.packages("fUnitRoots")
library(fUnitRoots)
#  deterministic component either a constant "mu" or a constant with linear trend "tau".
# lags="short" sets the number of lags to $\root 4 \of {4 \times (n/100)}$, whereas lags="long" sets the number of lags to $\root 4 \of {12 \times (n/100)}$. If lags="nil" is choosen,
urkpssTest(data_pop_data, type = c("mu"), lags = c("long"))

pacf(data_pop_data,lag.max=100)
acf(data_pop_data,lag.max=100)

#최대우도(ML)' 또는 '조건부 제곱합(CSS)'을 최소화할 수 있는 적합 방법
arima_model_data <- auto.arima(data_pop_data)

library(lmtest)

coeftest(arima_model_data)

confint(arima_model_data)

install.packages("FitAR")

arima_model_data

#fit_auto = auto.arima(data_pop2, trace=TRUE)
arima_mod_data = arima(data_pop_data, order=c(4,1,3))

#arima
ts.plot(fitted(arima_mod_data), div_solar_data, col=c("blue", "red"))

#sarima
ts.plot(fitted(arima_model_data), div_solar_data, col=c("blue", "red"))

accuracy(arima_model_data)

accuracy(arima_mod_data)

install.packages("Metrics")
library(Metrics)

#arima
smape(div_solar_data, fitted(arima_mod_data))

sarima_data = fitted(arima_model_data)

"""arima"""

data_fit = fitted(arima_mod_data)
solar_24_data = div_solar_data[(length(div_solar_data)*0.8) :length(div_solar_data)]
fit_24_data = data_fit[(length(data_fit)*0.8) :length(data_fit)]

t = 1:length(solar_24_data)
ts_data1 = ts(solar_24_data[which(solar_24_data > 0)], t)
ts_data2 = ts(fit_24_data[which(solar_24_data > 0)], t)

#arima
ts.plot(ts_data1, ts_data2, col=c("blue", "red"))

#arima
smape(fit_24_data, solar_24_data)

mMAPE <- function(actual, predicted){
  return (mean(abs(actual - predicted) / max(actual))*100)
}

mape(ts_data1, ts_data2)

mMAPE(ts_data1, ts_data2)

smape(ts_data1, ts_data2)

"""sarima"""

data_fit = fitted(arima_model_data)
solar_24_data = div_solar_data[(length(div_solar_data) *0.8) :length(div_solar_data)]
fit_24_data = data_fit[(length(data_fit) *0.8) :length(data_fit)]

t = 1:length(solar_24_data)
ts_data1 = ts(solar_24_data[which(solar_24_data > 0)], t)
ts_data2 = ts(fit_24_data[which(solar_24_data > 0)], t)

#arima
ts.plot(ts_data1, ts_data2, col=c("blue", "red"), main="ARIMA (5,0,0)")
