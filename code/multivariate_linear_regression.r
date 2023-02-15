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

data = read.csv(file="/content/kor.csv", header=T)

div_solar = data$div_solar
div_solar_summer = data_summer$div_solar
div_solar_fall = data_fall$div_solar
div_solar_winter = data_winter$div_solar

last = div_solar[(length(div_solar)-239):length(div_solar)]
last_summer = div_solar_summer[(length(div_solar_summer)-239):length(div_solar_summer)]
last_fall = div_solar_fall[(length(div_solar_fall)-239):length(div_solar_fall)]
last_winter = div_solar_winter[(length(div_solar_winter)-239):length(div_solar_winter)]

last_num = length(last[last>0])
last_summer_num = length(last_summer[last_summer>0])
last_fall_num = length(last_fall[last_fall>0])
last_winter_num = length(last_winter[last_winter>0])

last_num
last_summer_num
last_fall_num
last_winter_num

# spring
# 반응변수 Div_solar
# 예측변수 = temp, rain, sunshine, radiation
temp = data$temp
rain = data$rain
sunshine = data$sunshine
radiation = data$insolation
cloud_amount = data$cloud_amount	
cloud_level = data$cloud_level	
cloud_ceiling = data$cloud_ceiling	
snow = data$snow	
dust_pm25 = as.numeric(as.character(data$dust_pm25))
dust_pm10 = data$dust_pm10	
div_solar = data$div_solar

temp = temp[div_solar > 0]
rain = rain[div_solar > 0]
sunshine = sunshine[div_solar > 0]
radiation = radiation[div_solar > 0]
cloud_amount = cloud_amount[div_solar > 0]
cloud_level = cloud_level[div_solar > 0]
cloud_ceiling = cloud_ceiling[div_solar > 0]
snow = snow[div_solar > 0]
dust_pm25 = dust_pm25[div_solar > 0]
dust_pm10 = dust_pm10[div_solar > 0]
div_solar = div_solar[div_solar > 0]

div_solar = scale(div_solar, center = min(div_solar), scale = max(div_solar)-min(div_solar))
temp = scale(temp, center = min(temp), scale = max(temp)-min(temp))
rain = scale(rain, center = min(rain), scale = max(rain)-min(rain))
sunshine = scale(sunshine, center = min(sunshine), scale = max(sunshine)-min(sunshine))
radiation = scale(radiation, center = min(radiation), scale = max(radiation)-min(radiation))
cloud_amount = scale(cloud_amount, center = min(cloud_amount), scale = max(cloud_amount)-min(cloud_amount))
cloud_level = scale(cloud_level, center = min(cloud_level), scale = max(cloud_level)-min(cloud_level))
cloud_ceiling = scale(cloud_ceiling, center = min(cloud_ceiling), scale = max(cloud_ceiling)-min(cloud_ceiling))
snow = scale(snow, center = min(snow), scale = max(snow)-min(snow))
dust_pm25 = scale(dust_pm25, center = min(dust_pm25), scale = max(dust_pm25)-min(dust_pm25))
dust_pm10 = scale(dust_pm10, center = min(dust_pm10), scale = max(dust_pm10)-min(dust_pm10))

model = lm(div_solar ~ temp+rain+sunshine+radiation+cloud_amount + cloud_level + cloud_ceiling + snow + dust_pm25 + dust_pm10 )
summary(model)

# summer
# 반응변수 Div_solar
# 예측변수 = temp, rain, sunshine, radiation
temp_summer = data_summer$temp
rain_summer = data_summer$rain
sunshine_summer = data_summer$sunshine
radiation_summer = data_summer$insolation
cloud_amount_summer = data_summer$cloud_amount	
cloud_level_summer = data_summer$cloud_level	
cloud_ceiling_summer = data_summer$cloud_ceiling	
snow_summer = data_summer$snow	
dust_pm25_summer = as.numeric(as.character(data_summer$dust_pm25))
dust_pm10_summer = data_summer$dust_pm10	
div_solar_summer = data_summer$div_solar

temp_summer = temp_summer[div_solar_summer > 0]
rain_summer = rain_summer[div_solar_summer > 0]
sunshine_summer = sunshine_summer[div_solar_summer > 0]
radiation_summer = radiation_summer[div_solar_summer > 0]
cloud_amount_summer = cloud_amount_summer[div_solar_summer > 0]
cloud_level_summer = cloud_level_summer[div_solar_summer > 0]
cloud_ceiling_summer = cloud_ceiling_summer[div_solar_summer > 0]
snow_summer = snow_summer[div_solar_summer > 0]
dust_pm25_summer = dust_pm25_summer[div_solar_summer > 0]
dust_pm10_summer = dust_pm10_summer[div_solar_summer > 0]
div_solar_summer = div_solar_summer[div_solar_summer > 0]

div_solar_summer = scale(div_solar_summer, center = min(div_solar_summer), scale = max(div_solar_summer)-min(div_solar_summer))
temp_summer = scale(temp_summer, center = min(temp_summer), scale = max(temp_summer)-min(temp_summer))
rain_summer = scale(rain_summer, center = min(rain_summer), scale = max(rain_summer)-min(rain_summer))
sunshine_summer = scale(sunshine_summer, center = min(sunshine_summer), scale = max(sunshine_summer)-min(sunshine_summer))
radiation_summer = scale(radiation_summer, center = min(radiation_summer), scale = max(radiation_summer)-min(radiation_summer))
cloud_amount_summer = scale(cloud_amount_summer, center = min(cloud_amount_summer), scale = max(cloud_amount_summer)-min(cloud_amount_summer))
cloud_level_summer = scale(cloud_level_summer, center = min(cloud_level_summer), scale = max(cloud_level_summer)-min(cloud_level_summer))
cloud_ceiling_summer = scale(cloud_ceiling_summer, center = min(cloud_ceiling_summer), scale = max(cloud_ceiling_summer)-min(cloud_ceiling_summer))
snow_summer = scale(snow_summer, center = min(snow_summer), scale = max(snow_summer)-min(snow_summer))
dust_pm25_summer = scale(dust_pm25_summer, center = min(dust_pm25_summer), scale = max(dust_pm25_summer)-min(dust_pm25_summer))
dust_pm10_summer = scale(dust_pm10_summer, center = min(dust_pm10_summer), scale = max(dust_pm10_summer)-min(dust_pm10_summer))

model_summer = lm(div_solar_summer ~ temp_summer+rain_summer+sunshine_summer+radiation_summer+cloud_amount_summer + cloud_level_summer + cloud_ceiling_summer + dust_pm25_summer + dust_pm10_summer )
summary(model_summer)

# fall
# 반응변수 Div_solar
# 예측변수 = temp, rain, sunshine, radiation
temp_fall = data_fall$temp
rain_fall = data_fall$rain
sunshine_fall = data_fall$sunshine
radiation_fall = data_fall$insolation
cloud_amount_fall = data_fall$cloud_amount	
cloud_level_fall = data_fall$cloud_level	
cloud_ceiling_fall = data_fall$cloud_ceiling	
snow_fall = data_fall$snow	
dust_pm25_fall = as.numeric(as.character(data_fall$dust_pm25))
dust_pm10_fall = data_fall$dust_pm10	
div_solar_fall = data_fall$div_solar

temp_fall = temp_fall[div_solar_fall > 0]
rain_fall = rain_fall[div_solar_fall > 0]
sunshine_fall = sunshine_fall[div_solar_fall > 0]
radiation_fall = radiation_fall[div_solar_fall > 0]
cloud_amount_fall = cloud_amount_fall[div_solar_fall > 0]
cloud_level_fall = cloud_level_fall[div_solar_fall > 0]
cloud_ceiling_fall = cloud_ceiling_fall[div_solar_fall > 0]
snow_fall = snow_fall[div_solar_fall > 0]
dust_pm25_fall = dust_pm25_fall[div_solar_fall > 0]
dust_pm10_fall = dust_pm10_fall[div_solar_fall > 0]
div_solar_fall = div_solar_fall[div_solar_fall > 0]

div_solar_fall = scale(div_solar_fall, center = min(div_solar_fall), scale = max(div_solar_fall)-min(div_solar_fall))
temp_fall = scale(temp_fall, center = min(temp_fall), scale = max(temp_fall)-min(temp_fall))
rain_fall = scale(rain_fall, center = min(rain_fall), scale = max(rain_fall)-min(rain_fall))
sunshine_fall = scale(sunshine_fall, center = min(sunshine_fall), scale = max(sunshine_fall)-min(sunshine_fall))
radiation_fall = scale(radiation_fall, center = min(radiation_fall), scale = max(radiation_fall)-min(radiation_fall))
cloud_amount_fall = scale(cloud_amount_fall, center = min(cloud_amount_fall), scale = max(cloud_amount_fall)-min(cloud_amount_fall))
cloud_level_fall = scale(cloud_level_fall, center = min(cloud_level_fall), scale = max(cloud_level_fall)-min(cloud_level_fall))
cloud_ceiling_fall = scale(cloud_ceiling_fall, center = min(cloud_ceiling_fall), scale = max(cloud_ceiling_fall)-min(cloud_ceiling_fall))
snow_fall = scale(snow_fall, center = min(snow_fall), scale = max(snow_fall)-min(snow_fall))
dust_pm25_fall = scale(dust_pm25_fall, center = min(dust_pm25_fall), scale = max(dust_pm25_fall)-min(dust_pm25_fall))
dust_pm10_fall = scale(dust_pm10_fall, center = min(dust_pm10_fall), scale = max(dust_pm10_fall)-min(dust_pm10_fall))

model_fall = lm(div_solar_fall ~ temp_fall+rain_fall+sunshine_fall+radiation_fall+cloud_amount_fall + cloud_level_fall + cloud_ceiling_fall + dust_pm25_fall + dust_pm10_fall )
summary(model_fall)

# winter
# 반응변수 Div_solar
# 예측변수 = temp, rain, sunshine, radiation
div_solar_winter = data_winter$div_solar
temp_winter = data_winter$temp
rain_winter = data_winter$rain
sunshine_winter = data_winter$sunshine
radiation_winter = data_winter$insolation
cloud_amount_winter = data_winter$cloud_amount	
cloud_level_winter = data_winter$cloud_level	
cloud_ceiling_winter = data_winter$cloud_ceiling	
snow_winter = data_winter$snow	
dust_pm25_winter = as.numeric(as.character(data_winter$dust_pm25))
dust_pm10_winter = data_winter$dust_pm10	


temp_winter = temp_winter[div_solar_winter > 0]
rain_winter = rain_winter[div_solar_winter > 0]
sunshine_winter = sunshine_winter[div_solar_winter > 0]
radiation_winter = radiation_winter[div_solar_winter > 0]
cloud_amount_winter = cloud_amount_winter[div_solar_winter > 0]
cloud_level_winter = cloud_level_winter[div_solar_winter > 0]
cloud_ceiling_winter = cloud_ceiling_winter[div_solar_winter > 0]
snow_winter = snow_winter[div_solar_winter > 0]
dust_pm25_winter = dust_pm25_winter[div_solar_winter > 0]
dust_pm10_winter = dust_pm10_winter[div_solar_winter > 0]
div_solar_winter = div_solar_winter[div_solar_winter > 0]

div_solar_winter = scale(div_solar_winter, center = min(div_solar_winter), scale = max(div_solar_winter)-min(div_solar_winter))
temp_winter = scale(temp_winter, center = min(temp_winter), scale = max(temp_winter)-min(temp_winter))
rain_winter = scale(rain_winter, center = min(rain_winter), scale = max(rain_winter)-min(rain_winter))
sunshine_winter = scale(sunshine_winter, center = min(sunshine_winter), scale = max(sunshine_winter)-min(sunshine_winter))
radiation_winter = scale(radiation_winter, center = min(radiation_winter), scale = max(radiation_winter)-min(radiation_winter))
cloud_amount_winter = scale(cloud_amount_winter, center = min(cloud_amount_winter), scale = max(cloud_amount_winter)-min(cloud_amount_winter))
cloud_level_winter = scale(cloud_level_winter, center = min(cloud_level_winter), scale = max(cloud_level_winter)-min(cloud_level_winter))
cloud_ceiling_winter = scale(cloud_ceiling_winter, center = min(cloud_ceiling_winter), scale = max(cloud_ceiling_winter)-min(cloud_ceiling_winter))
snow_winter = scale(snow_winter, center = min(snow_winter), scale = max(snow_winter)-min(snow_winter))
dust_pm25_winter = scale(dust_pm25_winter, center = min(dust_pm25_winter), scale = max(dust_pm25_winter)-min(dust_pm25_winter))
dust_pm10_winter = scale(dust_pm10_winter, center = min(dust_pm10_winter), scale = max(dust_pm10_winter)-min(dust_pm10_winter))

model_winter = lm(div_solar_winter ~ temp_winter+rain_winter+sunshine_winter+radiation_winter+cloud_amount_winter + cloud_level_winter + cloud_ceiling_winter + snow_winter + dust_pm25_winter + dust_pm10_winter )
summary(model_winter)

# rain <- 유의하지 않는 변수
# Multiple R-squared: 0.5264,-> 최종 회귀모형이 예측변수들의 52.6% 설명함
model2 = lm(div_solar ~ temp+radiation + cloud_amount +cloud_level + snow+ dust_pm10)
summary(model2)

# rain <- 유의하지 않는 변수
# Multiple R-squared:  0.5482,-> 최종 회귀모형이 예측변수들의 54.8% 설명함
model_summer2 = lm(div_solar_summer ~ temp_summer+sunshine_summer+radiation_summer+cloud_amount_summer+cloud_level_summer+cloud_ceiling_summer+dust_pm10_summer+dust_pm25_summer)
summary(model_summer2)

# rain <- 유의하지 않는 변수
# Multiple R-squared: 0.5268,-> 최종 회귀모형이 예측변수들의 52.6% 설명함
model_fall2 = lm(div_solar_fall ~ temp_fall+sunshine_fall+radiation_fall+cloud_amount_fall+cloud_ceiling_fall)
summary(model_fall2)

# Multiple R-squared: 0.3788,-> 최종 회귀모형이 예측변수들의 37.8% 설명함
model_winter2 = lm(div_solar_winter ~ temp_winter+sunshine_winter+radiation_winter+cloud_amount_winter+cloud_level_winter+cloud_ceiling_winter+dust_pm10_winter+snow_winter)
summary(model_winter2)

relweights <- function(fit,...){
    R <- cor(fit$model)
    nvar <- ncol(R)
    rxx <- R[2:nvar, 2:nvar]
    rxy <- R[2:nvar, 1]
    svd <- eigen(rxx)
    evec <- svd$vectors
    ev <- svd$values
    delta <- diag(sqrt(ev))
    lambda <- evec %*% delta %*% t(evec)
    lambdasq <- lambda ^ 2
    beta <- solve(lambda) %*% rxy
    rsquare <- colSums(beta ^ 2)
    rawwgt <- lambdasq %*% beta ^ 2
    import <- (rawwgt / rsquare) * 100
    import <- as.data.frame(import)
    row.names(import) <- names(fit$model[2:nvar])
    names(import) <- "Weights"
    import <- import[order(import),1, drop=FALSE]
    dotchart(import$Weights, labels=row.names(import),
                 xlab="% of R-Square", pch=19,
                 main="Relative Importance of Predictor Variables",
                 sub=paste("Total R-Square=", round(rsquare, digits=3)),
                 ...)
    return(import)
}

result = relweights(model2, col="blue")

result = relweights(model_summer2, col="blue")

result = relweights(model_fall2, col="blue")

result = relweights(model_winter2, col="blue")

library(tidyverse)

summary(model2)$coefficient

summary(model_summer2)$coefficient

summary(model_fall2)$coefficient

summary(model_winter2)$coefficient

confint(model2)

confint(model_summer2)

confint(model_fall2)

confint(model_winter2)

sigma(model2)/mean(div_solar)

sigma(model_summer2)/mean(div_solar_summer)

sigma(model_fall2)/mean(div_solar_fall)

sigma(model_winter2)/mean(div_solar_winter)

t = 1:length(model2$fitted.values)
ts1 = ts(div_solar , t)
ts2 =ts(model2$fitted.values ,t)

ts.plot(ts1, ts2, col = c("blue", "red") )

t = 1:length(model_summer2$fitted.values)
ts_summer1 = ts(div_solar_summer , t)
ts_summer2 =ts(model_summer2$fitted.values ,t)

ts.plot(ts_summer1, ts_summer2, col = c("blue", "red") )

t = 1:length(model_fall2$fitted.values)
ts_fall1 = ts(div_solar_fall , t)
ts_fall2 =ts(model_fall2$fitted.values ,t)

ts.plot(ts_fall1, ts_fall2, col = c("blue", "red") )

t = 1:length(model_winter2$fitted.values)
ts_winter1 = ts(div_solar_winter , t)
ts_winter2 =ts(model_winter2$fitted.values ,t)

ts.plot(ts_winter1, ts_winter2, col = c("blue", "red") )

accuracy(model2)

accuracy(model_summer2)

accuracy(model_fall2)

accuracy(model_winter2)

install.packages("Metrics")
library(Metrics)

#smape(actual, predicted)
smape(ts1, ts2)

#smape(actual, predicted)
smape(ts_summer1, ts_summer2)

#smape(actual, predicted)
smape(ts_fall1, ts_fall2)

#smape(actual, predicted)
smape(ts_winter1, ts_winter2)

spring_fit = model2$fitted
solar_24 = div_solar[(length(div_solar)-last_num) :length(div_solar)]
fit_24 = spring_fit[(length(div_solar)-last_num) :length(spring_fit)]

t = 1:length(solar_24)
ts1 = ts(solar_24, t)
ts2 = ts(fit_24, t)

fit_24

ts.plot(ts1,ts2, col = c("blue","red"))

summer_fit = model_summer2$fitted
solar_24_summer = div_solar_summer[(length(div_solar_summer)-last_summer_num) :length(div_solar_summer)]
fit_24_summer = summer_fit[(length(div_solar_summer)-last_summer_num) :length(summer_fit)]

t = 1:length(solar_24_summer)
ts_summer1 = ts(solar_24_summer, t)
ts_summer2 = ts(fit_24_summer, t)

fit_24_summer

ts.plot(ts_summer1,ts_summer2, col = c("blue","red"))

fall_fit = model_fall2$fitted
solar_24_fall = div_solar_fall[(length(div_solar_fall)-last_fall_num) :length(div_solar_fall)]
fit_24_fall = fall_fit[(length(div_solar_fall)-last_fall_num) :length(fall_fit)]

t = 1:length(solar_24_fall)
ts_fall1 = ts(solar_24_fall, t)
ts_fall2 = ts(fit_24_fall, t)

ts.plot(ts_fall1,ts_fall2, col = c("blue","red"))
fit_24_fall

winter_fit = model_winter2$fitted
solar_24_winter = div_solar_winter[(length(div_solar_winter)-last_winter_num) :length(div_solar_winter)]
fit_24_winter = winter_fit[(length(div_solar_winter)-last_winter_num) :length(winter_fit)]

t = 1:length(solar_24_winter)
ts_winter1 = ts(solar_24_winter, t)
ts_winter2 = ts(fit_24_winter, t)

fit_24_winter

ts.plot(ts_winter1,ts_winter2, col = c("blue","red"))



mMAPE <- function(actual, predicted){
  return (mean(abs(actual - predicted) / max(actual))*100)
}

# ts1[ts1 == 0] <- 1
# ts_summer1[ts_summer1 == 0] <- 1
# ts_fall1[ts_fall1 == 0] <- 1
# ts_winter1[ts_winter1 == 0] <- 1


mape(ts1, ts2)
mape(ts_summer1, ts_summer2)
mape(ts_fall1, ts_fall2)
mape(ts_winter1, ts_winter2)

mMAPE(ts1, ts2)
mMAPE(ts_summer1, ts_summer2)
mMAPE(ts_fall1, ts_fall2)
mMAPE(ts_winter1, ts_winter2)

smape(ts1, ts2)
smape(ts_summer1, ts_summer2)
smape(ts_fall1, ts_fall2)
smape(ts_winter1, ts_winter2)



act = ts1[ts1 > 0] 
pred  = ts2[ts1 > 0] 

act_summer = ts_summer1[ts_summer1 > 0] 
pred_summer  = ts_summer2[ts_summer1> 0] 

act_fall = ts_fall1[ts_fall1 > 0] 
pred_fall  = ts_fall2[ts_fall1 > 0] 

act_winter = ts_winter1[ts_winter1 > 0] 
pred_winter = ts_winter2[ts_winter1 > 0]

mMAPE <- function(actual, predicted){
  return (mean(abs(actual - predicted) / max(actual))*100)
}

mape(act, pred)
mape(act_summer, pred_summer)
mape(act_fall, pred_fall)
mape(act_winter, pred_winter)

mMAPE(act, pred)
mMAPE(act_summer, pred_summer)
mMAPE(act_fall, pred_fall)
mMAPE(act_winter, pred_winter)

smape(act, pred)
smape(act_summer, pred_summer)
smape(act_fall, pred_fall)
smape(act_winter, pred_winter)

t = 1:length(act)
ts.plot(ts(act, t), ts(pred, t), col = c("blue","red"))
t = 1:length(act_summer)
ts.plot(ts(act_summer, t), ts(pred_summer, t), col = c("blue","red"))
t = 1:length(act_fall)
ts.plot(ts(act_fall, t), ts(pred_fall, t), col = c("blue","red"))
t = 1:length(act_winter)
ts.plot(ts(act_winter, t), ts(pred_winter, t), col = c("blue","red"))



