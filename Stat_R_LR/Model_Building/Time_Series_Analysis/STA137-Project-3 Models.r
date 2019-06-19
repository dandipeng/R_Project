
library(astsa)
library(forecast)


# description of the data
# setwd("D:/R/R-HW")
rent=read.csv("Time_Series_Analysis/2beds1.csv",header=T)  #Rename the variables
rent=ts(rent$X2beds,start=c(2010,11),frequency=12)  #Read the data as time series

# data construction
str(rent)
class(rent)

# estimating trend
time = 1:length(rent) #Creat time
fit1 = lm(rent ~ time)
fit2 = lm(rent ~ poly(time,2, raw=T))
fit3 = lm(rent ~ poly(time,3, raw=T))

plot(time, rent, type="l")
lines(time, fitted(fit1), col="purple")
lines(time, fitted(fit2), col="blue")
lines(time, fitted(fit3), col="red")

# estimating seasonality manually
de_trend = rent - fitted(fit3)

period = 12
cycles = length(de_trend)/period
data_matrix = t(matrix(de_trend, ncol=cycles))
seas_data = matrix(0, cycles, period)
for (j in 1:cycles){
  rmean = mean(data_matrix[j,])
  for (i in 1:period)
    seas_data[j,i] = data_matrix[j,i]-rmean 
}
Cycles = rep(colMeans(seas_data), cycles)
Trend = ts(fit3$fitted.values,start=c(2010,11),frequency=12)
Cycles = ts(Cycles,start=c(2010,11),frequency=12)
smooth = Cycles + Trend

# Using decompose
dec = decompose(rent)
#Draw the seasonality
s_t=ts(dec$seasonal,start=c(2010,11),frequency=12)

#Plot the deseasonalized data
des=rent-s_t
plot.ts(des,main="deseasonalized data")

################trend#######################
#fit the trend
fit_m_t=lm(des~poly(time,3))
m_t=fitted.values(fit_m_t)

#Draw the reestimated trend line
plot(t,des,type='l',main="trend",ylab="deseasonized data")
lines(t, m_t, col="red")

################random######################
#Detrend the data
Y_t_2=des-m_t
plot.ts(Y_t_2,main="rough part",ylab="Y_t_2")
smooth2 = dec$seasonal + m_t

#plot the trend and seasonality part
plot(Trend, type = 'l',main='Trend of Process 1')
plot(Cycles, type='l',main='Seasonality of Process 1')

plot(m_t, type = 'l',main='Trend of Process 2')
plot(dec$seasonal, type='l',main='Seasonality of Process 2')

par(mfrow = c(1,2))
plot(time, rent, type="l")
lines(time, smooth, col="red")
plot(time, rent, type="l")
lines(time, smooth2, col="blue")

# Assess the random component
Y_t = rent-smooth
Y_t_2 = rent-smooth2
par(mfrow = c(1,1))
ts.plot(Y_t, main='Rough Part of Process 1')
ts.plot(Y_t_2, main='Rough Part of Process 2')
acf(Y_t,main='ACF of Process 1')
pacf(Y_t,main='PACF of Process 1')
acf(Y_t_2,main='ACF of Process 2')
pacf(Y_t_2,main='PACF of Process 2')

#second round seasonality fitting
data_matrix2 = t(matrix(Y_t, ncol=cycles))
seas_data2 = matrix(0, cycles, period)
for (j in 1:cycles){
  rmean2 = mean(data_matrix2[j,])
  for (i in 1:period)
    seas_data2[j,i] = data_matrix2[j,i]-rmean 
}
Cycles2 = rep(colMeans(seas_data2), cycles)
Cycles2 = ts(Cycles, start=c(2010,11),frequency=12)
ts.plot(Cycles2, main='second round seasonality fitting')
Cycles2
#Y_t_2nd
Y_t_2nd = Y_t-Cycles2
par(mfrow = c(1,1))
ts.plot(Y_t_2nd,main='second round seasonality fitting rough part')
acf(Y_t_2nd)
pacf(Y_t_2nd)

par(mfrow = c(1,2))
ts.plot(Y_t, main='Rough Part of Process 1')
ts.plot(Y_t_2nd,main='second round seasonality fitting rough part')

par(mfrow = c(1,2))
# model1
auto.arima(Y_t)
arma1 = auto.arima(Y_t)

## model2
auto.arima(Y_t_2)
arma2 = auto.arima(Y_t_2)

##model3 2-round seasonality fitting
auto.arima(Y_t_2nd)
arma3 = auto.arima(Y_t_2nd)

#ACF & PACF for three models
acf(arma1$residuals)
pacf(arma1$residuals)

acf(arma2$residuals)
pacf(arma2$residuals)

acf(arma3$residuals)
pacf(arma3$residuals)

#Fitted value against true value
library(tseries)
#Model 1
fitvalue1=fitted.values(fit3)+Cycles+fitted.values(arma1)
seqplot.ts(fitvalue1, rent,colx = "red", coly = "blue", xlab = "Time", 
           ylab = "rent price", main = "Comparision 1")   #comparision graph
legend('bottomright',legend=c('fitted value','true value'),
       pch=c('l','l'),col=c('red','blue'))
#Model 2
fitvalue2=fitted.values(fit_m_t)+s_t+fitted.values(arma2)
seqplot.ts(fitvalue2, rent,colx = "red", coly = "blue", xlab = "Time", 
           ylab = "rent price", main = "Comparision 2")   #comparision graph
legend('bottomright',legend=c('fitted value','true value'),
       pch=c('l','l'),col=c('red','blue'))
#Model 3
fitvalue3=fitted.values(fit3)+Cycles+Cycles2+fitted.values(arma3)
seqplot.ts(fitvalue3, rent,colx = "red", coly = "blue", xlab = "Time", 
           ylab = "rent price", main = "Comparision 3")   #comparision graph
legend('bottomright',legend=c('fitted value','true value'),
       pch=c('l','l'),col=c('red','blue'))

####################################
##Prediction
#Predict the residual
x_t_hat1=forecast(arma1,h=8,level = c(0.95))  #predict the rent price of 2 beds in the following 8 months
x_t_hat2=forecast(arma2,h=8,level = c(0.95))
x_t_hat3=forecast(arma3,h=8,level = c(0.95))
plot(x_t_hat1)  #Draw the plot
plot(x_t_hat2)
plot(x_t_hat3)

#Predict the future
future_time=c(85:92) #predict the next 8 monthly rental price
future = data.frame(future_time=c(85:92))
#Model 1
pred_trend1=predict(fit3, data.frame(time=future_time))   #trend prediction
pred_season1=as.numeric(Cycles[1:8])   #seasonarity prediction
pred_smooth1=pred_trend1+pred_season1   #smooth part prediction
pred_random1 = data.frame(t = x_t_hat1$mean, t = x_t_hat1$lower, t = x_t_hat1$upper)   #resid prediction
pred1 = sapply(1:3, function(k) pred_random1[,k]+pred_smooth1)   #final prediction data

    
#Model 2
pred_trend2=predict(fit_m_t, data.frame(time=future_time))   #trend prediction
pred_season2=as.numeric(s_t[1:8])   #seasonarity prediction
pred_smooth2=pred_trend2+pred_season2   #smooth part prediction
pred_random2 = data.frame(t = x_t_hat2$mean, t = x_t_hat2$lower, t = x_t_hat2$upper)   #resid prediction
pred2 = sapply(1:3, function(k) pred_random2[,k]+pred_smooth2)   #final prediction data
    
#Model 3
pred_trend3=predict(fit3, data.frame(time=future_time))   #trend prediction
pred_season3=as.numeric(Cycles[1:8]+Cycles2[1:8])   #seasonarity prediction
pred_smooth3=pred_trend3+pred_season3   #smooth part prediction
pred_random3 = data.frame(t = x_t_hat3$mean, t = x_t_hat3$lower, t = x_t_hat3$upper)   #resid prediction
pred3 = sapply(1:3, function(k) pred_random3[,k]+pred_smooth3)   #final prediction data
par(mfrow=c(1,3))
#plot the graph of Process 1
pred_mean1 = ts(pred1[,1],start=c(2017,11),frequency=12)
pred_lwr1 = ts(pred1[,2],start=c(2017,11),frequency=12)
pred_upr1 = ts(pred1[,3],start=c(2017,11),frequency=12)
fnl1=cbind(rent,pred_lwr1,pred_mean1,pred_upr1)
ts.plot(fnl1,gpars= list(col=c('black','blue','red','orange')),main='Prediction in Process 1')
legend('bottomright',legend=c('current','pred_lwr',"pred_mean","pred_upr"),
       pch=c('l','l','l','l'),col=c('black','blue','red','orange'))
#plot the graph of Process 2
pred_mean2 = ts(pred2[,1],start=c(2017,11),frequency=12)
pred_lwr2 = ts(pred2[,2],start=c(2017,11),frequency=12)
pred_upr2 = ts(pred2[,3],start=c(2017,11),frequency=12)
fnl2=cbind(rent,pred_lwr2,pred_mean2,pred_upr2)
ts.plot(fnl2,gpars= list(col=c('black','blue','red','orange')),main='Prediction in Process 2')
legend('bottomright',legend=c('current','pred_lwr',"pred_mean","pred_upr"),
       pch=c('l','l','l','l'),col=c('black','blue','red','orange'))
#plot the graph of Process 3
pred_mean3 = ts(pred3[,1],start=c(2017,11),frequency=12)
pred_lwr3 = ts(pred3[,2],start=c(2017,11),frequency=12)
pred_upr3 = ts(pred3[,3],start=c(2017,11),frequency=12)
fnl3=cbind(rent,pred_lwr3,pred_mean3,pred_upr3)
ts.plot(fnl3,gpars= list(col=c('black','blue','red','orange')),main='Prediction in Process 3')
legend('bottomright',legend=c('current','pred_lwr',"pred_mean","pred_upr"),
       pch=c('l','l','l','l'),col=c('black','blue','red','orange'))
