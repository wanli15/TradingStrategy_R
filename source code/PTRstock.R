#########################   Model Built for Return  ARMA(2,2)  ##########################################################
getSymbols("PTR",scr="yahoo",from=as.Date("2010-11-30"),to=as.Date("2015-11-30"))

close_price<-PTR[,"PTR.Adjusted",drop=FALSE]
close_price #Get a rough plot of the price
plot.ts(close_price)
acf(close_price)
pacf(close_price)

return_price<-diff(log(close_price))#return
re_price<-na.omit(return_price)
plot.ts(return_price)# remove the null one 
# 1258 sample for return 

y<-arima(x=re_price,order=c(2,0,2))
y
confint(y) # test for the model coefficients 
error<-residuals(y) 
Box.test(error,lag=20,type='Ljung') # test for the auto correlation of the residuals

# Coefficients:
#         ar1      ar2     ma1     ma2             intercept
#      -0.3051  -0.9154  0.3110  0.9523            -4e-04
#s.e.   0.0396   0.0602  0.0294  0.0505             5e-04

# so y[t]=-0.3051*y[t-1]-0.9154*y[t-2]+0.3110*e[t-1]+0.9523*e[t-2]
# drift is tested to be zero

#sigma^2 estimated as 0.0002985:  log likelihood = 3320.34,  aic = -6628.67

#test for confidence interval
#               2.5 %       97.5 %
#ar1       -0.382718656 -0.227574894
#ar2       -1.033438597 -0.797409640
#ma1        0.253508129  0.368581016
#ma2        0.853258957  1.051382314
#intercept -0.001414686  0.000534836

#Box-Ljung test
#data:  error
#X-squared = 25.638, df = 20, p-value = 0.1781