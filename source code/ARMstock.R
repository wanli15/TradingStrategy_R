getSymbols("ARM.L",scr="yahoo",from=as.Date("2010-11-30"),to=as.Date("2015-11-30"))

close_price<-ARM.L[,"ARM.L.Adjusted",drop=FALSE]
close_price #Get a rough plot of the price
plot.ts(close_price)
acf(close_price)
pacf(close_price)

return_price<-diff(log(close_price))#return
re_price<-na.omit(return_price)
plot.ts(return_price)# remove the null one 
# 1258 sample for return 


#y<-arima(x=re_price,order=c(1,0,1))
y<-garchFit(~garch(1,1),data=re_price,trace=FALSE)
summary(y)
error<-residuals(y) 
error[1:10]



#Coefficient(s):
#  mu       omega      alpha1       beta1  
#1.0036e-03  1.5396e-05  3.6236e-02  9.2746e-01  

#Std. Errors:
#based on Hessian 

#Error Analysis:
#Estimate  Std. Error  t value Pr(>|t|)    
#mu     1.004e-03   5.560e-04    1.805  0.07110 .  
#omega  1.540e-05   7.780e-06    1.979  0.04783 *  
#alpha1 3.624e-02   1.336e-02    2.713  0.00667 ** 
#beta1  9.275e-01   2.910e-02   31.877  < 2e-16 ***

#Standardised Residuals Tests:
#Statistic p-Value     
#Jarque-Bera Test   R    Chi^2  418.9212  0           
#Shapiro-Wilk Test  R    W      0.9705736 1.391907e-1
#jung-Box Test     R    Q(10)  5.506453  0.8548866   
#Ljung-Box Test     R    Q(15)  9.138897  0.8701436   
#Ljung-Box Test     R    Q(20)  10.65898  0.9545913   
#Ljung-Box Test     R^2  Q(10)  7.263557  0.7003512   
#Ljung-Box Test     R^2  Q(15)  10.72817  0.7716151   
#Ljung-Box Test     R^2  Q(20)  14.68727  0.7940136   
#LM Arch Test       R    TR^2   7.966914  0.7877106 

#Information Criterion Statistics:
#  AIC       BIC       SIC      HQIC 
#-4.944011 -4.928064 -4.944030 -4.938027 


