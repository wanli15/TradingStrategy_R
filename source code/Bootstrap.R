N<-length(re_price)
pricesim<-matrix(nrow=(N+1),ncol=500)  #Create simulated price in matrix named pricesim
re_pricesim<-matrix(nrow=N,ncol=500)  #Create simulated return in matrix named re_pricesim
maerror<-matrix(nrow=N,ncol=500)   #Create ma error term in matrix named maerror
se<-matrix(nrow=N,ncol=500) 
er<-matrix(nrow=N,ncol=500)#Create standard error term in matrix named se
bootstrap_error<-matrix(nrow=N,ncol=500) #Create boostraping error in matrix named boostrap_error 


###################################### Model For ARMA(2, 2) ##########################################################################################
set.seed(123456)
ar1<-0.3051;ar2<--0.9154;ma1<-0.3110;ma2<-0.9523; sigma<-sqrt(0.0002985)
pricesim[1,]<-close_price[1]  
pricesim[2,]<-close_price[2]
pricesim[3,]<-close_price[3]
re_pricesim[1,]<-re_price[1]
re_pricesim[2,]<-re_price[2]


for(j in 1:500)
{
  maerror[,j]<-rnorm(N,sd=sigma)  #create N error term for ma with mean 0 and sigma 
  bootstrap_error[,j]<-sample(error,N,replace=TRUE) # boostrap the residuals from empirical model
  
  for ( i in 3:N)
  {
    re_pricesim[i,j]<-ar1*re_pricesim[i-1,j]+ar2*re_pricesim[i-2,j]+ma1*maerror[i-1,j]+ma2*maerror[i-2,j]+bootstrap_error[i,j] #ARMA(2,2)model for return
    pricesim[i+1,j]<-pricesim[i,j]*exp(re_pricesim[i,j])  # simluate price
  }
  
}

pricesim[1:3,255:260]
#################################

######################################### Model For GARCH(1, 1) ###################################################################################
set.seed(123456)
omega<-1.5396e-05; alpha1<-3.6236e-02; beta1<-9.2746e-01; 
sigmasquare<-matrix(nrow=N,ncol=500)
sigmasquare[1,]<-((re_price[1]-error[1])/rnorm(1))^2
er[1,]<-(re_price[1]-error[1])
pricesim[1,]<-close_price[1]
pricesim[2,]<-close_price[2]
re_pricesim[1,]<-re_price[1]



for(j in 1:500)
{
  
  se[,j]<-rnorm(N)#create standard normal error (se)with mean 0 and 1
  bootstrap_error[,j]<-sample(error,N,replace=TRUE) # boostrap the residuals from empirical model
  
  for ( i in 2:N)
  {
    
    sigmasquare[i,j]<-omega+alpha1*er[1,j]+beta1*sigmasquare[i-1,j]  
    #calculate sigma square at time t based on sigma square at t-1 and error term square(sigma*se at t-1)
    er[i,j]<-sqrt(sigmasquare[i,j])* se[i,j] # calculate error term at time t based on sigma*se
    re_pricesim[i,j]<-er[i,j]+bootstrap_error[i,j] #GARCH(1,1) model, calculate return by error term add boostrap error terms 
    pricesim[i+1,j]<-pricesim[i,j]*exp(re_pricesim[i,j])  # simluate price
    
  }
  
}

#pricesim[1:800 ,1:5]
#re_pricesim[1:100,1:5]
#sigmasquare[1:100,1:5]