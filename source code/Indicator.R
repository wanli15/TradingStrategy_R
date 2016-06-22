pricesample<-cbind(close_price,pricesim)
# Aggregrate all the sample=501, 500 simulated plus original one 
#str(pricesample)
#pricesample[1:500,1:2]

#################################   MACD Raw Indicator Created  #############################################

MACDindicator<-MACD(pricesample[,1],nFast=12, nSlow=26, nSig=9) # Initialize the MACD and signals

for (k in 2:501)
{ 
  temp<-MACD(pricesample[,k],nFast=12, nSlow=26, nSig=9)
  MACDindicator<-cbind(MACDindicator,temp) # Aggregrate all the MACD and signals
}

class(MACDindicator)
str(MACDindicator)
MACDindicator[ 1:100,1:4]

 #################################   RSI Raw Indicator Created  ###########################################
RSIindicator<-RSI(pricesample[,1],n=14,maType="WMA")

for (k in 2:501)
{ 
  temp<-RSI(pricesample[,k],n=14,maType="EMA")
  RSIindicator<-cbind(RSIindicator,temp) # Aggregrate all the MACD and signals
}

str(RSIindicator)
RSIindicator[1:100,1:5]

################################  MACD Trade Price Singal Call ###########################################
remove(trade)
trade<-matrix(nrow=1259,ncol=501) # Create matrix to contain buy and sell signal prices;
flag<-0 # for round trip trading guarantee
temp_L <- temp_I<-1 # Create index of price when quoting 
#l<-i<-1
MACDindicator_mirror <- as.matrix(MACDindicator)
pricesample_mirror <- as.matrix(pricesample)
#MCDindicator_mirror[1:100,1:5]
#pricesample_mirror[1:100,1:5]

for (l in seq(1,1002,2) )
{
  for (i in 35:1257)
  {
    if(flag%%2 == 0)
    {
      if( MACDindicator_mirror[i-1,l] < MACDindicator_mirror[i-1,l+1] && MACDindicator_mirror[i,l]<MACDindicator_mirror[i,l+1] 
          && MACDindicator_mirror[i+1,l]>MACDindicator_mirror[i+1,l+1] && MACDindicator_mirror[i+2,l] > MACDindicator_mirror[i+2,l+1] )   
        # When MACD cross signal from below to above, 
        #equally the difference changes from <0 to >0, then buy
      {
        trade[i+2,(l+1)/2]<-pricesample_mirror[i+2,(l+1)/2] # quote the price for long position ( positive value for buy price)
        flag<-flag+1 # when buy finishes, switch to sell for round trip;
        temp_L <- (l+1)/2 # save the colum index when the point of price calls
        temp_I <- i+2  # save the row index when the point of price calls
        
      } 
    }   
    if(flag%%2 == 1)
    {
      if(MACDindicator_mirror[i-1,l] > MACDindicator_mirror[i-1,l+1] && MACDindicator_mirror[i,l]>MACDindicator_mirror[i,l+1] 
         && MACDindicator_mirror[i+1,l]<MACDindicator_mirror[i+1,l+1] && MACDindicator_mirror[i+2,l]< MACDindicator_mirror[i+2,l+1]) 
        # When MACD cross signal from below to above, 
      { #equally the difference changes from >0 to <0, then sell
        trade[i+2,(l+1)/2]<-pricesample_mirror[i+2,(l+1)/2]*(-1) # quote the price for short position (negative value for sell or short points)
        flag<-flag+1 # when sell finishes, switch to buy for round trip;
        
        if(pricesample_mirror[i+2,(l+1)/2]/pricesample_mirror[temp_I,temp_L] <= 0.99){  # loss limit control, when sell price/buy price<=0.9, 
          # quoted price turn back to NA to give away the buy/sell signals
          trade[temp_I,temp_L] <- NA
          trade[i+2,(l+1)/2] <- NA
        }
      } 
    }
    
  }
}

#Re permutation for the tradeprice
remove(tradeprice)
tradeprice <- matrix( nrow=1259, ncol=501)    # rearrange the signal prices in matrix to order the buy and sell signal price sequentently
i <- j <- m <- n <- 1                
for (j in 1:501 )              
{
  for (i in 1:1259)
  {  
    if( !is.na(trade[i,j]) && m <= 1259 ) # filter out the NA at first
    {
      tradeprice[m,n] <- trade[i,j] 
      m <- m+1 
    }
  }
  m <- 1
  n <- n+1
}

#tradeprice[1:100,1:10]
#################################### RSI Trade Price Signal Call #############################################
flag<-0
temp_L <- temp_I<-1 #initialize back to 1
trade2<-matrix(nrow=1259,ncol=501)
min0<-rep(40,501)
RSIindicator[1:100,1:10]


RSIindicator_mirror <- as.matrix(RSIindicator)
pricesample_mirror <- as.matrix(pricesample)

for (j in 1:501 ){
  for (i in 15:1259)
  {
    if(flag%%2 == 0)
    {
      if (!is.na(RSIindicator_mirror[i,j]) && RSIindicator_mirror[i,j]<30) 
      {
        trade2[i,j]<-pricesample_mirror[i,j]
        temp_L <- j # save the colum index when the point of price calls
        temp_I <- i  # save the row index when the point of price calls
        flag <- (flag+1) # when buy finishes, switch to sell for round trip;
      }
    }
    else if(flag%%2 == 1)
    {
      if(!is.na(RSIindicator_mirror[i,j])&& RSIindicator_mirror[i,j]>65)
      {
        trade2[i,j] <- pricesample_mirror[i,j]*(-1)  # quote the price for short position (negative value for sell or short points)
        flag <- (flag+1) # when sell finishes, switch to buy for round trip;
        
        if(  pricesample_mirror[i,j]/pricesample_mirror[temp_I,temp_L] <= 0.95 )
        {  # loss limit control, when sell price/buy price<=0.9, 
          # quoted price turn back to NA to give away the buy/sell signals
          trade2[temp_I,temp_L] <- NA
          trade2[i,j] <- NA
        }
      }
    }
  }
}

#class(pricesample_mirror[369,15])
#RSIindicator_mirror[369:370,15:16]
#trade2[1:100,15:16]


#Re permutation for the tradeprice
tradeprice2 <- matrix( nrow=1259, ncol=501)    # rearrange the signal prices in matrix to order the buy and sell signal price sequentently
i <- j <- m <- n <- 1                
for (j in 1:501 )              
{
  for (i in 1:1259)
  {  
    if( !is.na(trade2[i,j]) && m <= 1259 ) # filter out the NA at first
    {
      tradeprice2[m,n] <- trade2[i,j] 
      m <- m+1 
    }
  }
  m <- 1
  n <- n+1
}


#tradeprice2 [1:100,334:335]
#remove(tradeprice2)
