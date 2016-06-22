#####################################Strategy and Performance calculation####################################################################################################

##############################cal_re begin with mix of buy-sell/ short-buy2close strategy###################################################################

cal_re<-rep(1,501)  #initialize the calcuated total return value to 1 for iterating
cal_rate<-rep(0.0,501) # calculate the wining percent
pos_rate <- all_rate <- 1 #initialize the positive return times and all the reutrn times
log_tradeprice <- 1 
all_trade<-rep(0,501)
flag <- TRUE
m <- n <- 1

for (m in 1:501 )
{
  if(tradeprice[1,m]>0 && !is.na(tradeprice[1,m]))
    flag <- FALSE 
  for (n in seq(1,1258,2))  # for round trips
  {
    
    if(!is.na(tradeprice[n,m]) && !is.na(tradeprice[n+1,m])) 
    {
      if(flag) 
        log_tradeprice <- log(tradeprice[n,m]/tradeprice[n+1,m]*(-1)) # for single round return calculation,
      # negative price(short) as numerator, positive price(buy) as denominator
      else
        log_tradeprice <- log(tradeprice[n+1,m]/tradeprice[n,m]*(-1))
        cal_re[m]= cal_re[m] * log_tradeprice # calculate total return 
      if(log_tradeprice>0) 
      {# to calculate the positive return times
        pos_rate=pos_rate+1 
      }
      all_rate = all_rate+1 
    }# to calculate the total trading times
  }
  cal_rate[m] <- pos_rate/all_rate #calculate the wining percent
  all_trade[m] <-all_rate #calculate the trading times
  pos_rate <- 1 # reset 1 to calculate for next strategy
  all_rate <- 1 #reset 1 to calculate for next strategy
}
#calculate the wining percent
cal_re[1]
cal_rate[1]
all_trade[1]

mean(cal_re[2:501])
mean(cal_rate[2:501])
mean(all_trade[2:501])

########################################cal2_re begin with buy sell strategy###########################################################################
cal2_re<-rep(1,501)
cal2_rate<-rep(0.0,501)
pos_rate <- all_rate <- 1
log_tradeprice <- 1
all_trade<-rep(0,501)
flag <- TRUE
m <- n <- begin <- 1

for (m in 1:501 )
{
  for(n in 1:1258){
    if(!is.na(tradeprice[n,m]) && tradeprice[n,m]>0){  #Find the first signal price >0 for buy sell strategy
      begin <- n
      flag <- tradeprice[n,m]<0
      break
    }
  }
  n <- 1
  for (n in seq(begin,500,2))
  {
    if(!is.na(tradeprice[n,m]) && !is.na(tradeprice[n+1,m]))
    {
      if(flag)
        log_tradeprice <- log(tradeprice[n,m]/tradeprice[n+1,m]*(-1)) #Calculate the round return
      else
        log_tradeprice <- log(tradeprice[n+1,m]/tradeprice[n,m]*(-1))
        cal2_re[m]= cal2_re[m] * log_tradeprice # for total return calculation
      if(log_tradeprice>0)
        {
        pos_rate=pos_rate+1 
        }# to calculate the positive return times
        all_rate = all_rate+1 # to calculate the total trading times
    }
  }
  cal2_rate[m] <- pos_rate/all_rate #calculate the wining percent
  all_trade[m] <-all_rate #calculate the trading times
  pos_rate <- 1 
  all_rate <- 1
}



cal2_re[1]
cal2_rate[1]
all_trade[1]

mean(cal2_re[2:501])
mean(cal2_rate[2:501])
mean(all_trade[2:501])

#####################################cal_3 begin with short, buy2close strategy###############################################
cal3_re<-rep(1,501)
cal3_rate<-rep(0.0,501)
pos_rate <- all_rate <- 1
log_tradeprice <- 1
all_trade<-rep(0,501)
flag <- TRUE
m <- n <- begin <- 1

for (m in 1:501 )
{
  for(n in 1:1258){
    if(!is.na(tradeprice[n,m]) && tradeprice[n,m]<0){ #find the first signal price<0 for short-buy2close strategy
      begin <- n
      flag <- tradeprice[n,m]<0
      break
    }
  }
  n <- 1
  for (n in seq(begin,500,2))
  {
    if(!is.na(tradeprice[n,m]) && !is.na(tradeprice[n+1,m]))
    {
      if(flag)
        log_tradeprice <- log(tradeprice[n,m]/tradeprice[n+1,m]*(-1)) # for single round return calculation,
      else
        log_tradeprice <- log(tradeprice[n+1,m]/tradeprice[n,m]*(-1))
        cal3_re[m]= cal3_re[m] * log_tradeprice # for return calculation
      if(log_tradeprice>0)
        {
        pos_rate=pos_rate+1 
        }# to calculate the positive return times
      all_rate = all_rate+1 # to calculate the total trading times
    }
  }
  cal3_rate[m] <- pos_rate/all_rate #calculate the wining percent
  all_trade[m] <-all_rate #calculate the trading times
  pos_rate <- 1 
  all_rate <- 1
}

cal3_re[1]
cal3_rate[1]
all_trade[1]

mean(cal3_re[2:501])
mean(cal3_rate[2:501])
mean(all_trade[2:501])

#########################  cal_re4 begin with mix of buy-sell/ short-buy2close strategy #######################################

cal4_re<-rep(1,501)  #initialize the calcuated total return value to 1 for iterating
cal4_rate<-rep(0.0,501) # calculate the wining percent
pos_rate <- all_rate <- 1 #initialize the positive return times and all the reutrn times
all_trade<-rep(0,501)
log_tradeprice2 <- 1 
flag <- TRUE
m <- n <- 1
for (m in 1:501 )
{
  if(tradeprice2[1,m] > 0 && !is.na(tradeprice2[1,m]))
    flag <- FALSE 
 
   for (n in seq(1,1258,2))  # for round trips
  {
    if(!is.na(tradeprice2[n,m]) && !is.na(tradeprice2[n+1,m])) 
    { 
      if(flag) 
        log_tradeprice2 <- log(tradeprice2[n,m]/tradeprice2[n+1,m]*(-1)) # for single round return calculation,
      # negative price(short) as numerator, positive price(buy) as denominator
      else
        log_tradeprice2 <- log(tradeprice2[n+1,m]/tradeprice2[n,m]*(-1))
        cal4_re[m]= cal4_re[m] * log_tradeprice2 # calculate total return for one sample
      if(log_tradeprice2>0) # to calculate the positive return times
        {
        pos_rate=pos_rate+1
        }
        all_rate= all_rate+1 # to calculate the total trading times
    }
  }
  cal4_rate[m] <- pos_rate/all_rate #calculate the wining percent
  all_trade[m] <-all_rate #calculate the trading times
  pos_rate <- 1 # reset 1 to calculate for next strategy
  all_rate <- 1 #reset 1 to calculate for next strategy
}

cal4_re[1]
cal4_rate[1]
all_trade[1]

mean(cal4_re[2:501])
mean(cal4_rate[2:501])
mean(all_trade[2:501])

################################## cal5_re begin with buy sell strategy ############################################
cal5_re<-rep(1,501)
cal5_rate<-rep(0.0,501)
all_trade<-rep(0,501)
pos_rate <- all_rate <- 1
log_tradeprice5 <- 1
flag <- TRUE
m <- n <- begin <- 1

for (m in 1:501 )
{
  for(n in 1:1258){
    if(!is.na(tradeprice2[n,m]) && tradeprice2[n,m]>0){  #Find the first signal price >0 for buy sell strategy
      begin <- n
      flag <- tradeprice2[n,m]<0
      break
    }
  }
  n <- 1
  for (n in seq(begin,1258,2))
  {
    if(!is.na(tradeprice2[n,m]) && !is.na(tradeprice2[n+1,m]))
    {
      if(flag)
        log_tradeprice5 <- log(tradeprice2[n,m]/tradeprice2[n+1,m]*(-1)) #Calculate the round return
      else
        log_tradeprice5 <- log(tradeprice2[n+1,m]/tradeprice2[n,m]*(-1))
        cal5_re[m]= cal5_re[m] * log_tradeprice5 # calculate total return for one sample
      if(log_tradeprice5>0)
        {
        pos_rate=pos_rate+1 
        }# to calculate the positive return times
        all_rate = all_rate+1 # to calculate the total trading times
    }
  }
  cal5_rate[m] <- pos_rate/all_rate #calculate the wining percent
  all_trade[m] <-all_rate #calculate the trading times
  pos_rate <- 1 
  all_rate <- 1
}

cal5_re[1]
cal5_rate[1]
all_trade[1]

mean(cal5_re[2:501])
mean(cal5_rate[2:501])
mean(all_trade[2:501])


################################## cal6_re begin with short, buy2close strategy ############################################

cal6_re<-rep(1,501)
cal6_rate<-rep(0.0,501)
pos_rate <- all_rate <- 1
log_tradeprice6 <- 1
flag <- TRUE
m <- n <- begin <- 1

for (m in 1:501 )
{
  for(n in 1:1258){
    if(!is.na(tradeprice2[n,m]) && tradeprice2[n,m]<0){  #Find the first signal price <0 for short, buy2close strategy
      begin <- n
      flag <- tradeprice2[n,m]<0
      break
    }
  }
  n <- 1
  for (n in seq(begin,1258,2))
  {
    if(!is.na(tradeprice2[n,m]) && !is.na(tradeprice2[n+1,m]))
    {
      if(flag)
        log_tradeprice6 <- log(tradeprice2[n,m]/tradeprice2[n+1,m]*(-1)) #Calculate the round return
      else
        log_tradeprice6 <- log(tradeprice2[n+1,m]/tradeprice2[n,m]*(-1))
      cal6_re[m]= cal6_re[m] * log_tradeprice6 # for return calculation
      if(log_tradeprice6>0)
        pos_rate=pos_rate+1 # to calculate the positive return times
      all_rate = all_rate+1 # to calculate the total trading times
    }
  }
  cal6_rate[m] <- pos_rate/all_rate
  pos_rate <- 1 
  all_rate <- 1
}

cal6_re[1]
cal6_rate[1]
all_trade[1]

mean(cal6_re[2:501])
mean(cal6_rate[2:501])
mean(all_trade[2:501])


#write.csv(x=cal6_re,file="PTR Short per RSI.csv")
#write.csv(x=cal6_rate,file="PTR Short (WP)per RSI.csv")
#write.csv(x=all_trade,file="PTR Short (TT)per RSI.csv")

######Maxdown measure#########################################################################################################
maxdown<-rep(0,501)
maxprice<-rep(0,501)
minprice<-rep(0,501)

for(j in 1:501){
  
  maxprice[j]<-max(pricesample[,j])
  minprice[j]<-min(pricesample[,j])
  maxdown[j]<-log(minprice[j]/maxprice[j])
  
}
write.csv(x=maxdown,file="maxdown.csv")

maxdown[1]
mean(maxdown[2:501])
t(maxdown)
remove(maxdown)

####Output file#############################

write.csv(,file="Wan_Li project strategy and performance measure.csv")
