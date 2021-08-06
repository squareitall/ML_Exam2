rm(list=ls())
library(quantmod)
library(mosaic)
library(foreach)


df_sym=stockSymbols()
table(df_sym$ETF)

#Selected the ones where ETF is True
df_sym=subset(df_sym,df_sym$ETF==TRUE)

summary(df_sym)

#
table(df_sym$Financial.Status)
df_sym=subset(df_sym,df_sym$Financial.Status=="Normal (Default)")

lt_syms=df_sym[,1]


lt_var=c()
skip=c()
obs=c()
for (i in 1:length(lt_syms)){
  cat(i,'\t')
  sym=lt_syms[i]
  
  tryit = try(getSymbols(sym,from= "2016-01-01",env=NULL))
  if(inherits(tryit, "try-error")){
    cat('error',i)
    skip=c(skip,i)
    i = i+1
  } else {
  
  temp_df=as.data.frame(getSymbols(sym,from= "2016-01-01",env=NULL))
  if (dim(temp_df)[1]>0){
  # adj_df=adjustOHLC(temp_df)#Ask sir about this
  
  obs=c(obs,dim(temp_df)[1])
  ret=ClCl(temp_df)
  
  temp_ret_var=var(ret[-1])
  
  lt_var=c(lt_var,temp_ret_var)
  }
else{
  skip=c(skip,i)
}
  }}
  


df_filt=data.frame(sym=lt_syms[-skip],var=lt_var,num_obs=obs)

df_final=subset(df_filt,df_filt$num_obs==max(df_filt$num_obs))

dff=df_final[order(df_final$var,decreasing=TRUE),]



syms=c(tail(dff,2)$sym,head(dff,3)$sym)

mystocks = c("WMT", "TGT", "XOM", "MRK", "JNJ")
mystocks=syms
myprices = getSymbols(mystocks, from = "2016-01-01")#,env=NULL)


# A chunk of code for adjusting all stocks
# creates a new object adding 'a' to the end
# For example, WMT becomes WMTa, etc

# Combine all the returns in a matrix
all_returns = cbind(ClCl(BIB),
                     ClCl(WOOD),
                     ClCl(XT),
                     ClCl(BIS),
                     ClCl(FBZ)
                     )
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

boxplot(all_returns)
# Compute the returns from the closing prices
pairs(all_returns)

# Sample a random return from the empirical joint distribution
# This simulates a random day
return.today = resample(all_returns, 1, orig.ids=FALSE)

# Update the value of your holdings
# Assumes an equal allocation to each asset
total_wealth = 10000
my_weights = c(0.2,0.2,0.2, 0.2, 0.2)
holdings = total_wealth*my_weights
holdings = holdings*(1 + return.today)

# Compute your new total wealth
holdings
total_wealth = sum(holdings)
total_wealth

# Now loop over two trading weeks
# let's run the following block of code 5 or 6 times
# to eyeball the variability in performance trajectories

## begin block
total_wealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * total_wealth
n_days = 10  # capital T in the notes
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)  # sampling from R matrix in notes
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')
## end block

# Now simulate many different possible futures
# just repeating the above block thousands of times
initial_wealth = 10000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = weights * total_wealth
  n_days = 10
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

# each row is a simulated trajectory
# each column is a data
head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)





