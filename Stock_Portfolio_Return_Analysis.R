rm(list=ls(all=TRUE))
#-------------------------------------------------------------------------------
library(tseries)
library(vars)
library(tis)
library(quantmod)
library(dplyr)
library(leaps)
library(car)
#-------------------------------------------------------------------------------
#Casino ETF
bjk<-get.hist.quote('BJK', start ="2016-01-01", end="2018-08-01")
bjk_returns<-timeSeries::getReturns(bjk$Close) 

#Alcohol ETF (Contains Smirnoff holdings!!)
deo<-get.hist.quote('DEO', start ="2016-01-01", end="2018-08-01")
deo_returns<-timeSeries::getReturns(deo$Close)

#Marijuana ETF
mj<-get.hist.quote('MJ', start ="2016-01-01", end="2018-08-01")
mj_returns<-timeSeries::getReturns(mj$Close)

#Assemble data frame: 
portfolio<-na.omit(data.frame(bjk_returns, deo_returns, mj_returns))
names(portfolio)<-c("bjk", "deo", "mj")
row.names(portfolio)<-NULL
#Remove outlier: 
portfolio<-portfolio[which(abs(portfolio[,3])<3),]
#-------------------------------------------------------------------------------
#Helper function: 
geom_return<-function(x){
	if(is.data.frame(x)){
		returns<-c()
		for(i in 1:ncol(x)){
			returns<-append(returns, prod(1+x[,i])-1)
		}
		return(returns)
	}
	return(prod(1+x)-1)
}
#Sample: 
set.seed(1)

#Daily Returns: 
portfolio_returns_daily<-c()
for(i in 1:10000){
	sample_portfolio<-portfolio[sample(1:nrow(portfolio), 50, replace=TRUE),]
	portfolio_returns_daily<-rbind(portfolio_returns_daily, mean(rowMeans(sample_portfolio)))
}

#Annualized Returns
portfolio_returns<-c()
for(i in 1:10000){
	sample_portfolio<-portfolio[sample(1:nrow(portfolio), 252, replace=TRUE),]
	portfolio_returns<-rbind(portfolio_returns, mean(geom_return(sample_portfolio)))
}
#-------------------------------------------------------------------------------
#Summary statistics:
#Daily Returns 
range(portfolio_returns_daily)
mean(portfolio_returns_daily)
sd(portfolio_returns_daily)
truehist(portfolio_returns_daily, 30, col='slategray', xlab="Returns", 
		 ylab="Normalized Frequency", main="Sampled Portfolio Returns")
lines(density(portfolio_returns_daily), col='deepskyblue', lwd=3)

#Annualized Returns:
range(portfolio_returns)
mean(portfolio_returns)
sd(portfolio_returns)
truehist(portfolio_returns, 30, col='slategray', xlab="Returns", 
		 ylab="Normalized Frequency", main="Sampled Portfolio Returns")
lines(density(portfolio_returns), col='deepskyblue', lwd=3)
#-------------------------------------------------------------------------------
#Distribution
jarque.bera.test(portfolio_returns) #Fails normality test...
library(fitdistrplus)
descdist(as.numeric(portfolio_returns), boot=100) #seems to be closer to lognormal