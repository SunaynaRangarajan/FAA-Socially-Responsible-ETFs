#References 

#Keller, W. J. (2012). Generalized Momentum and Flexible Asset Allocation (FAA):
#An Heuristic Approach. SSRN Electronic Journal. doi:10.2139/ssrn.2193735

#Kipnis, I. (2014, October 28). QuantStrat TradeR. Retrieved from An Attempt At Replicating Flexible Asset Allocation (FAA):
#https://quantstrattrader.wordpress.com/2014/10/20/an-attempt-at-replicating-flexible-asset-allocation-faa/

#Kipnis, I. (2014, November 25). QuantStrat TradeR. Retrieved from An Update on Flexible Asset Allocation: 
#https://quantstrattrader.wordpress.com/2014/11/25/an-update-on-flexible-asset-allocation/

packages <- c("PerformanceAnalytics", "quantmod","xts","scales")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(scales)

adPrices <- read.csv("adPrices.csv")
adPrices$Index <- as.Date(adPrices$Index)
adPrices <- xts(adPrices[,-1], order.by=adPrices[,1])

esg_data  <- read.csv("ESG_Score.csv")
esg_data  <- esg_data[,c(1,2)]
esg <- esg_data[,2]
names(esg) <- esg_data[,1]


FAA <- function(prices, monthsLookback = 1,
                weightMom = 1, weightVol = .5, weightCor = .5, weightesg = 0.5,
                riskFreeName = "VGSH", bestN = 3) 
  {
  
  returns <- Return.calculate(prices)
  monthlyEps <- endpoints(prices, on = "months")
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  tmp <- list()
  dates <- list()
  
  for(i in 2:(length(monthlyEps) - monthsLookback)) {
  
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    momentum <- momentum[,!is.na(momentum)]
    priceData <- priceData[,names(momentum)]
    returnsData <- returnsData[,names(momentum)]
    
    momRank <- rank(momentum)
    vols <- data.frame(StdDev(returnsData))
    volRank <- rank(-vols)
    cors <- cor(returnsData, use="complete.obs")
    corRank <- rank(-rowSums(cors))
    esg <- esg
    esgRank <- rank(esg)
    
    totalRank <- rank(weightMom*momRank + weightVol*volRank + weightCor*corRank + weightesg*esgRank)
    topNvals <- as.vector(totalRank[order(totalRank, decreasing = TRUE)][1:bestN]) 
    
    #compute weights
    longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
    longs[momentum < -0.1] <- 0 #Momentum < -10% is removed at the end.
    longs <- longs/sum(longs) #equal weight all candidates
    longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
    names(longs) <- names(totalRank) 
    
    
    #append removed names (those with momentum < -10%)
    removedZeroes <- rep(0, ncol(returns)) 
    names(removedZeroes) <- names(returns)[!names(returns) %in% names(longs)]
    longs <- c(longs, removedZeroes)
    
    #reorder to be in the same column order as original returns/prices
    longs <- data.frame(t(longs))
    longs <- longs[, names(returns)]
    
    #append lists
    tmp[[i]] <- longs
    dates[[i]] <- index(returnsData)[nrow(returnsData)]
  }
  
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = TRUE)
  colnames(strategyReturns) <- paste(monthsLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
}

FAA <- function(prices, monthsLookback = 1,
                weightMom = 1, weightVol = .5, weightCor = .5, weightesg = 0.5,
                riskFreeName = "VGSH", bestN = 3) 
{
  
  returns <- Return.calculate(prices)
  monthlyEps <- endpoints(prices, on = "months")
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  tmp <- list()
  dates <- list()
  
  for(i in 2:(length(monthlyEps) - monthsLookback)) {
    #subset data
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    
    #perform computations
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    momentum <- momentum[,!is.na(momentum)]
    #momentum[is.na(momentum)] <- -1 #set any NA momentum to negative 1 to keep R from crashing
    priceData <- priceData[,names(momentum)]
    returnsData <- returnsData[,names(momentum)]
    
    momRank <- rank(momentum)
    vols <- data.frame(StdDev(returnsData))
    volRank <- rank(-vols)
    cors <- cor(returnsData, use="complete.obs")
    corRank <- rank(-rowSums(cors))
    esg <- esg
    esgRank <- rank(esg)
    
    totalRank <- rank(weightMom*momRank + weightVol*volRank + weightCor*corRank + weightesg*esgRank)
    topNvals <- as.vector(totalRank[order(totalRank, decreasing = TRUE)][1:bestN]) 
    
    #compute weights
    longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
    longs[momentum < -0.1] <- 0 #Momentum < -10% is removed at the end.
    longs <- longs/sum(longs) #equal weight all candidates
    longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
    names(longs) <- names(totalRank) 
    
    
    #append removed names (those with momentum < -10%)
    removedZeroes <- rep(0, ncol(returns)) #Check and edit this
    names(removedZeroes) <- names(returns)[!names(returns) %in% names(longs)]
    longs <- c(longs, removedZeroes)
    
    #reorder to be in the same column order as original returns/prices
    longs <- data.frame(t(longs))
    longs <- longs[, names(returns)]
    
    #append lists
    tmp[[i]] <- longs
    dates[[i]] <- index(returnsData)[nrow(returnsData)]
  }
  
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = TRUE)
  colnames(strategyReturns) <- paste(monthsLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
}


FAA <- function(prices, monthsLookback = 1,
                weightMom = 1, weightVol = .5, weightCor = .5, weightesg = 0.5,
                riskFreeName = "VGSH", bestN = 3) 
{
  
  returns <- Return.calculate(prices)
  monthlyEps <- endpoints(prices, on = "months")
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  tmp <- list()
  dates <- list()
  
  for(i in 2:(length(monthlyEps) - monthsLookback)) {
    #subset data
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    
    #perform computations
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    momentum <- momentum[,!is.na(momentum)]
    #momentum[is.na(momentum)] <- -1 #set any NA momentum to negative 1 to keep R from crashing
    priceData <- priceData[,names(momentum)]
    returnsData <- returnsData[,names(momentum)]
    
    momRank <- rank(momentum)
    vols <- data.frame(StdDev(returnsData))
    volRank <- rank(-vols)
    cors <- cor(returnsData, use="complete.obs")
    corRank <- rank(-rowSums(cors))
    esg <- esg
    esgRank <- rank(esg)
    
    totalRank <- rank(weightMom*momRank + weightVol*volRank + weightCor*corRank + weightesg*esgRank)
    topNvals <- as.vector(totalRank[order(totalRank, decreasing = TRUE)][1:bestN]) 
    
    #compute weights
    longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
    longs[momentum < -0.1] <- 0 #Momentum < -10% is removed at the end.
    longs <- longs/sum(longs) #equal weight all candidates
    longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
    names(longs) <- names(totalRank) 
    
    
    #append removed names (those with momentum < -10%)
    removedZeroes <- rep(0, ncol(returns)) #Check and edit this
    names(removedZeroes) <- names(returns)[!names(returns) %in% names(longs)]
    longs <- c(longs, removedZeroes)
    
    #reorder to be in the same column order as original returns/prices
    longs <- data.frame(t(longs))
    longs <- longs[, names(returns)]
    
    #append lists
    tmp[[i]] <- longs
    dates[[i]] <- index(returnsData)[nrow(returnsData)]
  }
  
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = TRUE)
  colnames(strategyReturns) <- paste(monthsLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
}

Cash_inv <- function(prices, monthsLookback = 1,
                weightMom = 1, weightVol = .5, weightCor = .5, weightesg = 0.5,
                riskFreeName = "VGSH", bestN = 3) 
{
  
  returns <- Return.calculate(prices)
  monthlyEps <- endpoints(prices, on = "months")
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  tmp <- list()
  dates <- list()
  
  for(i in 2:(length(monthlyEps) - monthsLookback)) {
    #subset data
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthsLookback],]
    
    #perform computations
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    momentum <- momentum[,!is.na(momentum)]
    #momentum[is.na(momentum)] <- -1 #set any NA momentum to negative 1 to keep R from crashing
    priceData <- priceData[,names(momentum)]
    returnsData <- returnsData[,names(momentum)]
    
    momRank <- rank(momentum)
    vols <- data.frame(StdDev(returnsData))
    volRank <- rank(-vols)
    cors <- cor(returnsData, use="complete.obs")
    corRank <- rank(-rowSums(cors))
    esg <- esg
    esgRank <- rank(esg)
    
    totalRank <- rank(weightMom*momRank + weightVol*volRank + weightCor*corRank + weightesg*esgRank)
    topNvals <- as.vector(totalRank[order(totalRank, decreasing = TRUE)][1:bestN]) 
    
    #compute weights
    longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
    longs[momentum < -0.1] <- 0 #Momentum < -10% is removed at the end.
    longs <- longs/sum(longs) #equal weight all candidates
    longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
    names(longs) <- names(totalRank) 
    
    
    #append removed names (those with momentum < -10%)
    removedZeroes <- rep(0, ncol(returns)) #Check and edit this
    names(removedZeroes) <- names(returns)[!names(returns) %in% names(longs)]
    longs <- c(longs, removedZeroes)
    
    #reorder to be in the same column order as original returns/prices
    longs <- data.frame(t(longs))
    longs <- longs[, names(returns)]
    
    #append lists
    tmp[[i]] <- longs
    dates[[i]] <- index(returnsData)[nrow(returnsData)]
  }
  
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = TRUE)
  colnames(strategyReturns) <- paste(monthsLookback, weightMom, weightVol, weightCor, sep="_")
  return(percent(sum(weights$VGSH>0)/nrow(weights)))
}

benchmark <- function (stock_symbol) {
  benchmark_Data <- new.env()
  benchmark = getSymbols(toString(stock_symbol), src="yahoo", env=benchmark_Data, from="2017-02-15", to="2019-03-29")
  benchmark = na.omit(na.locf(do.call(merge, eapply(benchmark_Data, Ad))))
  benchmark_return = Return.calculate(benchmark)
 return(benchmark_return)                                                                  
}
