##
##  CVXR Project 1
##

## Clear environment
rm(list = ls())

## Load libraries
library(CVXR)

numSecurities=3585 
numInvestments = 249
numSectors = 11
numCapTiers = 3
numMarkets = 2
numCountries = 27
setwd("~/UW/CFRM-507/2022/project1")
benchmarkDataFileName <- "BenchmarkData.csv"
portfolioDataFileName <- "PortfolioHoldings.csv"
benchmarkData <- read.csv(file.path(benchmarkDataFileName))


# Change names to eliminate periods
colnames(benchmarkData)[1] <- c("ID") 
colnames(benchmarkData)[7] <- c("MarketCap") 
colnames(benchmarkData)[8] <- c("CapTier")
colnames(benchmarkData)[9] <- c("CurrentPrice")
portfolioData <- read.csv(file.path(portfolioDataFileName))

# Change names to eliminate periods
colnames(portfolioData)[1] <- c("ID") 
colnames(portfolioData)[2] <- c("NumberOfShares") 
colnames(portfolioData)[3] <- c("PurchasePrice") 

# change MarketCap and current price to appropriate units
benchmarkData$MarketCap <- as.numeric(gsub(",", "", benchmarkData$MarketCap))
benchmarkData$CurrentPrice <- as.numeric(benchmarkData$CurrentPrice)
sectorNames <- unique(benchmarkData$Sector)
capTierNames <- unique(benchmarkData$CapTier)
regionNames <- unique(benchmarkData$Region)
countryNames <- unique(benchmarkData$Country)
marketNames <- c('Dev', 'Emerging')

#parameters
#sector i_j
benchmarkData$IDNum <- as.numeric(substr(benchmarkData$ID,2,nchar(benchmarkData$ID)))
portfolioData$IDNum <- as.numeric(substr(portfolioData$ID,2,nchar(portfolioData$ID)))
NumPortfolioHoldings = dim(portfolioData)[1]
sectordf = matrix(0, nrow = numSecurities, ncol = numSectors)
colnames(sectordf) = sectorNames
for(i in 1:numSecurities){
  s = benchmarkData$Sector[benchmarkData$IDNum == i]
  sectordf[i,which(sectorNames == s)] = 1
}
#captier I_j
capTierdf = matrix(0, nrow = numSecurities, ncol = numCapTiers)
colnames(capTierdf) = capTierNames
for(i in 1:numSecurities){
  s = benchmarkData$CapTier[benchmarkData$IDNum == i]
  capTierdf[i,which(capTierNames == s)] = 1
}
#market i_j
marketdf = matrix(0, nrow = numSecurities, ncol = numMarkets)
colnames(marketdf) = marketNames
for(i in 1:numSecurities){
  s <- ifelse(benchmarkData$Region[i] %in% 
                                        c("U.S.","Western Europe","Japan","Canada"), 
                                      TRUE, FALSE)
 if(s == TRUE){
   marketdf[i,1] = 1
 }
  else{
    marketdf[i,2] = 1
  }
}

#country I_j
countrydf = matrix(0, nrow = numSecurities, ncol = numCountries)
colnames(countrydf) = countryNames
for(i in 1:numSecurities){
  s = benchmarkData$Country[benchmarkData$IDNum == i]
  countrydf[i,which(countryNames == s)] = 1
}

#stocks i_k
stocksdf = matrix(0, nrow = numSecurities, ncol = numInvestments)
for(i in 1:numSecurities){
  s = which(portfolioData$IDNum == i)
  if(length(s) > 0){
    stocksdf[i, s] = 1
  }
}

#values _k
value = c()
for(i in 1:nrow(portfolioData)){
  price = benchmarkData[which(benchmarkData$IDNum == 
                                portfolioData[i, 'IDNum']), 'CurrentPrice']
  num_shares = portfolioData[i, 'NumberOfShares']
  value = c(value, price*num_shares)
}

#values_i
value_i = c()
for(i in 1:numSecurities){
  s = sum(portfolioData$NumberOfShares[which(portfolioData$IDNum == i)])
  price = benchmarkData$CurrentPrice[i]
  value_i = c(value_i, s*price)
}

#sector weights
SectorCap <- data.frame(SectorName = sectorNames)
SectorCap$MarketCap <- unlist(lapply(sectorNames, FUN = function(sname){
  sum(benchmarkData$MarketCap[benchmarkData$Sector ==  sname])}))
SectorCap$Percent <- SectorCap$MarketCap/sum(SectorCap$MarketCap)

#cap tier weights
CapTierCap <- data.frame(TierName = capTierNames)
CapTierCap$MarketCap <- unlist(lapply(capTierNames, FUN = function(sname){
  sum(benchmarkData$MarketCap[benchmarkData$CapTier ==  sname])}))
CapTierCap$Percent <- CapTierCap$MarketCap/sum(CapTierCap$MarketCap)

#market weights
MarketCap <- data.frame(TierName = marketNames)
MarketCap$MarketCap <- c(sum(benchmarkData$MarketCap[
  benchmarkData$Region %in% c("U.S.","Western Europe","Japan","Canada")]),
  sum(benchmarkData$MarketCap[
    !benchmarkData$Region %in% c("U.S.","Western Europe","Japan","Canada")]))
MarketCap$Percent <- MarketCap$MarketCap/sum(MarketCap$MarketCap)

#country weights
CountryCap <- data.frame(CountryName = countryNames)
CountryCap$MarketCap <- unlist(lapply(countryNames, FUN = function(sname){
  sum(benchmarkData$MarketCap[benchmarkData$Country ==  sname])}))
CountryCap$Percent <- CountryCap$MarketCap/sum(CountryCap$MarketCap)

#cost
cost = portfolioData$PurchasePrice

#price
price = benchmarkData$CurrentPrice
price_k = c()
for(i in 1:nrow(portfolioData)){
  s = benchmarkData$CurrentPrice[benchmarkData$IDNum == portfolioData[i, 'IDNum']]
  price_k = c(price_k, s)
}


#tcost
totalcap = sum(benchmarkData$MarketCap)
tcost = matrix(0, nrow = numSecurities, ncol = 2) #numSecurities and IdNum, tcost
for(i in 1:nrow(benchmarkData)){
  cap = CountryCap$Percent[CountryCap$CountryName == 
                               benchmarkData[i, 'Country']]
  if(cap < 0.02){
    tcost[i, ] = c(benchmarkData[i, 'IDNum'], 0.002)
    #tcost = c(tcost, 0.002)
  }
  else if (cap > 0.02){
    #tcost = c(tcost, 0.008)
    tcost[i, ] = c(benchmarkData[i, 'IDNum'], 0.0008)
  }
}
tcostdf = as.data.frame(tcost) 
tcost_k = c()
for(i in 1:nrow(portfolioData)){
  tc = tcostdf$V2[portfolioData[i, 'IDNum'] == tcostdf$V1]
  tcost_k = c(tcost_k, tc)
}

#variables
X = Variable(numSecurities) #dollar value of security i after trades
P = Variable(numSecurities) # dollar amount of security i purchased
S = Variable(numInvestments) # dollar amount of investment k sold
W = Variable(1)# dollar value of portfolio after trades

NetRealizedGain = Variable(1)
NetRealizedLosses = Variable(1)

#Constraints
constraints <- list()

#non negativity
constraints = append(constraints, value - S >= 0)
constraints = append(constraints, X >= 0)
constraints = append(constraints, P >= 0)
constraints = append(constraints, S >= 0)
constraints = append(constraints, W >= 0)
constraints = append(constraints, NetRealizedGain >= 0)
constraints = append(constraints, NetRealizedLosses >= 0)

amount_initially_held = (stocksdf%*%value)
amount_sold = (stocksdf%*%S)
constraints = append(constraints, X == (amount_initially_held + P - amount_sold))

#W constraint
constraints = append(constraints, W == sum(X))

#Budget constraint
outflow = 15000000
purchases = sum((1+tcost[,2])*P)
sold = sum((1-tcost_k) * S)
constraints = append(constraints, (outflow) == sold - purchases)

# Limit on Capital losses
constraints = append(constraints, NetRealizedLosses <= 0.1*W)

#tax rate
#LHS = sum(((1-tcost_k)*price_k-cost)*S/((1-tcost_k)*price_k))
LHS = sum((((1-tcost_k)*price_k-cost)/((1-tcost_k)*price_k))*S)
RHS = NetRealizedGain - NetRealizedLosses
constraints = append(constraints, LHS == RHS)

#sector 
for(i in 1:numSectors){
  left = 0.95*SectorCap$Percent[i] * W
  mid = sum(sectordf[,i]*X)
  right = 1.05*SectorCap$Percent[i] * W
  constraints = append(constraints, left <= mid)
  constraints = append(constraints, mid <= right)
}

#market
for(i in 1:numMarkets){
  left = 0.92*MarketCap$Percent[i] * W
  mid = sum(marketdf[,i]*X)
  right = 1.08*MarketCap$Percent[i] * W
  constraints = append(constraints, left <= mid)
  constraints = append(constraints, mid <= right)
}

#cap
for(i in 1:numCapTiers){
  left = 0.9*CapTierCap$Percent[i] * W
  mid = sum(capTierdf[,i]*X)
  right = 1.1*CapTierCap$Percent[i] * W
  constraints = append(constraints, left <= mid)
  constraints = append(constraints, mid <= right)
}

#objective
o1 = sum(tcost[,2] * P)
o2 = sum(tcost_k * S)
o3 = 0.15 * NetRealizedGain
objective = o1+o2+o3
Model <- Problem(Minimize(objective), constraints)
solution = solve(Model, verbose=TRUE, solver="GLPK")
solution$status
solution$value
solution$getValue(NetRealizedGain)
solution$getValue(NetRealizedLosses)
solution$getValue(W)
solution$getDualValue(Model@constraints[[10]])

#calculate portfolio stats
#sector caps
p_sectorCap = t(solution$getValue(X)) %*% sectordf
p_sectorCap = p_sectorCap/sum(p_sectorCap)
p_sectorCap

#cap tiers
p_capTierCap = t(solution$getValue(X)) %*% capTierdf
p_capTierCap = p_capTierCap/sum(p_capTierCap)
p_capTierCap

#Market caps
p_marketCap = t(solution$getValue(X)) %*% marketdf
p_marketCap = p_marketCap/sum(p_marketCap)
p_marketCap

#calculate benchmark stats
#sector cap
b_sectorCap = solution$getValue(W) * SectorCap$Percent
names(b_sectorCap) = SectorCap$SectorName
b_sectorCap

#cap tier
b_capTierCap = solution$getValue(W) * CapTierCap$Percent
names(b_capTierCap) = CapTierCap$TierName
b_capTierCap

#market caps
b_marketCap = solution$getValue(W) * MarketCap$Percent
names(b_marketCap) = MarketCap$TierName
b_marketCap

#money to taxes and transaction costs
tx_costs = sum(tcost[,2] * solution$getValue(P)) + sum(tcost_k * solution$getValue(S))
tx_costs
taxes = 0.15*solution$getValue(NetRealizedGain)
taxes

#portfolio stats before rebalancing
sum(value) #portfolio value 
sum(value) - solution$getValue(W)
#sectorcap
p_sectorCap_o = value_i %*% sectordf
p_sectorCap_o

#cap tier
p_capTierCap_o = value_i %*% capTierdf
p_capTierCap_o

#market cap
p_marketCap_o = value_i %*% marketdf
p_marketCap_o

#embedded loss
#eloss = sum(value) - sum(portfolioData$NumberOfShares*portfolioData$PurchasePrice)
#eloss
