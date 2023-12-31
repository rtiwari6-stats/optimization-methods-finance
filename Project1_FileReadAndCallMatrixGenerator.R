################################################
################################################
####
####
####   CFRM 507, Project 1 Solver
####
####   Written by Steve Murray, (C)2020
####
####
################################################
################################################



######################################
##
##  some preliminaries
##
######################################

##  clear current objects in memory
rm(list = ls())
debugSwitch = FALSE

## set working directory
workingDirectory <- "C:/Users/arize/Documents/CFRM/CFRM507/2020/Project 1"
benchmarkDataFileName <- "BenchmarkData.csv"
portfolioDataFileName <- "PortfolioHoldings.csv"

## Reference to matrix generator function
source(file.path(workingDirectory,"Project1_MatrixGenerator.R"))

## Set cash inflow into portfolio
Inflow = 10000000   ## value for full size problem
#Inflow = 10000      ## value for small data set

######################################
##
## read in and organize benchmark data
##
######################################

benchmarkData <- read.csv(file.path(workingDirectory,benchmarkDataFileName))

# Change names to eliminate periods
colnames(benchmarkData)[7] <- c("MarketCap") 
colnames(benchmarkData)[8] <- c("CapTier")

# change MarketCap and current price to appropriate units
benchmarkData$MarketCap <- benchmarkData$MarketCap
benchmarkData$Price <- benchmarkData$Price

# sort benchmark data by stock ID
benchmarkData$IDNum <- as.numeric(substr(benchmarkData$ID,2,nchar(benchmarkData$ID)))
order.port <- order(benchmarkData$IDNum)
benchmarkData <- benchmarkData[order.port,]

# organize benchmark data characteristics
sectorNames <- unique(benchmarkData$Sector)
capTierNames <- unique(benchmarkData$CapTier)
regionNames <- unique(benchmarkData$Region)
benchmarkData$IsDevMarket <- ifelse(benchmarkData$Region %in% c("U.S.","Western Europe","Japan","Canada"), TRUE, FALSE)

SectorCap <- data.frame(SectorName = sectorNames)
SectorCap$MarketCap <- unlist(lapply(sectorNames, FUN = function(sname){
  sum(benchmarkData$MarketCap[benchmarkData$Sector ==  sname])}))
SectorCap$Percent <- SectorCap$MarketCap/sum(SectorCap$MarketCap)

CapTierCap <- data.frame(TierName = capTierNames)
CapTierCap$MarketCap <- unlist(lapply(capTierNames, FUN = function(sname){
  sum(benchmarkData$MarketCap[benchmarkData$CapTier ==  sname])}))
CapTierCap$Percent <- CapTierCap$MarketCap/sum(CapTierCap$MarketCap)

DevCapPercent <- sum(benchmarkData$MarketCap[benchmarkData$IsDevMarket])/
                    sum(benchmarkData$MarketCap)
EMCapPercent <- 1.0 - DevCapPercent
USCapPercent <- sum(benchmarkData$MarketCap[benchmarkData$Country == "U.S."])/
  sum(benchmarkData$MarketCap)

NumSectors = length(sectorNames)
NumDevStocks = sum(benchmarkData$IsDevMarket)
NumEMStocks = sum(!benchmarkData$IsDevMarket)
NumUSStocks = sum(with(benchmarkData, Country == "U.S."))
NumStocks = dim(benchmarkData)[1]

######################################
##
## read in and organize portfolio holdings data
##
######################################

portfolioData <- read.csv(file.path(workingDirectory,portfolioDataFileName))
NumPortfolioHoldings = dim(portfolioData)[1]

# Change names to eliminate periods
colnames(portfolioData)[1] <- c("ID") 
colnames(portfolioData)[2] <- c("NumberOfShares") 
colnames(portfolioData)[3] <- c("PurchasePrice") 

# scale tax lot purchase prices 
#portfolioData$PurchasePrice <- portfolioData$PurchasePrice/scalar
portfolioData$PurchasePrice <- portfolioData$PurchasePrice

# sort portfolio data by stock ID
portfolioData$IDNum <- as.numeric(substr(portfolioData$ID,2,nchar(portfolioData$ID)))
order.port <- order(portfolioData$IDNum)
portfolioData <- portfolioData[order.port,]

# assign lot numbers
LotNum <- array(0,dim = NumPortfolioHoldings)
LotNum[1] = 1
# there's got to be a more efficient way than this for loop, but I'm to tired to figure it out right now
for(nlot in 2:NumPortfolioHoldings){   
  LotNum[nlot] = ifelse(portfolioData$IDNum[nlot] == portfolioData$IDNum[nlot-1],LotNum[nlot-1] + 1, 1)
}

InitialAmount <- unlist(lapply(1:NumStocks, FUN = function(nstock){
  sum(portfolioData$NumberOfShares[portfolioData$ID == benchmarkData$ID[nstock]])
}))
InitialValue <- InitialAmount * benchmarkData$Price

######################################
##
## matrix generator
##      the constraints are assumed to have the form
##              LHS <= A * x <= RHS, where x are the decision variables
##
##      constraintMatrix components
##              NumRows - number of rows in constraint matrix
##              NumCols - number of columns in constraint matrix
##              NumNonZeros - number of non-zero coefficients in constraint matrix
##              Objective - objective function coefficients
##              Upper - variable upper bounds
##              Lower - variable lower bounds
##              rowIndexes - row indexes of non-zero entries in constraint matrix
##              colIndexes - column indexes of non-zero entries in constraint matrix
##              NonZeroValues - values of non-zero entries in constraint matrix
##              LHS - left hand side coefficients of constraints
##              RHS - right hand side coefficients of constraints
##
##      setting debugSwitch to TRUE will cause a dense representation of the constraint 
##        matrix to be printed to a file named DebugMatrix.csv which may be useful for debugging
##        The format of this file is
##                    O b j e c t i v e
##               0    U p p e r            0
##                    L o w e r
##              LHS   Constraint Matrix   RHS
##
######################################

constraintMatrix <- MatrixGenerator(debugSwitch)


######################################
##
## setup the linear program
##
######################################
