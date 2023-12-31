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
setwd("~/UW/CFRM-507/2022/project1")
benchmarkDataFileName <- "BenchmarkData.csv"
portfolioDataFileName <- "PortfolioHoldings.csv"

## Reference to matrix generator function
source(file.path("Project1_MatrixGenerator.R"))

## Set cash inflow into portfolio
outflow = 15000000   ## value for full size problem
#Inflow = 10000      ## value for small data set

######################################
##
## read in and organize benchmark data
##
######################################

benchmarkData <- read.csv(file.path(benchmarkDataFileName))

# Change names to eliminate periods
colnames(benchmarkData)[1] <- c("ID") 
colnames(benchmarkData)[7] <- c("MarketCap") 
colnames(benchmarkData)[8] <- c("CapTier")
colnames(benchmarkData)[9] <- c("CurrentPrice")

# change MarketCap and current price to appropriate units
benchmarkData$MarketCap <- as.numeric(gsub(",", "", benchmarkData$MarketCap))
benchmarkData$CurrentPrice <- as.numeric(benchmarkData$CurrentPrice)

# sort benchmark data by stock ID
#benchmarkData$IDNum <- as.numeric(substr(benchmarkData$ID,2,nchar(benchmarkData$ID)))
#order.port <- order(benchmarkData$IDNum)
#benchmarkData <- benchmarkData[order.port,]

# organize benchmark data characteristics
sectorNames <- unique(benchmarkData$Sector)
capTierNames <- unique(benchmarkData$CapTier)
regionNames <- unique(benchmarkData$Region)
countryNames <- unique(benchmarkData$Country)
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

CountryCap <- data.frame(CountryName = countryNames)
CountryCap$MarketCap <- unlist(lapply(countryNames, FUN = function(sname){
  sum(benchmarkData$MarketCap[benchmarkData$Country ==  sname])}))
CountryCap$Percent <- CountryCap$MarketCap/sum(CountryCap$MarketCap)

#USCapPercent <- sum(benchmarkData$MarketCap[benchmarkData$Country == "U.S."])/
#  sum(benchmarkData$MarketCap)

NumSectors = length(sectorNames)
NumDevStocks = sum(benchmarkData$IsDevMarket)
NumEMStocks = sum(!benchmarkData$IsDevMarket)
#NumUSStocks = sum(with(benchmarkData, Country == "U.S."))
NumStocks = dim(benchmarkData)[1]

######################################
##
## read in and organize portfolio holdings data
##
######################################

portfolioData <- read.csv(file.path(portfolioDataFileName))
NumPortfolioHoldings = dim(portfolioData)[1]

# Change names to eliminate periods
colnames(portfolioData)[1] <- c("ID") 
colnames(portfolioData)[2] <- c("NumberOfShares") 
colnames(portfolioData)[3] <- c("PurchasePrice") 

# scale tax lot purchase prices 
#portfolioData$PurchasePrice <- portfolioData$PurchasePrice/scalar
portfolioData$PurchasePrice <- portfolioData$PurchasePrice

# sort portfolio data by stock ID
#portfolioData$IDNum <- as.numeric(substr(portfolioData$ID,2,nchar(portfolioData$ID)))
#order.port <- order(portfolioData$IDNum)
#portfolioData <- portfolioData[order.port,]

# assign lot numbers
#LotNum <- array(0,dim = NumPortfolioHoldings)
#LotNum[1] = 1
# there's got to be a more efficient way than this for loop, but I'm to tired to figure it out right now
#for(nlot in 2:NumPortfolioHoldings){   
#  LotNum[nlot] = ifelse(portfolioData$IDNum[nlot] == portfolioData$IDNum[nlot-1],LotNum[nlot-1] + 1, 1)
#}

InitialAmount <- unlist(lapply(1:NumStocks, FUN = function(nstock){
  sum(portfolioData$NumberOfShares[portfolioData$ID == benchmarkData$ID[nstock]])
}))
#value_k : dollar value of investment
InitialValue <- InitialAmount * benchmarkData$CurrentPrice

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
MatrixGenerator <- function(debugSwitch)
{
  
  EffectivelyInfinity = Inf
  nrows <- NumStocks + 2 * NumSectors + 16
  ncols <- NumPortfolioHoldings + 2 * NumStocks + 3
  
  constraintMatrix <- matrix(0,nrow = nrows, ncol = ncols)
  RHS <- array(EffectivelyInfinity, dim = nrows)
  LHS <- array(-EffectivelyInfinity, dim = nrows)
  Upper <- array(EffectivelyInfinity, dim = ncols)
  Lower <- array(-EffectivelyInfinity, dim = ncols)
  
  Objective <- array(0, dim = ncols)
  
  # X_i definition, rows 1:NumStocks
  for(nstock in 1:NumStocks){
    constraintMatrix[nstock,1+nstock] = -1
    constraintMatrix[nstock, 1 + NumStocks + nstock] = benchmarkData$CurrentPrice[nstock]
    lot_indexes <- which(portfolioData$ID == benchmarkData$ID[nstock])
    total_shares_held <- sum(portfolioData$NumberOfShares[lot_indexes])
    for(nlot in 1:length(lot_indexes)){
      constraintMatrix[nstock, 1 + 2 * NumStocks + lot_indexes[nlot]] = -benchmarkData$CurrentPrice[nstock]
    }
    RHS[nstock] = -benchmarkData$CurrentPrice[nstock] * total_shares_held
    LHS[nstock] = RHS[nstock]
  }
  
  # X definition, row NumStocks + 1
  constraintMatrix[NumStocks + 1,1] = -1
  constraintMatrix[NumStocks + 1, 2:(1+nstock)] = 1
  RHS[NumStocks + 1] = 0
  LHS[NumStocks + 1] = RHS[NumStocks + 1]
  
  # Sector constraints, rows (NumStocks + 2):(NumStocks + 2 * NumSectors + 1)
  for(nsector in 1:NumSectors){
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 1,1] = -(1 + 0.05) * SectorCap$Percent[nsector]
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 2,1] = -(1 - 0.05) * SectorCap$Percent[nsector]
    sector_indexes = which(benchmarkData$Sector == sectorNames[nsector])
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 1,1+ sector_indexes] = 1
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 2,1+ sector_indexes] = 1
    RHS[NumStocks + 1 + 2 * (nsector-1) + 1 ] = 0
    LHS[NumStocks + 1 + 2 * (nsector-1) + 2 ] = 0
  }
  
  # Developed Markets constraints, rows (NumStocks + 2 * NumSectors + 2):(NumStocks + 2 * NumSectors + 3)
  constraintMatrix[NumStocks + 2 * NumSectors + 2, 1] = -(1 + 0.08) * DevCapPercent
  constraintMatrix[NumStocks + 2 * NumSectors + 3,1] = -(1 - 0.08) * DevCapPercent
  dev_indexes = which(benchmarkData$IsDevMarket == TRUE)
  constraintMatrix[NumStocks + 2 * NumSectors + 2,1+ dev_indexes] = 1
  constraintMatrix[NumStocks + 2 * NumSectors + 3,1+ dev_indexes] = 1
  RHS[NumStocks + 2 * NumSectors + 2 ] = 0
  LHS[NumStocks + 2 * NumSectors + 3 ] = 0
  
  
  # Emerging Markets constraints, rows (NumStocks + 2 * NumSectors + 4):(NumStocks + 2 * NumSectors + 5)
  constraintMatrix[NumStocks + 2 * NumSectors + 4, 1] = -(1 + 0.08) * EMCapPercent
  constraintMatrix[NumStocks + 2 * NumSectors + 5,1] = -(1 - 0.08) * EMCapPercent
  em_indexes = which(benchmarkData$IsDevMarket == FALSE)
  constraintMatrix[NumStocks + 2 * NumSectors + 4,1+ em_indexes] = 1
  constraintMatrix[NumStocks + 2 * NumSectors + 5,1+ em_indexes] = 1
  RHS[NumStocks + 2 * NumSectors + 4 ] = 0
  LHS[NumStocks + 2 * NumSectors + 5 ] = 0
  
  # Large Cap constraints, rows (NumStocks + 2 * NumSectors + 6):(NumStocks + 2 * NumSectors + 7)
  constraintMatrix[NumStocks + 2 * NumSectors + 6, 1] = -(1 + 0.1) * CapTierCap$Percent[which(CapTierCap$TierName == "Large")]
  constraintMatrix[NumStocks + 2 * NumSectors + 7,1] = -(1 - 0.1) * CapTierCap$Percent[which(CapTierCap$TierName == "Large")]
  lc_indexes = which(benchmarkData$CapTier == "Large")
  constraintMatrix[NumStocks + 2 * NumSectors + 6,1+ lc_indexes] = 1
  constraintMatrix[NumStocks + 2 * NumSectors + 7,1+ lc_indexes] = 1
  RHS[NumStocks + 2 * NumSectors + 6 ] = 0
  LHS[NumStocks + 2 * NumSectors + 7 ] = 0
  
  # Mid Cap constraints, rows (NumStocks + 2 * NumSectors + 8):(NumStocks + 2 * NumSectors + 9)
  constraintMatrix[NumStocks + 2 * NumSectors + 8, 1] = -(1 + 0.1) * CapTierCap$Percent[which(CapTierCap$TierName == "Mid")]
  constraintMatrix[NumStocks + 2 * NumSectors + 9,1] = -(1 - 0.1) * CapTierCap$Percent[which(CapTierCap$TierName == "Mid")]
  mc_indexes = which(benchmarkData$CapTier == "Mid")
  constraintMatrix[NumStocks + 2 * NumSectors + 8,1+ mc_indexes] = 1
  constraintMatrix[NumStocks + 2 * NumSectors + 9,1+ mc_indexes] = 1
  RHS[NumStocks + 2 * NumSectors + 8 ] = 0
  LHS[NumStocks + 2 * NumSectors + 9 ] = 0
  
  # Small Cap constraints, rows (NumStocks + 2 * NumSectors + 10):(NumStocks + 2 * NumSectors + 11)
  constraintMatrix[NumStocks + 2 * NumSectors + 10, 1] = -(1 + 0.1) * CapTierCap$Percent[which(CapTierCap$TierName == "Small")]
  constraintMatrix[NumStocks + 2 * NumSectors + 11,1] = -(1 - 0.1) * CapTierCap$Percent[which(CapTierCap$TierName == "Small")]
  sc_indexes = which(benchmarkData$CapTier == "Small")
  constraintMatrix[NumStocks + 2 * NumSectors + 10,1+ sc_indexes] = 1
  constraintMatrix[NumStocks + 2 * NumSectors + 11,1+ sc_indexes] = 1
  RHS[NumStocks + 2 * NumSectors + 10 ] = 0
  LHS[NumStocks + 2 * NumSectors + 11 ] = 0
  
  # U.S. Stock constraints, rows (NumStocks + 2 * NumSectors + 12):(NumStocks + 2 * NumSectors + 13)
  # constraintMatrix[NumStocks + 2 * NumSectors + 12, 1] = -(1 + 0.05) * USCapPercent
  # constraintMatrix[NumStocks + 2 * NumSectors + 13,1] = -(1 - 0.05) * USCapPercent
  # us_indexes = which(benchmarkData$Country == "U.S.")
  # constraintMatrix[NumStocks + 2 * NumSectors + 12,1+ us_indexes] = 1
  # constraintMatrix[NumStocks + 2 * NumSectors + 13,1+ us_indexes] = 1
  # RHS[NumStocks + 2 * NumSectors + 12 ] = 0
  # LHS[NumStocks + 2 * NumSectors + 13 ] = 0
  
  
  # Realized Gain or Loss, rows (NumStocks + 2 * NumSectors + 14):(NumStocks + 2 * NumSectors + 15)
  for(nstock in 1:NumStocks){
    lot_indexes <- which(portfolioData$IDNum == benchmarkData$ID)
    total_shares_held <- sum(portfolioData$NumberOfShares[lot_indexes])
    for(nlot in 1:length(lot_indexes)){
      constraintMatrix[NumStocks + 2 * NumSectors + 14, 1 + 2 * NumStocks + lot_indexes[nlot]] = 
        benchmarkData$Price[nstock] * 
        ifelse(benchmarkData$Country[nstock] == "U.S.",1 - 0.001, 1 - 0.002) -
        portfolioData$PurchasePrice[lot_indexes[nlot]]
    }
  }
  constraintMatrix[NumStocks + 2 * NumSectors + 14,NumPortfolioHoldings + 2 * NumStocks + 2] = -1
  constraintMatrix[NumStocks + 2 * NumSectors + 14,NumPortfolioHoldings + 2 * NumStocks + 3] = 1
  
  RHS[NumStocks + 2 * NumSectors + 14] = 0
  LHS[NumStocks + 2 * NumSectors + 14] = 0
  
  constraintMatrix[NumStocks + 2 * NumSectors + 15,NumPortfolioHoldings + 2 * NumStocks + 3] = -1
  constraintMatrix[NumStocks + 2 * NumSectors + 15,1] = 0.1
  LHS[NumStocks + 2 * NumSectors + 15] = 0
  
  # Balance constraint
  for(nstock in 1:NumStocks){
    lot_indexes <- which(portfolioData$IDNum == nstock)
    total_shares_held <- sum(portfolioData$NumberOfShares[lot_indexes])
    for(nlot in 1:length(lot_indexes)){
      constraintMatrix[NumStocks + 2 * NumSectors + 16, 1 + 2 * NumStocks + lot_indexes[nlot]] = 
        benchmarkData$Price[nstock] * ifelse(benchmarkData$Country[nstock] == "U.S.",1 - 0.001, 1 - 0.002)
    }
    constraintMatrix[NumStocks + 2 * NumSectors + 16, 1 + NumStocks + nstock] = 
      - benchmarkData$Price[nstock] * ifelse(benchmarkData$Country[nstock] == "U.S.",1 + 0.001, 1 + 0.002)
  }
  #  RHS[NumStocks + 2 * NumSectors + 16] = -Inflow/scalar
  #  LHS[NumStocks + 2 * NumSectors + 16] = -Inflow/scalar
  RHS[NumStocks + 2 * NumSectors + 16] = -Inflow
  LHS[NumStocks + 2 * NumSectors + 16] = -Inflow
  
  
  #  Set variable upper and lower bounds
  Lower <- array(0, dim = ncols)
  Upper[(2 + 2 * NumStocks):(1 + 2 * NumStocks + NumPortfolioHoldings)] <- portfolioData$NumberOfShares
  
  #  which(!constraintMatrix == 0)
  #  constraintMatrix[ which(!constraintMatrix == 0)]
  
  #  Set objective coefficients
  Objective[1] = 1
  Objective[ncols - 1] = -0.25
  Objective[ncols] = 0.25
  
  
  
  #  Convert dense matrix into sparse format
  rowIndexes <- row(constraintMatrix)[which(!constraintMatrix == 0)]
  colIndexes <- col(constraintMatrix)[which(!constraintMatrix == 0)]
  NonZeroValues <- constraintMatrix[cbind(rowIndexes,colIndexes)]
  
  if(debugSwitch){
    ZeroBlock = matrix(0,nrow = 3, ncol = 1)
    outputMatrix <- cbind(rbind(ZeroBlock,matrix(LHS,ncol = 1)),
                          rbind(Objective, Upper, Lower, constraintMatrix),
                          rbind(ZeroBlock,matrix(RHS,ncol = 1)))
    write.csv(outputMatrix,file.path(workingDirectory,"DebugMatrix.csv"))
  }
  return(list("rowIndexes" = rowIndexes, 
              "colIndexes" = colIndexes, 
              "NonZeroValues" = NonZeroValues,
              "constraintMatrix" = constraintMatrix, 
              "RHS" = RHS,
              "LHS" = LHS,
              "Upper" = Upper,
              "Lower" = Lower,
              "Objective" = Objective,
              "NumRows" = nrows,
              "NumCols" = ncols,
              "NumNonZeros" = length(NonZeroValues)))
}

constraintMatrix <- MatrixGenerator(debugSwitch)


######################################
##
## setup the linear program
##
######################################

library(glpkAPI)
## initialize model
lp<- initProbGLPK()

# maximize objective GLP_Max (minimize with GLP_MIN)
setObjDirGLPK(lp,GLP_MAX)

# tell model how many rows and columns
addRowsGLPK(lp, constraintMatrix$NumRows)
addColsGLPK(lp, constraintMatrix$NumCols)

# add column limits
setColsBndsGLPK(lp,1:constraintMatrix$NumCols, constraintMatrix$Lower, constraintMatrix$Upper)
setRowsBndsGLPK(lp,1:constraintMatrix$NumRows,constraintMatrix$LHS,constraintMatrix$RHS)
setObjCoefsGLPK(lp,1:constraintMatrix$NumCols,constraintMatrix$Objective)

# set column kinds/types: binary = 0, integer = 1, continuous = 2
setColsKindGLPK(lp,1:constraintMatrix$NumCols,rep(1,constraintMatrix$NumCols))

#  uncomment following lines to check values in lp object
# getColsLowBndsGLPK(lp,1:constraintMatrix$NumCols)
# getColsUppBndsGLPK(lp,1:constraintMatrix$NumCols)
# getRowsLowBndsGLPK(lp,1:constraintMatrix$NumRows)
# getRowsUppBndsGLPK(lp,1:constraintMatrix$NumRows)
# getColsKindGLPK(lp,1:constraintMatrix$NumCols)

# load constraint matrix coefficients
loadMatrixGLPK(lp,constraintMatrix$NumNonZeros,
                 constraintMatrix$rowIndexes, 
                 constraintMatrix$colIndexes,
                 constraintMatrix$NonZeroValues)

# solve LP problem using Simplex Method
solveSimplexGLPK(lp)
#solveInterior(lp)
#solveMIPGLPK(lp)

# get results of solution
# solve status 5 = optimal solution found
getSolStatGLPK(lp)
status_codeGLPK(getSolStatGLPK(lp))

# objective function value
getObjValGLPK(lp)
# value of variables in optimal solution
getColsPrimGLPK(lp)
# status of each variable in optimal solution 1 = basic variable
getColsStatGLPK(lp)

# get dual values for each row/constraint
getRowsDualGLPK(lp)

# save sensitivity report to a file
# this report is only available if solve was by simplex method
printRangesGLPK(lp, numrc = 0, rowcol = NULL, fname = "sensitivity.csv")

ObjectiveValue <- getObjValGLPK(lp)
PortValue <- getColsPrimGLPK(lp)[1]
StockValue <- getColsPrimGLPK(lp)[2:(1+NumStocks)]
PurchaseAmount <- getColsPrimGLPK(lp)[(2+NumStocks):(1 + 2 * NumStocks)]
PurchaseValue <- benchmarkData$Price * PurchaseAmount
LotSaleAmount <- getColsPrimGLPK(lp)[(2 + 2 * NumStocks):(1 + 2 * NumStocks + NumPortfolioHoldings)]
SellAmount <- unlist(lapply(1:NumStocks, FUN = function(nstock){
    sum(LotSaleAmount[portfolioData$ID == benchmarkData$ID[nstock]])
  }))
SellValue <- benchmarkData$Price * SellAmount
PurchaseCostOfLots <- sum(portfolioData$NumberOfShares * portfolioData$PurchasePrice)
StockAmount <- StockValue / benchmarkData$Price  
RebalSummary <-  cbind(InitialValue, PurchaseValue, SellValue, StockValue, 
                       benchmarkData$Price,InitialAmount,PurchaseAmount, SellAmount, StockAmount)
colnames(RebalSummary)[5] <- "Price"

