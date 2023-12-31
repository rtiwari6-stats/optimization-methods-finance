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
  
  # H_i definition, rows 1:NumStocks
  for(nstock in 1:NumStocks){
    constraintMatrix[nstock,1+nstock] = -1
    constraintMatrix[nstock, 1 + NumStocks + nstock] = benchmarkData$Price[nstock]
    lot_indexes <- which(portfolioData$IDNum == nstock)
    total_shares_held <- sum(portfolioData$NumberOfShares[lot_indexes])
    for(nlot in 1:length(lot_indexes)){
      constraintMatrix[nstock, 1 + 2 * NumStocks + lot_indexes[nlot]] = -benchmarkData$Price[nstock]
    }
    RHS[nstock] = -benchmarkData$Price[nstock] * total_shares_held
    LHS[nstock] = RHS[nstock]
  }
  
  # H definition, row NumStocks + 1
  constraintMatrix[NumStocks + 1,1] = -1
  constraintMatrix[NumStocks + 1, 2:(1+nstock)] = 1
  RHS[NumStocks + 1] = 0
  LHS[NumStocks + 1] = RHS[NumStocks + 1]

    # Sector constraints, rows (NumStocks + 2):(NumStocks + 2 * NumSectors + 1)
  for(nsector in 1:NumSectors){
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 1,1] = -(1 + 0.08) * SectorCap$Percent[nsector]
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 2,1] = -(1 - 0.08) * SectorCap$Percent[nsector]
    sector_indexes = which(benchmarkData$Sector == sectorNames[nsector])
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 1,1+ sector_indexes] = 1
    constraintMatrix[NumStocks + 1 + 2 * (nsector - 1) + 2,1+ sector_indexes] = 1
    RHS[NumStocks + 1 + 2 * (nsector-1) + 1 ] = 0
    LHS[NumStocks + 1 + 2 * (nsector-1) + 2 ] = 0
  }
  
  # Developed Markets constraints, rows (NumStocks + 2 * NumSectors + 2):(NumStocks + 2 * NumSectors + 3)
  constraintMatrix[NumStocks + 2 * NumSectors + 2, 1] = -(1 + 0.09) * DevCapPercent
  constraintMatrix[NumStocks + 2 * NumSectors + 3,1] = -(1 - 0.09) * DevCapPercent
  dev_indexes = which(benchmarkData$IsDevMarket == TRUE)
  constraintMatrix[NumStocks + 2 * NumSectors + 2,1+ dev_indexes] = 1
  constraintMatrix[NumStocks + 2 * NumSectors + 3,1+ dev_indexes] = 1
  RHS[NumStocks + 2 * NumSectors + 2 ] = 0
  LHS[NumStocks + 2 * NumSectors + 3 ] = 0
  
  
  # Emerging Markets constraints, rows (NumStocks + 2 * NumSectors + 4):(NumStocks + 2 * NumSectors + 5)
  constraintMatrix[NumStocks + 2 * NumSectors + 4, 1] = -(1 + 0.09) * EMCapPercent
  constraintMatrix[NumStocks + 2 * NumSectors + 5,1] = -(1 - 0.09) * EMCapPercent
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
  constraintMatrix[NumStocks + 2 * NumSectors + 12, 1] = -(1 + 0.05) * USCapPercent
  constraintMatrix[NumStocks + 2 * NumSectors + 13,1] = -(1 - 0.05) * USCapPercent
  us_indexes = which(benchmarkData$Country == "U.S.")
  constraintMatrix[NumStocks + 2 * NumSectors + 12,1+ us_indexes] = 1
  constraintMatrix[NumStocks + 2 * NumSectors + 13,1+ us_indexes] = 1
  RHS[NumStocks + 2 * NumSectors + 12 ] = 0
  LHS[NumStocks + 2 * NumSectors + 13 ] = 0
  
  
  # Realized Gain or Loss, rows (NumStocks + 2 * NumSectors + 14):(NumStocks + 2 * NumSectors + 15)
  for(nstock in 1:NumStocks){
    lot_indexes <- which(portfolioData$IDNum == nstock)
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
