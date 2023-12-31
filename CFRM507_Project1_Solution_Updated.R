################################################
################################################
####
####
####   CFRM 507, Project 1 Solver
####
####   Written by Steve Murray, (C)2022
####
####
################################################
################################################



######################################
##
##  some preliminaries
##
######################################

##  clear environment
rm(list = ls())
debugSwitch = FALSE

## include libraries
library(tidyverse)
library(CVXR)
library(Matrix)
library(glpkAPI)

######
##
##  File locations (You will need to change the workingFolder)
##
######

## set folders and files
workingFolder <- "C:\\Users\\rohan\\Documents\\UW\\CFRM-507\\2022\\project1"
#benchmarkDataFileName <- "BenchmarkData_Small.csv"
#portfolioDataFileName <- "PortfolioHoldings_Small.csv"
benchmarkDataFileName <- "BenchmarkData.csv"
portfolioDataFileName <- "PortfolioHoldings.csv"

#######
##
##  Rework the data to calculate all the necessary values
##
#######

## Read in raw data
benchmark_df <- read.csv(file.path(workingFolder,benchmarkDataFileName))
portfolio_df <- read.csv(file.path(workingFolder,portfolioDataFileName))

## parameters
#redemptionAmount <- 15000000
#redemptionAmount <- 1.50598e+07
redemptionAmount <-  1.40428e+07 
#redemptionAmount <- 1000
DevMkts <- c("Australia","Austria", "Belgium", "Denmark", "Finland",
             "France", "Germany", "Italy", "Netherlands", "Norway",
             "Spain", "Sweden", "Switzerland",  "United Kingdom", "Canada",
             "Japan")

######
## Parse benchmark data and add new values to benchmark_df data frame
######
benchmark_df$SecurityIndex <- 1:dim(benchmark_df)[1]
benchmark_df$Market <- ifelse(benchmark_df$Country %in% DevMkts,"Developed","EM")
uniqueMarkets <- unique(benchmark_df$Market)
uniqueMarkets <- data.frame(Market = uniqueMarkets) %>% 
                      arrange(Market) %>%
                      mutate(MarketIndex = row_number())

uniqueCountries <- unique(benchmark_df$Country)
uniqueCountries <- data.frame(Country = uniqueCountries) %>% 
                      arrange(Country) %>%
                      mutate(CountryIndex = row_number())

uniqueCapTiers <- unique(benchmark_df$Cap.Tier)
uniqueCapTiers <- data.frame(Cap.Tier = uniqueCapTiers) %>% 
                      arrange(Cap.Tier) %>%
                      mutate(CapTierIndex = row_number())

uniqueSectors <- unique(benchmark_df$Sector)
uniqueSectors <- data.frame(Sector = uniqueSectors) %>% 
                      arrange(Sector) %>%
                      mutate(SectorIndex = row_number())

benchmark_df <- left_join(benchmark_df, uniqueMarkets, by = c("Market"))
benchmark_df <- left_join(benchmark_df, uniqueCountries, by = c("Country"))
benchmark_df <- left_join(benchmark_df, uniqueCapTiers, by = c("Cap.Tier"))
benchmark_df <- left_join(benchmark_df, uniqueSectors, by = c("Sector"))

benchmark_df$MarketCap <- as.numeric(gsub(",","",benchmark_df$Market.Cap..US))
totalMarketCap <- sum(benchmark_df$MarketCap)

benchmark_df$Weight <- benchmark_df$MarketCap / totalMarketCap

#####
## Calculate weights for sectors, cap tiers, markets, countries
#####

SectorWeight_df <- benchmark_df %>% 
                    group_by(Sector) %>% 
                    summarize(Weight = sum(Weight)) %>% 
                    ungroup()
SectorWeight_df <- left_join(SectorWeight_df,uniqueSectors)
                  

CapTierWeight_df <- benchmark_df %>%
                    group_by(Cap.Tier) %>%
                    summarize(Weight = sum(Weight)) %>%
                    ungroup()
CapTierWeight_df <- left_join(CapTierWeight_df, uniqueCapTiers)

MarketWeight_df <- benchmark_df %>%
                    group_by(Market) %>%
                    summarize(Weight = sum(Weight)) %>%
                    ungroup()
MarketWeight_df <- left_join(MarketWeight_df, uniqueMarkets)

CountryWeight_df <- benchmark_df %>%
                    group_by(Country) %>%
                    summarize(Weight = sum(Weight)) %>%
                    ungroup()
CountryWeight_df <- left_join(CountryWeight_df, uniqueCountries)

benchmark_df <- left_join(benchmark_df, CountryWeight_df, by = c("Country","CountryIndex"))

colnames(benchmark_df)[which(colnames(benchmark_df) == "Weight.x")] = "Weight"
colnames(benchmark_df)[which(colnames(benchmark_df) == "Weight.y")] = "CountryWeight"

## Set transaction costs
benchmark_df$tcost <- ifelse(benchmark_df$CountryWeight > 0.02,0.0008, 0.002)

######
## Parse portfolio data and add new values to portfolio_df data frame
######

portfolio_df$InvestmentIndex <- 1:dim(portfolio_df)[1]

uniqueInvestments <- unique(portfolio_df$Identifier)

portfolio_df <- left_join(portfolio_df, benchmark_df %>% select("Identifier","Current.Price","SecurityIndex","tcost")) 

portfolio_df$Value <- portfolio_df$Current.Price * portfolio_df$Number.of.Shares

portfolio_df$GainLossCoef <- ((1 - portfolio_df$tcost) * portfolio_df$Current.Price - portfolio_df$Purchase.Price) /
                                ((1- portfolio_df$tcost) * portfolio_df$Current.Price)

totalPortfolioValue <- sum(portfolio_df$Value)

#######
##
##  Build matrices needed to specify optimization problem in CVXR
##
#######

# create variables to hold dimensions of matrices
numSecurity <- dim(benchmark_df)[1]
numSector <- dim(SectorWeight_df)[1]
numInvestment <- dim(portfolio_df)[1]
numCapTier <- dim(CapTierWeight_df)[1]
numCountry <- dim(CountryWeight_df)[1]
numMarket <- dim(MarketWeight_df)[1]

##  Create indicator matrices

Sector <- as.matrix(sparseMatrix(as.vector(benchmark_df$SecurityIndex),
                                  as.vector(benchmark_df$SectorIndex), 
                                  x=rep(1,numSecurity)))

CapTier <- as.matrix(sparseMatrix(as.vector(benchmark_df$SecurityIndex),
                                  as.vector(benchmark_df$CapTierIndex),
                                  x = rep(1,numSecurity)))

Market <- as.matrix(sparseMatrix(as.vector(benchmark_df$SecurityIndex),
                                 as.vector(benchmark_df$MarketIndex),
                                 x = rep(1,numSecurity)))

Country <- as.matrix(sparseMatrix(as.vector(benchmark_df$SecurityIndex),
                                  as.vector(benchmark_df$CountryIndex),
                                  x = rep(1,numSecurity)))

Stock <- as.matrix(sparseMatrix(as.vector(c(portfolio_df$SecurityIndex,numSecurity)),
                                as.vector(c(portfolio_df$InvestmentIndex,numInvestment)),
                                x = c(rep(1,numInvestment),0)), 
                   nrow = numSecurity, ncol = numInvestment)

## Construct matrices to hold other model data

Value <- as.matrix(portfolio_df$Value, ncol = 1)

SectorWeight <- as.matrix(SectorWeight_df$Weight, ncol = 1)

CapTierWeight <- as.matrix(CapTierWeight_df$Weight, ncol = 1)

MarketWeight <- as.matrix(MarketWeight_df$Weight, ncol = 1)

CountryWeight <- as.matrix(CountryWeight_df$Weight, ncol = 1)

Cost <- as.matrix(portfolio_df$Purchase.Price, ncol = 1)

investmentPrice <- as.matrix(portfolio_df$Current.Price, ncol = 1)

securityPrice <- as.matrix(benchmark_df$Current.Price, ncol = 1)

investmenttcost <- as.matrix(portfolio_df$tcost, ncol = 1)

securitytcost <- as.matrix(benchmark_df$tcost, ncol = 1)

gain_loss_coef <- as.matrix(portfolio_df$GainLossCoef, ncol = 1)

###
#
#  Build the model
#
#     Assume variables correspond to formulation are in following order
#     X -- dollar value of security i after trades i = 1..3585, indexes 1 to 3585
#     P -- dollar value of security i purchased i = 1..3585, indexes 3586 to 7170
#     S -- dollar value of security k sold k = 1..249, indexes 7171 to 7419
#     W -- dollar value of portfolio after trades, index 7420
#     NG -- NetRealizedGain, index 7421
#     NL -- NetRealizedLoss, index 7422
#
#     Constraints are
#     Define W, index 1
#     Budget, index 2
#     Realized Loss Limit, index 3
#     Realized Gain - Realized Loss = sum of investment gains and losses, index 4
#     Sector constraints (lower bound first)
#     Market constraints (lower bound first)
#     CapTier constraints (lower bound first)
#
#     Simple bounds include nonnegativity and upper bounds on Sale variables
#
##

X <- Variable(numSecurity)
P <- Variable(numSecurity)
S <- Variable(numInvestment)
W <- Variable(1)
NG <- Variable(1)
NL <- Variable(1)

sector_range <- 0.05
market_range <- 0.08
captier_range <- 0.1

# Set ranges wide for testing
#sector_range <- 2
#market_range <- 2
#captier_range <- 2

# indexing for creating glpk constraint matrix
X_indexes <- 1:numSecurity
P_indexes <- (numSecurity + 1):(2 * numSecurity)
S_indexes <- (2 * numSecurity + 1):(2 * numSecurity + numInvestment)
W_index <- 2 * numSecurity + numInvestment + 1
NG_index <- 2 * numSecurity + numInvestment + 2
NL_index <- 2 * numSecurity + numInvestment + 3
BigM <- sum(portfolio_df$Value * 10)
constraint_tolerance <- 1.0e-5

# W Definition constraint
constraint_W_defn <- W == sum(X)

row_indexes <- rep(1, numSecurity + 1)
col_indexes <- c(X_indexes, W_index)
mat_values <- c(rep(1, numSecurity), -1)
lhs <- c(0 - constraint_tolerance)
rhs <- c(0 + constraint_tolerance)

# Budget constraint
constraint_budget <- redemptionAmount + t(1+securitytcost) %*% P ==
                      t(1-investmenttcost) %*% S

row_indexes <- c(row_indexes, rep(2, numSecurity + numInvestment))
col_indexes <- c(col_indexes, P_indexes, S_indexes)
mat_values <- c(mat_values, -(1+securitytcost), t(1-securitytcost) %*% Stock)
lhs <- c(lhs, redemptionAmount - constraint_tolerance)
rhs <- c(rhs, redemptionAmount + constraint_tolerance)

# Loss limit constraint
constraint_loss_limit <- NL <= 0.1 * W

row_indexes <- c(row_indexes, 3, 3)
col_indexes <- c(col_indexes, W_index, NL_index)
mat_values <- c(mat_values, 0.1, -1)
lhs <- c(lhs, 0)
rhs <- c(rhs, BigM)


# Gain loss constraint
constraint_gain_loss <- t(gain_loss_coef) %*% S == NG - NL

row_indexes <- c(row_indexes, rep(4, numInvestment + 2))
col_indexes <- c(col_indexes, S_indexes, NG_index, NL_index)
mat_values <- c(mat_values, gain_loss_coef[,1], -1, 1)
lhs <- c(lhs, 0 - constraint_tolerance)
rhs <- c(rhs, 0 + constraint_tolerance)

row_offset = 4

# Sector weights
constraint_sector_weight_lower <- (1 - sector_range) * SectorWeight * W <= t(Sector) %*% X
constraint_sector_weight_upper <- (1 + sector_range) * SectorWeight * W >= t(Sector) %*% X

row_indexes <- c(row_indexes, row_offset + benchmark_df$SectorIndex,row_offset + 1:numSector)
col_indexes <- c(col_indexes, benchmark_df$SecurityIndex, rep(W_index,numSector))
mat_values <- c(mat_values, rep(1,numSecurity), -(1 - sector_range) * SectorWeight[,1])
lhs <- c(lhs, rep(0,numSector))
rhs <- c(rhs, rep(BigM, numSector))
row_offset <- row_offset + numSector

row_indexes <- c(row_indexes, row_offset + benchmark_df$SectorIndex,row_offset + 1:numSector)
col_indexes <- c(col_indexes, benchmark_df$SecurityIndex, rep(W_index,numSector))
mat_values <- c(mat_values, rep(-1,numSecurity), (1 + sector_range) * SectorWeight[,1])
lhs <- c(lhs, rep(0,numSector))
rhs <- c(rhs, rep(BigM, numSector))
row_offset <- row_offset + numSector

# Market weights
constraint_market_weight_lower <- (1 - market_range) * MarketWeight * W <= t(Market) %*% X
constraint_market_weight_upper <- (1 + market_range) * MarketWeight * W >= t(Market) %*% X

row_indexes <- c(row_indexes, row_offset + benchmark_df$MarketIndex,row_offset + 1:numMarket)
col_indexes <- c(col_indexes, benchmark_df$SecurityIndex, rep(W_index,numMarket))
mat_values <- c(mat_values, rep(1, numSecurity), -(1 - market_range) * MarketWeight[,1])
lhs <- c(lhs, rep(0,numMarket))
rhs <- c(rhs, rep(BigM, numMarket))
row_offset <- row_offset + numMarket

row_indexes <- c(row_indexes, row_offset + benchmark_df$MarketIndex,row_offset + 1:numMarket)
col_indexes <- c(col_indexes, benchmark_df$SecurityIndex, rep(W_index,numMarket))
mat_values <- c(mat_values, rep(-1, numSecurity), (1 + market_range) * MarketWeight[,1])
lhs <- c(lhs, rep(0,numMarket))
rhs <- c(rhs, rep(BigM, numMarket))
row_offset <- row_offset + numMarket

# Cap Tier weights
constraint_captier_weight_lower <- (1 - captier_range) * CapTierWeight * W <= t(CapTier) %*% X
constraint_captier_weight_upper <- (1 + captier_range) * CapTierWeight * W >= t(CapTier) %*% X

row_indexes <- c(row_indexes, row_offset + benchmark_df$CapTierIndex,row_offset + 1:numCapTier)
col_indexes <- c(col_indexes, benchmark_df$SecurityIndex, rep(W_index,numCapTier))
mat_values <- c(mat_values, rep(1, numSecurity), -(1 - captier_range) * CapTierWeight[,1])
lhs <- c(lhs, rep(0,numCapTier))
rhs <- c(rhs, rep(BigM, numCapTier))
row_offset <- row_offset + numCapTier

row_indexes <- c(row_indexes, row_offset + benchmark_df$CapTierIndex,row_offset + 1:numCapTier)
col_indexes <- c(col_indexes, benchmark_df$SecurityIndex, rep(W_index,numCapTier))
mat_values <- c(mat_values, rep(-1, numSecurity), (1 + captier_range) * CapTierWeight[,1])
lhs <- c(lhs, rep(0,numCapTier))
rhs <- c(rhs, rep(BigM, numCapTier))
row_offset <- row_offset + numCapTier

# Security inventory
constraint_security_inventory <- Stock %*% Value + P - Stock %*% S == X

row_indexes <- c(row_indexes, rep(row_offset + 1:numSecurity,2), 
                                row_offset +  portfolio_df$SecurityIndex)
col_indexes <- c(col_indexes, X_indexes, P_indexes, 2 * numSecurity + portfolio_df$InvestmentIndex)
mat_values <- c(mat_values, rep(1, numSecurity), rep(-1, numSecurity), rep(1,numInvestment))
temp <- Stock %*% Value
lhs <- c(lhs, temp[,1]- constraint_tolerance)
rhs <- c(rhs, temp[,1] + constraint_tolerance)

constraint_non_negative_X <- X >= 0
 
constraint_non_negative_P <- P >= 0

constraint_non_negative_S <- S >= 0

constraint_limit_sales <- S <= Value

constraint_non_negative_gains <- NG >= 0

constraint_non_negative_losses <- NL >= 0 

#####
##  Check glpk constraint matrix
#####
DenseConstMatrix <- as.matrix(sparseMatrix(row_indexes, col_indexes, x =mat_values),
                   nrow = max(row_indexes), ncol = max(col_indexes))


constraints <- list(constraint_W_defn,
                    constraint_budget,
                    constraint_loss_limit,
                    constraint_gain_loss,
                    constraint_sector_weight_lower,
                    constraint_sector_weight_upper,
                    constraint_market_weight_lower,
                    constraint_market_weight_upper,
                    constraint_captier_weight_lower,
                    constraint_captier_weight_upper,
                    constraint_non_negative_X,
                    constraint_non_negative_P,
                    constraint_non_negative_S,
                    constraint_limit_sales,
                    constraint_non_negative_gains,
                    constraint_non_negative_losses,
                    constraint_security_inventory)

constraints <- list(constraint_W_defn,
                    constraint_budget,
#                    constraint_loss_limit,
#                    constraint_gain_loss,
#                    constraint_sector_weight_lower,
#                    constraint_sector_weight_upper,
#                    constraint_market_weight_lower,
#                    constraint_market_weight_upper,
#                    constraint_captier_weight_lower,
#                    constraint_captier_weight_upper,
                    constraint_non_negative_X,
                    constraint_non_negative_P,
                    constraint_non_negative_S,
                    constraint_limit_sales,
                    constraint_non_negative_gains,
                    constraint_non_negative_losses,
                    constraint_security_inventory)

objective <- t(securitytcost) %*% P + t(investmenttcost) %*% S + 0.15 * NG

#####
##
##  Solve with CVXR
##
#####
# prob <- Problem(Minimize(objective), constraints)
# 
# CVXR::installed_solvers()
# #result <- solve(prob)
# #result <- solve(prob, verbose = TRUE, solver = "GLPK")
# #result <- solve(prob, solver = "SCS")
# result <- solve(prob, num_iter = 1000, solver = "ECOS_BB")
# 
# result$status
# result$value
# 
# # extract solution values
# X_cvxr <- result$getValue(X)
# P_cvxr <- result$getValue(P)
# S_cvxr <- result$getValue(S)
# W_cvxr <- result$getValue(W)
# NG_cvxr <- result$getValue(NG)
# NL_cvxr <- result$getValue(NL)
# 
# 
# ## look at dual values from cvxr solution
# result$getDualValue(constraints(prob)[[1]])
# result$getDualValue(constraint_budget)
# result$getDualValue(constraints[[2]])
# result$getDualValue(prob@getDualValue(constraint_security_inventory))

#glpk_data <- get_problem_data(prob, "GLPK")

#######
##
##  Solve with glpkAPI
##
#######

objCoef <- c(rep(0,numSecurity), securitytcost, investmenttcost, 0, 0.15, 0)

numNonZero <- length(row_indexes)
## model dimensions
nrows <- max(row_indexes)
ncols <- max(col_indexes)

# column upper and lower bounds
clower <- rep(0, 2 * numSecurity + numInvestment + 3)
cupper <- c(rep(BigM, 2 * numSecurity), Value, rep(BigM,3))

## solve using glpk via the API

## initialize model
lp<- initProbGLPK()

# maximize objective GLP_Max (minimize with GLP_MIN)
setObjDirGLPK(lp,GLP_MIN)

# tell model how many rows and columns
addRowsGLPK(lp, nrows)
addColsGLPK(lp, ncols)

# add column limits
setColsBndsGLPK(lp,c(1:ncols), clower, cupper)
setRowsBndsGLPK(lp,c(1:nrows),lhs, rhs)
setObjCoefsGLPK(lp,c(1:ncols),objCoef)

#setColsKindGLPK(lp,1:ncols,c(GLP_CV,1))   
# load constraint matrix coefficients
loadMatrixGLPK(lp,numNonZero, row_indexes, col_indexes, mat_values)

# solve LP problem using Simplex Method
solveSimplexGLPK(lp)


# get results of solution
# solve status 5 = optimal solution found
getSolStatGLPK(lp)
status_codeGLPK(getSolStatGLPK(lp))

# objective function value
getObjValGLPK(lp)

# value of variables in optimal solution
temp <- getColsPrimGLPK(lp)
X_glpk <- temp[X_indexes]
P_glpk <- temp[P_indexes]
S_glpk <- temp[S_indexes]
W_glpk <- temp[W_index]
NG_glpk <- temp[NG_index]
NL_glpk <- temp[NL_index]

# status of each variable in optimal solution 1 = basic variable
getColsStatGLPK(lp)

# get dual values for each row/constraint
getRowDualGLPK(lp,1)
getRowDualGLPK(lp,2)
getRowDualGLPK(lp,3)

# this is supposed to get all at once, but doesn't seem to work
#getRowsDualGLPK(lp)

# save sensitivity report to a file
printRangesGLPK(lp, numrc = 0, rowcol = NULL, fname = "sensitivity.txt")

#getRowsPrimGLPK(lp)

####
## end of solving with glpk
####


#####
##
## Review solution
##
#####

# copy either glpk or cvxr solution 
# to "solution" variables for checking
X_solution <- X_glpk
P_solution <- P_glpk
S_solution <- S_glpk
W_solution <- W_glpk
NG_solution <- NG_glpk
NL_solution <- NL_glpk

## Calculate transaction costs
t(securitytcost) %*% P_solution + t(investmenttcost) %*% S_solution
t(securitytcost) %*% P_solution
t(investmenttcost) %*% S_solution


## Calculate tax liability
0.15 * NG_solution

##  Determine solution components that are not zero within some tolerance
tolerance <- 1.0e-5
NonZeroHoldingIndexes <- which(X_solution >= tolerance)
NonZeroPurchaseIndexes <- which(P_solution >= tolerance)
NonZeroSaleIndexes <- which(S_solution >= tolerance)

W_solution
NG_solution
NL_solution

## print solution to Console window
cat(cbind(NonZeroHoldingIndexes,X_solution[NonZeroHoldingIndexes]), sep = '\n')
cbind(NonZeroPurchaseIndexes, P_solution[NonZeroPurchaseIndexes])
cbind(NonZeroSaleIndexes, S_solution[NonZeroSaleIndexes])


t(gain_loss_coef) %*% S_solution
NG_solution
NL_solution
NG_solution - NL_solution
W_solution

c("Solution","coef","Gain/Loss","current","Purchase","value","tcost")
head(cbind(S_solution,
           gain_loss_coef, 
           portfolio_df$Current.Price - portfolio_df$Purchase.Price, 
           portfolio_df$Current.Price, 
           portfolio_df$Purchase.Price, 
           portfolio_df$Value,
           portfolio_df$tcost))

# cbind(W_cvxr, W_glpk)
# cbind(NL_cvxr, NL_glpk)
# cbind(NG_cvxr, NG_glpk)
# cbind(X_cvxr, X_glpk)
# cbind(P_cvxr, P_glpk)
# cbind(S_cvxr, S_glpk)

# Check constraints

# W definition
W_solution - sum(X_solution)

# Budget constraint
redemptionAmount + t(1+securitytcost) %*% P_solution -
  t(1-securitytcost) %*% Stock %*% S_solution

# loss limit constraint
0.1 * W_solution - NL_solution 

# gain/loss constraint
t(gain_loss_coef) %*% S_solution - (NG_solution - NL_solution)

# sector weights
cbind(0.95 * SectorWeight * W_solution, t(Sector) %*% X_solution, 1.05 * SectorWeight * W_solution)

# cap tier weights
cbind(0.92 * CapTierWeight * W_solution, t(CapTier) %*% X_solution, 1.08 * CapTierWeight * W_solution)

# market weights
cbind(0.9 * MarketWeight * W_solution, t(Market) %*% X_solution, 1.1 * MarketWeight * W_solution)

# security inventory
sum(abs(Stock %*% Value + P_solution - Stock %*% S_solution - X_solution) >= tolerance)

# X non-negativity
sum(X_solution < 0)

# P non-negativity
sum(P_solution < 0)

# S non-negativity
sum(S_solution < 0)

# NL, NG
NL_solution < 0
NG_solution < 0

#portfolio calculations
#sector caps
p_sectorCap = t(X_solution) %*% Sector
p_sectorCap = p_sectorCap/sum(p_sectorCap)
p_sectorCap

#cap tier caps
p_capTierCap = t(X_solution) %*% CapTier
p_capTierCap = p_capTierCap/sum(p_capTierCap)
p_capTierCap

#market caps
p_MarketCap = t(X_solution) %*% Market
p_MarketCap = p_MarketCap/W_solution
p_MarketCap

portfolio_df$Identifier  <- as.numeric(substr(portfolio_df$Identifier ,2,nchar(portfolio_df$Identifier)))
x_p = rep(0, 3585)
x_p[portfolio_df$Identifier] = portfolio_df$Value

sc = t(x_p) %*% Sector
sc = sc / sum(sc)
sc

mc = t(x_p) %*% Market
mc = mc / sum(mc)
mc

cc = t(x_p) %*% CapTier
cc = cc / sum(cc)
cc