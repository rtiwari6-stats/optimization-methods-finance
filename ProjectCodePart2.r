library("tidyverse")
library("quadprog")
library("MASS")
library(plyr)

library(RColorBrewer)
library(ggplot2)
myColors <- brewer.pal(5,"Set1")

setwd("~/UW/CFRM-507/2022/finalproject")
#TODO
# plot utility in simulation per client
# # times each client crossed retirement limit
# count of each mix in dp table

####
#
#   Assign parameters
#
####

NumAssets = 7
NumMixes = 7

# Number of scenarios for solving (STEP 1).  You will probably want to change this.
NumScenarios = 1000

# Number of paths for simulating results (STEP 2).  You will probably want to change this.
NumSimulationPaths = 1000


## All monetary units are measured in $1000s
Seed = 40
RetirementAge = 67
YoungestAge = 50
SavingsRate = 0.15
Target = 1540.8   ## All monetary units are measured in $1000s
gamma = 1/100000

LowestBalance = 100
BalanceStep = 100
NumBalances = 50

mixfees = c(0.0018, 0.0016, 0.0014, 0.0012, 0.0010, 0.008, 0.006)

## Key information about your clients
ClientData <- data_frame(Age = c(50,51,52,53,56,56,56,58,61,64), 
                         InitialBalance = c(700,600,600,250,1000,800,500,1500,1000,1600))
ClientNames = c("Amy Abrams", "Bob Brown", "Carla Clausen", "Darrin Dorne", "Eric Evans",
                "Francine Farnsworth", "Giovanni Granville", "Heloise Hart", "Issac Iverson",
                "Jennifer Jones")

####
#
#  Declare return assumptions
#
####

# These are statistics of normal returns for the asset classes
Averages = c(0.06, 0.059, 0.07, 0.056, 0.032, 0.052, 0.015)
Stdevs = c(0.191, 0.202, 0.268, 0.207, 0.038, 0.07, 0.058)
CorrelMat = c(1.00, 0.74, 0.67, 0.74, 0.13, 0.47, 0.02,
              0.74, 1.00, 0.70, 0.78, 0.09, 0.46, 0.00,
              0.67, 0.70, 1.00, 0.66, 0.07, 0.45, -0.03,
              0.74, 0.78, 0.66, 1.00, 0.10, 0.37, -0.03,
              0.13, 0.09, 0.07, 0.10, 1.00, 0.10, 0.10,
              0.47, 0.46, 0.45, 0.37, 0.10, 1.00, 0.55,
              0.02, 0.00, -0.03, -0.03, 0.10, 0.55, 1.00)
CorrelMat <- matrix(CorrelMat, nrow = NumAssets, ncol = NumAssets)
AssetAverages = matrix(Averages[1:NumAssets],nrow = 1, ncol = NumAssets)
AssetCovar = diag(Stdevs) %*% CorrelMat %*% diag(Stdevs)

# check that matrix is positive semi-definite
# by confirming existence of Cholesky factorization
chol(AssetCovar)

####
#
#  Declare portfolio (mix) weights
#
####

MixWeights <- c(0.40, 0.30, 0.10, 0.10, 0.05, 0.05, 0.00,
                0.35, 0.25, 0.10, 0.10, 0.10, 0.10, 0.00,
                0.25, 0.21, 0.08, 0.08, 0.30, 0.08, 0.00,
                0.23, 0.19, 0.06, 0.06, 0.40, 0.06, 0.00,
                0.21, 0.15, 0.04, 0.05, 0.45, 0.05, 0.05,
                0.16, 0.12, 0.04, 0.04, 0.50, 0.04, 0.10,
                0.13, 0.09, 0.02, 0.03, 0.55, 0.03, 0.15)
MixWeights <- matrix(MixWeights, nrow = NumAssets, ncol = NumMixes)

## Calculate statistics of the Mixes based on the asset class behavior
MixAverages <-  AssetAverages %*% MixWeights
MixCovar <- t(MixWeights) %*% AssetCovar %*% MixWeights
MixSd = sqrt(diag(MixCovar))

####
#
#  Salary and Savings functions
#
####

Salary = function(age){
  return ((95-0.02*((50-age)^2)*1))
} 

Savings = function(age){
  return (Salary(age) * 0.15)
}

####
#
# Utility function at retirement
#
####

Utility <- function(balance){
  ifelse(balance > 1.2 * Target,log(balance),
         ifelse(balance > 0.8 * Target, log(balance) - gamma * (1.2 * Target - balance)^2,
                log(balance) - gamma * (0.4^2 * Target^2)))
}

####
#
#  Value Table lookup
#
####

#VTable (Age, balance, value)
#look up the value and bestmix for that age, balance combination
VTableLookup <- function(age, balance){
  ## if balance is higher than highest value in table, return highest value in table for that balance
  if(balance > max(VTable$Balance[VTable$Age == age])) return(data.frame(Balance = balance, Value = max(VTable$Value[VTable$Age == age])))
  if(balance < min(VTable$Balance[VTable$Age == age])) return(data.frame(Balance = balance, Value = min(VTable$Value[VTable$Age == age])))
  lowerBalance <- max(VTable$Balance[which(VTable$Age == age & VTable$Balance <= balance)])
  lowerIndex <- which(VTable$Age == age & VTable$Balance == lowerBalance)
  lowerValue <- VTable$Value[lowerIndex]
  lowerMix <- VTable$Mix[lowerIndex]
  
  upperBalance <- min(VTable$Balance[which(VTable$Age == age & VTable$Balance >= balance)])
  upperValue <- VTable$Value[which(VTable$Age == age & VTable$Balance == upperBalance)]
  upperMix <- VTable$Mix[which(VTable$Age == age & VTable$Balance == upperBalance)]
  if(upperBalance == lowerBalance){
    ratio = 1
  } else{
    ratio = (upperBalance - balance)/(upperBalance - lowerBalance)
  }
  if(ratio > 0.5) BestMix = lowerMix
  if(ratio <= 0.5) BestMix = upperMix
  
  return(data.frame(Balance = balance, 
                    Value = ratio * lowerValue + (1 - ratio) * upperValue, 
                    Mix = BestMix))
}

####
#
#  Evaluate Expected Utility Function
#
####

# Evaluation Age: Current age
# Evaluation Balance: Current account balance
#Find the best mix and value for the age, balance combination considering the mixSamples
#TODO: ANNUAL FEE
EvaluateCase <- function(EvaluationAge, EvaluationBalance, MixSample, ValueTable){
  # note in mixSample each mix is a column
  # each mix here represents a decision
  # take mix by mix and calculate expected utility for that age+1, and new balance combination
  ExpectedUtilityForMix <- lapply(1:dim(MixSample)[2], FUN = function(nmix){
    # This takes the the returns from nth mix and multiplies it by the money invested to get the resulting balance after that decision
    # Nodebalances is the possible set of balances at this point we can get by selecting the mix
    NodeBalances = (EvaluationBalance + Savings(EvaluationAge)) * (1 + MixSample[,nmix])
    NodeValues <- lapply(1:length(NodeBalances), FUN = function(nentry){
      #Find the value for the next age, balance combination
      VTableLookup(EvaluationAge + 1, NodeBalances[nentry])
    }) %>% bind_rows()
    # calculate expected utility for that mix
    data.frame(EUtil = mean(NodeValues$Value), Mix = nmix)
  }) %>% bind_rows()
  #Find the best mix and best value by maximizing expected utility for that mix.
  BestMix = ExpectedUtilityForMix$Mix[which(ExpectedUtilityForMix$EUtil == max(ExpectedUtilityForMix$EUtil))]
  BestValue = ExpectedUtilityForMix$EUtil[which(ExpectedUtilityForMix$EUtil == max(ExpectedUtilityForMix$EUtil))]
  # return the best combination
  return(data.frame(Age = EvaluationAge, Balance = EvaluationBalance, Mix = BestMix, Value = BestValue))
}

############################ Done with helper functions ################
#
# The main script starts here
#


####
#
#  Create 1-year collection of returns for each Mix
#
####

set.seed(Seed)
# calculate the random returns for 1 year because we are simulating a year at a time.
assetSamples = mvrnorm(n = NumScenarios, mu = AssetAverages, Sigma = AssetCovar)
MixSample = exp(assetSamples) - 1
MixSample = MixSample %*% MixWeights
MixSample = MixSample * (1- mixfees)

# Check statistics
sample_column_means <- apply(MixSample,2, mean)
sample_column_stdevs <- apply(MixSample, 2, sd)
sample_column_means
sample_column_stdevs
sample_covar <- cov(MixSample)
cbind(t(MixAverages), sample_column_means, t(MixAverages) - sample_column_means)

####
#
#  Build value look up table, VTable
#
####

## Range of balances to be evaluated
Balance <- LowestBalance + BalanceStep * 0:NumBalances

# this sets up a data frame where each row has age=67 and balance is the list of possible balances 
# the utility value associated with each balance is included
VTable <- data.frame(Age = RetirementAge, Balance = Balance, Value = Utility(Balance), Mix = 0)

#loop runs backwards from 66 to youngest
LoopTime <- system.time(for(age in (RetirementAge - 1):YoungestAge){
  # all possible balances for that age
  AgeTable <- data.frame(Balance = Balance, Age = age)
  
  #takes each row of the age table (which is the length of Balance vector) and applies EvaluateCase
  AgeTable <- lapply(1:(dim(AgeTable)[1]), function(nentry){
    # here, we pass an age, a balance and the samples. We also pass the VTable which is the memoized datastructure of results calculated so far
    EvaluateCase(AgeTable$Age[nentry], AgeTable$Balance[nentry], MixSample, VTable)
  }) %>% bind_rows()
  
  VTable <- rbind(VTable, AgeTable)
})

write.csv(VTable, 'VTable.csv')
#VTable = read.csv('VTable.csv', header = TRUE)

LoopTime


########
##
##  End of STEP 1, the solve portion
##
########

## Begin step 2 the simulation of the solution in an out-of-sample tree

## some pictures
legend_Mixes = as.factor(VTable$Mix)
myPlot <- ggplot(VTable,aes(x=Age, y = Balance, fill = legend_Mixes)) +
  ggtitle("Solution Mix recommendations") +
  geom_tile() + geom_hline(yintercept = Target) +
  scale_color_brewer(palette="Spectral") + xlab("Age in years") + ylab("Balance in $k")
plot(myPlot)

####
#
# STEP 2:
#  Evaluate outcomes for each client
#    by simulating through a multi-year tree different from 
#    single-year tree used to create solution
####


## Create new tree to evaluate out-of-sample behavior
set.seed(Seed * 17)
#MultiYearMixSample <- mvrnorm(n = NumSimulationPaths * (RetirementAge - YoungestAge),
#                              mu = MixAverages, Sigma = MixCovar)
assetSamples = mvrnorm(n = NumSimulationPaths * (RetirementAge - YoungestAge), mu = AssetAverages, Sigma = AssetCovar)
MultiYearMixSample = exp(assetSamples) - 1
MultiYearMixSample = MultiYearMixSample %*% MixWeights
MultiYearMixSample = MultiYearMixSample * (1- mixfees)

# Check statistics
sample_column_means <- apply(MultiYearMixSample,2, mean)
sample_column_stdevs <- apply(MultiYearMixSample, 2, sd)
sample_column_means
sample_column_stdevs
sample_covar <- cov(MultiYearMixSample)
cbind(t(MixAverages), sample_column_means, t(MixAverages) - sample_column_means)

# Lookup solution for immediate allocation decision for each client
NumClients = dim(ClientData)[1]

ClientData <- lapply(1:NumClients, FUN = function(nclient){
  # take client's current age and starting balance to find the starting mix and value associated.
  TableInfo = VTableLookup(ClientData$Age[nclient], ClientData$InitialBalance[nclient])
  data.frame(Age = ClientData$Age[nclient], 
             InitialBalance = ClientData$InitialBalance[nclient], 
             InitialMix = TableInfo$Mix, 
             EUtil = TableInfo$Value)
}) %>% bind_rows()

write.csv(ClientData, 'ClientData.csv')
#ClientData = read.csv('ClientData.csv', header = TRUE)

##  Evaluate the clients' situation at each node in the out-of-sample tree
# going through each client
LoopTime <- system.time(ClientResults <- lapply(1:NumClients, function(nclient){
  # take client age
  clientAge = ClientData$Age[nclient]
  # setup this client data
  tempdf <- data_frame(Client = nclient,
                       Node = 0, 
                       Age = clientAge,
                       Balance = ClientData$InitialBalance[nclient],
                       Mix = ClientData$InitialMix[nclient])
  NodeByNodeResults <- tempdf
  # go from next year till retirement
  for(thisAge in (clientAge + 1):RetirementAge){
    count = 0
    # take each simulation path
    Agedf <- lapply(1:NumSimulationPaths, FUN = function(thisPath){
      Node = (thisAge - clientAge - 1)*NumSimulationPaths + thisPath
      ParentNode = max(Node - NumSimulationPaths, 0)
      Balance = (NodeByNodeResults$Balance[NodeByNodeResults$Node == ParentNode] + Savings(thisAge)) *
        (1 + MultiYearMixSample[Node,NodeByNodeResults$Mix[NodeByNodeResults$Node == ParentNode]])
      #ignore NA
      if(length(Balance) > 0 && !is.na(Balance)) {
      TableInfo = VTableLookup(thisAge, Balance)
      data_frame(Client = nclient, Node = Node, Age = thisAge, Balance = Balance, Mix = TableInfo$Mix)      
      }
      
    }) %>% bind_rows() # end of all simulation paths
    NodeByNodeResults <- rbind(NodeByNodeResults, Agedf)
  }
  NodeByNodeResults
}) %>% bind_rows()
)
ClientResults <- ClientResults %>% mutate(Utility = Utility(Balance))
LoopTime


write.csv(ClientResults, 'ClientResults.csv')
#ClientResults = read.csv('ClientResults.csv', header = TRUE)

#data analysis on VTable
#How many imes we recommend the mixes?
count(VTable, "Mix")

#utility of target
Utility(Target)

#probability of hitting target
c = ClientResults %>% group_by(Client) %>% filter(Balance >= Target) %>% tally()
c$n / (NumSimulationPaths * (RetirementAge - ClientData$Age))

#expected wealth at retirement
retired = ClientResults %>% group_by(Client) %>% filter(Age >= RetirementAge) 
s = aggregate(retired$Balance , by=list(retired$Client), FUN=sum)
s$x/NumSimulationPaths

#sd of wealth at retirement
retired = ClientResults %>% group_by(Client) %>% filter(Age >= RetirementAge) 
aggregate(retired$Balance , by=list(retired$Client), FUN=sd)

#expected utility at retirement
retired = ClientResults %>% group_by(Client) %>% filter(Age >= RetirementAge) 
s = aggregate(Utility(retired$Balance) , by=list(retired$Client), FUN=sum)
s$x/NumSimulationPaths


#find expected allocations of each client
#multiple tables for each client
#age, mix1, mix2, .. mixn
# client, age, mix1..mixn
client_alloc = data.frame(matrix(ncol = 2 + NumMixes, nrow = 0))
colnames(client_alloc) = c("Client", "Age", 1:NumMixes)
#pick each client, for each age, find the mix distribution
#divide by the simulation length
for(i in 1:NumClients){
  startage = ClientData$Age[i]
  for(ag in (startage : (RetirementAge-1))){
    data = ClientResults %>% filter(Client == i) %>% filter(Age == ag) %>% group_by(Mix) %>% filter(!is.na(Mix)) %>% tally()
    row = rep(0, NumMixes)
    row[data$Mix] = data$n/sum(data$n)
    client_alloc[nrow(client_alloc)+1, ] = c(i, ag, paste(format(row*100, digits = 3), "%"))
  }
}
write.csv(client_alloc, "Client_Alloc.csv")

par(mar=c(1,1,1,1))
par(mfrow = c(5,2))
#plot wealth distributions of clients
for(i in 1:NumClients){
  clientname = ClientNames[i]
  endWealth = ClientResults %>% filter(Client == i) %>% filter(Age >= RetirementAge)
  hist(endWealth$Balance, main = "", probability = TRUE, xlab = "Wealth")
  title(clientname, line = -1, adj=1)
  lines(density(endWealth$Balance))
}