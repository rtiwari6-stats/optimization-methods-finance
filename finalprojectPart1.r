setwd("~/UW/CFRM-507/2022/finalproject")

# salary 
salary = function(age){
  return ((95-0.02*((50-age)^2)*1))
} 

savings = function(age){
  return (salary(age) * 0.15)
}

#target account balance
tab = (salary(66)*0.6)/0.035
tab

#annual savings for each client
clientData = data.frame(TomorrowAge = c(50,51,52,53,56,56,56,58,61,64), 
                        CurrentAccountBalance = c(700,600,600,250,1000,800,500,1500,1000,1600))
rownames(clientData) = c("Amy Abrams", "Bob Brown", "Carla Clausen", "Darrin Dorne", "Eric Evans",
                         "Francine Farnsworth", "Giovanni Granville", "Heloise Hart", "Issac Iverson",
                         "Jennifer Jones")
eqs = list()
for(i in 1 : nrow(clientData)){
  start = clientData[i,1]
  ages = seq(from=start, to=66, by=1)
  sav = savings(ages)
  df  = data.frame(ages = ages, savings=sav)
  write.csv(df, paste(rownames(clientData)[i], ".csv", sep=""), row.names = FALSE)
  #generate equations for constant rate of return
  cab = clientData[i,2]
  s = paste(cab, "*(1+r)^(67-", start, ")", sep="")
  for(j in ages){
    s = paste(s, "+" , sav[j- start + 1], "*(1+r)^(67-", j, ")", sep="")
  }
  eqs = append(eqs, paste(s,  "= 1540.8", sep=""))
}

#calculate constant rate of return if retirement is delayed by 1 year
eqs = list()
for(i in 1 : nrow(clientData)){
  start = clientData[i,1]
  ages = seq(from=start, to=67, by=1)
  sav = savings(ages)
  #generate equations for constant rate of return
  cab = clientData[i,2]
  s = paste(cab, "*(1+r)^(68-", start, ")", sep="")
  for(j in ages){
    s = paste(s, "+" , sav[j- start + 1], "*(1+r)^(68-", j, ")", sep="")
  }
  eqs = append(eqs, paste(s,  "= 1529.486", sep=""))
}