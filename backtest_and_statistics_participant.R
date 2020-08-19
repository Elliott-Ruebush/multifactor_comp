# Backtesting

## Install and call required packages

library(data.table)
library(zoo)
library(PerformanceAnalytics)

# set working directory
setwd("C:/Users/erueb/dev/SIF/multifactor_comp")

# create vector of file paths
file_name=paste0("output/",paste(2008:2012), ".csv")


# import csv files
j = 2008
for (i in c(1:length(file_name))){
  
  data = setDT(read.csv(file_name[i]))
  data$year <- rep(j,nrow(data))
  j=j+1
  
  if (i == 1) holdings = data 
  else holdings = rbind(holdings,data)
}

# import full sample of monthly returns of stocks

returns=setDT(read.csv('myret_oos1.csv'))


long = holdings[LS == 'L']
short = holdings[LS == 'S']

# compute long and short portfolio returns
for (i in c(2008:2012)) {
  longRet = returns[permno %in% long[year == i]$permno  & year == i+1 ]
  longRet = longRet[, .(longRet = mean(RETU, na.rm = TRUE)), by = .(year,month)]
  
  shortRet = returns[permno %in% short[year == i]$permno & year == i+1]
  shortRet = shortRet[, .(shortRet = mean(RETU, na.rm = TRUE)), by = .(year,month)]
  
  tmp = merge(longRet,shortRet, by = c('year','month'))
  
  if (i == 2008){
    output = tmp
  } else{
    output = rbind(output,tmp)
  }
  
}



# compute combined LS returns
output[, LSret := longRet - shortRet]
LSport = output

# draw performance graph
output$yrmo <-  as.yearmon(paste(output$year, output$month), "%Y %m")
charts.PerformanceSummary(output[,c('yrmo','LSret')])

# charts.PerformanceSummary(output[,c('yrmo','LSret',"longRet","shortRet")])

###################################################################################;
## performance assessment

FFdata = setDT(read.csv("myff.csv"))

FFdata[, date := as.Date(as.character(dateff), format="%Y%m%d")]
FFdata[, year := year(date)]
FFdata[, month := month(date)]


LSport = merge(LSport,FFdata, by = c('year','month'))[order(year,month)]
LSport[, y := LSret]


# simple mean
apply(LSport[,.(longRet,shortRet,LSret)], 2, mean)


# geometric mean
LSport[,lapply(.(longRet,shortRet,LSret), function(x) prod(1 + x)^(1/.N)-1)]


# Sharpe ratio
(sr = LSport[, mean(LSret) / sd(LSret)])
# cat("Sharpe ratio is: ", sr)

# annualized SR
sr * sqrt(12)


# FF 3 factor
FF3 = lm(LSret ~ mktrf + smb + hml, LSport)
summary(FF3)

# Carhart 4 factor
C4 = lm(LSret ~ mktrf + smb + hml + umd, LSport)
summary(C4)


# information ratio
(ir = coef(C4)[1] / sd(C4$residuals))

# annualized IR
ir * sqrt(12)

# 10 industry portfolios
I10 = lm(LSret ~ NoDur + Durbl + Manuf + Enrgy + HiTec + Telcm + Shops + Hlth + Utils + Other, LSport)
summary(I10)

# 10 industry portfolios and Carhart
C4I10 = lm(LSret ~ mktrf + smb + hml + umd + NoDur + Durbl + Manuf + Enrgy + HiTec + Telcm + Shops + Hlth + Utils + Other, LSport)
print(summary(C4I10))
               
cat("Sharpe ratio is: ", sr * sqrt(12))