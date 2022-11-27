rm(list = ls())
## load the required packages
require(data.table)
require(ggplot2)
library(rmarkdown)  # to generate pdf file
library(tinytex)    # to generate pdf file
library(readxl)     # to read Excel files
library(car)        
library(lmtest)     
library(sandwich)   
library(matlib)
library(tseries)
library(latex2exp)  # to get formulas into plots
library(quantmod)
library(Quandl)
library(PerformanceAnalytics)

## automatic detection of current file location 
setwd("~/Desktop/R/Semester 2/Asset Management")
# setwd('C:/Users/...')

##############################################################################
#
#     Read in the data as data.table and set keys (sorts the data automatically) #
#
##############################################################################
d <- as.data.table(read.csv2("DE_data_long.csv"))
setkey(d, ISIN, mdate)
ff <- as.data.table(read.csv2("DE_FF_Factors.csv"))
setkey(ff, mdate)
alo=  as.data.table(read.csv2("DE_data_annual_long.csv"))
lo = as.data.table(read.csv2("DE_data_long.csv"))
data = merge(d,ff, by="mdate")

#Calculate Excess Return
data$EXRET = data$RET-data$Rf
data$EXRET11 = data$RET11-data$Rf

##############################################
# define our function for sorting portfolios #
##############################################
###define portfolios
# arguments are
# x: a vector of returns (or characteristics)
# P: the number of portfolios we want
# we assign P=2 as a default value, so unless we want a different number of portfolios than 2, we do not need to set a value for P 
PsortPort <- function(x, P = 2) {
  # first, determine the breakpoints of the sorting variable
  b <- c(min(x), median(x), max(x))
  # then, assign a portfolio-number to each stock-observation in a particular month
  p <- cut(x, breaks = b, labels = FALSE, include.lowest = TRUE, right = FALSE)
  # return the portfolio-number and add a "p" in front
  return(paste0("p",p))
}

#For Momentum we divide the beta subportfolios each into 10 momentum portfolios
MsortPort <- function(x, P = 10) {
  # first, determine the breakpoints of the sorting variable
  b <- quantile(x, 0:P/P, na.rm = TRUE)
  # then, assign a portfolio-number to each stock-observation in a particular month
  p <- cut(x, breaks = b, labels = FALSE, include.lowest = TRUE, right = FALSE)
  # return the portfolio-number and add a "p" in front
  return(paste0("p",p))
}

#For SMB we divide the stocks into 2 portfolios
SsortPort <- function(x, P = 2) {
  # first, determine the breakpoints of the sorting variable
  b <- quantile(x, 0:P/P, na.rm = TRUE)
  # then, assign a portfolio-number to each stock-observation in a particular month
  p <- cut(x, breaks = b, labels = FALSE, include.lowest = TRUE, right = FALSE)
  # return the portfolio-number and add a "p" in front
  return(paste0("p",p))
}

##################################################################################3
##########    Preparing the data ###############

# lag betas by 1 months
data[,beta_lag1 := shift(beta, n = 1, type = "lag"), by = ISIN]

# lag 11-month return by 2 months
data[,EXRET11_lag2 := shift(EXRET11, n = 2, type = "lag"), by = ISIN]

# lag size by 1 months
data[,size_lag1 := shift(ME, n = 1, type = "lag"), by = ISIN]


##############    Sorting Beta Portfolio   #########################
# apply our sortPort function to each month (by=mdate) separately
# only use months, where beta_lag1 is not missing (!is.na)
data[!is.na(beta_lag1), betaport := PsortPort(beta_lag1), by = mdate]
data[!is.na(size_lag1), smbport := SsortPort(size_lag1), by = mdate]

#### Create sub-portfolios for beta portfolio 1 & 2
beta_p1 = subset(data, betaport == "p1")
beta_p2 = subset(data, betaport == "p2")

############   Momentum   #########
#    Sorting Beta Portfolio  
beta_p1[!is.na(EXRET11_lag2), momport := MsortPort(EXRET11_lag2), by = mdate]

beta_p2[!is.na(EXRET11_lag2), momport := MsortPort(EXRET11_lag2), by = mdate]

# calculate (equal weighted) portfolio returns for each of the 10 portfolios in each month
low_momret.long <- beta_p1[!is.na(EXRET11_lag2), list(port.return = mean(EXRET)), keyby = list(momport, mdate)]

high_momret.long <- beta_p2[!is.na(EXRET11_lag2), list(port.return = mean(EXRET)), keyby = list(momport, mdate)]

# convert from long to wide format
low_momret.wide <- dcast.data.table(low_momret.long, formula="mdate~momport", value.var="port.return")

high_momret.wide <- dcast.data.table(high_momret.long, formula="mdate~momport", value.var="port.return")


# calculate long-short winner-loser portfolio (excess) return
# leverage low-beta-high-mom with 1.5 and  and high-beta-low-mom with 0.5
beta_mom_x = data.frame(low_momret.wide$mdate, low_momret.wide$p10, high_momret.wide$mdate,high_momret.wide$p1)

beta_mom_x$BWML = (low_momret.wide$p10*1.5)-(high_momret.wide$p1*0.5)

#Rename columns
names(beta_mom_x)[names(beta_mom_x) == 'low_momret.wide.mdate'] <- 'mdate'
names(beta_mom_x)[names(beta_mom_x) == 'low_momret.wide.p10'] <- 'p10'
names(beta_mom_x)[names(beta_mom_x) == 'high_momret.wide.p1'] <- 'p1'

#Delete one Date column
beta_mom_x$high_momret.wide.mdate <- NULL

############   SMB   #########

# calculate (equal weighted) portfolio returns for each of the 10 portfolios in each month
smbret.long <- data[!is.na(size_lag1), list(port.return = mean(EXRET)), keyby = list(smbport, mdate)]

# convert from long to wide format
smbret.wide <- dcast.data.table(smbret.long, formula="mdate~smbport", value.var="port.return")

# calculate long-short winner-loser portfolio (excess) return
smbret.wide[, SMB := p1-p2]

################################
# Volatility Estimation #
################################
#Note on testing:
#Want to generate a test set of data and training set of data
#Will use last 6 months (recommended by Barroso and Santa-Clara, 2015)
################################
# analyze the momentum returns #
################################
#First calculate Rolling 6 months vols 

vol_windows = 6
rolling_vol <- na.omit(rollapply(beta_mom_x$BWML, vol_windows, 
                        function(x) StdDev(x)))

vol_BWML_6 = data.frame(rolling_vol)


vol= data.frame(c(NA,NA,NA,NA,NA, vol_BWML_6$rolling_vol))
colnames(vol) = c("vol_6")
beta_mom_x$vol_6 =vol$vol_6 

##For comparison calculate EWMA
library(quarks)
returns <- beta_mom_x$BWML
date <- beta_mom_x$mdate
cvar <- ewma(x = returns, lambda = 0.94)
beta_mom_x$csig <- sqrt(cvar)

#Delete NA
beta_mom_vol = na.omit(beta_mom_x)

library(data.table)
#lag sigmas for one month
beta_mom_vol$EWMA_lag1= shift(beta_mom_vol$csig, n=1L, type=c("lag"), give.names=FALSE)
beta_mom_vol$roll_vol_lag1= shift(beta_mom_vol$vol_6, n=1L, type=c("lag"), give.names=FALSE)

#Omit NA
beta_mom_vol = na.omit(beta_mom_vol)
#Set target Volatility
target_sigma  = 0.03
#Calculate Leverage
beta_mom_vol$lev_roll6 = target_sigma/beta_mom_vol$roll_vol_lag1
beta_mom_vol$lev_EWMA = target_sigma/beta_mom_vol$EWMA_lag1
#max leverage of 1
beta_mom_vol$lev_roll6[beta_mom_vol$lev_roll6>1] <- 1
beta_mom_vol$lev_EWMA[beta_mom_vol$lev_EWMA>1] <-1


#Calculate new return
beta_mom_vol$roll6 = (beta_mom_vol$lev_roll6 * beta_mom_vol$BWML) 
beta_mom_vol$EWMA = beta_mom_vol$lev_EWMA * beta_mom_vol$BWML

merged_beta_smb = merge(beta_mom_vol,smbret.wide, by="mdate")

merged_beta_smb$myroll = 0 
############## RERUN and and short SMB (long BMS) 
for(i in 1:nrow(merged_beta_smb)) {      
  if (merged_beta_smb$lev_roll6[i] < 1) {
    merged_beta_smb$myroll[i] = merged_beta_smb$roll6[i] - ((1-merged_beta_smb$lev_roll6[i]) * merged_beta_smb$SMB[i])
  } else{
    merged_beta_smb$myroll[i] = merged_beta_smb$roll6[i]
  }
}

merged_beta_smb = na.omit(merged_beta_smb)


################################
# analyze the portfolio returns #
################################

# merge our portfolios with FF factors
portfolios = data.frame(merged_beta_smb$mdate, merged_beta_smb$BWML, merged_beta_smb$EWMA, merged_beta_smb$myroll, merged_beta_smb$SMB)
colnames(portfolios) = c("mdate","BWML", "EWMA", "Portfolio","NEWSMB")
portfolios <- merge(portfolios, ff, by="mdate")

# run a CAPM regression and save it in a variable
# notice: Everything is already initially calculated as Excess Return

reg.result_BWML <- lm(formula="BWML~MktRf",data=portfolios)
reg.result_roll6 <- lm(formula="Portfolio~MktRf",data=portfolios)
reg.result_EWMA <- lm(formula="EWMA~MktRf",data=portfolios)

# summarize the regression result
summary(reg.result_BWML)
summary(reg.result_roll6)
summary(reg.result_EWMA)
# calculate the standard deviation of the residuals (for the information ratio)
sigma.epsilon_BWML <- sd( residuals(reg.result_BWML) )
sigma.epsilon_roll6 <- sd( residuals(reg.result_roll6) )
sigma.epsilon_EWMA <- sd( residuals(reg.result_EWMA) )
# Information Ratio
BWML_IR = coef(reg.result_BWML)[1] / sigma.epsilon_BWML * sqrt(12) 
roll6_IR = coef(reg.result_roll6)[1] / sigma.epsilon_roll6 * sqrt(12) 
EWMA_IR = coef(reg.result_EWMA)[1] / sigma.epsilon_EWMA * sqrt(12)

IR = rbind(BWML_IR,roll6_IR, EWMA_IR )
colnames(IR) = "IR"
print(IR)
# Sharpe Ratio
print( mean(portfolios$BWML) / sd(portfolios$BWML) * sqrt(12) )
print( mean(portfolios$Portfolio) / sd(portfolios$Portfolio) * sqrt(12) )
print( mean(portfolios$EWMA) / sd(portfolios$EWMA) * sqrt(12) )

##########################
# plot cumulated returns #
##########################
##Convert data frame to data.table
setDT(portfolios)

# convert portfolios returns to long format (ggplot requires this)
plotdata <- melt.data.table(portfolios, id.vars="mdate", variable.name="portfolio", value.name="return")

# merge each return with risk-free rate in that month
plotdata <- merge(plotdata,ff[,list(mdate,Rf)],by="mdate")

# calculate 1 + portfolio return (not excess!) 
# so, since all of the factors are long-short portfolios, 
# we need to add the risk-free rate (that's what you earn on your collateral for the short-side)
plotdata[portfolio %in% c("BWML","MktRf","HML","SMB", "Portfolio", "EWMA", "NEWSMB"), cumret := (1 + return + Rf), by = portfolio]

# if we want to plot the 10 past-return-sorted portfolios later (later, we do not do it, yet)
# then we do not need to add the risk-free rate, because these are not excess returns!
# so we filter by saying, whenever the portfolio is NOT in this list of portfolionames, by using "!"
plotdata[!portfolio%in%c("BWML","MktRf","HML","SMB", "Portfolio", "EWMA", "NEWSMB"), cumret := (1 + return), by = portfolio]

# set 1 dollar for first date, i.e., end of August 1991 (your initial investment)
initial.investment <- data.table(mdate = 199108, portfolio = unique(plotdata[, portfolio]), return = NA, Rf = NA, cumret = 1)
plotdata <- rbind(initial.investment, plotdata)

# calculate cumulated return (that's why we needed the 1+ earlier)
# now we get 1*(1+return_1stmonth)*(1+return_2ndmonth)*...
# so the cumulated amount of dollars of that portfolio
plotdata[, cumret := cumprod(cumret), by = portfolio]

# convert mdates to actual dates (ggplot then knows its a date and that 12 is the last month in a year, etc.)
plotdata[, date := as.Date(paste0(substr(mdate,1,4), "-", substr(mdate, 5, 6), "-01"))]

# create a ggplot that plots the cumulated returns (of just WML, Market, HML and SMB) with a log-scale
# ggplot is a really powerful plotting package
# documentation available at: http://ggplot2.tidyverse.org/reference/
library(ggplot2)
plt <- ggplot(plotdata[portfolio%in%c("BWML","MktRf","HML","SMB", "Portfolio", "EWMA", "NEWSMB")], 
              aes(x=date, y=cumret, group=portfolio, colour=portfolio)) +
  geom_line(size=1.2) +
  ylab("Portfolio value (dollars)") +
  xlab("Date") +
  scale_y_log10() +
  annotation_logticks(sides = "lr")
print(plt) 

portfolios$dollar = 0
portfolios$dollar[1] = 1000

for(i in 2:nrow(portfolios)) {      
  portfolios$dollar[i] = portfolios$dollar[i-1]*(1+portfolios$Portfolio[i])
}



