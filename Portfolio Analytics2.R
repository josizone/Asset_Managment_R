library(PerformanceAnalytics)
library(quantmod)
library(Quandl)
library(lubridate)
library("xts")
#Transformation from data drame to time series
porto = portfolios
porto$mdate <- paste0(porto$mdate, "01")
porto$mdate = as.Date(porto$mdate, format ="%Y%m%d")
data_ts <- xts(porto[,2:10], porto$mdate)
#display cumulative returns
chart.CumReturns(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], geometric = T, main = "Cumulative Performance", legend.loc="topleft", ylab = "cumperf")
chart.Bar(data_ts[,c('Portfolio','BWML','SMB', 'MktRf')], main = "Periodic Performance",  legend.loc="bottomleft", ylab = "perf")
chart.Drawdown(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], main = "Drawdown", legend.loc="bottomleft", ylab = "drawdown")
chart.RollingPerformance(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], width = 12, main = "Rolling Performance", legend.loc="topleft", ylab = "YoY")
chart.RollingCorrelation(data_ts[,c('Portfolio','BWML','NEWSMB'), drop = FALSE],  data_ts[,6, drop = FALSE], width = 12, main = "Rolling Correlation", legend.loc="bottomleft")

#Correlation
table.Correlation(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')],data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')])


#Descriptive
table.Stats(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')])

#Numerical display
table.AnnualizedReturns(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], scale = 12)

#maxDrawdown
maxDrawdown(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], weights = NULL, geometric = TRUE, invert = TRUE)

# Value at Risk
#single
VaR_Hist = VaR(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], p=0.95, weights = NULL, portfolio_method = "single", method = "historical")
VaR_Gaus = VaR(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], p=0.95, weights = NULL, portfolio_method = "single", method = "gaussian")
VaR_Mod = VaR(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], p=0.95, weights = NULL, portfolio_method = "single", method = "modified")

VaR_All = rbind(VaR_Hist, VaR_Gaus, VaR_Mod)
rownames(VaR_All) = c("Hist", "Gaus", "Mod")
print(VaR_All)
#Expected Shortfall
ES_Hist = CVaR(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], p=0.95,  method = "historical")
ES_Gaus = CVaR(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], p=0.95,  method = "gaussian")
ES_Mod = CVaR(data_ts[,c('Portfolio','BWML','MktRf','NEWSMB')], p=0.95,  method = "modified")

ES_All = rbind(ES_Hist, ES_Gaus, ES_Mod)
rownames(ES_All) = c("Hist", "Gaus", "Mod")
print(ES_All)

#MM2
M2_roll6 = Modigliani(data_ts$Portfolio, data_ts$MktRf, Rf = data_ts$Rf)
M2_EWMA= Modigliani(data_ts$EWMA, data_ts$MktRf, Rf = data_ts$Rf)
M2_WML= Modigliani(data_ts$BWML, data_ts$MktRf, Rf = data_ts$Rf)


M2 = rbind(M2_WML,M2_roll6, M2_EWMA)
rownames(M2) = c("BWML","roll6",  "EWMA")
colnames(M2) = "M2"
print(M2)



