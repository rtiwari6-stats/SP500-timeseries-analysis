#626 project (quantmod)
library(quantmod)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(grid)
library(forecast)
library(tseries)
library(seastests)
library(TSstudio)
library(astsa)
#pull data
getSymbols("SPY", src = 'yahoo', 
           from = "1993-01-29", to = "2023-06-07") # Note this can be current date too
#XTS object!
str(SPY)

#print some data
head(SPY)

#Using getSymbols for fred data too to get XTS types
getSymbols.FRED("UNRATE", from = "1993-01-29", to = "2023-06-07", env=globalenv()) #unemployment rate 
#NOTE: Data is till 2023-05-01 only
head(UNRATE)
#plot
autoplot(UNRATE,ts.colour = "dodgerblue3", ylab = "Unemployment Rate", main="Unemployment Rate from 1993-01 to 2023-05")

getSymbols.FRED("CPALTT01USM657N", from = "1993-01-29", to = "2023-06-07", env=globalenv()) #CPI or inflation
CPI = CPALTT01USM657N
names(CPI) = c("CPI") 
#NOTE: Only available till 2023-03-01
head(CPI)
#plot
autoplot(CPI,ts.colour = "dodgerblue3", ylab = "Consumer Price Index", main="Consumer Price Index from 1993-01 to 2023-03")

#create plot function for S&P 500
plot_all_spy_series = function(my_spy, title="SPY data from 1993-01-29 to 2023-06-07", nRow = 2, date_breaks=waiver(), date_labels=waiver(), angle=0){
  #plot s&p 500 series adj. closing price column
  g0 <- autoplot(my_spy$SPY.Adjusted, ts.colour = "dodgerblue3",
                 ylab = "SPY daily adj. closing price") + scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +  theme(axis.text.x = element_text(angle = angle))
  #plot s&P 500 Open, High, Low, and Volume columns
  
  g1 <- autoplot(my_spy$SPY.Open, ts.colour = "dodgerblue3",
                 ylab = "SPY daily opening price") + scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +  theme(axis.text.x = element_text(angle = angle))
  
  g2 <- autoplot(my_spy$SPY.High, ts.colour = "dodgerblue3", 
                 ylab = "SPY daily high price") + scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +  theme(axis.text.x = element_text(angle = angle))
  
  g3 <- autoplot(my_spy$SPY.Low, ts.colour = "dodgerblue3", 
                 ylab = "SPY daily low price") + scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +  theme(axis.text.x = element_text(angle = angle))
  
  g4 <- autoplot(my_spy$SPY.Volume, ts.colour = "dodgerblue3",
                 ylab = "SPY daily share volume") + scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +  theme(axis.text.x = element_text(angle = angle))
  
  grid.arrange(g0, g1, g2, g3, g4, nrow = nRow, top=textGrob(title))
}

#plot all spy columns
plot_all_spy_series(SPY)

#Seasonality checks
#Try with monthly data 
SPY_monthly = to.monthly(SPY) #convert the OHLC series to monthly
#plot all monthly series
plot_all_spy_series(SPY_monthly, title="SPY Monthly data from 1993-01-29 to 2023-06-07", 
                    nRow=ncol(SPY_monthly)-1, date_breaks = '3 months', date_labels = '%b %Y', angle=90)

#next try quarterly data
SPY_quarterly = to.quarterly(SPY) #convert the OHLC series to quarterly
plot_all_spy_series(SPY_quarterly, title="SPY Quarterly data from 1993-01-29 to 2023-06-07", 
                    nRow=ncol(SPY_quarterly)-1, date_breaks = '3 months', date_labels = '%b %Y', angle=90)

#check for seasonality using decomposition
#let us write a decomposed series plotter first
decomp_func <- function(dataset, nRow = 3, title = "Decomposition Plots for SPY") {
  
  open_dc = decompose(spy_monthly_ts[,"SPY.Open"])
  high_dc = decompose(spy_monthly_ts[,"SPY.High"])
  low_dc = decompose(spy_monthly_ts[,"SPY.Low"])
  close_dc = decompose(spy_monthly_ts[,"SPY.Close"])
  volume_dc = decompose(spy_monthly_ts[,"SPY.Volume"])                     
  Adjusted_dc = decompose(spy_monthly_ts[,"SPY.Adjusted"])
  
  #can write another function for this                
  open_dc_xts = merge.xts(as.xts(open_dc$seasonal), as.xts(open_dc$x), as.xts(open_dc$trend), as.xts(open_dc$random))
  names(open_dc_xts) = c("Seasonal", "Data", "Trend", "Random")
  
  high_dc_xts = merge.xts(as.xts(high_dc$seasonal), as.xts(high_dc$x), as.xts(high_dc$trend), as.xts(high_dc$random))
  names(high_dc_xts) = c("Seasonal", "Data", "Trend", "Random")
  
  low_dc_xts = merge.xts(as.xts(low_dc$seasonal), as.xts(low_dc$x), as.xts(low_dc$trend), as.xts(low_dc$random))
  names(low_dc_xts) = c("Seasonal", "Data", "Trend", "Random")
  
  close_dc_xts = merge.xts(as.xts(close_dc$seasonal), as.xts(close_dc$x), as.xts(close_dc$trend), as.xts(close_dc$random))
  names(close_dc_xts) = c("Seasonal", "Data", "Trend", "Random")
  
  volume_dc_xts = merge.xts(as.xts(volume_dc$seasonal), as.xts(volume_dc$x), as.xts(volume_dc$trend), as.xts(volume_dc$random))
  names(volume_dc_xts) = c("Seasonal", "Data", "Trend", "Random")
  
  adjusted_dc_xts = merge.xts(as.xts(Adjusted_dc$seasonal), as.xts(Adjusted_dc$x), as.xts(Adjusted_dc$trend), as.xts(Adjusted_dc$random))
  names(adjusted_dc_xts) = c("Seasonal", "Data", "Trend", "Random")
  
  p1 <- autoplot(open_dc_xts, ylab = "SPY.Open", main="")+ scale_x_date(date_breaks = '3 months', date_labels = "%b %Y") +  theme(axis.text.x = element_text(angle = 90))
  p2 <- autoplot(high_dc_xts, ylab = "SPY.High", main="")+ scale_x_date(date_breaks = '3 months', date_labels = "%b %Y") +  theme(axis.text.x = element_text(angle = 90))
  p3 <- autoplot(low_dc_xts, ylab = "SPY.Low", main="")+ scale_x_date(date_breaks = '3 months', date_labels = "%b %Y") +  theme(axis.text.x = element_text(angle = 90))
  p4 <- autoplot(close_dc_xts, ylab = "SPY.Close", main="")+ scale_x_date(date_breaks = '3 months', date_labels = "%b %Y") +  theme(axis.text.x = element_text(angle = 90))
  p5 <- autoplot(volume_dc_xts, ylab = "SPY.Volume", main="")+ scale_x_date(date_breaks = '3 months', date_labels = "%b %Y") +  theme(axis.text.x = element_text(angle = 90))
  p6 <- autoplot(adjusted_dc_xts, ylab = "SPY.Adjusted", main="")+ scale_x_date(date_breaks = '3 months', date_labels = "%b %Y") +  theme(axis.text.x = element_text(angle = 90))
  
  grid.arrange(p1,p2,p3,p4,p5,p6, nrow = nRow, top=textGrob(title))
  
}

#start and end are hard coded!!
#daily - decompose, stl do not work so not seasonality for any column.

#monthly
spy_monthly_ts = ts(data = coredata(SPY_monthly), start = c(1993,1), end = c(2023,6), frequency = 12)
decomp_func(spy_monthly_ts, title="Decomposition Plots for Monthly SPY")

#Quarterly
spy_quarterly_ts = ts(data = coredata(SPY_quarterly), start = c(1993,1), end = c(2023,6), frequency = 4)
decomp_func(spy_quarterly_ts, title="Decomposition Plots for Quarterly SPY")

#create log returns on adjusted price (monthly)
#note that this is a random walk because we take the difference x(t) - x(t-1) as the first step to compute the returns
# whatever is left is supposed to be random error.
#step 1 convert to a data frame (can be tibble too)
#this is daily series, so we have daily returns
SPY_monthly$logDiff.Adjusted = diff(log(SPY_monthly$SPY.Adjusted))

#plot log s&p 500 series
autoplot(SPY_monthly$logDiff.Adjusted, ts.colour = "dodgerblue3", main = "SPY monthly log returns from 1993-01-29 to 2023-06-07", 
         xlab = "Observation Date", ylab = "SPY daily adj. log returns") 

#let's try adf test on logged and original for SPY
adf.test(na.omit(SPY_monthly$logDiff.Adjusted)) # null hypothesis rejected. STATIONARY! at 0.05
adf.test(na.omit(SPY_monthly$SPY.Adjusted)) # not stationary  at 0.05

#adf tests for CPI and UNRate
adf.test(UNRATE$UNRATE) #stationary at 0.05
adf.test(CPI$CPI) #stationary at 0.05

#seasonality test for SPY monthly
isSeasonal(SPY_monthly$logDiff.Adjusted, freq = 12) # returns False!
isSeasonal(SPY_monthly$SPY.Adjusted, freq = 12) # returns False!

#seasonality test for CPI and Unrate
isSeasonal(UNRATE$UNRATE, freq = 12) #FALSE
isSeasonal(CPI$CPI, freq = 12) #TRUE

#decomposing CPI and UNRATE
CPI_ts = ts(data = coredata(CPI), start = c(1993,1), end = c(2023,5), frequency = 12)
autoplot(decompose(CPI_ts), main="Decomposing CPI monthly series") #clear seasonality

UNRATE_ts = ts(data = coredata(UNRATE), start = c(1993,1), end = c(2023,3), frequency = 12)
autoplot(decompose(UNRATE_ts), main="Decomposing UNRATE monthly series") #clear seasonality

#focussing on UNRATE
#check acf
acf(UNRATE$UNRATE) #doesn't decay much

#1st difference
UNRATE_diff1 = na.omit(diff(UNRATE$UNRATE))
acf(UNRATE_diff1) #WOW looks great!!
UNRATE_diff1_ts = ts(data = coredata(UNRATE_diff1), start = c(1993,1), end = c(2023,3), frequency = 12)
autoplot(UNRATE_diff1_ts, main="1st difference UNRATE monthly series", ylab="diff(UNRATE)")
autoplot(decompose(UNRATE_diff1_ts), main="Decomposing first difference UNRATE monthly series") #clear seasonality

#6th difference (seasonality is 6?)
UNRATE_diff6 = na.omit(diff(UNRATE$UNRATE, lag=6))
acf(UNRATE_diff6) # we have larger acfs
UNRATE_diff6_ts = ts(data = coredata(UNRATE_diff6), start = c(1993,1), end = c(2023,3), frequency = 12)
autoplot(UNRATE_diff6_ts, main="seasonal difference UNRATE monthly series", ylab="diff(UNRATE,lag=6)")
autoplot(decompose(UNRATE_diff6_ts), main="Decomposing 6th difference UNRATE monthly series") #clear seasonality

#1st difference of the seasonal difference
UNRATE_1diff6 = na.omit(diff(UNRATE_diff6$UNRATE))
acf(UNRATE_1diff6) # we have now have better acfs (lag 6 is most correlated)
UNRATE_1diff6_ts = ts(data = coredata(UNRATE_1diff6), start = c(1993,1), end = c(2023,3), frequency = 12)
autoplot(UNRATE_1diff6_ts, main="1st differencee of seasonal difference UNRATE monthly series", ylab="diff(diff(UNRATE,lag=6))")
autoplot(decompose(UNRATE_1diff6_ts), main="Decomposing 1st difference of seasonal difference UNRATE monthly series") #clear seasonality though seems better

#lets just try to difference the seasonal trend
UNRATE_ts = ts(data = coredata(UNRATE), start = c(1993,1), end = c(2023,3), frequency = 12)
UNRATE_seasdiff = UNRATE_ts-decompose(UNRATE_ts)$seasonal
autoplot(decompose(UNRATE_seasdiff), main="Decomposing seasonal difference (from decompose fit) UNRATE monthly series") #looks good
acf(UNRATE_seasdiff) # doesn't look good yet
autoplot(UNRATE_seasdiff, main="seasonal difference(from decompose fit) UNRATE monthly series", ylab="diff(diff(UNRATE,lag=6))")

#first difference of the seasonal trend
UNRATE_seasdiff1 = diff(UNRATE_seasdiff)
acf(UNRATE_seasdiff1)
autoplot(decompose(UNRATE_seasdiff1), 
         main="Decomposing first difference of seasonal difference (from decompose fit) UNRATE monthly series") #looks good
autoplot(UNRATE_seasdiff1, 
         main="first difference of seasonal difference(from decompose fit) UNRATE monthly series", 
         ylab="diff(diff(UNRATE,lag=6))") # looks reasonable but has variance problem

#do the same but this time with log UNRATE to stabilize variance
UNRATE_log_ts = log(UNRATE_ts)
UNRATE_log_seasdiff = UNRATE_log_ts-decompose(UNRATE_log_ts)$seasonal
UNRATE_log_seasdiff1 = diff(UNRATE_log_seasdiff) 
autoplot(decompose(UNRATE_log_seasdiff1), 
         main="Decomposing first difference of seasonal difference (from decompose fit) log UNRATE monthly series") #looks good
autoplot(UNRATE_log_seasdiff1, 
         main="first difference of seasonal difference(from decompose fit) log UNRATE monthly series", 
         ylab="diff(diff(UNRATE,lag=6))") # looks reasonable but has some variance problem
acf(UNRATE_log_seasdiff1)



# #focussing on CPI
# #12th difference (seasonality is 12?)
# CPI_diff12 = na.omit(diff(CPI$CPI, lag=12))
# acf(CPI_diff12) # looks pretty good!
# CPI_diff12_ts = ts(data = coredata(CPI_diff12), start = c(1993,1), end = c(2023,5), frequency = 12)
# autoplot(CPI_diff12_ts, main="seasonal difference CPI monthly series", ylab="diff(UNRATE,lag=12)") #reasonable
# autoplot(decompose(CPI_diff12_ts), main="Decomposing 12th difference CPI monthly series") #some seasonality
# 
# #1st difference of the seasonal difference
# CPI_1diff12 = na.omit(diff(CPI_diff12$CPI))
# acf(CPI_1diff12) # still looks pretty good
# CPI_1diff12_ts = ts(data = coredata(CPI_1diff12), start = c(1993,1), end = c(2023,3), frequency = 12)
# autoplot(CPI_1diff12_ts, main="1st differencee of seasonal difference CPI monthly series", ylab="diff(diff(UNRATE,lag=12))") # more or less stationary
# autoplot(decompose(CPI_1diff12_ts), 
#          main="Decomposing 1st difference of 12th difference UNRATE monthly series") #some seasonality still present
# 
# #lets just try to difference the seasonal trend
# UNRATE_ts = ts(data = coredata(UNRATE), start = c(1993,1), end = c(2023,3), frequency = 12)
# UNRATE_seasdiff = UNRATE_ts-decompose(UNRATE_ts)$seasonal
# autoplot(decompose(UNRATE_seasdiff), main="Decomposing seasonal difference (from decompose fit) UNRATE monthly series") #looks good
# acf(UNRATE_seasdiff) # doesn't look good yet
# autoplot(UNRATE_seasdiff, main="seasonal difference(from decompose fit) UNRATE monthly series", ylab="diff(diff(UNRATE,lag=6))")
# 
# #first difference of the seasonal trend
# UNRATE_seasdiff1 = diff(UNRATE_seasdiff)
# acf(UNRATE_seasdiff1)
# autoplot(decompose(UNRATE_seasdiff1), 
#          main="Decomposing first difference of seasonal difference (from decompose fit) UNRATE monthly series") #looks good
# autoplot(UNRATE_seasdiff1, 
#          main="first difference of seasonal difference(from decompose fit) UNRATE monthly series", 
#          ylab="diff(diff(UNRATE,lag=6))") # looks reasonable
# 
