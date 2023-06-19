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

#let's just use decompose
UNRATE_ts = ts(data = coredata(UNRATE), start = c(1993,1), end = c(2023,5), frequency = 12) #copying this over
UNRATE_decomp = decompose(UNRATE_ts)
UNRATE_stationary = na.omit(UNRATE_ts - UNRATE_decomp$trend) #detrend
acf(UNRATE_stationary) #acf shows some larger values and doesn't decay quickly so we take 1st diff
UNRATE_stationary = diff(UNRATE_stationary, lag=12) # take a seasonal difference
acf(UNRATE_stationary) #looks better
autoplot(UNRATE_stationary, 
         main="first difference of seasonal & trend difference(from decompose fit) UNRATE monthly series", 
         ylab="diff(diff(UNRATE_ts - UNRATE_decomp$seasonal - UNRATE_decomp$trend)") # looks good to me!
adf.test(UNRATE_stationary) # stationary
isSeasonal(UNRATE_stationary) #UNRATE_stationary is the stationary UNRATE

#Focussing on CPI
CPI_ts = ts(data = coredata(CPI), start = c(1993,1), end = c(2023,3), frequency = 12)
CPI_decomp = decompose(CPI_ts)
CPI_stationary = na.omit(CPI_ts - CPI_decomp$seasonal - CPI_decomp$trend) #detrend, deseasonalized
acf(CPI_stationary) #acf shows some larger values and doesn't decay quickly so we can take 1st diff
#First diff doesn't change the result much except makes the ACF look better
#CPI_stationary = diff(CPI_stationary)
#acf(CPI_stationary) #looks better
autoplot(CPI_stationary, 
         main="seasonal & trend difference (from decompose fit) CPI monthly series", 
         ylab="CPI_ts - CPI_decomp$seasonal - CPI_decomp$trend") # looks good to me!
isSeasonal(CPI_stationary) # returns False!
adf.test(CPI_stationary) #CPI_stationary is the stationary UNRATE

#let's drop SPY.Adjusted column because we won't use it.
SPY_monthly = na.omit(SPY_monthly[, c("SPY.Open", "SPY.High", "SPY.Low", "SPY.Close", "SPY.Volume", "logDiff.Adjusted")])

#build our dataset
project_data = na.omit(merge.xts(SPY_monthly, CPI_stationary, UNRATE_stationary))
head(project_data)
tail(project_data)

#CCF of S&P500 and UNRATE
ccf(as.numeric(project_data$logDiff.Adjusted), as.numeric(project_data$UNRATE_stationary), 
    main="SPY logdiff Adjusted Closing vs Unemployment Rate")

#CCF of S&P500 and CPI
ccf(as.numeric(project_data$logDiff.Adjusted), as.numeric(project_data$CPI_stationary), 
    main="SPY logdiff Adjusted Closing vs CPI")

#Correlation Matrix
panel.cor <- function(x,y,...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r <- round(cor(x,y), 2)
  text(0.5, 0.5, r, cex=1.75)
}
pairs(cbind(SPY_logdiff_Adjusted_Closing = as.numeric(project_data$logDiff.Adjusted), CPI=as.numeric(project_data$CPI_stationary),
            Unemployment_Rate = as.numeric(project_data$UNRATE_stationary), Volume= as.numeric(project_data$SPY.Volume)),
      col="dodgerblue3", lower.panel = panel.cor)
