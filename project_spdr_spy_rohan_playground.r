#626 project (quantmod)
library(quantmod)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(grid)
library(forecast)
library(tseries)
library(seastests)
#pull data
getSymbols("SPY", src = 'yahoo', 
           from = "1993-01-29", to = "2023-06-07") # Note this can be current date too
#XTS object!
str(SPY)

#print some data
head(SPY)

#create plot function
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

#check for seasonality
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


#we see an obvious increasing trend in the adjusted closing process. We try a trend-only model to SPY
#diagnostic plots have been commented out to minimize the number of plots generated
spy_ts = as.ts(SPY)
trend = time(spy_ts)
plot_trend = function(ts, line){
  plot(spy_ts[,ts], main = paste("Linear trend for", ts))
  abline(line)
}

par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(3,2))
#TODO: axis naming and scales
for(i in 1:ncol(spy_ts)){
  m = lm(spy_ts[,i] ~ trend, na.action = NULL)
  plot_trend(colnames(spy_ts)[i], m)
}

#try quadratic trend for adjusted price
plot_quad_trend = function(ts, curve){
  pred = predict(curve)
  ix = base::sort(trend, index.return=T)
  plot(spy_ts[,ts], main = paste("Quad Linear trend for", ts))
  lines(trend[ix], pred[ix],lwd=2, col="blue")
}
par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(3,2))
#TODO: axis naming and scales
for(i in 1:ncol(spy_ts)){
  m = lm(spy_ts[,i] ~ trend + I(trend ^ 2), na.action = NULL)
  plot_quad_trend(colnames(spy_ts)[i], m)
}

#try splines for adjusted price
spline_trend=function(ts, gam1, gam2, gam3){
  ord <- order(trend)
  plot(spy_ts[,ts], main = paste("Non Linear trend for", ts))
  lines(trend[ord], fitted(gam1)[ord], lwd=3, col="red")
  lines(trend[ord], fitted(gam2)[ord], lwd=3, col="blue")
  lines(trend[ord], fitted(gam3)[ord], lwd=3, col="green")
}

library(mgcv)
par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(3,2))
#TODO: axis naming and scales
for(i in 1:ncol(spy_ts)){
  gam_4 = mgcv::gam(spy_ts[,"SPY.Adjusted"]~s(trend, k=10,bs="cr"))
  gam_25 = mgcv::gam(spy_ts[,"SPY.Adjusted"]~s(trend, k=25,bs="cr"))
  gam_200 = mgcv::gam(spy_ts[,"SPY.Adjusted"]~s(trend, k=200,bs="cr"))
  spline_trend(colnames(spy_ts)[i], gam_4, gam_25, gam_200)
}# still not sufficient, we need need a large number of splines to capture the fluctuations in the trend!

#try boxcox
spy_adjusted_bc = BoxCox(spy_ts[,"SPY.Adjusted"], lambda = BoxCox.lambda(spy_ts[,"SPY.Adjusted"]))
trend_spy_bc = lm(spy_adjusted_bc
                  ~ trend, na.action = NULL)
summary(trend_spy_bc)
#plot(trend_spy_bc) #doesn't look good
plot(spy_adjusted_bc)

par(mar=c(5,4,4,1))
par(mfrow=c(1,1))

#create log returns on adjusted price
#step 1 convert to a data frame (can be tibble too)
#this is daily series, so we have daily returns
SPY$logDiff.Adjusted = diff(log(SPY$SPY.Adjusted))

#plot log s&p 500 series
autoplot(SPY$logDiff.Adjusted, ts.colour = "dodgerblue3", main = "SPY log returns from 1993-01-29 to 2023-06-07", 
         xlab = "Observation Date", ylab = "SPY daily adj. log returns") 

#let's try adf test on logged and original
adf.test(na.omit(SPY$logDiff.Adjusted)) # null hypothesis rejected. STATIONARY!
adf.test(na.omit(SPY$SPY.Adjusted)) # not stationary 

#try some lag plots on the logdiff series
# a month of lags
# no pattern observed
lag.plot(na.omit(SPY$logDiff.Adjusted), lags=30)

#seasonality test
isSeasonal(SPY$logDiff.Adjusted, freq = 365) # returns False!
stl(na.omit(SPY$logDiff.Adjusted)) # doesn't work, so not seasonal

#autocorrelation
acf(SPY$logDiff.Adjusted, na.action = na.pass, main="DiifLog SPY returns 1993-01-29 to 2023-06-07") #almost white noise except perhaps at lag 1?
pacf(SPY$logDiff.Adjusted, na.action = na.pass, main="DiifLog SPY returns 1993-01-29 to 2023-06-07") # some significant values and a tail-off?
auto.arima(SPY$logDiff.Adjusted) #ARIMA(1,0,4) with non-zero mean




