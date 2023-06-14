#626 project (quantmod)
library(quantmod)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(grid)
library(forecast)
library(caret)
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
trend_spy = lm(spy_ts[,"SPY.Adjusted"] ~ trend, na.action = NULL)
summary(trend_spy)
#plot(trend_spy) #trend model doesn't appear to be valid as per diag plots

#try quadratic trend
trend_spy_quad = lm(spy_ts[,"SPY.Adjusted"] ~ trend + I(trend ^ 2), na.action = NULL)
summary(trend_spy_quad)#trend model doesn't appear to be valid
plot(spy_ts[,"SPY.Adjusted"])
abline(trend_spy)
pred = predict(trend_spy_quad)
ix = base::sort(trend, index.return=T)
#add polynomial curve to plot
plot(spy_ts[,"SPY.Adjusted"])
lines(trend[ix], pred[ix],lwd=2, col="blue")

#try splines
library(mgcv)
trend_spy_gam_4 = mgcv::gam(spy_ts[,"SPY.Adjusted"]~s(trend, k=10,bs="cr"))
trend_spy_gam_25 = mgcv::gam(spy_ts[,"SPY.Adjusted"]~s(trend, k=25,bs="cr"))
trend_spy_gam_200 = mgcv::gam(spy_ts[,"SPY.Adjusted"]~s(trend, k=200,bs="cr"))
#trend_spy_gam_2000 = mgcv::gam(spy_ts[,"SPY.Adjusted"]~s(trend, k=2000,bs="cr")) too slow!
gam.check(trend_spy_gam_4)
gam.check(trend_spy_gam_25)
gam.check(trend_spy_gam_200)
#gam.check(trend_spy_gam_2000)
ord <- order(trend)
plot(spy_ts[,"SPY.Adjusted"])
lines(trend[ord], fitted(trend_spy_gam_4)[ord], lwd=3, col="red")
lines(trend[ord], fitted(trend_spy_gam_25)[ord], lwd=3, col="blue")
lines(trend[ord], fitted(trend_spy_gam_200)[ord], lwd=3, col="green") # still not sufficient, we need need a large number of splines to capture the fluctuations in the trend!
#lines(trend[ord], fitted(trend_spy_gam_2000)[ord], lwd=3, col="yellow")

#try boxcox
trend_spy_bc = lm(BoxCox(spy_ts[,"SPY.Adjusted"], lambda = BoxCox.lambda(spy_ts[,"SPY.Adjusted"]))
                  ~ trend, na.action = NULL)
summary(trend_spy_bc)
#plot(trend_spy_bc) #doesn't look good


#create log returns
#step 1 convert to a data frame (can be tibble too)
spy_df = data.frame(date=index(SPY), coredata(SPY))
spy_df$logDiff.Adjusted = diff(log(SPY$SPY.Adjusted))d
#remove first row because first row of the diff is NA
spy_df = spy_df[-1,]

#show some data
head(spy_df)
#plot log s&p 500 series
autoplot(spy_df$logDiff.Adjusted, ts.colour = "dodgerblue3", main = "SPY log returns from 1993-01-29 to 2023-06-07", 
         xlab = "Observation Date", ylab = "SPY daily adj. log returns") 
