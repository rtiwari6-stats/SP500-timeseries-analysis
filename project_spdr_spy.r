#626 project (quantmod)

#load data
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

#note that this is a random walk because we take the difference x(t) - x(t-1) as the first step to compute the returns
SPY_monthly$SimpleReturns.Adjusted = diff(SPY_monthly$SPY.Adjusted)

#plot log s&p 500 series
autoplot(SPY_monthly$SimpleReturns.Adjusted, ts.colour = "dodgerblue3", main = "SPY monthly simple returns from 1993-01-29 to 2023-06-07", 
         xlab = "Observation Date", ylab = "SPY daily adj. simple returns") 

#let's try adf test on logged and original for SPY
adf.test(na.omit(SPY_monthly$SimpleReturns.Adjusted)) # null hypothesis rejected. STATIONARY! at 0.05
adf.test(na.omit(SPY_monthly$SPY.Adjusted)) # not stationary  at 0.05 (p-value 0.9785)

#adf tests for CPI and UNRate
adf.test(UNRATE$UNRATE) #not stationary at 0.05 (p-value 0.4415)
adf.test(CPI$CPI) #stationary at 0.05

#seasonality test for SPY monthly
isSeasonal(SPY_monthly$SimpleReturns.Adjusted, freq = 12) # returns False!
isSeasonal(SPY_monthly$SPY.Adjusted, freq = 12) # returns False!

#seasonality test for CPI and Unrate
isSeasonal(UNRATE$UNRATE, freq = 12) #FALSE
isSeasonal(CPI$CPI, freq = 12) #TRUE

#decomposing CPI and UNRATE
CPI_ts = ts(data = coredata(CPI), start = c(1993,1), end = c(2023,5), frequency = 12)
autoplot(decompose(CPI_ts), main="Decomposing CPI monthly series") 

UNRATE_ts = ts(data = coredata(UNRATE), start = c(1993,1), end = c(2023,3), frequency = 12)
autoplot(decompose(UNRATE_ts), main="Decomposing UNRATE monthly series")

#Note: At this point spy_monthly is stationary, CPI is seasonal but stationary and UNRATE isn't stationary.

#focusing on UNRATE
#check acf
acf(UNRATE$UNRATE) #doesn't decay and not stationary - so we try differencing

#let's just use decompose
UNRATE_ts = ts(data = coredata(UNRATE), start = c(1993,1), end = c(2023,5), frequency = 12) #copying this over
UNRATE_decomp = decompose(UNRATE_ts)
UNRATE_stationary = na.omit(diff(UNRATE$UNRATE)) #detrend
acf(UNRATE_stationary) #acf seems almost like white noise
autoplot(UNRATE_stationary, 
         main="first difference of UNRATE monthly series", 
         ylab="UNRATE") # looks good to me!
adf.test(UNRATE_stationary) # stationary
isSeasonal(UNRATE_stationary) #stationary and non seasonal

#Focusing on CPI
CPI_ts = ts(data = coredata(CPI), start = c(1993,1), end = c(2023,3), frequency = 12)
CPI_decomp = decompose(CPI_ts)
CPI_stationary = na.omit(diff(CPI$CPI, lag=12)) # take a seasonal difference
acf(CPI_stationary) #acf shows some larger values and doesn't decay quickly but overall not too bad
autoplot(CPI_stationary, 
         main="seasonally differenced CPI monthly series", 
         ylab="CPI") # looks good to me!
isSeasonal(CPI_stationary) # returns False!
adf.test(CPI_stationary) #stationary and non seasonal

#let's drop SPY.Adjusted column because we won't use it.
SPY_monthly = na.omit(SPY_monthly[, c("SPY.Open", "SPY.High", "SPY.Low", "SPY.Close", "SPY.Volume", "SimpleReturns.Adjusted")])

#build our dataset
project_data = na.omit(merge.xts(SPY_monthly, CPI_stationary, UNRATE_stationary))
head(project_data)
tail(project_data)

#CPI is a growth rate, so a ratio of current-previous value and previous value. 
#UNRATE is a %, so again the same ratio as CPI but multiplied by 100.  
#So, we  could just divide the UNRATE by 100 so we have everything non %s  

project_data$UNRATE = project_data$UNRATE/100 #remove %


#CCF of S&P500 and UNRATE
ccf(as.numeric(project_data$SimpleReturns.Adjusted), as.numeric(project_data$UNRATE), 
    ylab = "CCF",
    main="SPY Simple Adjusted Returns vs Unemployment Rate")

#CCF of S&P500 and CPI
ccf(as.numeric(project_data$SimpleReturns.Adjusted), as.numeric(project_data$CPI),
    ylab = "CCF",
    main="SPY Simple Adjusted Returns vs CPI")

#Correlation Matrix
panel.cor <- function(x,y,...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r <- round(cor(x,y), 2)
  text(0.5, 0.5, r, cex=1.75)
}
pairs(cbind(SPY_SimpleReturns = as.numeric(project_data$SimpleReturns.Adjusted), CPI=as.numeric(project_data$CPI),
            Unemployment_Rate = as.numeric(project_data$UNRATE), Volume= as.numeric(project_data$SPY.Volume)),
      col="dodgerblue3", lower.panel = panel.cor)


# Lag plots 

# fixed corr and labels
SPY_adjusted = project_data$SimpleReturns.Adjusted
lag1.plot(SPY_adjusted, corr = T, 12, col="dodgerblue3") # SimpleReturns.Adjusted plotted against its past values 

#added extra underscore to SPY_adjusted_ because this one is numeric
SPY_adjusted_ = as.numeric(project_data$SimpleReturns.Adjusted)
CPI_val= as.numeric(project_data$CPI)
lag2.plot(SPY_adjusted_,CPI_val, corr = T, 8, col="dodgerblue3") # CPI vs lagged SimpleReturns.Adjusted values

UNRATE_val = as.numeric(project_data$UNRATE)
lag2.plot(SPY_adjusted_,UNRATE_val, corr = T, 8, col="dodgerblue3") # UNRATE vs lagged SimpleReturns.Adjusted values


#Regressions - Linear,Quad,Cubic Trends in SPY Data
spy_ts = na.omit(as.ts(SPY[, c("SPY.Open", "SPY.High", "SPY.Low", "SPY.Close", "SPY.Volume", "SPY.Adjusted")]))
trend = time(spy_ts)

#Linear,Quadratic,Cubic Trend
plot_linear_quad_cubic_trend = function(ts, curve1,curve2,curve3){
  pred1 = predict(curve1)
  pred2 = predict(curve2)
  pred3 = predict(curve3)
  ix = base::sort(trend, index.return=T)  
  
  old.par <- par(no.readonly = TRUE,cex.lab=0.7, cex.axis=0.9)
  par(mar = c(2.5,2,2.5, 2))
  
  plot(spy_ts[,ts], main = paste("Trends for", ts),col="grey")
  lines(trend[ix], pred1[ix],lwd=2, col="red")
  lines(trend[ix], pred2[ix],lwd=2, col="blue")
  lines(trend[ix], pred3[ix],lwd=2, col="green")
  legend("topleft", legend=c('Original Data','Linear','Quadratic','Cubic'), cex=.6,
         text.col=1, bg=gray(1,.65), adj=.25,lty = c(1, 1,1),
         col = c("grey","red","blue","green"))
  on.exit(par(old.par))
}

par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(2,3), oma = c(0,0,2,0))


for(i in 1:ncol(spy_ts)){
  linear = lm(spy_ts[,i] ~ trend, na.action = NULL)
  quadratic = lm(spy_ts[,i] ~ trend + I(trend ^ 2), na.action = NULL)
  cubic = lm(spy_ts[,i] ~ trend + I(trend ^ 2) + I(trend ^ 3), na.action = NULL)
  title("Regression for SPY Data - Linear, Quadratic, Cubic", line = 0, outer = TRUE)
  plot_linear_quad_cubic_trend(colnames(spy_ts)[i], linear,quadratic,cubic)
}

#lowess smoother with different spans
dummydataforplot = na.omit(as.ts(project_data[, c("SPY.Open", "SPY.High", "SPY.Low", "SPY.Close", "SPY.Volume", "SimpleReturns.Adjusted","CPI","UNRATE")]))
plot_lowess_smoother = function(ts){
  m1 = lowess(dummydataforplot[, ts], f=0.5)
  m2 = lowess(dummydataforplot[, ts], f=0.05)
  m3 = lowess(dummydataforplot[, ts], f=0.005)
  m4 = lowess(dummydataforplot[, ts], f=0.0005)
  m5 = lowess(dummydataforplot[, ts], f=0.00005)
  old.par <- par(no.readonly = TRUE,cex.lab=0.7, cex.axis=0.9)
  par(mar = c(2.5,2,2.5, 2))
  
  plot(dummydataforplot[,ts], main = paste(ts))
  lines(m1, lwd=2, col="red")
  lines(m2, lwd=2, col="blue")
  lines(m3, lwd=2, col="green")
  lines(m3, lwd=2, col="lightblue")
  lines(m3, lwd=2, col="purple")
  legend("topleft",                                              
         col = c("red", "blue", "green", 'lightblue', "purple"),
         lty = 1,
         lwd = 2,
         c("0.5", "0.05", "0.005", "0.0005", "0.00005"))
  on.exit(par(old.par))
}
par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(3,3), oma = c(0,0,2,0))

#TODO: Please fix naming and scales
for(i in 1:ncol(dummydataforplot)){
  title("Lowess smoothing with different spans for: ", line = 0, outer = TRUE)
  plot_lowess_smoother(colnames(dummydataforplot)[i])
}

# Work in progress 
###############################################################################################333333
#  Fittting regression models 


cor(project_data$CPI, (project_data$CPI)^2) # not collinear 
cor(project_data$UNRATE, (project_data$UNRATE)^2) # collinear 

# Model 1 - glance() allows us to see AIC and BIC values 
trend = time(project_data$SimpleReturns.Adjusted) # time is trend
fit1 = lm(project_data$SimpleReturns.Adjusted ~ trend + project_data$CPI + project_data$UNRATE, na.action=NULL)
summary(fit1)
broom::glance(fit1)

# Model 2
trend = time(project_data$SimpleReturns.Adjusted) # time is trend
CPI_squared = project_data$CPI^2
fit2 = lm(project_data$SimpleReturns.Adjusted ~ trend + project_data$CPI + CPI_squared, na.action=NULL)
summary(fit2)
broom::glance(fit2)


# Model 3
trend = time(project_data$SimpleReturns.Adjusted) # time is trend
CPI_squared = project_data$CPI^2
fit3 = lm(project_data$SimpleReturns.Adjusted ~ trend + project_data$CPI + CPI_squared + project_data$UNRATE, na.action=NULL)
summary(fit3)
broom::glance(fit3)

#################################################################################################
