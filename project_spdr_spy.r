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
library(car)
library(robustbase) 


#pull data
getSymbols("SPY", src = 'yahoo', 
           from = "1993-01-29", to = "2023-06-07") # Note this can be current date too
#XTS object!
str(SPY)

#print some data
head(SPY)
#plot
autoplot(SPY,ts.colour = "dodgerblue3", ylab = "SPY", main="SPY from 1993-01 to 2023-05")

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

#using getSymbols to get VIX data from Y!
#pull data
getSymbols("^VIX", src = 'yahoo', 
           from = "1993-01-29", to = "2023-06-07") # Note this can be current date too
#XTS object!
str(VIX)

#print some data
head(VIX) #from 2014 onwards only

#plot it
autoplot(VIX,ts.colour = "dodgerblue3", ylab = "VIX", main="Volatility Index (VIX) from 1993-01 to 2023-06")



#try BOX COX 




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

#daily SPY - decompose, stl do not work so not seasonality for any column.

#note that this is a random walk because we take the difference x(t) - x(t-1) as the first step to compute the returns
SPY$LogReturns.Adjusted = diff(log(SPY$SPY.Adjusted))

#plot log s&p 500 series
autoplot(SPY$LogReturns.Adjusted, ts.colour = "dodgerblue3", main = "SPY log returns from 1993-01-29 to 2023-06-07", 
         xlab = "Observation Date", ylab = "SPY daily adj. log returns") 

#let's try adf test on logged and original for SPY
adf.test(na.omit(SPY$LogReturns.Adjusted)) # null hypothesis rejected (p-value 0.01). STATIONARY! at 0.05
adf.test(na.omit(SPY$SPY.Adjusted)) # not stationary  at 0.05 (p-value 0.9756)

#adf tests for CPI and UNRate
adf.test(UNRATE$UNRATE) #not stationary at 0.05 (p-value 0.4415)
adf.test(CPI$CPI) #stationary at 0.05

#seasonality test for SPY monthly
isSeasonal(SPY$LogReturns.Adjusted, freq = 12) # returns False!
isSeasonal(SPY$SPY.Adjusted, freq = 12) # returns False!

#seasonality test for CPI and Unrate
isSeasonal(UNRATE$UNRATE, freq = 12) #FALSE
isSeasonal(CPI$CPI, freq = 12) #TRUE

#decomposing CPI and UNRATE
CPI_ts = ts(data = coredata(CPI), start = c(1993,1), end = c(2023,5), frequency = 12)
autoplot(decompose(CPI_ts), main="Decomposing CPI monthly series") 

UNRATE_ts = ts(data = coredata(UNRATE), start = c(1993,1), end = c(2023,3), frequency = 12)
autoplot(decompose(UNRATE_ts), main="Decomposing UNRATE monthly series")

#Note: At this point spy_monthly is stationary, CPI is seasonal but stationary and UNRATE isn't stationary.

m = acf(na.omit(SPY$LogReturns.Adjusted))
print(m)

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

#focus on VIX
VIX = na.omit(VIX)
adf.test(VIX$VIX.Adjusted) #p-value 0.01, stationary!
isSeasonal(VIX$VIX.Adjusted) # TRUE but can't get stl or decompose to work so we don't do anything!
acf(VIX$VIX.Adjusted) #this decays reasonably well

#let's drop NA values
SPY = na.omit(SPY)

#build our dataset
#First expand unrate and cpi
CPI_stationary = na.locf(merge(CPI_stationary, foo=zoo(NA, order.by=seq(start(CPI_stationary), end(CPI_stationary),
                                                             "day",drop=F)))[, 1])
UNRATE_stationary = na.locf(merge(UNRATE_stationary, foo=zoo(NA, order.by=seq(start(UNRATE_stationary), end(UNRATE_stationary),
                                                                           "day",drop=F)))[, 1])
vix_adjusted_log_diff = na.omit(diff(log(VIX$VIX.Adjusted)))
project_data = na.omit(merge.xts(SPY, CPI_stationary, UNRATE_stationary, VIX$VIX.Adjusted, vix_adjusted_log_diff))
head(project_data)
tail(project_data)

#CPI is a growth rate, so a ratio of current-previous value and previous value. 
#UNRATE is a %, so again the same ratio as CPI but multiplied by 100.  
#So, we  could just divide the UNRATE by 100 so we have everything non %s  

project_data$UNRATE = project_data$UNRATE/100 #remove %


#CCF of UNRATE and S&P500 
ccf(as.numeric(project_data$UNRATE), as.numeric(project_data$LogReturns.Adjusted), 
    ylab = "CCF",
    main=" Unemployment Rate vs SPY Log Adjusted Returns")

#Correlation Matrix
panel.cor <- function(x,y,...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r <- round(cor(x,y), 2)
  text(0.5, 0.5, r, cex=1.75)
}
pairs(cbind( CPI=as.numeric(project_data$CPI),SPY_LogReturns = as.numeric(project_data$LogReturns.Adjusted),
             Unemployment_Rate = as.numeric(project_data$UNRATE), Volume= as.numeric(project_data$SPY.Volume),  
             VIX = as.numeric(project_data$VIX.Adjusted)), 
      col="dodgerblue3", lower.panel = panel.cor)


#CCF of CPI and S&P500  
ccf(as.numeric(project_data$CPI),as.numeric(project_data$LogReturns.Adjusted),
    ylab = "CCF",
    main="CPI vs SPY Log Adjusted Returns")


#CCF of VIX and S&P500
ccf(as.numeric(project_data$VIX.Adjusted),as.numeric(project_data$LogReturns.Adjusted),
    ylab = "CCF",
    main="VIX vs SPY Log Adjusted Returns")

# Lag plots 

# fixed corr and labels
SPY_adjusted = project_data$LogReturns.Adjusted
lag1.plot(SPY_adjusted, corr = T, 12, col="dodgerblue3") # Log Returns plotted against its past values 

#added extra underscore to SPY_adjusted_ because this one is numeric
SPY_adjusted_ = as.numeric(project_data$LogReturns.Adjusted)
CPI_val= as.numeric(project_data$CPI)
lag2.plot(CPI_val,SPY_adjusted_, corr = T, 12, col="dodgerblue3") # CPI vs lagged SimpleReturns.Adjusted values

#lag2.plot(SPY_adjusted_, CPI_val, corr = T, 31, col="dodgerblue3") # CPI vs lagged SimpleReturns.Adjusted values


UNRATE_val = as.numeric(project_data$UNRATE)
lag2.plot(UNRATE_val,SPY_adjusted_, corr = T, 12, col="dodgerblue3") # UNRATE vs lagged SimpleReturns.Adjusted values

VIX_val_ = as.numeric(project_data$VIX.Adjusted)
lag2.plot(VIX_val_,SPY_adjusted_, corr = T, 12, col="dodgerblue3") # UNRATE vs lagged VIX values


#fitting lag regression line

object_unrate = ts.intersect( UNRATE_val, spyL1 = lag(SPY_adjusted_,+1) )
summary(fit1 <- lm(spyL1~ UNRATE_val, data=object_unrate, na.action=NULL))

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
dummydataforplot = na.omit(as.ts(project_data[, c("SPY.Open", "SPY.High", "SPY.Low", "SPY.Close", "SPY.Volume", "LogReturns.Adjusted","CPI","UNRATE", "VIX.Adjusted")]))
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
         c("0.5", "0.05", "0.005", "0.0005", "0.00005"),cex=0.7)
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
trend = time(project_data$LogReturns.Adjusted) # time is trend
fit1 = lm(project_data$LogReturns.Adjusted ~ trend + project_data$CPI + project_data$UNRATE, na.action=NULL)
summary(fit1)
broom::glance(fit1)

# Model 2
trend = time(project_data$LogReturns.Adjusted) # time is trend
CPI_squared = project_data$CPI^2
fit2 = lm(project_data$LogReturns.Adjusted ~ trend + project_data$CPI + CPI_squared, na.action=NULL)
summary(fit2)
broom::glance(fit2)


# Model 3
trend = time(project_data$LogReturns.Adjusted) # time is trend
CPI_squared = project_data$CPI^2
fit3 = lm(project_data$LogReturns.Adjusted ~ trend + project_data$CPI + CPI_squared + project_data$UNRATE, na.action=NULL)
summary(fit3)
broom::glance(fit3)

# Model 4

fit4 = lm(project_data$LogReturns.Adjusted ~ project_data$CPI + project_data$UNRATE, na.action=NULL)
summary(fit4)
broom::glance(fit4)

# Model 5 - we're removing the first entry for SimpleReturns.Adjusted and UNRATE to have equal number of observations as CPI
fit5 = lm(project_data$LogReturns.Adjusted[-1] ~ na.omit(lag(project_data$CPI, k = 1)) + project_data$UNRATE[-1], na.action=NULL)
summary(fit5)
broom::glance(fit5)


# Model 6 - we're removing the first two entries for SimpleReturns.Adjusted 
#           and first entry of CPI to have equal number of observations as UNRATE
fit6 = lm(project_data$LogReturns.Adjusted[-(1:2)] ~ na.omit(lag(project_data$CPI, k = 1))[-1] + 
            na.omit(lag(project_data$UNRATE, k = 2)), na.action=NULL)
summary(fit6)
broom::glance(fit6)

# Model 7 - let's add VIX into the model
fit7 = lm(project_data$LogReturns.Adjusted ~ project_data$CPI + 
            project_data$UNRATE + project_data$VIX.Adjusted)
summary(fit7)
broom::glance(fit7)

#Model 8

fit8 = lm(project_data$LogReturns.Adjusted ~ project_data$CPI + 
            project_data$UNRATE + project_data$SPY.Volume + project_data$VIX.Adjusted)
summary(fit8)
broom::glance(fit8)

plot(fit8)


fit9 = lm(project_data$LogReturns.Adjusted ~ log(project_data$VIX.Adjusted))
summary(fit9)


#USING BOX COX, Model 1

bcfit1 = lm(project_data$LogReturns.Adjusted~project_data$VIX.Adjusted)
summary(bcfit1)

bcfit2 = lm(project_data$LogReturns.Adjusted~vix_adj_bc)
summary(bcfit2)

## Box-Cox transformation of model
#fit_1a <- lm(project_data$LogReturns.Adjusted~as.numeric(vix_adj_bc))
#bc_fit_1a <- powerTransform(fit_1a, family = "bcnPower")
#summary(bc_fit_1a)

#LMROB- robust regression

lmrob1 = lmrob(project_data$LogReturns.Adjusted ~project_data$VIX.Adjusted)
summary(lmrob1)
plot(lmrob1,5)

lmrob2 = lmrob(project_data$LogReturns.Adjusted ~log(project_data$VIX.Adjusted))
summary(lmrob2)
plot(lmrob2, 5)

#Regression using CPI, UNRATE, and VIX (Adjusted Log-Diff)

# Model 9 - let's add VIX (Adjusted Log-Diff) into the model
fit9 = lm(project_data$LogReturns.Adjusted ~ project_data$CPI + 
            project_data$UNRATE + project_data$VIX.Adjusted.1)
summary(fit9)
broom::glance(fit9)
#################################################################################################
