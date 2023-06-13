#626 project (quantmod)
library(quantmod)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(grid)

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
plot_all_spy_series(SPY_monthly, title="SPY Monthly data from 1993-01-29 to 2023-06-07", nRow=ncol(SPY_monthly)-1, date_breaks = '3 months', date_labels = '%b %Y', angle=90)

#next try quarterly data
SPY_quarterly = to.quarterly(SPY) #convert the OHLC series to quarterly
plot_all_spy_series(SPY_quarterly, title="SPY Quarterly data from 1993-01-29 to 2023-06-07", nRow=ncol(SPY_quarterly)-1, date_breaks = '3 months', date_labels = '%b %Y', angle=90)

#Below is the decomposition plot function. I didn't include SPY.Volume because the decompose() function 
#was returning an error for this. I think it's because decompose() only works with seasonal data
#and SPY.volume exhibits no seasonality.

decomp_func <- function(dataset, nRow = 2, title = "Decomposition Plots") {
            
          
            p1 <- autoplot(decompose(as.ts(dataset$SPY.Open)), ylab = "SPY.Open")
            p2 <- autoplot(decompose(as.ts(dataset$SPY.Adjusted)), ylab = "SPY.Adjusted")
            p3 <- autoplot(decompose(as.ts(dataset$SPY.Low)), ylab = "SPY.Low")
            p4 <- autoplot(decompose(as.ts(dataset$SPY.High)), ylab = "SPY.High")
           # p5 <- autoplot(decompose(as.ts(dataset$SPY.Voume)))
            
            grid.arrange(p1,p2,p3,p4, nrow = nRow, top=textGrob(title))
         
            
}

# Decomposition plots for monthly data
decomp_func(SPY_monthly)
# Decomposition plots for quarterly data
decomp_func(SPY_quarterly)



#create log returns
#step 1 convert to a data frame (can be tibble too)
spy_df = data.frame(date=index(SPY), coredata(SPY))
spy_df$logDiff.Adjusted = diff(log(SPY$SPY.Adjusted))
#remove first row because first row of the diff is NA
spy_df = spy_df[-1,]

#show some data
head(spy_df)
#plot log s&p 500 series
autoplot(spy_df$logDiff.Adjusted, ts.colour = "dodgerblue3", main = "SPY log returns from 1993-01-29 to 2023-06-07", 
         xlab = "Observation Date", ylab = "SPY daily adj. log returns") 
