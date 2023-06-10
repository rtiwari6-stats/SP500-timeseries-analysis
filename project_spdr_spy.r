#626 project (quantmod)
library(quantmod)
library(ggplot2)
library(ggfortify)
library(gridExtra)

#pull data
getSymbols("SPY", src = 'yahoo', 
           from = "1993-01-29", to = "2023-06-07") # Note this can be current date too
#XTS object!
str(SPY)

#print some data
head(SPY)

#plot s&p 500 series
autoplot(SPY$SPY.Adjusted, ts.colour = "dodgerblue3", main = "SPY data from 1993-01-29 to 2023-06-07",
         xlab = "Observation Date", ylab = "SPY daily adj. closing price") 


#plot s&P 500 Open, High, Low, and Volume columns

g1 <- autoplot(SPY$SPY.Open, ts.colour = "dodgerblue3", main = "SPY data from 1993-01-29 to 2023-06-07",
         xlab = "Observation Date", ylab = "SPY daily opening price")

g2 <- autoplot(SPY$SPY.High, ts.colour = "dodgerblue3", main = "SPY data from 1993-01-29 to 2023-06-07",
         xlab = "Observation Date", ylab = "SPY daily high price")

g3 <- autoplot(SPY$SPY.Low, ts.colour = "dodgerblue3", main = "SPY data from 1993-01-29 to 2023-06-07",
         xlab = "Observation Date", ylab = "SPY daily low price")

g4 <- autoplot(SPY$SPY.Volume, ts.colour = "dodgerblue3", main = "SPY data from 1993-01-29 to 2023-06-07",
         xlab = "Observation Date", ylab = "SPY daily share volume")

grid.arrange(g1, g2, g3, g4, nrow = 2)

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
