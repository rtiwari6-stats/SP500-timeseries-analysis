#626 project (quantmod)
library(quantmod)
library(ggplot2)
library(ggfortify)


#pull data
getSymbols("SPY", src = 'yahoo', 
           from = "1993-01-29", to = "2023-06-07") # Note this can be current date too
#XTS object!
str(SPY)

#print some data
head(SPY)

#plot s&p 500 series
autoplot(SPY$SPY.Adjusted, ts.colour = "dodgerblue3", main = "SPY data from 1993-01-29 to 2023-06-07") + 
  labs(x = "Observation Date", y = "SPY daily adj. closing price") 

#create log returns
#step 1 convert to a data frame (can be tibble too)

spy_df = data.frame(date=index(SPY), coredata(SPY))
spy_df$logDiff.Adjusted = diff(log(SPY$SPY.Adjusted))
#remove first row because first row of the diff is NA
spy_df = spy_df[-1,]

#show some data
head(spy_df)
#plot s&p 500 series
ggplot(data = spy_df, mapping = aes(x = date, y = logDiff.Adjusted)) +
  geom_line() +
  labs(x = "Observation Date", y = "SPY daily log returns") + 
  ggtitle("SPY log returns from 1993-01-29 to 2023-06-07")
