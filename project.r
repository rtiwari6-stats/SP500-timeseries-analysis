#626 project
library(fredr)
library(dplyr)
library(ggplot2)

#set api key for current R session (get your own after creating a FRED account!)
#ref: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key("6cc5853e4ab7ecda22685767624a167e")

#get s&p 500 data
ts_sp500 = fredr(
  series_id = "SP500",
  observation_start = as.Date("2013-06-10"),
  observation_end = as.Date("2023-06-06")
)
#show some data
head(ts_sp500)

#plot s&p 500 series
ggplot(data = ts_sp500, mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "s&p500 daily closing price", color = "Series") + ggtitle("S&P 500 data from 2013-06-10 to 2023-06-06")


#create S&P 500 log returns
#Ref: Textbook example 1.3
ts_sp500_difflog = tibble(date = ts_sp500$date[2:length(ts_sp500$date)], series_id = paste(ts_sp500$series_id[2:length(ts_sp500$series_id)], 'logreturns'),
                          value = diff(log(ts_sp500$value)))
#show some data
head(ts_sp500_difflog)
#plot s&p 500 series
ggplot(data = ts_sp500_difflog, mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "s&p500 daily log returns", color = "Series") + ggtitle("S&P 500 log returns from 2013-06-10 to 2023-06-06")
