###############
# @Author: Aaron
# @Description: Extracting the word counts for each year into a new data frame and plot it
# @Status: Working, need more work
#
rm(list = ls())

webforum = read.csv("C:/Users/CP9/Documents/FIT3152/Assignment folder/webforum.csv")
attach(webforum)

mo = strftime(webforum$Date, "%m")
yr = strftime(webforum$Date, "%Y")
wc = webforum$WC
dd = data.frame(mo, yr, wc)

dd.agg = aggregate(wc ~ mo + yr, dd, FUN = sum)
dd.agg$date = as.POSIXct(paste(dd.agg$yr, dd.agg$mo, "01", sep = "-"))


# creating the line plot
yr_agg = aggregate(dd.agg$wc ~ dd.agg$yr, dd.agg, FUN = sum)
colnames(yr_agg) = c("Year", "WC")
library(ggplot2)
library(scales)
ggplot(yr_agg, aes(yr_agg$Year, yr_agg$WC, group = 1)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Year", y = "Word Counts", title = "Word Counts vs Year") + 
  scale_y_continuous(labels = comma)

