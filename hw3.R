#Name: Yujue Wang
#Assignment 3 - Building Models in R

# Reset the workspace
rm(list=ls())
# Set the working directory
setwd("D:\\RPI\\2019Spring\\Supply Chain Analytics\\HW3")
# Import the data
customerorders = read.table("Lokad_Orders.tsv", sep="\t", head=T)
attach(customerorders)
# Formatting the dataset
PurchaseDate = as.Date(Date, "%d-%m-%y")
customerorders = cbind(customerorders, PurchaseDate)

#1 Create a unit cost data object using the NetAmount and Quantity
library(ggplot2)
unit_cost = NetAmount / Quantity
customerorders = cbind(customerorders, unit_cost)
#  Histogram with a larger number of blocks
qplot(unit_cost, geom = "histogram", main = "Unit Cost", binwidth = 20)

#2 Create a dataset of all customer orders with unit costs below $100
newdataset = customerorders[which(unit_cost < 100),]
head(newdataset)

#3 Re-run the linear model of Quantity vs NetAmount on the subset you create in #2.  
#  Evaluate the fit of the model using a pertinent metric.
newlm_unitcost = lm(newdataset$NetAmount ~ newdataset$Quantity)
objects(newlm_unitcost)
summary(newlm_unitcost)


#4 Create the 4 diagnostic graphs and comment on the appropriateness of the model
par(mfrow = c(2,2))
plot(newlm_unitcost)
plot(newdataset$NetAmount ~ newdataset$Quantity)
abline(newlm_unitcost)
objects(newlm_unitcost)
# 1st Graph: 
# The residuals are assigned above 0 rather than randomly assigned around 0, 
# so it is not equial error variance.
# 2nd Graph:
# The line is flat, so it is not normally distributed
# 3rd Graph:
# The graph shows that the homoscedasticity is not met.
# 4th Graph:
# Shows that there are influential cases.




#5 Plot a 5-period future NetAmount monthly forecast
#  Report the 95% prediction interval (low and high) for the last month of the forecast
#  Using the summary of the model you create. 
mo = strftime(PurchaseDate, "%m")
yr = strftime(PurchaseDate, "%y")
date = paste(mo, "01", yr, sep = "-")
date = as.Date(date, "%m-%d-%y")
agg_qty = aggregate(Quantity ~ date, FUN = sum)
agg_NetAmt = aggregate(NetAmount ~ date, FUN = sum)
customerorders_agg = cbind(agg_NetAmt, agg_qty)



detach(customerorders)
attach(customerorders_agg)
ts.plot(NetAmount)
plot(NetAmount~customerorders_agg$date, type = "l")
library(forecast)
TS_NetAmt <- ts(NetAmount, frequency = 12, start = c(2013, 1))
Pred5 <- ses(TS_NetAmt, h = 5, level = 95)
summary(Pred5)

detach(customerorders_agg)
