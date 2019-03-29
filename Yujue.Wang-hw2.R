#Yujue Wang
#Assignment 2

#2 Reset the workspace
rm(list=ls())

#3 Set the working directory
setwd("D:\\RPI\\Supply Chain Analytics\\HW2")

#4 Import the data called "purchaseorders"
# Data includes headings --> head=True
purchaseorders <- read.table("Lokad_PurchaseOrders.tsv", sep="\t", head=T)

#5 A logical vector for partial open orders
# The Received quantity <= 10% * Purchase Order Quantity
partial_openorder = ifelse(purchaseorders$Received < 0.1 * purchaseorders$Quantity,
                           TRUE, FALSE)
purchaseorders = cbind(purchaseorders, partial_openorder)

#6a 
# Transform date to the standrad data format and import into dataset
DelDate = as.Date(purchaseorders$DeliveryDate, "%d-%m-%y")
PODate = as.Date(purchaseorders$Date, "%d-%m-%y")
purchaseorders = cbind(purchaseorders, DelDate, PODate)
# Filter the open PO and import into dataset
open_PO = ifelse(purchaseorders$Received < purchaseorders$Quantity, "Open PO", "Closed PO")
purchaseorders = cbind(purchaseorders, open_PO)
# Standardize the currency unit
NetDollars = 	ifelse(purchaseorders$Currency == "GBP", purchaseorders$NetAmount * 1.29, 
			ifelse(purchaseorders$Currency == "EUR", purchaseorders$NetAmount * 1.15,
				purchaseorders$NetAmount))
purchaseorders = cbind(purchaseorders, NetDollars)
# Plot the graph
plot(x = purchaseorders$PODate[open_PO == "Open PO"], 
     y = purchaseorders$NetDollars[open_PO == "Open PO"], 
     col = ifelse(purchaseorders$partial_openorder == TRUE, "red", "green"), 
     xlab = "Open PO", ylab = "Net Amount(in dollars)")

#6b
# Create a panel of three graphs identical to the one above, but one for each location
par(mfrow=c(3,1))
plot(x = purchaseorders[open_PO == "Open PO",]$PODate, 
     y = purchaseorders[open_PO == "Open PO",]$NetDollars, 
     col = ifelse(purchaseorders$partial_openorder == TRUE, "red", "green"), 
     xlab = "Open PO", ylab = "Net Amount(in dollars)")
plot(x = purchaseorders[open_PO == "Open PO",]$PODate, 
     y = purchaseorders[open_PO == "Open PO",]$NetDollars, 
     col = ifelse(purchaseorders$partial_openorder == TRUE, "red", "green"), 
     xlab = "Open PO", ylab = "Net Amount(in dollars)")
plot(x = purchaseorders[open_PO == "Open PO",]$PODate, 
     y = purchaseorders[open_PO == "Open PO",]$NetDollars, 
     col = ifelse(purchaseorders$partial_openorder == TRUE, "red", "green"), 
     xlab = "Open PO", ylab = "Net Amount(in dollars)")


#6c
# Calculate the NetAmount (in Dollars) that has yet to be received from 
# the partially-delivered PO's
# Calculate the value per unit * Received Quantity
# Logipro has the most partially delivered in terms of dollars
NetRecevied <- (purchaseorders$NetDollars/purchaseorders$Quantity)*purchaseorders$Received
purchaseorders = cbind(purchaseorders, NetRecevied)
tapply(purchaseorders$NetRecevied[open_PO=="Open PO"],
       purchaseorders$Supplier[open_PO=="Open PO"],sum)

#6d
# There is no difference among three locations
unit_cost=purchaseorders$NetDollars/purchaseorders$Quantity
purchaseorders = cbind(purchaseorders,unit_cost)

hist(unit_cost[purchaseorders$Loc=="Chicago"], main = "Unit Cost in Chicago", 
     xlab = "Unit Cost", col = "black")
hist(unit_cost[purchaseorders$Loc=="Los Angeles"], main = "Unit Cost in Los Angeles", 
     xlab = "Unit Cost", col = "blue")
hist(unit_cost[purchaseorders$Loc=="New-York"], main = "Unit Cost in New-York", 
     xlab = "Unit Cost", col = "red")

#7a Generate random numbers from a standard normal distribution 
n = 0
i = 1
while (i>0) {
  if(rnorm(1, mean = 0, sd = 1) <= 3) {
    n = n+1
    i = i+1
  } else {break}
}
print(i)

#7b Generate 1000 weeks of random data at the three warehouses 
#using the rnorm command for each of these warehouses
#add up the random demands in each week
A <- rnorm(1000, mean = 30, sd = 5)
B <- rnorm(1000, mean = 20, sd = 8)
C <- rnorm(1000, mean = 40, sd = 12)
Sum <- A+B+C
warehouse <- data.frame(A,B,C,Sum)
head(warehouse)
mean(warehouse$Sum)
sd(warehouse$Sum)
