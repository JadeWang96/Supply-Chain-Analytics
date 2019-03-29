#Name: Yujue Wang
#Assignment 4 - Bootstrapping and Simulation

# Reset the workspace
rm(list=ls())
library(boot)
# Set the working directory
setwd("D:\\RPI\\2019Spring\\Supply Chain Analytics\\HW4")

# 1	Load the R dataset mtcars 
# Using bootstrapping come up with a standard error metric 
# for calculating the median mpg in the dataset.  
data(mtcars)
mpg <- mtcars$mpg
medianfunc <- function(x, i){median(x[i])}
# Calculate the median
bootMedian <- boot(mpg, medianfunc, 100)
bootMedian

# 2 Use the boot function to compute a different statistic
rfunc <- function(formula, x, i) {
  summary(lm(formula, data = x[i,]))$r.squared
}
rsquared <- boot(data = mtcars, statistic = rfunc, R = 1000, formula = hp~disp)
# Compute the average
mean(rsquared$t)
hist(rsquared$t, main = "Different R-Squared Values")
boot.ci(rsquared, conf = 0.95)

# 3 A sensitivity analysis on the Monte Carlo simulation 
rm(list=ls())
options(warn = -1)
nreps = 10000
mean_comp = 200
sd_comp = 30
instock= 0.95
stockouts =rep(NA, nreps)
# a
# Run the simulation
for (rep in 1:nreps) {
  components = rnorm(1, mean_comp, sd_comp)
  components = round(components)
  stockouts[rep] = dbinom(components, components, instock)
}
mean(1-stockouts, na.rm = TRUE)
# b
IQR(stockouts, na.rm = TRUE)

# c
# Change the averge number and run simulation again
# 100
mean_comp = 100
for (rep in 1:nreps) {
  components = rnorm(1, mean_comp, sd_comp)
  components = round(components)
  stockouts[rep] = dbinom(components, components, instock)
}
mean(1-stockouts, na.rm = TRUE)
IQR(stockouts, na.rm = TRUE)

# 50
mean_comp = 50
for (rep in 1:nreps) {
  components = rnorm(1, mean_comp, sd_comp)
  components = round(components)
  stockouts[rep] = dbinom(components, components, instock)
}
mean(1-stockouts, na.rm = TRUE)
IQR(stockouts, na.rm = TRUE)

# Conslusion: As the decline of average number of components, 
# stock rate and IQR will increase.

# d
# Change the standard deviation of the number of the component
# 20
sd_comp = 20
mean_comp = 200
for (rep in 1:nreps) {
  components = rnorm(1, mean_comp, sd_comp)
  components = round(components)
  stockouts[rep] = dbinom(components, components, instock)
}
mean(1-stockouts, na.rm = TRUE)
IQR(stockouts, na.rm = TRUE)

# 10
sd_comp = 10
mean_comp = 200
for (rep in 1:nreps) {
  components = rnorm(1, mean_comp, sd_comp)
  components = round(components)
  stockouts[rep] = dbinom(components, components, instock)
}
mean(1-stockouts, na.rm = TRUE)
IQR(stockouts, na.rm = TRUE)

# Conclusion: The decline of standard deviation has on impact on
# (a) and (b).