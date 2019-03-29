#Name: Yujue Wang
#Assignment 1 - Exploring Data with R


#2 Reset the workspace
rm(list=ls())

#3 Set the working directory
setwd("D:\\RPI\\Supply Chain Analytics\\HW1")

#4 Import the data called realestate
realestate <- read.csv("datagovbldgrexus.csv", header=TRUE)

#5 Provide the summary statistics of the data
summary(realestate)

#6 Print the numbers of rows and columns (dimension) in the realestate
dim(realestate)

#7 Attach realestate
attach(realestate)

#8 The numbers of unique Region Codes
length(unique(Region.Code))

#9 Plot a histogram of parking spaces
#Title: "Parking Space Histogram"
#x-axis: "Number of Spots"
#y-axis: "Frequency"
hist(Total.Parking.Spaces, freq = TRUE,
     main = paste("Parking Space Histogram"), 
     xlab = "Number of Spots", ylab = "Frequency")

#10 The number of buildings per state
# Use the apply function
tapply(Property.Type[Property.Type=="BUILDING"],
       Bldg.State[Property.Type=="BUILDING"],
       length)

#11 The average square footage per state
tapply(Bldg.ANSI.Usable, Bldg.State, mean)

#12 The scatterplot of Bldg ANSI Usable and Total Parking Spaces
#According to the scatterplot, there is no correlation between Bldg ANSI Usable and Total Parking Spaces
plot(x = Bldg.ANSI.Usable, y = Total.Parking.Spaces, 
     main = "The correlation bewteen Bldg ANSI Usable and Total Parking Spaces ",
     xlab = "Bldg ANSI Usable", ylab = "Total Parking Spaces", col = "blue")


