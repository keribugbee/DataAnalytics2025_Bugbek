library("ggplot2")
library("readr")

## read dataset
setwd("C:/Users/15184/Downloads/Spring 2025/Data Analytics/Lab 2 data")

dataset <- read.csv("NY-House-Dataset.csv") 

View(dataset)
#tips
attach(dataset) # sets the ‘default’ object 

PROPERTYSQFT # prints out values EPI_data$EPI.new

NAs <- is.na(PROPERTYSQFT) # records True values if the value is NA 

PROPERTYSQFT.noNAs <- PROPERTYSQFT[!NAs] # filters out NA values, new array 
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)

lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

# COPIED FROM HERE



NAs <- is.na(BEDS) # records True values if the value is NA 

BEDS.noNAs <- BEDS[!NAs] # filters out NA values, new array 
ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point()
## filter data
dataset <- dataset[dataset$BEDS<195000000,]

dataset <- dataset[dataset$BEDS!=2184.207862,]

dataset$BEDS[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~BEDS, data = dataset)

lmod <- lm(log10(PRICE)~log10(BEDS), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~BEDS, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(BEDS), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = BEDS, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

# COPIED FROM HERE

sum(is.na(dataset$BATH))  # Count NA values
sum(is.nan(dataset$BATH))  # Count NaN values
sum(is.infinite(dataset$BATH))  # Count Inf values
sum(dataset$BATH <= 0)
dataset <- dataset[dataset$BATH > 0, ]
NAs <- is.na(BATH) # records True values if the value is NA 

BATH.noNAs <- BATH[!NAs] # filters out NA values, new array 
ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point()
## filter data
dataset <- dataset[dataset$BATH<195000000,]

dataset <- dataset[dataset$BATH!=2184.207862,]

dataset$BATH[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~BATH, data = dataset)

lmod <- lm(log10(PRICE)~log10(BATH), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~BATH, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(BATH), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


