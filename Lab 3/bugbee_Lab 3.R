###################
##### Abalone #####
###################

# read dataset
setwd("C:/Users/15184/Downloads/Spring 2025/Data Analytics/Lab 3/")
albone <- read.csv("abalone_dataset.csv") 
 
dataset <- albone 

## View the dataset
View(dataset)
attach(dataset)

# Exercise 1 

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"


sqrt(4176)

#knn model 1

knn.predicted <- knn(train = dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k = 13)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset$age.group)

#knn model 2

knn.predicted <- knn(train = dataset[,5:8], test = dataset[,5:8], cl = dataset$age.group, k = 13)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset$age.group)

##Based on the accuracy scores the second model is better. 
########################################################

library(class)
library(caret)

# list of k
k.list <- c(59,61,63,65,67,69,71) 

# empty list for accuracy
accuracy.list <- c()

# loop: train&predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  
  knn.predicted <- knn(train = dataset[,5:8], test = dataset[,5:8], cl = dataset$age.group, k = k)
  
  contingency.table <- table(knn.predicted, dataset$age.group, dnn=list('predicted','actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset$age.group)
  
  accuracy.list <- c(accuracy.list,accuracy)

}


# plot acccuracy with k, limiting values of y axis between .9 & 1
plot(k.list,accuracy.list,type = "b", ylim = c(.65,.7))

## Alternatively:

## train and evaluate multiple knn models to find optimal k
knn.model <- train(dataset[,5:8], dataset$age.group, method = "knn", tuneLength = 10, trControl = trainControl(method = "cv"))

# print model outputs
print(knn.model)

#### for both approaches k=19 has the highest accuracy. 

#############################################
# Exercise 2 


## run kmeans
k = 19
iris.km <- kmeans(dataset[,5:8], centers = k)


## get and plot clustering output 

assigned.clusters <- as.factor(dataset$age.group)

ggplot(dataset, aes(x = whole_weight, y = shucked_wieght, colour = as.factor(assigned.clusters))) +
  geom_point()


## WCSS: total within cluster sum of squares
dataset.km$tot.withinss

dataset.km$cluster


## run tests with multiple k values and plot WCSS
k.list <- c(2,3,4,5,6)

wcss.list <- c()

for (k in k.list) {
  
  dataset.km <- kmeans(dataset[,5:8], centers = k)
  
  wcss <- dataset.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  ## get and plot clustering output 
  assigned.clusters <- as.factor(dataset.km$cluster)
  
  ggplot(dataset, aes(x = whole_weight, y = shucked_wieght, colour = assigned.clusters)) +
    geom_point()
  
}

plot(k.list,wcss.list,type = "b")


### based on the plot, around k=64 is the optimal k-value 


