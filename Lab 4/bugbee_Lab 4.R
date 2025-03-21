library(caret)
library(ggfortify)
library(ggplot2)
library(class)

# read dataset
setwd("C:/Users/15184/Downloads/Spring 2025/Data Analytics/Lab 4/")
wine <- read.csv("wine.csv") 
 
dataset <- wine 

# PCA with wine dataset 

wine.df <- wine
head(wine.df)
attach(wine)

X <- wine.df[, 2:14]  # Features (excluding class)
Y <- wine.df[, 1]     # Class label


## scatter plot of 2 variables colored by class
ggplot(X, aes(x = Alcohol, y = Malic.acid, color = Y, fill = Y)) + geom_point() + 
  stat_ellipse(type = "t",geom = "polygon",alpha = 0.4)

####### PCA #######

principal_components <- princomp(X, cor = TRUE, score = TRUE)

summary(principal_components)

principal_components$loadings


# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# loadings
principal_components$loadings[,1:3]

# filter out columns
# PCA on filtered stuff
filtered_wine <- wine
filtered_wine <- subset(filtered_wine, select = -c(Ash, Color.intensity))

X <- filtered_wine[,2:12]
Y <- as.factor(filtered_wine[,1])

principal_components <- princomp(X, cor = TRUE, score = TRUE)

summary(principal_components)

principal_components$loadings

# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

## using autoplot() function to plot the components
autoplot(principal_components, data = filtered_wine, colour = 'Class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# loadings
principal_components$loadings[,1:3]

# KNN on Original Dataset

n = 125
s_wine <- sample(n,n*.8)

## create train & test sets based on sampled indexes 
wine.train <- wine[s_wine,]

wine.test <- wine[-s_wine,]

# simple estimate of k
k = round(sqrt(n))

k <- k-1

knn.predicted <- knn(train = wine.train[,2:14], test = wine.test[,2:14], cl = wine.train[,1], k = 3)
# create contingency table/ confusion matrix 
contingency.table1 <- table(knn.predicted, wine.test[,1], dnn=list('predicted','actual'))
contingency.table1
# calculate classification accuracy
sum(diag(contingency.table1))/length(wine.test[,1])

cm = as.matrix(table(Actual = wine.test$Class, Predicted = knn.predicted))

#cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n

accuracy
###################################################################
print(cm)
dim(cm)

diag_vals <- diag(cm)  # Extract diagonal elements (correct classifications)
rowsums <- rowSums(cm) # Sum of each row (actual counts)
colsums <- colSums(cm) # Sum of each column (predicted counts)

precision <- ifelse(colsums > 0, diag_vals / colsums, 0)
recall <- ifelse(rowsums > 0, diag_vals / rowsums, 0)
f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)

actual_labels <- factor(wine.test$Class, levels = unique(wine.train$Class))
predicted_labels <- factor(knn.predicted, levels = unique(wine.train$Class))

cm <- table(Actual = actual_labels, Predicted = predicted_labels)

length(diag_vals)
length(rowsums)
length(colsums)

# Ensure both actual and predicted labels have the same factor levels
all_classes <- union(levels(factor(wine.train$Class)), levels(as.factor(wine.test$Class)))

actual_labels <- factor(wine.test$Class, levels = unique(wine.train$Class)) 
predicted_labels <- factor(knn.predicted, levels = levels(actual_labels)) 

# Create a properly formatted confusion matrix
cm <- table(Actual = actual_labels, Predicted = predicted_labels)

# Compute classification metrics safely
diag_vals <- diag(cm)
rowsums <- rowSums(cm)
colsums <- colSums(cm)

precision <- ifelse(colsums > 0, diag_vals / colsums, 0)
recall <- ifelse(rowsums > 0, diag_vals / rowsums, 0)
f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)

# Display the results in a table
data.frame(recall, precision, f1)


metrics_df <- data.frame(Recall = recall, Precision = precision, F1_Score = f1)
print(metrics_df)

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

summary(wine.test$Class)

# KNN on PCA scores
pca <- as.data.frame(principal_components$scores[,1:3])

#View(pca)
pca$Class <- filtered_wine$Class

n = 125
s_wine <- sample(n,n*.8)

## create train & test sets based on sampled indexes 
pca.train <- pca[s_wine,]

pca.test <- pca[-s_wine,]

# simple estimate of k
k = round(sqrt(n))

k <- k-1

knn.predicted <- knn(train = pca.train[,1:3], test = pca.test[,1:3], cl = pca.train[,4], k = 3)
# create contingency table/ confusion matrix 
contingency.table1 <- table(knn.predicted, pca.test[,4], dnn=list('predicted','actual'))
contingency.table1
# calculate classification accuracy
sum(diag(contingency.table1))/length(pca.test[,4])

cm = as.matrix(table(Actual = pca.test$Class, Predicted = knn.predicted))

#cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy = sum(diag)/n

accuracy

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

summary(pca.test$Class)

# NOTE: The reason the F1 score couldn’t be calculated for Class 3 is because the KNN model didn’t predict that class at all.