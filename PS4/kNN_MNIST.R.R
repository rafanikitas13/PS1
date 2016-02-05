# load necessary packages
library("class")

# load data
train.mnist <- read.csv("MNIST_training.csv",header = F)
test.mnist  <- read.csv("MNIST_test.csv",header=F)

# inputs
pixels <- as.matrix(train.mnist[,-1])
labels <- as.numeric(train.mnist[,1])
test <- as.matrix(test.mnist)

# knn classifier from function in built function in R
predictions <- knn(pixels,test,labels,k=8)

# save data in csv format 
write.csv(file = "MNIST_predictions.csv",predictions)