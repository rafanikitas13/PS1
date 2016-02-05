# loading in required packages
if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("ggplot2")) install.packages("ggplot2")

############### my kNN function #################
kNN <-function(features, labels, memory = NULL,  k = 1,  p = 2, type='train'){
  
  # load the required libraries
  library(assertthat)
  
  # check the inputs
  assert_that(k %in% 1:nrow(features))
  assert_that(p %in% c(1, 2, Inf))
  assert_that(type %in% c("train", "predict"))
  not_empty(features)
  not_empty(labels)
  is.count(k)
  is.string(type)
  if (type == "train") {
    assert_that(nrow(features) == length(labels))
  }else if (type == "predict") {
    assert_that(not_empty(memory) & 
                  ncol(memory) == ncol(features))
  }
  
  
  
  # Compute the distance between each point and all others 
  noObs <- nrow(features)
  noVars <- ncol(features)
  
  # Compute the distance between each point and all others 
  # according to the similarity measure
  if (type == "train") {
    distMatrix <- matrix(NA, noObs, noObs)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(features[obs,])
      probeExpanded <- matrix(probe, nrow = noObs, ncol = noVars, byrow = TRUE)
      
      # computing distances between the probe and exemplars in the training 
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(features -  probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
      }  
    }
  } else if (type == "predict") {
    noMemory <- nrow(memory) 
    distMatrix <- matrix(NA, noObs, noMemory)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(features[obs,])
      probeExpanded <- matrix(probe, nrow = noMemory, ncol = noVars, byrow = TRUE)
      
      # computing distances between the probe and exemplars in the memory
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(memory - probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
      }  
    }
  }
  
  # Finding the neighbors!
  # Sort the distances in increasing numerical order and pick the first 
  # k neighbors
  neighbors <- apply(distMatrix, 1, order) 
  
  # Compute and return the most frequent class in the k nearest neighbors
  predLabels <- rep(NA, noObs)
  prob <- rep(NA, noObs)
  for (obs in 1:noObs) {
    predLabels[obs] <- as.numeric(names(tail(sort(table(labels[neighbors[1:k, obs]])),1)))
    prob[obs] <- round(tail(sort(table(labels[neighbors[1:k, obs]])),1)/k,4)
  }
  
  # examine only the performance for training
  if (type == "train") {
    conf_matrix<- table(predLabels, labels)
    accuracy <- mean(predLabels == labels)
  } else if (type == "predict") {
    conf_matrix <- NA
    accuracy <- NA
  }
  
  # return the results: predicted labels,probabilities,accuracy and the
  # confusion matrix
  return(list(predLabels = predLabels, 
              prob = prob,
              accuracy = accuracy,
              conf_matrix = conf_matrix))
} 
###################################################################


################ genXOR function ########################
genXOR <- function(noObs=200, seed=1111) {
  
  # check the inputs
  assert_that(is.scalar(noObs) && is.double(noObs))
  assert_that(is.scalar(seed) && is.double(seed))
  
  # defining a function for generating bivariate normal data
  genBVN <- function(n = 1, muXY = c(0,1), sigmaXY = diag(2)) {
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
  }
  
  # generate XOR data and add some simple names
  set.seed(seed)
  class1 <- rbind(genBVN(noObs, c(1,1), diag(2)),
                  genBVN(noObs, c(10,10), diag(2)) )
  class2 <- rbind(genBVN(noObs, c(1,10), diag(2)),
                  genBVN(noObs, c(10,1), diag(2)) )
  dataset <- rbind(cbind(class1, 0), cbind(class2, 1))
  dataset <- as.data.frame(dataset)
  colnames(dataset) <- c("x1", "x2", "y")
  
  return(dataset)
}
#####################################################################

# generate data
data <- genXOR()
colnames(data)<-c('dimension1','dimension2','Label')
data$Label<-as.factor(data$Label)
features <- data[,1:2]
labels <- data[,3]

# predict with kNN function
prediction <- kNN(features,labels,k = 5, p = 2, type = "train")

#Save predictions in csv format
predictions <- data.frame(cbind(data,
                                predLabels = prediction$predLabels,
                                prob= prediction$prob))

write.csv(predictions, file="predictions.csv", row.names = FALSE)

# create a grid for plot
dim1 <- seq(min(data$dimension1), max(data$dimension1), by=0.5)
dim2 <- seq(min(data$dimension2), max(data$dimension2), by=0.5)
grid <- expand.grid(x1=dim1, x2=dim2)

# predict labels for grid points
predata <- kNN( grid, labels, features, k=5, p=2, type="predict")
predlabels<-predata$predLabels
geometric_boundaries<-as.factor(predata$predLabels)

# create plot
dataPlot <- ggplot(data=grid, aes(x=x1, y=x2, z=predlabels) ) +
  coord_cartesian(xlim=c(min(grid$x1),max(grid$x1)), ylim=c(min(grid$x2),max(grid$x2)))+
  geom_tile(aes(fill = geometric_boundaries), alpha = 0.3) +
  stat_contour(bins=1, size=1, colour="black") + 
  geom_point(data=data, size=3, aes(x=dimension1, y=dimension2, z=Label , color=Label), shape = 1) +
  labs(title = "Decision boundaries for GenXOR dataset")
 
dataPlot


# save plot
cairo_pdf("dataPlot.pdf")
print(dataPlot) 
dev.off()
