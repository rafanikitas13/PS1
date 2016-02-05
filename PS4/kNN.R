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
