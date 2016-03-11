
# AdaBoost Function

adaBoost <- function(formula, data, depth, noTrees, test ) {
  
  # Required Packages
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
  if (!require("rpart")) install.packages("rpart"); library(rpart)
  
  # Check the Inputs
  not_empty(formula);
  is.count(depth); is.count(noTrees);
  assert_that(is.data.frame(data));not_empty(data);
  
  # Extract data from formula
  lbls<-get.vars(lhs(formula)) 
  vrs<-get.vars(rhs(formula)) 
  
  y<-data[,lbls]
  X<-data[,vrs]
  
  y<-ifelse(y==0,-1,y)
  
  # Create matrices for the loop
  n<-nrow(X)
  
  predictions <- matrix(0,nrow=noTrees, ncol=n )
  predictions.test <- matrix(0, nrow=noTrees, ncol=nrow(test))
  
  # Initialize weights,error and alpha
  
  alpha <- rep(0,noTrees)
  error <- rep(0,noTrees)
  weights <- matrix(1/n,nrow=noTrees, n)
  

  weaklearner = function(formula, data, w , depth , method = "class") {
   
     environment(formula) <- environment()
    rpart(formula, data, weights = w,method = method, control = rpart.control(maxdepth = depth))
  }
  # i iterations
  for( i in 1:noTrees){
    
    weights.i <- weights[i,]
    
    
    tree <- weaklearner(formula , data , weights.i, depth)
    
    predictions[,i] <- as.numeric(as.character(predict(tree, type= "class")))
    
    predictions.test[,i] <- as.numeric(as.character(predict(tree,
                                                          newdata = test, type= "class")))
    
    ind <- as.numeric(predictions[,i]!=y)
    
    error[i] <- t(weights.i) %*% ind / sum(weights.i)
    
    alpha[i] <- log((1-error[i])/error[i])
    
    if(i < noTrees){
      weights[i+1,] <- weights.i * exp(alpha[i] * ind)
   
    }
  }
  
  #Final Result
  
  A<-diag(alpha)
  predLabels<-rowSums(predictions%*%A)
  predLabels<-sign(predLabels)
  
  predLabels.test<-rowSums(predictions.test%*%A)
  predLabels.test<-sign(predLabels.test)
  
  return(list(predLabels= predLabels, predLabels.test=predLabels.test))
  
}
  
  




