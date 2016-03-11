# Load data and packages 
source("adaBoost.R")

if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
if (!require("rpart")) install.packages("rpart"); library(rpart)

data <- read.csv("spambase.data", header=FALSE)


variables<-paste(' V',1:57, sep="" )
variables<-paste(variables, collapse="+")
formula<-as.formula(paste('V58~', variables))


nrow <-nrow(data)

train.ind <-sample(1:nrow,3500)

train<-data[train.ind,]
test<-data[-train,]

depths = seq(1,10)
my.train.err = rep(NA,length(depths))
my.test.err = rep(NA,length(depths))

pkg.train.err = rep(NA,length(depths))
pkg.test.err = rep(NA,length(depths))

for(k in depths){
  
  # Build the tree with the custom function
  my.adaBoost <- adaBoost(formula, train, 10, trees[i], test)
  
  my.train.err[k] <-1 - mean(my.adaBoost$predLabels == train[,c("V58")])
  my.test.err[k] <- 1 - mean(my.adaBoost$predLabels.test == test[,c("V58")])
  
  fit <- boosting(formula ,data = train,
                  mfinal = 10,control=rpart.control(maxdepth=k))
  pkg.train <-as.factor(fit$class)
  predBST.test <- predict(fit,newdata = test)
  pkg.test <- as.factor(predBST.test$class)
  
  pkg.train.err[k] <-1 - mean(pkg.train == train[,c("V58")])
  pkg.test.err[k]<- 1 - mean(pkg.test == test[,c("V58")])
  
}

# reshaping the data  
df <- data.frame(depth = depths,my.train.err , my.test.err , pkg.train.err , pkg.test.err)
df.melt <- melt(df,1,variable.name = "type",value.name = "error")
#plot
errPlot <- ggplot(data = df.melt, aes(x = depth, y = error, color = type)) + 
  geom_line() + 
  geom_point() +
  xlab("Tree depth") +
  ylab("Misclassification error") +
  theme_bw(base_size = 14, base_family = "Comic Sans")

# saving
cairo_pdf("cTree.pdf")
print(errPlot) 
dev.off()