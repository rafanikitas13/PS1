# necessary packages
library(compiler)
library(zoo)
library(rpart)
library(reshape2)
# load spambase data
data<- read.csv("spambase.data", header=FALSE)

nrow<- nrow(data)

subset<-sample(1:nrow,3000)

train.data <- data[subset,]
test.data <- data.frame(data[-subset,-ncol(data)])
colnames(test.data) <- colnames(train.data)[-ncol(data)]

# using cmpfun() to compile the function
findThreshold <- cmpfun(findThreshold)

depths <- seq(1,10)
my.train.err <- rep(NA,length(depths))
my.test.err <- rep(NA,length(depths))

pkg.train.err <- rep(NA,length(depths))
pkg.test.err <- rep(NA,length(depths))

for(k in depths){
  
  my.tree <- cTree(formula = V58 ~ ., data = train.data , depth = k , 
                  minPoints = 500, costFnc = ME, type = "predict", test.data = test.data)
  
  my.train.err[k] <- 1 - mean(my.tree$predLabels == train.data[,c("V58")])
  my.test.err[k] <- 1 - mean(my.tree$testLabels == data[-subset,c("V58")])
  
  fit <- rpart(formula = V58 ~ ., data = train.data, method = "class",
               control=rpart.control(maxdepth=k,minsplit= 500))
  pkg.train <- predict(fit, type = "class")
  pkg.test <- predict(fit, newdata = test.data,type = "class")
  
  pkg.train.err[k] <- 1 - mean(pkg.train == train.data[,c("V58")])
  pkg.test.err[k] <- 1 - mean(pkg.test == data[-subset,c("V58")])
  
}

# wrapping everything in the dataset for plotting
df <- data.frame(depth = depths,my.train.err , my.test.err , pkg.train.err , pkg.test.err)
df.melt <- melt(df,1,variable.name = "type",value.name = "error")
# plotting
Plot <- ggplot(data = df.melt, aes(x = depth, y = error, color = type)) + 
  geom_line(aes(linetype=type), size=1) + 
  geom_point() +
  xlab("Tree Depth") +
  ylab("Misclassification error") +
  theme_bw(base_size = 12, base_family="Helvetica")

# Save as pdf
cairo_pdf("cTree.pdf")
print(Plot) 
dev.off()