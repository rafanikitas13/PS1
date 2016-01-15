# load packages
library(mvtnorm)
library(ggplot2)

# create small wrapper functions 
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}
# creating a function for all of this
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

# generating some data
loanDf <- loanData(noApproved=50, noDenied=50, c(4, 150), c(10, 100), 
                   c(1,20), c(2,30), -0.1, 0.6, 1221)

# Add category Undecided
solvency_undec<-seq(50,150,length.out = 50)
PI_undec<-seq(2,10,length.out = 50)+rnorm(50,0,1)

Undecided<-cbind(PI_undec, solvency_undec, rep('Undecided',50), rep(0,50))
colnames(Undecided)<-colnames(loanDf)
data<-rbind(loanDf, Undecided)
data[,c(1,2,4)]<-apply(data[,c(1,2,4)],2,as.numeric)



#target variable for each category
data$target1<-c( rep(1,50), rep(0,100))

# DISCRIMINANT ANALYSIS

# Decision boundaries on deny
datafit <- lm(as.numeric(target) ~ solvency + PIratio + 1, data=data)

# Grabbing the coefficients
weights <- coef(datafit)[c("solvency", "PIratio")]
bias <- coef(datafit)[1]

# Computing the boundary
intercept <- (-bias + 0.5)/weights["PIratio"]
slope <- -(weights["solvency"]/weights["PIratio"])
bound<-c(intercept,slope)

# Decision boundaries on accept
datafit1 <- lm(as.numeric(target1) ~ solvency + PIratio + 1, data=data)

# Grabbing the coefficients
weights <- coef(datafit1)[c("solvency", "PIratio")]
bias <- coef(datafit1)[1]

# Computing the boundary
intercept <- (-bias + 0.5)/weights["PIratio"]
slope <- -(weights["solvency"]/weights["PIratio"])
bound1<-c(intercept,slope)

# Save data in csv format
data <- data[,1:3]

data$approved <- predict.lm(datafit1)
data$approved[data$approved >= 0.5] <- 1
data$approved[data$approved < 0.5] <- 0

data$denied <- predict.lm(datafit)
data$denied[data$denied >= 0.5] <- 1
data$denied[data$denied < 0.5] <- 0

data$undecided <- 0
data$undecided[data$approved == 0 & data$denied ==0] <- 1
write.csv(data, file = 'predictions.csv')

#Plot data with boundaries

print(ggplot)(data = data, aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
  geom_point(size=3) +
  xlab("Solvency") +
  ylab("Weight") +
  theme_bw() +
  geom_abline(intercept = bound[1], slope = bound[2])+
  geom_abline(intercept = bound1[1], slope = bound1[2])
ggsave("discFunction3C.pdf", scale = 1, width = 3, height = 3)


