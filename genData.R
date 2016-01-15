# load packages
library(ggplot2)
library(mvtnorm)

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
# Function for Benchmark Classification Dataset
genData<-function(nodata1=100, nodata2=100, mudata1=c(2,2),sddata1=c(1,1),rhodata1=0.5,seed=1111, save.data=FALSE, save.plot=FALSE){
  
  # Generate data1
  sigmadata1 <- sigmaXY(rhodata1, sddata1[1], sddata1[2])
  data1 <- genBVN(nodata1, seed, mudata1, sigmadata1)
  
  # Generate data2
  set.seed(seed)
  data2_x<-seq(-1,1,length.out = nodata2)
  data2_y<-seq(-1,1,length.out = nodata2)+rnorm(nodata2,0,0.5)
  data2<- cbind(data2_x,data2_y)
  
  target<-c(rep(0,nodata1), rep(1,nodata2))
  
  data<-data.frame(data1=data1,data2=data2,target=target)
  data$target<-as.factor(data$target)
  # Save data in csv format
  if(save.data==TRUE){
    write.csv(data, file = "dataset.csv")
  }
  
  # Create plot
  if(save.plot==TRUE){
     ggplot(data = data, aes(data1=data1, data2=data2, colour=target, fill=target)) +     
      geom_point() +
      xlab("data1") +
      ylab("data2") +
      theme_bw() 
    ggsave("dataPlot.pdf", scale = 1, width = 3, height = 3)
  }
  return(data)
}
genData(100,100,c(2,2),c(1,1),0.5,1111,save.data = TRUE,save.plot = TRUE) 