library(ggplot2)
library(lattice)
library(caret)
library(boot)
library(mlbench)
#data("PimaIndiansDiabetes")
data=read.csv(file.choose())
#data=PimaIndiansDiabetes
#data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
#data$class=ifelse(data$class=="Iris-setosa",1,0)
data$quality=ifelse(data$quality<6,1,0)
n_features=ncol(data)

dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]
x=as.matrix(train[,1:n_features-1])
y=train[,n_features]
xtest=test[,1:n_features-1]
ytest=test[,n_features]

#' Initialization of Optimization Values
#'
#' @description `weightInitialization` returns initial values for optimization based on the least-squares formula.
#'
#' @param x  a matrix containing the training dataset independent variable parameters.
#' @param y  a numeric vector containing training dataset classification values.
#'
#' @return a matrix containing initial values for optimization.
#' @export
#'
#' @examples
#'
#'
weightInitialization=function(x,y){
   init_weights=solve(t(x)%*%x)%*%t(x)%*%y
   init_intercept = mean(y) - (colMeans(x)%*%init_weights)
   init=c(init_intercept,init_weights)
   names(init)=c("intercept",colnames(x))
   return(init)
}
init=weightInitialization(x,y)