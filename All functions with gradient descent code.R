data=read.csv("iris_csv.csv")
#data=read.csv(file.choose())
head(data)

data
data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
data$class=ifelse(data$class==c("Iris-setosa"),1,0)
n_features=ncol(data)


data

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
weightInitialization(x,y)

#' Fitted logistic curve
#'
#' @description Calculates a predictive indicator of a dataset based on a logistic model. 
#'
#' @param result a numeric data expression or dataset to be evaluated using a logistic curve. 
#'
#' @return a vector of the predictors produced from `sigmoid` for each independent variable.
#' 
#' @export
#'
#' @examples
#' 
#' 
#' 
sigmoid=function(result){
  final_result = 1/(1+exp(-result))
  return(final_result)
}


#' Gradient Descent Optimization
#'
#' @param init a matrix containing initial numeric values for optimization.
#' @param x  a matrix containing the training dataset independent variable parameters. 
#' @param y  a numeric vector containing training dataset classification values.
#'
#' @return 
#' @export
#'
#' @examples
model_optimize=function(init, x, y){
  m = nrow(x)
  w=init[2:length(init)]
  b=init[1]
  #Prediction
  final_result = sigmoid(w%*%t(x)+b)
  Y_T = t(y)
  cost = (-1/m)*(sum((log(final_result)*Y_T) + (log(1-final_result)*(1-Y_T))))
  #Gradient calculation
  dw = (1/m)*((t(x)%*%t((final_result-t(y)))))
  db = (1/m)*(sum(final_result-t(y)))
  
  grads = c(db,dw)
  
  return(c(cost,grads))
}

#' Title
#'
#' @param init a matrix containing initial numeric values for optimization.
#' @param x  a matrix containing the training dataset independent variable parameters. 
#' @param y  a numeric vector containing training dataset classification values.
#' @param learning_rate the desired rate by which weight parameters are updated following optimization.
#' @param no_iterations the number of iterations to be used for optimization.
#'
#' @return
#' @export
#'
#' @examples
model_predict=function(init, x, y, learning_rate=0.05, no_iterations=1500){
  costs = c()
  w=init[2:length(init)]
  b=init[1]
  
  for(i in 1:no_iterations){
    optim = model_optimize(init,x,y)
    grads = c(optim[2:length(optim)])
    cost = optim[1]
    
    dw = c(grads[2:length(grads)])
    db = grads[1]
    #weight update
    w = w - (dw*learning_rate)
    b = b - (db*learning_rate)
    init=c(b,w)
    costs=c(costs,cost)
  }

  #final parameters
  
  coeff = c(b,w)
  gradient = c(db,dw)
  
  return(list(coeff,gradient,costs))
}

#' Title
#'
#' @param final_pred 
#' @param m the number of rows contained in the training dataset.
#' @param cut_off the designated cut-off value for prediction. 
#'
#' @return a vector of predicted y values 
#' @export
#'
#' @examples
predictor=function(final_pred, m, cut_off=0.5){
  y_pred = rep(0,m)
  for(i in 1:length(final_pred)){
    if(final_pred[i] > cut_off)
      y_pred[i] = 1
    else
      y_pred[i] = 0
  }
  return(y_pred)
}

init= weightInitialization(x,y)
init
#Gradient Descent
predict=model_predict(init, x, y)

coeff=unlist(predict[1])
names(coeff)=c("intercept",colnames(x))
gradient=unlist(predict[2])
costs = unlist(predict[3])
w=coeff[2:length(init)]
b=coeff[1]
final_train_pred = sigmoid(w%*%t(x)+b)
final_test_pred = sigmoid(w%*%t(xtest)+b)

m_tr =  nrow(x)
m_ts =  nrow(xtest)

y_tr_pred = predictor(final_train_pred, m_tr,cut_off=0.5)
y_ts_pred = predictor(final_test_pred, m_ts)

coeff
min(costs)
plot(c(1:length(costs)),costs,type="l")
table(ytest)
table(y_ts_pred)
df=data.frame(y_ts_pred,ytest)
install.packages("rlang")
install.packages("caret")
install.packages("ggplot2")
library(ggplot2)
library(lattice)
library(caret)

confusion_matrix=?confusionMatrix(as.factor(df$ytest), as.factor(df$y_ts_pred))
confusion_matrix


ggplot(df, aes(x=ytest, y=final_test_pred)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))


#' Title
#'
#' @param data 
#' @param alpha 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
bootstrapCI=function(data,alpha,n=20){
  n_features=ncol(data)
  beta=matrix(nrow=n,ncol=n_features)
  for(i in 1:n){
    n_features=ncol(data)
    dt = sort(sample(nrow(data), nrow(data)*.7))
    train<-data[dt,]
    x=as.matrix(train[,1:n_features-1])
    y=train[,n_features]
    
    init= weightInitialization(x,y)
    predict=model_predict(init, x, y)
    
    coeff=unlist(predict[1])
    beta[i,]=coeff
  }
  CI=matrix(nrow=n_features,ncol=3)
  for(i in 1:n_features){
    b1<-boot(beta[i,],function(u,i) mean(u[i]),R=n)
    g=boot.ci(b1,type=c("norm","basic","perc"),conf=1-alpha)$norm
    CI[i,]=as.vector(g)
  }
  rownames(CI)=c("intercept",colnames(x))
  colnames(CI)=c("Level","Lower","Upper")
  return(CI) 
}

CI=bootstrapCI(data,0.15,n=100)
CI
library(boot)
library(lattice)
x1<-rnorm(50,2,0.25)
b1<-boot(x1,function(u,i) mean(u[i]),R=1000)
CI=boot.ci(b1,type=c("norm"))
CI$norm
b1

n=20
CI

