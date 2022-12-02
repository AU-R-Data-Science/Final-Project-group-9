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

#' Logistic Regression
#'
#' @param theta a numerical coefficient vector
#' @param X a matrix containing the training dataset independent variable parameters.
#'
#' @return a cost estimator of the data
#' @export
#'
#' @examples

cost.glm <- function(theta,X) {
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  (1/m)*sum((-y*log(g)) - ((1-y)*log(1-g)))
}

x1 <- cbind(1, x)
predict=optim(par=init, fn = cost.glm, method='CG',
              X=x1)

coeff=predict$par
cost=predict$value

w=coeff[2:length(coeff)]
b=coeff[1]
final_test_pred = t(sigmoid(w%*%t(xtest)+b))

m_ts =  nrow(xtest)

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
predictor=function(final_pred, m, cutoff=0.5){
  y_pred = rep(0,m)
  for(i in 1:length(final_pred)){
    if(final_pred[i] > cutoff)
      y_pred[i] = 1
    else
      y_pred[i] = 0
  }
  return(y_pred)
}

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
    dt = sample(nrow(data), size=nrow(data),replace=TRUE)
    train<-data[dt,]
    x=as.matrix(train[,1:n_features-1])
    y=train[,n_features]

    init= weightInitialization(x,y)
    x1 <- cbind(1, x)
    predict=optim(par=init, fn = cost.glm, method='CG',
                  X=x1)

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

CI=bootstrapCI(data,0.05,n=20)
CI

glm_plot=function(ytest,xtest,i){
  m_ts=nrow(xtest)
  y_ts_pred = predictor(final_test_pred, m_ts)
  df=data.frame(var1=ytest,var2=xtest[,i])
  Pred_data <- data.frame(var2=seq(
    min(xtest[,1]), max(xtest[,1]),len=500))
  test_pred = sigmoid(w%*%t(xtest)+b)
  m_ts =  nrow(Pred_data)
  Pred_data$var1=predictor(test_pred, m_ts)

  return(ggplot(df, aes(x=var2, y=var1)) + geom_point() +
           stat_smooth(method="glm", color="green", se=FALSE,
                       method.args = list(family=binomial)))

}

glm_plot(ytest,xtest,2)

y_ts_pred = predictor(final_test_pred, m_ts)
confusion_matrix=confusionMatrix(as.factor(y_ts_pred), as.factor(ytest))
confusion_matrix

metricplot=function(y_prob,ytest,metric){
  m_tr =  nrow(y_prob)
  metric_data=matrix(nrow=9,ncol=7)
  colnames(metric_data)=c("Prevalence","Accuracy","Sensitivity","Specificity","False Discovery Rate","Diagnostic Odds Ratio","cutoff")
  for(i in 1:9){
    y_ts_pred = predictor(y_prob, m_tr,cutoff=i/10)
    confusion_matrix=confusionMatrix(as.factor(y_ts_pred), as.factor(ytest))
    metric_data[i,1]=as.vector(confusion_matrix$byClass[8])
    metric_data[i,2]=as.vector(confusion_matrix$overall[1])
    metric_data[i,3]=as.vector(confusion_matrix$byClass[1])
    metric_data[i,4]=as.vector(confusion_matrix$byClass[2])
    metric_data[i,5]=1-as.vector(confusion_matrix$byClass[3])
    metric_data[i,6]=metric_data[i,2]*metric_data[i,3]/((1-metric_data[i,2])*(1-metric_data[i,3]))
    metric_data[i,7]=i/10
  }
  return(plot(y=metric_data[,metric],x=metric_data[,7],type="l",xlab="cutoff",ylab=metric))
}

metricplot(final_test_pred,ytest,metric="Sensitivity")
