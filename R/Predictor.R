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
#' #' data=read.csv("iris_csv.csv")
#' data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
#' data$class=ifelse(data$class=="Iris-setosa",1,0)

#' n_features=ncol(data)
#' dt = sort(sample(nrow(data), nrow(data)*.7))
#' train<-data[dt,]
#' test<-data[-dt,]
#' x=as.matrix(train[,1:n_features-1])
#' y=train[,n_features]
#' xtest=test[,1:n_features-1]
#' ytest=test[,n_features]
#'
#' init=weightInitialization(x,y)
#' x1 <- cbind(1, x)
#'
#' g=sigmoid(x1%*%init)
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
#' data=read.csv("iris_csv.csv")
#' data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
#' data$class=ifelse(data$class=="Iris-setosa",1,0)

#' n_features=ncol(data)
#' dt = sort(sample(nrow(data), nrow(data)*.7))
#' train<-data[dt,]
#' test<-data[-dt,]
#' x=as.matrix(train[,1:n_features-1])
#' y=train[,n_features]
#' xtest=test[,1:n_features-1]
#' ytest=test[,n_features]
#'
#' init=weightInitialization(x,y)
#' x1 <- cbind(1, x)
#' predict=optim(par=init, fn = cost.glm, method='CG',
#'               X=x1)
#'
#' cost.glm <- function(theta,X) {
#'  m <- nrow(X)
#'  g <- sigmoid(X%*%theta)
#'  (1/m)*sum((-y*log(g)) - ((1-y)*log(1-g)))
#' }
#'

cost.glm <- function(theta,X) {
   m <- nrow(X)
   g <- sigmoid(X%*%theta)
   (1/m)*sum((-y*log(g)) - ((1-y)*log(1-g)))
}

# x1 <- cbind(1, x)
# predict=optim(par=init, fn = cost.glm, method='CG',
#               X=x1)
#
# coeff=predict$par
# cost=predict$value
#
# w=coeff[2:length(coeff)]
# b=coeff[1]
# final_test_pred = t(sigmoid(w%*%t(xtest)+b))
#
# m_ts =  nrow(xtest)

#' Dataset Prediction
#'
#' @param final_pred a numerical dataset containing the predictor values of a regression function
#' @param m the number of rows in the dataset
#' @param cutoff a value designating the cut off value between prediction conditions (default is 0.5)
#'
#' @return a matrix containing predicted y data
#' @export
#'
#' @examples
#' data=read.csv("iris_csv.csv")
#' data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
#' data$class=ifelse(data$class=="Iris-setosa",1,0)

#' n_features=ncol(data)
#' dt = sort(sample(nrow(data), nrow(data)*.7))
#' train<-data[dt,]
#' test<-data[-dt,]
#' x=as.matrix(train[,1:n_features-1])
#' y=train[,n_features]
#' xtest=test[,1:n_features-1]
#' ytest=test[,n_features]
#'
#' init=weightInitialization(x,y)
#' x1 <- cbind(1, x)
#' predict=optim(par=init, fn = cost.glm, method='CG',
#'               X=x1)
#'
#'coeff=unlist(predict[1])
#'names(coeff)=c("intercept",colnames(x))
#'cost=unlist(predict[2])

#'w=coeff[2:length(coeff)]
#'b=coeff[1]
#'final_train_pred = sigmoid(w%*%t(x)+b)
#'final_test_pred = sigmoid(w%*%t(xtest)+b)

#'m_tr =  nrow(x)
#'m_ts =  nrow(xtest)
#'
#'y_tr_pred = predictor(final_train_pred, m_tr,cutoff=0.5)
#'y_ts_pred = predictor(final_test_pred, m_ts)
#'
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

