# y_ts_pred = predictor(final_test_pred, m_ts)
# confusion_matrix=confusionMatrix(as.factor(y_ts_pred), as.factor(ytest))
# confusion_matrix
#

#' Confusion Matrix and Metric Plot
#'
#' @param y_prob the predicted y values of the test dataset
#' @param ytest the numeric y values of the test dataset
#'
#' @return a plot of the confusion matrix with a range of cutoff values
#' @export
#'
#' @examples
#' data=read.csv("iris_csv.csv")
#' data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
#' data$class=ifelse(data$class=="Iris-setosa",1,0)
#'
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
#'
#'coeff=unlist(predict[1])
#'names(coeff)=c("intercept",colnames(x))
#'cost=unlist(predict[2])
#'w=coeff[2:length(coeff)]
#'b=coeff[1]
#'
#'final_train_pred = sigmoid(w%*%t(x)+b)
#'final_test_pred = sigmoid(w%*%t(xtest)+b)
#'
#' metricplot(final_test_pred,ytest,metric="Accuracy")
#'
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

# metricplot(final_test_pred,ytest,metric="Sensitivity")
