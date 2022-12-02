
#' GLM Plot
#'
#' @param ytest the numeric y values of the test dataset
#' @param xtest the numeric x values of the test dataset
#' @param i the column of x values to be graphed
#'
#' @return a line plot of the y values and one independent variable (indicated by `i`)
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
#' glm_plot(ytest,xtest,2)
#'
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
# glm_plot(ytest,xtest,2)
#
# devtools::install_github("hadley/pkgdown")
# pkgdown::build_site("C:\Users\azk0132\OneDrive - Auburn University\Documents\GitHub\Final-Project-group-9\Logregression")
#

