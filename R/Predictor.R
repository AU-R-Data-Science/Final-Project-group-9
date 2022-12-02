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

