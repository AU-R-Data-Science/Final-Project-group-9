#' Title bootstrap CI
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


