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