#preprocessing
source("preprocessing.R")
logit <- function(data){
  #logistic regression
  set.seed(123)
  library(rpart)
  train = sample(1:nrow(data),(2/3)*nrow(data))
  
  #removing status since it is has a direct relationship with the class to be predicted
  featuresToRemove = c("STATUS")
  data.train = data[train,!names(data) %in% featuresToRemove]
  data.test = data[-train,!names(data) %in% featuresToRemove]
  
  #nrow(data.train[data.test$TARGET=="1",])/nrow(data.train)
  
  logit.reg <- glm(TARGET ~ ., data = data.train, family = "binomial")
  
  logit.predict <- predict(logit.reg, data.test, type = "response")
  
  actual <- as.character(data.test$TARGET)
  
  #doing ROC to find the Youden point
  library(ROCit)
  roc_logit <- rocit(score = logit.predict, class = actual)
  
  result_logit = data.frame(cbind(AUC=roc_logit$AUC, Cutoff=roc_logit$Cutoff, 
                                  TPR=roc_logit$TPR, FPR=roc_logit$FPR))
  head(result_logit)
  
  result_logit$diff = result_logit$TPR - result_logit$FPR
  result_logit[which.max(result_logit[, c("diff")]), ]
  
  plot(roc_logit, YIndex = T, col = c(2,4)) # Changing color
  
  cutoff <- result_logit[which.max(result_logit[, c("diff")]), 2]
  predict <- ifelse(logit.predict>cutoff, "1","0")
  
  #57% accuracy
  library(caret)
  cm <- table(predict, actual)
  confusionMatrix(cm)
}

logit(ccApproval)
cat('\014')
#notes for improvement
#svm
#precision recall curve
#test set 
#stratified sampling
