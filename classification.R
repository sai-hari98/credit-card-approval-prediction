set.seed(123)
library(rpart)
train = sample(1:nrow(ccApproval),(2/3)*nrow(ccApproval))
ccApproval.train = ccApproval[train,]
ccApproval.test = ccApproval[-train,]

decisionTree <- rpart(TARGET ~ ., ccApproval.train, method="class",
                        control = rpart.control(xval=0, minsplit = 1000))
decisionTree1 <- rpart(TARGET ~ ., ccApproval.train, method="class",
                      control = rpart.control(xval=10, minsplit=2, cp=0))
decisionTree
library(rpart.plot)
rpart.plot(decisionTree, type = 1, extra = 1, main="Classification Tree for Risk Users") 
rpart.plot(decisionTree1, type = 1, extra = 1, main="Classification Tree for Risk Users") 

prediction <- predict(decisionTree, ccApproval.test, type="class")
prediction1 <- predict(decisionTree1, ccApproval.test, type="class")

actual <- ccApproval.test$TARGET

cm <- table(prediction1,actual)

cm

accuracy <- (cm[1,1]+cm[2,2])/length(actual)
accuracy

#decision tree is getting a 100% accuracy based on the status attribute

