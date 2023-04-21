x <- data.frame(matrix(ncol=2,nrow=0))
new_cols <- c()
for(i in colnames(ccApproval)){
  if(is.factor(ccApproval[,i])){
    new_col <- paste(i,"NUM",sep="_")
    new_cols <- append(new_cols,i)
    x <- rbind(x,data.frame(oldCol=i,newCol=new_col))
    ccApproval[,new_col] <- as.numeric(ccApproval[,i]) 
  }
}

typeof(levels(ccApproval$CODE_GENDER))

library(smotefamily)
varsToSkipForSMOTE <- append(c("STATUS","ID"),new_cols)
smote <- SMOTE(ccApproval[,!names(ccApproval) %in% varsToSkipForSMOTE],ccApproval$TARGET_NUM)
smoteData <- smote$data
x$oldCol

levels(ccApproval[,"CNT_CHILDREN_NUM"])

vectest <- strsplit(levels(ccApproval[,"CNT_CHILDREN"])," ")
vectest
vec <- strsplit(levels(ccApproval$CODE_GENDER),"\"*\"")
levels(ccApproval[,"CNT_CHILDREN"])
typeof(vec[c(1,2,1,2)])
for(i in 1:nrow(x)){
  oldCol <- x[i,"oldCol"]
  newCol <- x[i,"newCol"]
  vec <- strsplit(levels(ccApproval[,oldCol]),"\"*\"")
  smoteData[,oldCol] <- unlist(vec[smoteData[,newCol]])
}

#remove the numeric vals for smote

install.packages('ROSE')
library(ROSE)

roseData <- ROSE(TARGET~.,500000,p=0.5,seed=123)

rm(list=ls())
