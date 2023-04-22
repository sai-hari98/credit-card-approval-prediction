x <- data.frame(matrix(ncol=2,nrow=0))
for(i in colnames(ccApproval)){
  if(is.factor(ccApproval[,i])){
    new_col <- paste(i,"NUM",sep="_")
    x <- rbind(x,data.frame(oldCol=i,newCol=new_col))
    ccApproval[,new_col] <- as.numeric(ccApproval[,i]) 
  }
}
#skipping old cols since they are categorical
old_cols <- x$oldCol

library(smotefamily)
varsToSkipForSMOTE <- append(c("STATUS","ID"),old_cols)
smote <- SMOTE(ccApproval[,!names(ccApproval) %in% varsToSkipForSMOTE],ccApproval$TARGET_NUM)
smoteData <- smote$data

for(i in 1:nrow(x)){
  oldCol <- x[i,"oldCol"]
  newCol <- x[i,"newCol"]
  vec <- strsplit(levels(ccApproval[,oldCol]),"\"*\"")
  smoteData[,oldCol] <- unlist(vec[smoteData[,newCol]])
}

#remove the numeric vals for smote
smoteLogitData = smoteData[,!names(smoteData) %in% c(x$newCol,"STATUS","class")]
for(i in 1:nrow(x)){
  colName <- x[i,"oldCol"]
  if(colName != 'STATUS'){
    smoteLogitData[,colName] = as.factor(smoteLogitData[,colName]) 
  }
}

logit(smoteLogitData)

rm(list=ls())
cat('\014')
#smote and glm has better accuracy but the FNR is high is smote. almost double the rate as without using smote.


?ROSE

formula <- as.formula(TARGET ~ CODE_GENDER+FLAG_OWN_CAR+FLAG_OWN_REALTY+CNT_CHILDREN+AMT_INCOME_TOTAL+NAME_EDUCATION_TYPE
                      +NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+DAYS_EMPLOYED+JOB+BEGIN_MONTHS+AGE)
rose = ROSE(formula, data=ccApproval, N=250000, p=0.7, seed=123)
roseData = rose$data

logit(roseData)
