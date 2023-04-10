ccApproval = read.csv("credit_card_approval.csv")

save(ccApproval, "ccApproval.RDS")

summary(ccApproval)

library(ggplot2)
ggplot(ccApproval,aes(AMT_INCOME_TOTAL,NAME_EDUCATION_TYPE))+geom_boxplot()

eduLevels = as.factor(ccApproval$NAME_EDUCATION_TYPE)
levels(eduLevels)

academicSal = ccApproval[ccApproval$NAME_EDUCATION_TYPE=="Academic degree",]
riskUsers = ccApproval[ccApproval$TARGET==1,]
(nrow(riskUsers)/nrow(ccApproval))*100
#0.03 percent users are risk users. need undersampling/oversampling?

ggplot(ccApproval,aes(abs(DAYS_BIRTH),AMT_INCOME_TOTAL))+geom_point(aes(color=NAME_EDUCATION_TYPE))
1962/537667
min(academicSal$AMT_INCOME_TOTAL)
max(academicSal$AMT_INCOME_TOTAL)
max(academicSal$AMT_INCOME_TOTAL)

meanDaysEmployed = mean(ccApproval$DAYS_EMPLOYED)
sdDaysEmployed = sd(ccApproval$DAYS_EMPLOYED)

normalizedVal = data.frame(DaysEmployed = (ccApproval$DAYS_EMPLOYED - meanDaysEmployed)/sdDaysEmployed)
summary(normalizedVal)


#do clustering to see if I can apply smote oversampling. 
# if there is a overlap of the clusters, then there is a chance that the new records created might be ambiguous
