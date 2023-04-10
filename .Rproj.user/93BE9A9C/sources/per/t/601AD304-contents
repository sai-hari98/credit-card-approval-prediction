#gender 
ccApproval$GENDER = as.factor(ccApproval$CODE_GENDER)
ccApproval$FLAG_OWN_CAR = as.factor(ccApproval$FLAG_OWN_CAR)
ccApproval$FLAG_OWN_REALTY = as.factor(ccApproval$FLAG_OWN_REALTY)
ccApproval$EDUCATION = as.factor(ccApproval$NAME_EDUCATION_TYPE)
ccApproval$HOUSING = as.factor(ccApproval$NAME_HOUSING_TYPE)
#converting age to years
ccApproval$AGE = round(abs(ccApproval$DAYS_BIRTH)/365,0)
ccApproval$DAYS_EMPLOYED = abs(ccApproval$DAYS_EMPLOYED)
ccApproval$TARGET = as.factor(ccApproval$TARGET)

levels(as.factor(ccApproval$JOB))
#chance c to p to make better meaning out of it. p means paid.
ccApproval$STATUS = ifelse(ccApproval$STATUS=='C','P',ccApproval$STATUS)
ccApproval$BEGIN_MONTHS = abs(ccApproval$BEGIN_MONTHS)

#smote
install.packages("performanceEstimation")
library("performanceEstimation")
ccApproval.smote <- smote(TARGET~.,ccApproval, perc.over=5000, perc.under=550)
