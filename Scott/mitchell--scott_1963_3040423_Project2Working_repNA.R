## PROJECT 2 WORKING - REPLACE NAs ##


cs_data = read.csv(file.choose()) ## file: cs-training_formatted_repNA.csv

head(cs_data)
summary(cs_data)
str(cs_data)


cs_data$SD2_f = as.factor(cs_data$SeriousDlqin2yrs) ##need as factor for EDA


library(ggplot2) ## EDA
library(scales) ## for scaling y-axis
library(plyr)
library(gridExtra) ##for plotting qplots on the same grid

##EDA on quantitative variables
attach(cs_data)
## x var comes 1st, y var comes 2nd

##Graph debt ratio levels
require(gridExtra)
DR_plot1 = qplot(DebtRatio, data = subset(cs_data,DebtRatio<=1), fill = SD2_f, 
                 breaks=c(0,.25,.5,.75,1), main="Debt Ratio Freq (DR <=1)")
DR_plot2 = qplot(DebtRatio, data = subset(cs_data,DebtRatio>1 & DebtRatio<=2), fill = SD2_f, 
                 breaks=c(1,1.25,1.5,1.75,2), main="Debt Ratio Freq (1<DR<=2)")
DR_plot3 = qplot(DebtRatio, data = subset(cs_data,DebtRatio>2 & DebtRatio<=10), fill = SD2_f, 
                 breaks=c(2,3,4,5,6,7,8,9,10), main="Debt Ratio Freq (2<DR<=10)")
DR_plot4 = qplot(DebtRatio, data = subset(cs_data,DebtRatio>10 & DebtRatio<=100), fill = SD2_f, 
                 breaks=c(10,25,40,60,80,100),main="Debt Ratio Freq (10<DR<=100)")
DR_plot5 = qplot(DebtRatio, data = subset(cs_data,DebtRatio>100), fill = SD2_f, 
                 main="Debt Ratio Freq (DR>100)")
grid.arrange(DR_plot1, DR_plot2, DR_plot3, ncol=3)
grid.arrange(DR_plot4, DR_plot5, ncol=2)


##Graph Revolving Utilization
require(gridExtra)
RUUL_plot1= qplot(RevolvingUtilizationOfUnsecuredLines, data =subset(cs_data,RevolvingUtilizationOfUnsecuredLines <= 0.25), 
                  fill = SD2_f, breaks=c(0,.05,.1,.15,.2,.25),main="RUUL Freq (0<RUUL<=0.25)")
RUUL_plot2= qplot(RevolvingUtilizationOfUnsecuredLines, data =subset(cs_data,RevolvingUtilizationOfUnsecuredLines > 0.25 &
                                                                       RevolvingUtilizationOfUnsecuredLines <= 1), 
                  fill = SD2_f, breaks=c(.25,.5,.75,1),main="RUUL Freq (0.25<RUUL<=1)")
RUUL_plot3= qplot(RevolvingUtilizationOfUnsecuredLines, data =subset(cs_data,RevolvingUtilizationOfUnsecuredLines > 1 &
                                                                       RevolvingUtilizationOfUnsecuredLines <= 2), 
                  fill = SD2_f, breaks=c(1,1.25,1.5,1.75,2),main="RUUL Freq (1<RUUL<=2)")
RUUL_plot4 = qplot(RevolvingUtilizationOfUnsecuredLines, data =subset(cs_data,RevolvingUtilizationOfUnsecuredLines > 2 &
                                                                        RevolvingUtilizationOfUnsecuredLines <= 10), 
                   fill = SD2_f, breaks=c(2,4,6,8,10),main="RUUL Freq (2<RUUL<=10)")
RUUL_plot5 = qplot(RevolvingUtilizationOfUnsecuredLines, data =subset(cs_data,RevolvingUtilizationOfUnsecuredLines > 10), 
                   fill = SD2_f,main="RUUL Freq (RUUL>=10)")
grid.arrange(RUUL_plot1, RUUL_plot2, RUUL_plot3, ncol=3)
grid.arrange(RUUL_plot4, RUUL_plot5, ncol=2)

##Graph Number Time 30-59 Past Due
require(gridExtra)
PD1_plot1 = qplot(NumberOfTime30.59DaysPastDueNotWorse, data = subset(cs_data,NumberOfTime30.59DaysPastDueNotWorse<=2), 
                 fill = SD2_f,main="PD1 Freq (PD<=2)")+
  scale_y_continuous(labels=comma)+scale_x_continuous(breaks=1:2)
PD1_plot2 = qplot(NumberOfTime30.59DaysPastDueNotWorse, data = subset(cs_data,NumberOfTime30.59DaysPastDueNotWorse>2 &
                                                                       NumberOfTime30.59DaysPastDueNotWorse<=20), 
                 fill = SD2_f,breaks=c(2,5,8,11,14,17,20),main="PD1 Freq (2<PD<=20)")
PD1_plot3 = qplot(NumberOfTime30.59DaysPastDueNotWorse, data = subset(cs_data,NumberOfTime30.59DaysPastDueNotWorse>20 &
                                                                       NumberOfTime30.59DaysPastDueNotWorse<=100), 
                 fill = SD2_f,breaks=c(20,40,60,80,100),main="PD1 Freq (20<PD<=100)")
grid.arrange(PD1_plot1, PD1_plot2, PD1_plot3, ncol=3)

##Graph Number Time 60-89 Past Due
require(gridExtra)
PD2_plot1 = qplot(NumberOfTime60.89DaysPastDueNotWorse, data = subset(cs_data,NumberOfTime60.89DaysPastDueNotWorse<=2), 
                  fill = SD2_f,main="PD2 Freq (PD<=2")+
  scale_y_continuous(labels=comma)+scale_x_continuous(breaks=1:2)
PD2_plot2 = qplot(NumberOfTime60.89DaysPastDueNotWorse, data = subset(cs_data,NumberOfTime60.89DaysPastDueNotWorse>2 &
                                                                        NumberOfTime60.89DaysPastDueNotWorse<=20), 
                  fill = SD2_f,breaks=c(2,5,8,11,14,17,20),main="PD2 Freq (2<PD<=20)")
PD2_plot3 = qplot(NumberOfTime60.89DaysPastDueNotWorse, data = subset(cs_data,NumberOfTime60.89DaysPastDueNotWorse>20 &
                                                                        NumberOfTime60.89DaysPastDueNotWorse<=100), 
                  fill = SD2_f,breaks=c(20,40,60,80,100),main="PD2 Freq (20<PD<=100)")
grid.arrange(PD2_plot1, PD2_plot2, PD2_plot3, ncol=3)

##Graph Number Time 90+ Past Due
require(gridExtra)
PD3_plot1 = qplot(NumberOfTimes90DaysLate, data = subset(cs_data,NumberOfTimes90DaysLate<=2), 
                  fill = SD2_f,main="PD3 Freq (PD<=2")+
  scale_y_continuous(labels=comma)+scale_x_continuous(breaks=1:2)
PD3_plot2 = qplot(NumberOfTimes90DaysLate, data = subset(cs_data,NumberOfTimes90DaysLate>2 &
                                                                        NumberOfTimes90DaysLate<=20), 
                  fill = SD2_f,breaks=c(2,5,8,11,14,17,20),main="PD3 Freq (2<PD<=20)")
PD3_plot3 = qplot(NumberOfTimes90DaysLate, data = subset(cs_data,NumberOfTimes90DaysLate>20 &
                                                                        NumberOfTimes90DaysLate<=100), 
                  fill = SD2_f,breaks=c(20,40,60,80,100),main="PD3 Freq (20<PD<=100)")
grid.arrange(PD3_plot1, PD3_plot2, PD3_plot3, ncol=3)


##Graph Monthly income

##99,999,999 used for NA values
require(gridExtra)
MI_plot1 = qplot(MonthlyIncome, data = cs_data, fill = SD2_f, main="Mo Inc Freq - Val v NA")+scale_y_continuous(labels=comma)+
  scale_x_continuous(labels=comma)
MI_plot2 = qplot(MonthlyIncome, data = subset(cs_data,MonthlyIncome<10000), fill = SD2_f, 
      breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),
      main="Mo Inc Freq (MI<10,000)")+scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)
MI_plot3 = qplot(MonthlyIncome, data = subset(cs_data,MonthlyIncome>=10000 & MonthlyIncome<20000), fill = SD2_f, 
      breaks=c(10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000),
      main="Mo Inc Freq (10k<=MI<20k)")+scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)
MI_plot4 = qplot(MonthlyIncome, data = subset(cs_data,MonthlyIncome>=20000 & MonthlyIncome<50000), fill = SD2_f, 
      breaks=c(20000,25000,30000,35000,40000,45000,50000),
      main="Mo Inc Freq (20k<=MI<50k)")+scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)
MI_plot5 = qplot(MonthlyIncome, data = subset(cs_data,MonthlyIncome>=50000 & MonthlyIncome<950000), fill = SD2_f, 
      breaks=c(50000,150000,250000,350000,450000,550000,650000,750000,850000,950000),
      main="Mo Inc Freq (50k<=MI<950k)")+scale_y_continuous(labels=comma)+scale_x_continuous(labels=comma)
grid.arrange(MI_plot1, MI_plot2, MI_plot3, ncol=3)
grid.arrange(MI_plot4, MI_plot5, ncol=2)

##Open Credit Lines & Loans

require(gridExtra)
OCL_plot1 = qplot(NumberOfOpenCreditLinesAndLoans, data = cs_data, fill = SD2_f, main="OCL Freq")
OCL_plot2 = qplot(NumberOfOpenCreditLinesAndLoans, data = subset(cs_data,NumberOfOpenCreditLinesAndLoans<10), 
                fill = SD2_f, breaks=c(0,1,2,3,4,5,6,7,8,9,10),main="OCL Freq (OCL<10)")+
                scale_y_continuous(labels=comma)
OCL_plot3 = qplot(NumberOfOpenCreditLinesAndLoans, 
                  data = subset(cs_data,NumberOfOpenCreditLinesAndLoans>=10 & NumberOfOpenCreditLinesAndLoans<20), 
                  fill = SD2_f, breaks=c(11,12,13,14,15,16,17,18,19,20),main="OCL Freq (10<=OCL<20)")+
                  scale_y_continuous(labels=comma)
OCL_plot4 = qplot(NumberOfOpenCreditLinesAndLoans, 
                  data = subset(cs_data,NumberOfOpenCreditLinesAndLoans>=20), 
                  fill = SD2_f, breaks=c(20,25,30,35,40,45,50,55,60),main="OCL Freq (OCL>=20)")+
                  scale_y_continuous(labels=comma)
grid.arrange(OCL_plot1, OCL_plot2, ncol=2)
grid.arrange(OCL_plot3, OCL_plot4, ncol=2)

##Real Estate Loans or Lines

require(gridExtra)
REL_plot1 = qplot(NumberRealEstateLoansOrLines, data = cs_data, fill = SD2_f, main="REL Freq")
REL_plot2 = qplot(NumberRealEstateLoansOrLines, data = subset(cs_data,NumberRealEstateLoansOrLines<10), 
                  fill = SD2_f, breaks=c(0,1,2,3,4,5,6,7,8,9,10),main="REL Freq (REL<10)")+
  scale_y_continuous(labels=comma)
REL_plot3 = qplot(NumberRealEstateLoansOrLines, 
                  data = subset(cs_data,NumberRealEstateLoansOrLines>=10 & NumberRealEstateLoansOrLines<20), 
                  fill = SD2_f, breaks=c(11,12,13,14,15,16,17,18,19,20),main="REL Freq (10<=REL<20)")+
  scale_y_continuous(labels=comma)
REL_plot4 = qplot(NumberRealEstateLoansOrLines, 
                  data = subset(cs_data,NumberRealEstateLoansOrLines>=20), 
                  fill = SD2_f, breaks=c(20,25,30,35,40,45,50,55,60),main="REL Freq (REL>=20)")+
  scale_y_continuous(labels=comma)
grid.arrange(REL_plot1, REL_plot2, ncol=2)
grid.arrange(REL_plot3, REL_plot4, ncol=2)

##Number of Dependents

require(gridExtra)
DEP_plot1 = qplot(NumberOfDependents, data = cs_data, fill = SD2_f, breaks=c(0,25,50,75,100),main="DEP Freq")
DEP_plot2 = qplot(NumberOfDependents, data = subset(cs_data,NumberOfDependents<4), 
                  fill = SD2_f, breaks=c(0,1,2,3,4),main="DEP Freq (DEP<4)")+
  scale_y_continuous(labels=comma)
DEP_plot3 = qplot(NumberOfDependents, 
                  data = subset(cs_data,NumberOfDependents>=4 & NumberOfDependents<7), 
                  fill = SD2_f, breaks=c(4,5,6,7),main="DEP Freq (4<=DEP<7)")+
  scale_y_continuous(labels=comma)
DEP_plot4 = qplot(NumberOfDependents, 
                  data = subset(cs_data,NumberOfDependents>=7 & NumberOfDependents<99), 
                  fill = SD2_f, breaks=c(7,9,11,13,15,17,19,21),main="DEP Freq (DEP>=7)")+
  scale_y_continuous(labels=comma)
grid.arrange(DEP_plot1, DEP_plot2, ncol=2)
grid.arrange(DEP_plot3, DEP_plot4, ncol=2)

##Age

require(gridExtra)

## 999 used for NA values
AGE_plot1 = qplot(age, data = subset(cs_data,age<999), breaks=c(20,30,40,50,60,70,80,90,100,110),
                  fill = SD2_f, main="AGE Freq")
AGE_plot2 = qplot(age, data = subset(cs_data,age>=21 & age<30), 
                  fill = SD2_f, breaks=c(21,23,25,27,29,31),main="AGE Freq (AGE<30)")+
  scale_y_continuous(labels=comma)
AGE_plot3 = qplot(age, 
                  data = subset(cs_data,age>=30 & age<50), 
                  fill = SD2_f, breaks=c(30,35,40,45,50),main="AGE Freq (30<=AGE<50)")+
  scale_y_continuous(labels=comma)
AGE_plot4 = qplot(age, 
                  data = subset(cs_data,age>=50), 
                  fill = SD2_f, breaks=c(50,60,70,80,90,100,110),main="AGE Freq (AGE>=50)")+
  scale_y_continuous(labels=comma)
grid.arrange(AGE_plot1, AGE_plot2, ncol=2)
grid.arrange(AGE_plot3, AGE_plot4, ncol=2)

#Tree libraries
library(tree)
##library(ISLR)
library(ggplot2)
##library(rpart)
##library(rpart.plot)
##library(C50)

#forest libraries
library(ElemStatLearn)
library(MASS)
library(randomForest)

#gradient boosting library
library(gbm)

#library for AUC
library(pROC)

length(cs_data$SeriousDlqin2yrs[cs_data$SeriousDlqin2yrs == 1]) /nrow(cs_data) ## only 6.7% of data had Serious Delinquency

set.seed(24)
train_ind = sample(nrow(cs_data), nrow(cs_data)/2)
train_cs_data = cs_data[train_ind,]
test_cs_data = cs_data[-train_ind,]

str(train_cs_data)
str(test_cs_data)


##MODELS##
###REGRESSION TREE###
reg_tree_SD2 = tree(SeriousDlqin2yrs~.-SD2_f, train_cs_data)
##above, don't use the factor version (SD2_f) as a predictor


plot(reg_tree_SD2)
text(reg_tree_SD2, pretty=0)
summary(reg_tree_SD2)

##CLASSIFICATION TREE DOESN'T WORK###
reg_tree_SD2_class = tree(SD2_f~.-SeriousDlqin2yrs, train_cs_data)
##above, don't use the numeric version (SeriousDlqin2yrs) as a predictor   

plot(reg_tree_SD2_class)
text(reg_tree_SD2_class, pretty=0)
summary(reg_tree_SD2_class)


##remove the 'SD2_f' and 'line' input from the dataset
head(train_cs_data)
str(train_cs_data)
train_cs_data_2 = train_cs_data[,-13]
train_cs_data_2 = train_cs_data_2[,-1]
str(train_cs_data_2)
str(test_cs_data)
test_cs_data_2 = test_cs_data[,-13]
test_cs_data_2 = test_cs_data_2[,-1]
str(test_cs_data_2)

###BAGGING### 
# Random forest help: http://www.inside-r.org/packages/cran/randomForest/docs/randomForest
bag_SD2 = randomForest(SeriousDlqin2yrs~., data = train_cs_data_2, mtry=10, importance=TRUE, ntree=250) 
print(bag_SD2)
round(importance(bag_SD2),2)

#ntree is optional parameter


##RANDOM FOREST##
rf_SD2 = randomForest(SeriousDlqin2yrs~., data = train_cs_data_2, mtry=sqrt(10), importance=TRUE, ntree=250) #ntree is optional parameter 
print(rf_SD2)
round(importance(rf_SD2),2)

##GRADIENT BOOSTING##
??gbm

gbm_SD2 = gbm(SeriousDlqin2yrs~., data = train_cs_data_2,
              n.trees=250, interaction.depth=5, shrinkage=0.2, distribution="bernoulli") ##default distribution
print(gbm_SD2)
summary(gbm_SD2)
gbm.perf(gbm_SD2, plot.it=FALSE)  ##helps w/ decision on optimal number of trees to build

##PREDICTIONS##

##import kaggle test data
kag_test_cs_data = read.csv(file.choose()) ## file: cs-training_formatted_repNA.csv
kag_test_cs_data$SD2_f = as.factor(kag_test_cs_data$SeriousDlqin2yrs) ##just to conform to training format
str(kag_test_cs_data)

##DECISION TREE##
reg_tree_pred_SD2 = predict(reg_tree_SD2, test_cs_data)
str(reg_tree_pred_SD2)
str(test_cs_data)
summary(reg_tree_pred_SD2)
auc(test_cs_data$SeriousDlqin2yrs,reg_tree_pred_SD2)

#kaggle test
kag_reg_tree_pred_SD2 = predict(reg_tree_SD2, kag_test_cs_data)
summary(kag_reg_tree_pred_SD2)
str(kag_reg_tree_pred_SD2)
length(kag_reg_tree_pred_SD2)

##BAGGING##
bagging_pred_SD2 = predict(bag_SD2, test_cs_data_2)
str(bagging_pred_SD2)
str(test_cs_data_2)
summary(bagging_pred_SD2)
auc(test_cs_data_2$SeriousDlqin2yrs, bagging_pred_SD2)

#kaggle test
kag_bagging_pred_SD2 = predict(bag_SD2, kag_test_cs_data)
summary(kag_bagging_pred_SD2)


##RANDOM FOREST##
rf_pred_SD2 = predict(rf_SD2, test_cs_data_2)
summary(rf_pred_SD2)
auc(test_cs_data_2$SeriousDlqin2yrs, rf_pred_SD2)

#kaggle test
kag_rf_pred_SD2 = predict(rf_SD2, kag_test_cs_data)
summary(kag_rf_pred_SD2)

##GRADIENT BOOSTING##
gbm_prob_pred_SD2 = predict(gbm_SD2, test_cs_data_2, n.trees=13, type="response") ##change the number of trees based on gbm.perf
summary(gbm_prob_pred_SD2)
auc(test_cs_data_2$SeriousDlqin2yrs, gbm_prob_pred_SD2)

#kaggle test
kag_gbm_pred_SD2 = predict(gbm_SD2, kag_test_cs_data, n.trees=13, type="response")
summary(kag_gbm_pred_SD2)

##WRITE KAGGLE PREDICTIONS TO CSV
write.csv(kag_reg_tree_pred_SD2, file = 'C:/Users/smitchell/Documents/Predictive Analytics/Project2/kag_tree.csv')
write.csv(kag_bagging_pred_SD2, file = 'C:/Users/smitchell/Documents/Predictive Analytics/Project2/kag_bagging.csv')
write.csv(kag_rf_pred_SD2, file = 'C:/Users/smitchell/Documents/Predictive Analytics/Project2/kag_rf.csv')
write.csv(kag_gbm_pred_SD2, file = 'C:/Users/smitchell/Documents/Predictive Analytics/Project2/kag_gbm.csv')
       




