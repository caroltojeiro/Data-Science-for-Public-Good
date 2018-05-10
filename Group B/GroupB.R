library("ggplot2")
load("dataABC.RData")
##########################################
Data<-dataABC
Data1<-Data[-1,c("LandUse","Total.Taxes.Owed", "AssessedValue", "Num.Open.Violations", "VacantBuilding", "WaterService", "YearBuilt", "Total", "Vacant_Pr", "Household_1", "Household_2", "Owner_Occupied" )]
Data1<-Data[-1,c("LandUse","Total.Taxes.Owed",)]


Syr_17_18_1<-Data1


str(Syr_17_18_1)
Syr_17_18_1$Occupied_Pr<-as.numeric(gsub("","",Syr_17_18_1$Occupied_Pr))
Syr_17_18_1$Vacant_Pr<-as.numeric(gsub("","",Syr_17_18_1$Vacant_Pr))
Syr_17_18_1$Household_1<-as.numeric(gsub("","",Syr_17_18_1$Household_1))
Syr_17_18_1$Household_2<-as.numeric(gsub("","",Syr_17_18_1$Household_2))
Syr_17_18_1$Owner_Occupied<-as.numeric(gsub("","",Syr_17_18_1$Owner_Occupied))
Syr_17_18_1$VacancyStatus_A<-as.numeric(gsub("","",Syr_17_18_1$VacancyStatus_A))

table(is.na(Syr_17_18_1))
colSums(is.na(Syr_17_18_1))
Syr_17_18_1$Num.Open.Violations[is.na(Syr_17_18_1$Num.Open.Violations)]<-median(Syr_17_18_1$Num.Open.Violations, na.rm = TRUE)
table(is.na(Syr_17_18_1$Num.Open.Violations))
Syr_17_18_1$YearBuilt[is.na(Syr_17_18_1$YearBuilt)]<-median(Syr_17_18_1$YearBuilt, na.rm = TRUE)
table(is.na(Syr_17_18_1$YearBuilt))
Syr_17_18_1$Occupied_Pr[is.na(Syr_17_18_1$Occupied_Pr)]<-median(Syr_17_18_1$Occupied_Pr, na.rm = TRUE)
table(is.na(Syr_17_18_1$Occupied_Pr))
Syr_17_18_1$Vacant_Pr[is.na(Syr_17_18_1$Vacant_Pr)]<-median(Syr_17_18_1$Vacant_Pr, na.rm = TRUE)
table(is.na(Syr_17_18_1$Vacant_Pr))
Syr_17_18_1$Household_1[is.na(Syr_17_18_1$Household_1)]<-median(Syr_17_18_1$Household_1, na.rm = TRUE)
table(is.na(Syr_17_18_1$Household_1))
Syr_17_18_1$Household_2[is.na(Syr_17_18_1$Household_2)]<-median(Syr_17_18_1$Household_2, na.rm = TRUE)
table(is.na(Syr_17_18_1$Household_2))
Syr_17_18_1$Owner_Occupied[is.na(Syr_17_18_1$Owner_Occupied)]<-median(Syr_17_18_1$Owner_Occupied, na.rm = TRUE)
table(is.na(Syr_17_18_1$Owner_Occupied))
Syr_17_18_1$VacancyStatus_A[is.na(Syr_17_18_1$VacancyStatus_A)]<-median(Syr_17_18_1$VacancyStatus_A, na.rm = TRUE)
table(is.na(Syr_17_18_1$VacancyStatus_A))



dim(Syr_17_18_1)
train=sample(1:nrow(Syr_17_18_1),11000)

set.seed(10)

install.packages("randomForest")
library("randomForest")


rf<-randomForest(Syr_17_18_1$Vacant_Pr ~ ., data = Syr_17_18_1, subset = train, importance = TRUE, ntree = 1000, keep.forest = TRUE)
#Call:
#  randomForest(formula = Syr_17_18_1$Vacant_Pr ~ ., data = Syr_17_18_1,      importance = TRUE, ntree = 1000, keep.forest = TRUE, subset = train) 
#Type of random forest: regression
#Number of trees: 1000
#No. of variables tried at each split: 4

#Mean of squared residuals: 5.453685e-06
#% Var explained: 99.96


varImpPlot(rf)

which.min(rf$mse)
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
Syr_17_18_1_new.pred.forest <- predict(rf,Syr_17_18_1)
print(Syr_17_18_1_new.pred.forest)
table2<-data.frame(Syr_17_18_1_new.pred.forest,Syr_17_18_1)
sum(table2$Syr_17_18_1_new.pred.forest>0.2 & table2$Syr_17_18_1_new.pred.forest<0.4 ) #1953
sum(table2$Syr_17_18_1_new.pred.forest>0.4 & table2$Syr_17_18_1_new.pred.forest<0.6) #313
sum(table2$Syr_17_18_1_new.pred.forest>0.6 & table2$Syr_17_18_1_new.pred.forest<0.8) #44
sum(table2$Syr_17_18_1_new.pred.forest>0.8 & table2$Syr_17_18_1_new.pred.forest<0.9) #11
sum(table2$Syr_17_18_1_new.pred.forest>0.9) #84

table2$decision<-"Percentage Vacant"
table2$decision[table2$Syr_17_18_1_new.pred.forest<0.2]<-"0%~20% Vacant"
table2$decision[table2$Syr_17_18_1_new.pred.forest>0.2 & table2$Syr_17_18_1_new.pred.forest<0.4]<-"20%~40% Vacant"
table2$decision[table2$Syr_17_18_1_new.pred.forest>0.4 & table2$Syr_17_18_1_new.pred.forest<0.6]<-"40%~60% Vacant"
table2$decision[table2$Syr_17_18_1_new.pred.forest>0.6 & table2$Syr_17_18_1_new.pred.forest<0.8]<-"60%~80% Vacant"
table2$decision[table2$Syr_17_18_1_new.pred.forest>0.8 & table2$Syr_17_18_1_new.pred.forest<0.9]<-"80%~90% Vacant"
table2$decision[table2$Syr_17_18_1_new.pred.forest>0.9]<-"90%~100% Vacant"

table2$Yes_No<-"No"
table2$Yes_No[table2$Syr_17_18_1_new.pred.forest<0.2]<-"No"
table2$Yes_No[table2$Syr_17_18_1_new.pred.forest>0.2 & table2$Syr_17_18_1_new.pred.forest<0.4]<-"Prelimineray Stage"
table2$Yes_No[table2$Syr_17_18_1_new.pred.forest>0.4 & table2$Syr_17_18_1_new.pred.forest<0.6]<-"Vulnerable"
table2$Yes_No[table2$Syr_17_18_1_new.pred.forest>0.6 & table2$Syr_17_18_1_new.pred.forest<0.8]<-"Yes"
table2$Yes_No[table2$Syr_17_18_1_new.pred.forest>0.8 & table2$Syr_17_18_1_new.pred.forest<0.9]<-"Yes"
table2$Yes_No[table2$Syr_17_18_1_new.pred.forest>0.9]<-"Yes"

sum(table2$decision=="0%~20% Vacant")#12860
sum(table2$decision=="20%~40% Vacant")#1953
sum(table2$decision=="40%~60% Vacant")#313
sum(table2$decision=="60%~80% Vacant")#44
sum(table2$decision=="80%~90% Vacant")#11
sum(table2$decision=="90%~100% Vacant")#84

sum(table2$Yes_No=="No")#12860
sum(table2$Yes_No=="Preliminary Stage")#1953
sum(table2$Yes_No=="Vulnerable")#313
sum(table2$Yes_No=="Yes")#139



Yes_No = data.frame(Status =factor(c("No (0%~20% Vacant)", "Preliminary Stage (20%~40% Vacant)", "Vulnerable (40%~60% Vacant)", "Yes (60%~100% Vacant)"), levels = c("No (0%~20% Vacant)", "Preliminary Stage (20%~40% Vacant)", "Vulnerable (40%~60% Vacant)", "Yes (60%~100% Vacant)")), Vacant_rate = c(12860, 1953, 313, 139))
colors = c("red", "blue", "green",  "purple")
Yes_No <-ggplot(Yes_No, aes(Status, Vacant_rate))
Yes_No+geom_bar(stat="identity",fill=colors)+geom_text(aes(label=Vacant_rate),
                                                      vjust=-0.25)+ggtitle("Vacancy Decision")






####################################################################


library("ggplot2")
Data <- read.csv(choose.files(), header = T)
Data1<-Data[-1,c("LandUse","Total.Taxes.Owed", "AssessedValue", "Num.Open.Violations", "VacantBuilding", "WaterService", "YearBuilt", "Total", "Occupied_Pr", "Vacant_Pr", "Household_1", "Household_2", "Owner_Occupied", "VacancyStatus_A" )]
Syr_17_18_1<-Data1

str(Syr_17_18_1)
Syr_17_18_1$Occupied_Pr<-as.numeric(gsub("","",Syr_17_18_1$Occupied_Pr))
Syr_17_18_1$Vacant_Pr<-as.numeric(gsub("","",Syr_17_18_1$Vacant_Pr))
Syr_17_18_1$Household_1<-as.numeric(gsub("","",Syr_17_18_1$Household_1))
Syr_17_18_1$Household_2<-as.numeric(gsub("","",Syr_17_18_1$Household_2))
Syr_17_18_1$Owner_Occupied<-as.numeric(gsub("","",Syr_17_18_1$Owner_Occupied))
Syr_17_18_1$VacancyStatus_A<-as.numeric(gsub("","",Syr_17_18_1$VacancyStatus_A))

table(is.na(Syr_17_18_1))
colSums(is.na(Syr_17_18_1))
Syr_17_18_1$Num.Open.Violations[is.na(Syr_17_18_1$Num.Open.Violations)]<-median(Syr_17_18_1$Num.Open.Violations, na.rm = TRUE)
table(is.na(Syr_17_18_1$Num.Open.Violations))
Syr_17_18_1$YearBuilt[is.na(Syr_17_18_1$YearBuilt)]<-median(Syr_17_18_1$YearBuilt, na.rm = TRUE)
table(is.na(Syr_17_18_1$YearBuilt))
Syr_17_18_1$Occupied_Pr[is.na(Syr_17_18_1$Occupied_Pr)]<-median(Syr_17_18_1$Occupied_Pr, na.rm = TRUE)
table(is.na(Syr_17_18_1$Occupied_Pr))
Syr_17_18_1$Vacant_Pr[is.na(Syr_17_18_1$Vacant_Pr)]<-median(Syr_17_18_1$Vacant_Pr, na.rm = TRUE)
table(is.na(Syr_17_18_1$Vacant_Pr))
Syr_17_18_1$Household_1[is.na(Syr_17_18_1$Household_1)]<-median(Syr_17_18_1$Household_1, na.rm = TRUE)
table(is.na(Syr_17_18_1$Household_1))
Syr_17_18_1$Household_2[is.na(Syr_17_18_1$Household_2)]<-median(Syr_17_18_1$Household_2, na.rm = TRUE)
table(is.na(Syr_17_18_1$Household_2))
Syr_17_18_1$Owner_Occupied[is.na(Syr_17_18_1$Owner_Occupied)]<-median(Syr_17_18_1$Owner_Occupied, na.rm = TRUE)
table(is.na(Syr_17_18_1$Owner_Occupied))
Syr_17_18_1$VacancyStatus_A[is.na(Syr_17_18_1$VacancyStatus_A)]<-median(Syr_17_18_1$VacancyStatus_A, na.rm = TRUE)
table(is.na(Syr_17_18_1$VacancyStatus_A))

library(kernlab)
library("ggplot2")
library(e1071)
library(gridExtra)

dim(Syr_17_18_1)
new_randIndex<-sample(1:dim(Syr_17_18_1)[1])
summary(new_randIndex)
length(new_randIndex)
head(new_randIndex)
new_cutPoint2_3<-floor(2*dim(Syr_17_18_1)[1]/3)
new_cutPoint2_3
new_trainData<-Syr_17_18_1[new_randIndex[1:new_cutPoint2_3],]
new_testData<-Syr_17_18_1[new_randIndex[(new_cutPoint2_3+1):dim(Syr_17_18_1)[1]],]

str(new_trainData)
str(new_testData)
new_model<-ksvm(Vacant_Pr~.,data=new_trainData)
new_model
#Support Vector Machine object of class "ksvm" 

#SV type: eps-svr  (regression) 
#parameter : epsilon = 0.1  cost C = 1 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  0.172506269765634 

#Number of Support Vectors : 722 

#Objective Function Value : -224.9327 
#Training error : 0.020328 

new_Pred<-predict(new_model,new_testData)
new_Pred
table<-data.frame(new_Pred,new_testData$Vacant_Pr)
table
new_testData<-cbind.data.frame(new_testData, new_Pred)
sum(table$new_Pred>0.2 & table$new_Pred<0.4) #700
sum(table$new_Pred>0.4 & table$new_Pred<0.6) #98
sum(table$new_Pred>0.6 & table$new_Pred<0.8) #11
sum(table$new_Pred>0.8 & table$new_Pred<0.9) #4
sum(table$new_Pred>0.9) #24
new_testData_Decision<-new_testData
new_testData_Decision$decision<-"Percentage Vacant"
new_testData_Decision$decision[new_testData_Decision$new_Pred<0.2]<-"0%~20% Vacant"
new_testData_Decision$decision[new_testData_Decision$new_Pred>0.2 & new_testData_Decision$new_Pred<0.4]<-"20%~40% Vacant"
new_testData_Decision$decision[new_testData_Decision$new_Pred>0.4 & new_testData_Decision$new_Pred<0.6]<-"40%~60% Vacant"
new_testData_Decision$decision[new_testData_Decision$new_Pred>0.6 & new_testData_Decision$new_Pred<0.8]<-"60%~80% Vacant"
new_testData_Decision$decision[new_testData_Decision$new_Pred>0.8 & new_testData_Decision$new_Pred<0.9]<-"80%~90% Vacant"
new_testData_Decision$decision[new_testData_Decision$new_Pred>0.9]<-"90%~100% Vacant"

new_testData_Decision$Yes_No<-"No"
new_testData_Decision$Yes_No[new_testData_Decision$new_Pred<0.2]<-"No"
new_testData_Decision$Yes_No[new_testData_Decision$new_Pred>0.2 & new_testData_Decision$new_Pred<0.4]<-"Prelimineray Stage"
new_testData_Decision$Yes_No[new_testData_Decision$new_Pred>0.4 & new_testData_Decision$new_Pred<0.6]<-"Vulnerable"
new_testData_Decision$Yes_No[new_testData_Decision$new_Pred>0.6 ]<-"Yes"

sum(new_testData_Decision$decision=="0%~20% Vacant")#4291
sum(new_testData_Decision$decision=="20%~40% Vacant")#663
sum(new_testData_Decision$decision=="40%~60% Vacant")#98
sum(new_testData_Decision$decision=="60%~80% Vacant")#15
sum(new_testData_Decision$decision=="80%~90% Vacant")#3
sum(new_testData_Decision$decision=="90%~100% Vacant")#19

sum(new_testData_Decision$Yes_No=="No")#4252
sum(new_testData_Decision$Yes_No=="Preliminary Stage")#700
sum(new_testData_Decision$Yes_No=="Vulnerable")#98
sum(new_testData_Decision$Yes_No=="Yes")#39

SVM_Yes_No = data.frame(Status =factor(c("No (0%~20% Vacant)", "Preliminary Stage (20%~40% Vacant)", "Vulnerable (40%~60% Vacant)", "Yes (60%~100% Vacant)"), levels = c("No (0%~20% Vacant)", "Preliminary Stage (20%~40% Vacant)", "Vulnerable (40%~60% Vacant)", "Yes (60%~100% Vacant)")), SVM_Vacant_rate = c(4252, 700, 98, 39))
colors = c("red", "blue", "green",  "purple")
SVM_Yes_No <-ggplot(SVM_Yes_No, aes(Status, SVM_Vacant_rate))
SVM_Yes_No+geom_bar(stat="identity",fill=colors)+geom_text(aes(label=SVM_Vacant_rate),
                                                           vjust=-0.25)+ggtitle("Vacancy Decision")







































