
library(kernlab)
library("ggplot2")
library(e1071)
library(gridExtra)
install.packages("caret")
install.packages("randomForest")
library("randomForest")
library("caret")
library("ggplot2")
##########################################
Data1 <- read.csv(choose.files(), header = T)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Data1,2,pMiss) #applying to columns

library(mice)
md.pattern(Data1) #shows the pattern of missing data

library(VIM)
aggr_plot <- aggr(Data1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Data1), cex.axis=.3,cex.numbers=0.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

library(DMwR)
Data1 <- knnImputation(Data1,3)
apply(Data1,2,pMiss)
View(Data1)

Syr_17_18_1<-Data1
str(Syr_17_18_1)

table(is.na(Syr_17_18_1))
colSums(is.na(Syr_17_18_1))

dim(Syr_17_18_1)
train=sample(1:nrow(Syr_17_18_1),nrow(Syr_17_18_1)*0.8)

set.seed(10)


##
rf<-randomForest(VacantBuilding ~  LandUse + Total.Taxes.Owed + Aggravated.assault + Arson + Robbery + Vehicle.theft+ Num.Open.Violations + AssessedValue + ZIP + YearBuilt +  Occupied_Pr + H_.1.3.P + H.4..P + Owner_Occupied, data = Syr_17_18_1, subset = train, importance = TRUE, ntree = 1000, keep.forest = TRUE, na.action = na.exclude)
print(rf)
plot(rf)
#Call:
#randomForest(formula = VacantBuilding ~ LandUse + Total.Taxes.Owed +      Num.Open.Violations + AssessedValue + ZIP + YearBuilt + Total_crimes +      Occupied_Pr + H_.1.3.P + H.4..P + Owner_Occupied, data = Syr_17_18_1,      importance = TRUE, ntree = 1000, keep.forest = TRUE, subset = train,      na.action = na.exclude) 
##Type of random forest: classification
#Number of trees: 1000
#No. of variables tried at each split: 3

#OOB estimate of  error rate: 4.39%
#Confusion matrix:
     # N   Y class.error
#N 11222 166  0.01457675
#Y   360 239  0.60100167


which.min(rf$mse)
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

#prediction in the complete data set using train set
Syr_17_18_1_new.pred.forest<- predict(rf,Syr_17_18_1)
print(Syr_17_18_1_new.pred.forest)

table2<-data.frame(Syr_17_18_1_new.pred.forest,Syr_17_18_1)
Syr_17_18_1<-data.frame(Syr_17_18_1,Syr_17_18_1_new.pred.forest)
#confusionMatrix
require("caret")
confusionMatrix(Syr_17_18_1_new.pred.forest, Syr_17_18_1$VacantBuilding)
#Confusion Matrix and Statistics
            #Reference
#Prediction   N     Y
        #N 14193    43
        #Y   138   610

#Accuracy : 0.9866          
#95% CI : (0.9846, 0.9884)
#No Information Rate : 0.9567          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.8491          
#Mcnemar's Test P-Value : 4.766e-12       
#Sensitivity : 0.9895          
#Specificity : 0.9214          
#Pos Pred Value : 0.9964          
#Neg Pred Value : 0.7995          
#Prevalence : 0.9567          
#Detection Rate : 0.9467          
#Detection Prevalence : 0.9501          
#Balanced Accuracy : 0.9555          
#'Positive' Class : N  

#total preicted Yes and No in the complete set
sum(table2$Syr_17_18_1_new.pred.forest=="N")#14331
sum(table2$Syr_17_18_1_new.pred.forest=="Y")#653

#Original Yes and No in the complete set
sum(table2$VacantBuilding=="N")#14236
sum(table2$VacantBuilding=="Y")#748

varImpPlot(rf)
vif <- importance(rf)
varImp(rf)
varImpPlot(rf)
varImpPlot(rf,sort = T, n.var = 5, main = "Top 5 variables")
varUsed(rf)

dev.off()

#variable influence barplot
bp <- barplot(t(vif/sum(vif)),las = 2)

Yes_No = data.frame(Status =factor(c("Predicted-No", "Predicted-Yes", "Actual-No", "Actual-Yes"), levels = c("Predicted-No", "Predicted-Yes", "Actual-No", "Actual-Yes")), Vacant_Properties = c(14379, 653, 14236, 748))
colors = c("red", "blue", "green",  "purple")
Yes_No <-ggplot(Yes_No, aes(Status, Vacant_Properties))
Yes_No+geom_bar(stat="identity",fill=colors)+geom_text(aes(label=Vacant_Properties),
                                                      vjust=-0.25)+ggtitle("Vacancy Decision")



####################
#######SVM

Syr_17_18_1$Vacant_Building<-0
Syr_17_18_1$Vacant_Building[Syr_17_18_1$VacantBuilding=="Y"]<-1
Syr_17_18_1$Vacant_Building[Syr_17_18_1$VacantBuilding=="N"]<-0

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

model_svm<-svm(Vacant_Building~LandUse + Total.Taxes.Owed + Num.Open.Violations + AssessedValue + ZIP + YearBuilt +  Occupied_Pr + H_.1.3.P + H.4..P + Owner_Occupied, data = new_trainData, cost = 500)
print(model_svm)

#Call:
 # svm(formula = Vacant_Building ~ LandUse + Total.Taxes.Owed + Num.Open.Violations + 
#AssessedValue + ZIP + YearBuilt + Total_crimes + Occupied_Pr + H_.1.3.P + H.4..P + 
  #Owner_Occupied, data = new_trainData)


#Parameters:
#  SVM-Type:  eps-regression 
#SVM-Kernel:  radial 
#cost:  1 
#gamma:  0.03448276
#epsilon:  0.1 


#Number of Support Vectors:  2403

#Prediction in the complete data set using train dataset
svm_new_Pred<-predict(model_svm, Syr_17_18_1)
svm_new_Pred
svm_table<-data.frame(svm_new_Pred,Syr_17_18_1$Vacant_Building)


Syr_17_18_1$svm_Predicted_vacancy<-"N"
Syr_17_18_1$svm_Predicted_vacancy[svm_new_Pred<0.5]<-"N"
Syr_17_18_1$svm_Predicted_vacancy[svm_new_Pred>0.5 ]<-"Y"

Syr_17_18_1$svm_Predicted_vacancy<-as.factor(Syr_17_18_1$svm_Predicted_vacancy)
Syr_17_18_1$VacantBuilding<-as.factor(Syr_17_18_1$VacantBuilding)

levels(Syr_17_18_1$svm_Predicted_vacancy)
levels(Syr_17_18_1$VacantBuilding)

svm_error <- svm_table$Syr_17_18_1.Vacant_Building - svm_new_Pred
rmse <- function(svm_error)
{
  sqrt(mean(svm_error^2))
}

svm_PredictionRMSE <- rmse(svm_error)  
print(svm_PredictionRMSE) #0.1684011

confusionMatrix(Syr_17_18_1$svm_Predicted_vacancy,Syr_17_18_1$VacantBuilding) 
#Confusion Matrix and Statistics
             #Reference
#Prediction     N     Y
          #N 14111    411
          #Y   125   337

#Accuracy :  0.9642         
#95% CI : (0.9611, 0.9671)
#No Information Rate : 0.9501          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.5395          
#Mcnemar's Test P-Value : < 2.2e-16       
#Sensitivity : 0.9912          
#Specificity : 0.4505           
#Pos Pred Value : 0.9717          
#Neg Pred Value : 0.7294         
#Prevalence : 0.9501         
#Detection Rate : 0.9417          
#Detection Prevalence : 0.9692          
#Balanced Accuracy : 0.7209          
#'Positive' Class : N 

sum(Syr_17_18_1$svm_Predicted_vacancy=="N")#14454
sum(Syr_17_18_1$svm_Predicted_vacancy=="Y")#530

sum(Syr_17_18_1$VacantBuilding=="N")#14236
sum(Syr_17_18_1$VacantBuilding=="Y")#748

#Plot sum of vacant buildings @predicted and original
SVM_Yes_No = data.frame(Status =factor(c("Predicted-No", "Predicted-Yes", "Actual-No", "Actual-Yes"), levels = c("Predicted-No", "Predicted-Yes", "Actual-No", "Actual-Yes")), Vacant_Properties = c(14510, 474, 14236, 748))
colors = c("red", "blue", "green",  "purple")
SVM_Yes_No <-ggplot(SVM_Yes_No, aes(Status, Vacant_Properties))
SVM_Yes_No+geom_bar(stat="identity",fill=colors)+geom_text(aes(label=Vacant_Properties),
                                                           vjust=-0.25)+ggtitle("Vacancy Decision")



###########################


##ksvm

Syr_17_18_1$Vacant_Building<-0
Syr_17_18_1$Vacant_Building[Syr_17_18_1$VacantBuilding=="Y"]<-1
Syr_17_18_1$Vacant_Building[Syr_17_18_1$VacantBuilding=="N"]<-0

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

#Prediction in the complete data set using train dataset
ksvm_new_model<-ksvm(Vacant_Building~LandUse + Total.Taxes.Owed + Num.Open.Violations + AssessedValue + ZIP + YearBuilt +  Occupied_Pr + H_.1.3.P + H.4..P + Owner_Occupied, data = new_trainData, C = 1000)
ksvm_new_model

#Support Vector Machine object of class "ksvm" 

#SV type: eps-svr  (regression) 
#parameter : epsilon = 0.1  cost C = 1000

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  0.1394129097845 

#Number of Support Vectors : 4055 

#Objective Function Value : -766467.2 
#Training error :0.21849 


ksvm_new_Pred<-predict(ksvm_new_model,Syr_17_18_1)
ksvm_new_Pred
table_ksvm_<-data.frame(ksvm_new_Pred,Syr_17_18_1$Vacant_Building)
table_ksvm_

Syr_17_18_1$ksvm_Predicted_vacancy<-"N"
Syr_17_18_1$ksvm_Predicted_vacancy[ksvm_new_Pred<0.5]<-"N"
Syr_17_18_1$ksvm_Predicted_vacancy[ksvm_new_Pred>0.5 ]<-"Y"

Syr_17_18_1$ksvm_Predicted_vacancy<-as.factor(Syr_17_18_1$ksvm_Predicted_vacancy)
Syr_17_18_1$VacantBuilding<-as.factor(Syr_17_18_1$VacantBuilding)

levels(Syr_17_18_1$ksvm_Predicted_vacancy)
levels(Syr_17_18_1$VacantBuilding)

ksvm_error <- table_ksvm_$Syr_17_18_1.Vacant_Building - ksvm_new_Pred
rmse <- function(ksvm_error)
{
  sqrt(mean(ksvm_error^2))
}

ksvm_PredictionRMSE <- rmse(ksvm_error)  
print(ksvm_PredictionRMSE) #0.163999

confusionMatrix(Syr_17_18_1$ksvm_Predicted_vacancy,Syr_17_18_1$VacantBuilding) 

#Confusion Matrix and Statistics

         #Reference
#Prediction     N     Y
         #N 14105   246
         #Y   131   502

#Accuracy : 0.9748          
#95% CI : (0.9722, 0.9773)
#No Information Rate : 0.9501          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.7139          
#Mcnemar's Test P-Value : 4.324e-09       
#Sensitivity : 0.9908          
#Specificity : 0.6711          
#Pos Pred Value : 0.9829          
#Neg Pred Value : 0.7930          
#Prevalence : 0.9501          
#Detection Rate : 0.9413          
#Detection Prevalence : 0.9578          
#Balanced Accuracy : 0.8310          
#'Positive' Class : N 


sum(Syr_17_18_1$ksvm_Predicted_vacancy=="N")#14351
sum(Syr_17_18_1$ksvm_Predicted_vacancy=="Y")#662

sum(Syr_17_18_1$VacantBuilding=="N")#14236
sum(Syr_17_18_1$VacantBuilding=="Y")#748

#Plot sum of vacant buildings @predicted and original
ksvm_Yes_No = data.frame(Status =factor(c("Predicted-No", "Predicted-Yes", "Actual-No", "Actual-Yes"), levels = c("Predicted-No", "Predicted-Yes", "Actual-No", "Actual-Yes")), Vacant_Properties = c(14724, 260, 14236, 748))
colors = c("red", "blue", "green",  "purple")
ksvm_Yes_No <-ggplot(ksvm_Yes_No, aes(Status, Vacant_Properties))
ksvm_Yes_No+geom_bar(stat="identity",fill=colors)+geom_text(aes(label=Vacant_Properties),
                                                           vjust=-0.25)+ggtitle("Vacancy Decision")


##accuracy check within the models

Syr_17_18_1$Syr_17_18_1_new.pred.forest<-as.factor(Syr_17_18_1$Syr_17_18_1_new.pred.forest)
Syr_17_18_1$svm_Predicted_vacancy<-as.factor(Syr_17_18_1$svm_Predicted_vacancy)
Syr_17_18_1$ksvm_Predicted_vacancy<-as.factor(Syr_17_18_1$ksvm_Predicted_vacancy)
Syr_17_18_1$VacantBuilding<-as.factor(Syr_17_18_1$VacantBuilding)


confusionMatrix(Syr_17_18_1$Syr_17_18_1_new.pred.forest,Syr_17_18_1$svm_Predicted_vacancy) 
#Confusion Matrix and Statistics

              #Reference
#Prediction     N     Y
         #N 14202   159
         #Y   316   371

#Accuracy : 0.9708         
#95% CI : (0.968, 0.9735)
#No Information Rate : 0.9692         
#P-Value [Acc > NIR] : 0.1229         
#Kappa : 0.592          
#Mcnemar's Test P-Value : <2e-16         
#Sensitivity : 0.9785         
#Specificity : 0.7294         
#Pos Pred Value : 0.9913         
#Neg Pred Value : 0.5193         
#Prevalence : 0.9692         
#Detection Rate : 0.9483         
#Detection Prevalence : 0.9567         
#Balanced Accuracy : 0.8540         
#'Positive' Class : N   

confusionMatrix(Syr_17_18_1$svm_Predicted_vacancy,Syr_17_18_1$ksvm_Predicted_vacancy) 

#Confusion Matrix and Statistics

            #Reference
#Prediction   N     Y
        #N 14294  231
        #Y   80   379

#Accuracy : 0.9708         
#95% CI : (0.968, 0.9735)
#No Information Rate : 0.9692         
#P-Value [Acc > NIR] : 0.1229         
#Kappa : 0.592          
#Mcnemar's Test P-Value : <2e-16         
#Sensitivity : 0.9785         
#Specificity : 0.7294         
#Pos Pred Value : 0.9913         
#Neg Pred Value : 0.5193         
#Prevalence : 0.9692         
#Detection Rate : 0.9483         
#Detection Prevalence : 0.9567         
#Balanced Accuracy : 0.8540         
#'Positive' Class : N 

#########################


#rf heatmap
ggplot(Syr_17_18_1, aes(x=YearBuilt,y=Syr_17_18_1_new.pred.forest, fill=Owner_Occupied)) + geom_tile() + scale_fill_gradient(low="sky blue", high = "blue")+labs(x="Age of the properties", y = "Vacant Building") + ggtitle("Vacancy of Syracuse (Random Forest Model)")

ggplot(Syr_17_18_1, aes(x=YearBuilt,y=Syr_17_18_1_new.pred.forest, fill=Owner_Occupied)) + geom_tile() + scale_fill_gradient(low="sky blue", high = "blue") +labs(x="Assessed Value of the property", y = "Predicted Vacancy") + ggtitle("Vacancy of Syracuse (Random Forest Model)")


#kSVM heatmap
ggplot(Syr_17_18_1, aes(x=YearBuilt,y=ksvm_Predicted_vacancy, fill=Owner_Occupied)) + geom_tile() + scale_fill_gradient(low="sky blue", high = "blue")+labs(x="Age of the properties", y = "Predicted Vacancy") + ggtitle("Vacancy of Syracuse (kSVM Model)")

ggplot(Syr_17_18_1, aes(x=AssessedValue,y=ksvm_Predicted_vacancy, fill=Owner_Occupied)) + geom_tile() + scale_fill_gradient(low="sky blue", high = "blue") +labs(x="Assessed Value of the property", y = "Predicted Vacancy") + ggtitle("Vacancy of Syracuse (kSVM Model)")

#SVM heatmap
ggplot(Syr_17_18_1, aes(x=YearBuilt,y=svm_Predicted_vacancy, fill=Owner_Occupied)) + geom_tile() + scale_fill_gradient(low="sky blue", high = "blue")+labs(x="Age of the properties", y = "Predicted Vacancy") + ggtitle("Vacancy of Syracuse (SVM Model)")

ggplot(Syr_17_18_1, aes(x=AssessedValue,y=svm_Predicted_vacancy, fill=Owner_Occupied)) + geom_tile() + scale_fill_gradient(low="sky blue", high = "blue") +labs(x="Assessed Value of the property", y = "Predicted Vacancy") + ggtitle("Vacancy of Syracuse (SVM Model)")




































