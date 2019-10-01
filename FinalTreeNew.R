#importing the dataset
Credit = read.csv("MiniProject1.csv")
#Structure of the dataset
str(Credit)
summary(Credit)
# Get names of variables in the dataset
names(Credit)

#Use subset to remove Outlier
Credit=subset(Credit, Credit$MonthlyIncome<10000)
#boxplot(Credit$MonthlyIncome,boxwex=0.2,xlab ='',ylab="Monthly Income",main = "MonthlyIncome without Outlier")
str(Credit)

#Remove Outlier
Credit=subset(Credit, Credit$Age<81)
#boxplot(Credit$Age,ylab="Age",main = "Age without Outlier")
nrow(Credit)

#Transform Sex coloumn into integer format
library(stringr)
Credit$Sex = str_replace(Credit$Sex,"Masculino","0")
Credit$Sex = str_replace(Credit$Sex,"Femenino","1")
Credit$Sex = as.numeric(Credit$Sex)
class(Credit$Sex)

# remove variables which contain too many NAs due to summary and the first colume (User ID)
summary(Credit)
colSums(is.na(Credit))
Credit = Credit [,-c(1,9,10)]
summary(Credit)

#Use mice package for NA
library(mice)
Credittemp <- mice(Credit,m=5,maxit=50,meth='pmm',seed=500)
Creditnew <- complete(Credittemp,1)
summary(Creditnew)

#high corelation and remove them
library(mlbench)
library(lattice)
library(ggplot2)
library(caret)

symnum(cor(Credit[,1:14],use=("complete.obs")))
#plot(Credit$TotalPaidAvg, Credit$LoanSizeAvg,main="Correlation between TotalPaidAvg and LoanSizeAvg",xlab="TotalPaidAvg",ylab="LoanSizeAvg")
#plot(Credit$MonthlyExpenses,Credit$MonthlyIncome,main="Correlation between MonthlyExpense and MonthlyIncome",xlab="MonthlyExpense",ylab="MonthlyIncome")

correlationMatrix <- cor(Creditnew[,1:14])
print(correlationMatrix)

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.6)
print(highlyCorrelated)

#delete high corrleated independent variable
Creditnew = Creditnew [,-c(6,12,13,5)]
str(Creditnew)

#group CreditRating categories
Creditnew$CreditRating[Creditnew$CreditRating %in% 1] <- 0
Creditnew$CreditRating[Creditnew$CreditRating %in% 1.5] <- 0
Creditnew$CreditRating[Creditnew$CreditRating %in% 2] <- 1
Creditnew$CreditRating[Creditnew$CreditRating %in% 2.5] <- 1
Creditnew$CreditRating[Creditnew$CreditRating %in% 3] <- 1
table(Creditnew$CreditRating)

#Baseline for the entire set
table(Creditnew$CreditRating)
187/(187+30)  #0.8617

#Classification Trees:
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
set.seed(100)
spl = sample.split(Creditnew$CreditRating, SplitRatio = 0.75)
CreditTrain = subset(Creditnew, spl==TRUE)
CreditTest = subset(Creditnew, spl==FALSE)

#CART model 1
CRTree = rpart(CreditRating ~ ., data = CreditTrain, method="class", minbucket=5)
CRTree
prp(CRTree)

#Learning about plots
prp(CRTree)
rpart.plot(CRTree)
print(CRTree)

# Make predictions
PredictCART = predict(CRTree, newdata = CreditTest, type = "class")
cmat_CART <- table(CreditTest$CreditRating, PredictCART)
cmat_CART

#lets now compute the overall accuracy
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART  #0.8181818

#Our baseline model accuracy is the most frequent outcome i.e. the reversal  denoted by code 1
(2+45)/(8+2+45) # 0.8545455

# ROC curve
library(ROCR)
PredictROC = predict(CRTree, newdata = CreditTest)
PredictROC

pred = prediction(PredictROC[,2], CreditTest$CreditRating)
perf= performance(pred, "tpr", "fpr")
plot(perf)

#AUC
as.numeric(performance(pred, "auc")@y.values)
#0.5199468

#CART model 2
CRTree = rpart(CreditRating ~ ., data = CreditTrain, method="class", minbucket=10)
CRTree
prp(CRTree)

#Learning about plots
prp(CRTree)
rpart.plot(CRTree)
print(CRTree)

# Make predictions
PredictCART = predict(CRTree, newdata = CreditTest, type = "class")
cmat_CART <- table(CreditTest$CreditRating, PredictCART)
cmat_CART

#lets now compute the overall accuracy
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART  #0.8181818

#Our baseline model accuracy is the most frequent outcome i.e. the reversal  denoted by code 1
(2+45)/(8+2+45) # 0.8545455

# ROC curve
library(ROCR)
PredictROC = predict(CRTree, newdata = CreditTest)
PredictROC

pred = prediction(PredictROC[,2], CreditTest$CreditRating)
perf= performance(pred, "tpr", "fpr")
plot(perf)

#AUC
as.numeric(performance(pred, "auc")@y.values)
#0.5199468

#CART model 3
CRTree = rpart(CreditRating ~ ., data = CreditTrain, method="class", minbucket=15)
CRTree
prp(CRTree)

#Learning about plots
prp(CRTree)
rpart.plot(CRTree)
print(CRTree)

# Make predictions
PredictCART = predict(CRTree, newdata = CreditTest, type = "class")
cmat_CART <- table(CreditTest$CreditRating, PredictCART)
cmat_CART

#lets now compute the overall accuracy
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART  #0.8

#Our baseline model accuracy is the most frequent outcome i.e. the reversal  denoted by code 1
(6+41)/(8+6+41) # 0.8545455

# ROC curve
library(ROCR)
PredictROC = predict(CRTree, newdata = CreditTest)
PredictROC

pred = prediction(PredictROC[,2], CreditTest$CreditRating)
perf= performance(pred, "tpr", "fpr")
plot(perf)

#AUC
as.numeric(performance(pred, "auc")@y.values)
#0.474734

# Install cross-validation packages
library(lattice)
library(ggplot2)
library(SparseM)
library(caret)
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
Save_CV <-train(as.factor(CreditRating) ~ ., data = CreditTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
Save_CV

# Create a new CART model
# read the final value used for cp at the prompt
CreditTreeCV = rpart(CreditRating ~ ., data = CreditTrain, method="class", cp = 0.5)
prp(CreditTreeCV)
rpart.plot(CreditTreeCV)
print(CreditTreeCV)
summary(CreditTreeCV)

# Make predictions
PredictCV = predict(CreditTreeCV, newdata = CreditTest, type = "class")
MyTab=table(CreditTest$CreditRating, PredictCV)
AccuCV = (MyTab[1,1]+MyTab[2,2])/sum(MyTab) 
table(AccuCV)
AccuCV
# 85.45%

