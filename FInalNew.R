#importing the dataset
Credit = read.csv("MiniProject1.csv")
#Structure of the dataset
str(Credit)
summary(Credit)
# Get names of variables in the dataset
names(Credit)

#Preliminaries Section
#Histogram of age
hist(Credit$Age, prob= T,xlab  ='',main='Histogram of Age ')
lines(density(Credit$Age,na.rm=T))
rug(jitter(Credit$Age))

#Histogram of FamilySize
hist(Credit$FamilySize,prob= T, xlab ='',main='Histogram of FamilySize')
lines(density(Credit$FamilySize,na.rm=T))
rug(jitter(Credit$FamilySize))

#Histogram of YearsAtThisHome
hist(Credit$YearsAtThisHome,prob= T, xlab ='',main='Histogram of YearsAtThisHome')
lines(density(Credit$YearsAtThisHome,na.rm=T))
rug(jitter(Credit$YearsAtThisHome))

#Histogram of MainHousehold
hist(Credit$MainHousehold,prob= T, xlab ='',main='Histogram of MainHousehold')
lines(density(Credit$MainHousehold,na.rm=T))
rug(jitter(Credit$MainHousehold))

#Histogram of MonthlyIncome
hist(Credit$MonthlyIncome,prob= T, xlab ='',main='Histogram of MonthlyIncome')
lines(density(Credit$MonthlyIncome,na.rm=T))
rug(jitter(Credit$MonthlyIncome))

#Histogram of MonthlyExpenses
hist(Credit$MonthlyExpenses,prob= T, xlab ='',main='Histogram of MonthlyExpenses')
lines(density(Credit$MonthlyExpenses,na.rm=T))
rug(jitter(Credit$MonthlyExpenses))

#Histogram of AvailableIncome
hist(Credit$AvailableIncome,prob= T, xlab ='',main='Histogram of AvailableIncome')
lines(density(Credit$AvailableIncome,na.rm=T))
rug(jitter(Credit$AvailableIncome))

#Histogram of PerCapitaAvailableIncome
hist(Credit$PerCapitaAvailableIncome,prob= T, xlab ='',main='Histogram of PerCapitaAvailableIncome')
lines(density(Credit$PerCapitaAvailableIncome,na.rm=T))
rug(jitter(Credit$PerCapitaAvailableIncome))

#Histogram of ParticipatesInCommunity
hist(Credit$ParticipatesInCommunity,prob= T, xlab ='',main='Histogram of ParticipatesInCommunity')
lines(density(Credit$ParticipatesInCommunity,na.rm=T))
rug(jitter(Credit$ParticipatesInCommunity))

#Histogram of SocialFabric
hist(Credit$SocialFabric,prob= T, xlab ='',main='Histogram of SocialFabric')
lines(density(Credit$SocialFabric,na.rm=T))
rug(jitter(Credit$SocialFabric))

#Histogram of LoanOpinion
hist(Credit$LoanOpinion,prob= T, xlab ='',main='Histogram of LoanOpinion')
lines(density(Credit$LoanOpinion,na.rm=T))
rug(jitter(Credit$LoanOpinion))

#Histogram of TotalPaidAvg
hist(Credit$TotalPaidAvg,prob= T, xlab ='',main='Histogram of TotalPaidAvg')
lines(density(Credit$TotalPaidAvg,na.rm=T))
rug(jitter(Credit$TotalPaidAvg))

#Histogram of LoanSizeAvg
hist(Credit$LoanSizeAvg,prob= T, xlab ='',main='Histogram of LoanSizeAvg')
lines(density(Credit$LoanSizeAvg,na.rm=T))
rug(jitter(Credit$LoanSizeAvg))

#Histogram of UpfrontPaymentAvg
hist(Credit$UpfrontPaymentAvg,prob= T, xlab ='',main='UpfrontPaymentAvg')
lines(density(Credit$UpfrontPaymentAvg,na.rm=T))
rug(jitter(Credit$UpfrontPaymentAvg))

#Histogram of LoanPeriodMonths
hist(Credit$LoanPeriodMonths,prob= T, xlab ='',main='LoanPeriodMonths')
lines(density(Credit$LoanPeriodMonths,na.rm=T))
rug(jitter(Credit$LoanPeriodMonths))

#Histogram of MonthlyPayment
hist(Credit$MonthlyPayment,prob= T, xlab ='',main='MonthlyPayment')
lines(density(Credit$MonthlyPayment,na.rm=T))
rug(jitter(Credit$MonthlyPayment))

#Histogram of CreditRating
hist(Credit$CreditRating,prob= T, xlab ='',main='CreditRating')
lines(density(Credit$CreditRating,na.rm=T))
rug(jitter(Credit$CreditRating))

#boxplot
boxplot(Credit$MonthlyIncome,boxwex=0.2,xlab ='',ylab="MonthlyIncome",main = "Monthly Income with Outlier")
rug(jitter(Credit$MonthlyIncome),side=2)
abline(h=mean(Credit$MonthlyIncome,na.rm=T),lty=2)

#Use subset to remove Outlier
Credit=subset(Credit, Credit$MonthlyIncome<10000)
boxplot(Credit$MonthlyIncome,boxwex=0.2,xlab ='',ylab="Monthly Income",main = "MonthlyIncome without Outlier")
nrow(Credit)

#boxplot
boxplot(Credit$Age,ylab="Age",main="Age with Outlier")
#Remove Outlier
Credit=subset(Credit, Credit$Age<81)
boxplot(Credit$Age,ylab="Age",main = "Age without Outlier")
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
Credittemp <- mice(Credit,m=5,maxit=50,meth='pmm',seed=420)
Creditnew <- complete(Credittemp,1)
summary(Creditnew)

#high corelation and remove them
library(mlbench)
library(lattice)
library(ggplot2)
library(caret)

symnum(cor(Credit[,1:14],use=("complete.obs")))
plot(Credit$TotalPaidAvg, Credit$LoanSizeAvg,main="Correlation between TotalPaidAvg and LoanSizeAvg",xlab="TotalPaidAvg",ylab="LoanSizeAvg")
plot(Credit$MonthlyExpenses,Credit$MonthlyIncome,main="Correlation between MonthlyExpense and MonthlyIncome",xlab="MonthlyExpense",ylab="MonthlyIncome")

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
186/(186+31)  #0.8571

colnames(Creditnew)

#Split into Training and Testing Set
library(caTools)
set.seed(144)
spl = sample.split(Creditnew$CreditRating, SplitRatio = 0.75)
CreditTrain = subset(Creditnew, spl==TRUE)
CreditTest = subset(Creditnew, spl==FALSE)

# Building a Logistic Regression Model: 
#Model1
CreditModelTrain = glm(CreditRating ~ ., data = CreditTrain, family=binomial)
summary(CreditModelTrain)

#Refine Model1
#Remove YearsAtThisHome
CreditTrain = CreditTrain [,-c(4)]
CreditModelTrain = glm(CreditRating ~ ., data = CreditTrain, family=binomial)
summary(CreditModelTrain)

#Remove Sex
CreditTrain = CreditTrain [,-c(1)]
CreditModelTrain = glm(CreditRating ~ ., data = CreditTrain, family=binomial)
summary(CreditModelTrain)

#Remove MonthlyExpenses 
CreditTrain = CreditTrain [,-c(3)]
CreditModelTrain = glm(CreditRating ~ ., data = CreditTrain, family=binomial)
summary(CreditModelTrain)

#The final Logistics Model
CreditModelTrain

#Evaluating the Model
#computing the accuracy of the model on the training set with a threshold of 0.5
PredictTrain = predict(CreditModelTrain, type="response")
table(CreditTrain$CreditRating, PredictTrain > 0.5)

#accuracy: 
(139+1)/(139+22+1+1) # 85.89% 
#Baseline
(1+139)/(139+22+1+1) # 85.89%

library(ROCR)
#ROC Curve for training set
ROCRpred = prediction(PredictTrain, CreditTrain$CreditRating)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,0.7))

#the AUC of our model on the training set.
as.numeric(performance(ROCRpred, "auc")@y.values)
#[1] 0.7478261

#make predictions for the test set
PredictTest = predict(CreditModelTrain, type="response",newdata=CreditTest)
table(CreditTest$CreditRating, PredictTest > 0.5)
(43)/(8+3+43)# 0.7962963

#ROC Curve for testing set
ROCRpred2 = prediction(PredictTest, CreditTest$CreditRating)
ROCCurve2 = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,0.7))

#the AUC of our model on the training set.
as.numeric(performance(ROCRpred2, "auc")@y.values)
#[1] 0.6847826

#Baseline
(3+43)/(3+8+43) #=85.19%

