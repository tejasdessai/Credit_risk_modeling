
dataset = read.csv('GermanCredit.csv')
str(dataset)

dataset <- dataset
# Create dummy variables for  Amount and Duration
dataset$Age_c1 <- ifelse(dataset$Age <=26,1,0)
dataset$Age_c2 <- ifelse(dataset$Age >26 & dataset$Age <=33,1,0)
dataset$Amount_c1 <- ifelse(dataset$Amount <=1260,1,0)
dataset$Amount_c2 <- ifelse(dataset$Amount >1260 & dataset$Amount <=4700,1,0)
dataset$Duration_c1 <- ifelse(dataset$Duration <=15,1,0)
dataset$Duration_c2 <- ifelse(dataset$Duration >15 & dataset$Duration <=30,1,0)
dataset$Class <- ifelse(dataset$Class = "Good",1,0)

library(caTools)
set.seed(123456) #random number seed made to make split
split = sample.split(dataset$Class, SplitRatio = 0.8) #split is for training set

training_set = subset(dataset,split == TRUE) #finally assigning
test_set = subset(dataset,split == FALSE) #finally assigning

classifier <- glm(formula = Class ~ ., family = binomial, data = training_set)
summary(classifier)

#check multicollinearity and drop useless variables
step(classifier, direction = "both")

install.packages("car")
library(car)
vif(classifier)


classifier <- glm(Class ~ 
                      #Amount +
                      #InstallmentRatePercentage + 
                      #ForeignWorker + 
                      CheckingAccountStatus.lt.0 +
                      CheckingAccountStatus.0.to.200 + 
                      CreditHistory.NoCredit.AllPaid + 
                      CreditHistory.ThisBank.AllPaid +
                      CreditHistory.PaidDuly + 
                      #CreditHistory.Delay + 
                      Purpose.NewCar + 
                      #Purpose.Furniture.Equipment + 
                      #Purpose.Radio.Television + 
                      #Purpose.Repairs +
                      Purpose.Education + 
                      #Purpose.Business + 
                      SavingsAccountBonds.lt.100 + 
                      #SavingsAccountBonds.100.to.500 + 
                      EmploymentDuration.4.to.7 + 
                      #Personal.Male.Single + 
                      OtherDebtorsGuarantors.None + 
                      OtherDebtorsGuarantors.CoApplicant + 
                      #Property.RealEstate + 
                      #OtherInstallmentPlans.Bank + 
                      #Housing.Rent +
                      Age_c1 + 
                      #Amount_c1 + 
                      Duration_c1 + Duration_c2, 
                    family = binomial, 
                    data = training_set)

#predicitng test set
prob_pred = predict(classifier, type = 'response', newdata = test_set[-10])

#we want 0 or 1 results. So we have to transform probabilities using ifelse
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#making the confusion matrix to compare. Most important
cm = table(test_set[,10], y_pred)
summary(classifier)


#ROC plot
library(ROCR)

perf.obj <- prediction(predictions=prob_pred,
                       labels=test_set$Class)
# Get data for ROC curve
roc.obj <- performance(perf.obj, measure="tpr", x.measure="fpr")
plot(roc.obj,
     main="Credit Risk model - ROC",
     xlab="False Positive Rate",
     ylab="True Positive Rate",
     col="blue")
abline(0,1,col="grey")

summary(classifier)
head(dataset)
summary(dataset)
int(dataset)
str(dataset)

cm
Accuracy = (cm[1,1] + cm[2,2])/(cm[1,1] +cm[1,2] + cm[2,1] + cm[2,2])*100
Precision = (cm[2,2])/(cm[2,2] +cm[1,2])*100
Recall = (cm[2,2])/(cm[2,2] +cm[2,1])*100
