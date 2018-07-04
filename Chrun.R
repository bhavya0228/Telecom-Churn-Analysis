# Loading all necessary libraries
library(readxl)
library(ggplot2)
library(corrplot)
library(caTools)
library(lmtest)
library(pscl)
library(caret)
library(e1071)
library(sandwich)
library(Deducer)
library(rJava)
library(gridExtra)

# Setting current working directory
setwd('C:/Suvo/PGP-BABI/Predictive Modelling Group Assignment')
getwd()

#### Load Data ####
data <- read_xlsx("Dataset_Cellphone.xlsx", sheet = "Data")

# data <- read.csv("Dataset_Cellphone_csv.csv", header=TRUE)



### Check for NA ###

any(is.na(data))

str(data)     #Chrun, Contract Renewal, DataPlan are of num type 
summary(data) #these should be converted to factor


#### Exploratory Data Analysis####

(sum(data$Churn)/length(data$Churn))*100 #14.5 is the Churn %

ggplot(data = data, aes(x = as.factor(Churn))) + 
  geom_bar(fill = "blue") +  theme_bw()

#ggplot(data = data, aes(x = AccountWeeks)) + geom_histogram(aes(fill = as.factor(Churn)))

a <- ggplot(data = data, aes(x = as.factor(ContractRenewal))) + 
  geom_bar(aes(fill = as.factor(Churn)), position = "fill")

ggplot(data = data, aes(x = as.factor(DataPlan))) + 
  geom_bar(aes(fill = as.factor(Churn)), position = "fill")

#ggplot(data = data, aes(x = DataUsage)) + geom_histogram(aes(fill = as.factor(Churn)),binwidth = 1)

b <- ggplot(data = data, aes(x = as.factor(CustServCalls))) + 
  geom_bar(aes(fill = as.factor(Churn)), position = "fill")
b

ggplot(data = data, aes(x = DayMins)) + 
  geom_histogram(aes(fill = as.factor(Churn)),binwidth = 50)

#ggplot(data = data, aes(x = DayCalls)) + geom_histogram(aes(fill = as.factor(Churn)),binwidth = 50)

ggplot(data = data, aes(x = MonthlyCharge)) + geom_histogram(aes(fill = as.factor(Churn)))

c <-ggplot(data = data, aes(x = OverageFee)) + 
  geom_histogram(aes(fill = as.factor(Churn)))

d <- ggplot(data = data, aes(x = RoamMins)) + 
  geom_histogram(aes(fill = as.factor(Churn)),position = "fill")

grid.arrange(a,b,c,d, nrow = 2)

#### Correlation Matrix ####

corrplot(cor(data)) #Data Usage and DataPlan & Monthly Charge are Correlated

# Binning the customer service calls for better model stability
data$DummyCustServCalls <- 'LOW'

data[data$CustServCalls %in% c(0,1,2,3),]$DummyCustServCalls <- 'LOW'
data[data$CustServCalls %in% c(4,5,6,7,8),]$DummyCustServCalls <- 'MEDIUM'
data[data$CustServCalls %in% c(9),]$DummyCustServCalls <- 'HIGH'

data$CustServCalls <- NULL

####Converting Churn, ContractRenewal, DataPlan, DummyCustServCalls Variables to factors
data$Churn <- as.factor(data$Churn)
data$ContractRenewal <- as.factor(data$ContractRenewal)
data$DataPlan <- as.factor(data$DataPlan)
data$DummyCustServCalls <- factor(data$DummyCustServCalls, ordered=TRUE)

#### Dividing data into Train and Test ####

set.seed(999)
split <- sample.split(data$Churn, SplitRatio = 0.7)

train.churn <- data[split == T,]
test.churn <- data[split == F,]


#### Running GLM on Train dataset ####

model1 <- glm(Churn ~ . , family = binomial(link = "logit"), 
              data = train.churn)

summary(model1)

### Running Diagnostics ####

lrtest(model1)
pR2(model1)
rocplot(model1)

# Checking for multicollinearity in dataset
vif(model1)

####Calculating Odds####

odds <- exp(coef(model1))
odds

#### Re-Runing the model with significant variables ####

model2= step(model1)
summary(model2) # AccountWeeks/DayCalls comes out to be insignificnat

# Modified model excluding insignificant variables identified
model2 <- glm(Churn ~ ContractRenewal + DataPlan + DummyCustServCalls  + DayMins + OverageFee + RoamMins, 
              family = binomial(link = "logit"), data = train.churn)

summary(model2)
lrtest(model2)
pR2(model2)

# Verifying that multicollinearity has been removed
vif(model2)

# Verifyign the AUC value improvement
rocplot(model2)

odds <- round(exp(coef(model2)),7)
odds

#### Prediction on Training Model ####

train.churn$prob <- predict(model2, type = 'response')

train.churn$Predicted <- ifelse(train.churn$prob<=0.2,0,1)
confusionMatrix(train.churn$Predicted,train.churn$Churn)

#### Running Predictions on Test Dataset ####

test.churn$prob <- predict(model2, newdata = test.churn, type = 'response')
test.churn$Predicted <- ifelse(test.churn$prob<=0.2,0,1)
confusionMatrix(test.churn$Predicted,test.churn$Churn) 



