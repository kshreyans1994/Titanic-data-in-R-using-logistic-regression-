#loading important libraries(step=1)
library(MASS)
library(car)
library(ggplot2)
library(caret)
library(corrplot)
library(DAAG)  
library(e1071)
require(dplyr)
library(readr)
library(readxl)
library(xlsx)
# Loading data set(step=2)
data<-read.xlsx("Titanic.xls",sheetIndex = 1)
View(data)

# checking structure of data 
str(data)  

# summary of data
summary(data)

#data preprocessing ,data cleaning(step=3)
#identifying unique values 
unique(data)

#dropping unique identifiers
data<-data[,-1]
View(data)
data<-data[,-3]
View(data)
data<-data[,-7]
View(data)

# selecting target_var 
colnames(data)[1] <- "Target_Var"
View(data)

#checking missing values 
apply(is.na(data),2,sum)

# % of NA's in dataframe
sum(is.na(data))/prod(dim(data)) *100

# % of NA's contains as row
nrow(data[!complete.cases(data),])/nrow(data) *100

# imputing missing values
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
data$Embarked[is.na(data$Embarked)] <- mode(data$Embarked)
colSums(is.na(data))
View(data)

# dropping( cabin )column as this column has more than 75% null values 
data <-data[-8]
dim(data)
colnames(data)
View(data)

# converting categorical values  to numeric values
data$Sex <- ifelse(data$Sex=='male',1,0)
data$Embarked <- ifelse(data$Embarked=='S',ifelse(data$Embarked=='C',1,2),3)
View(data)

#now data is clean from  null values and identifiers .clean for EDA

#Exploratory data analysis(step=4)
ggplot(data, aes(x=Target_Var))+geom_histogram()

ggplot(data, aes(x=Target_Var))+
  geom_histogram()+
  geom_density(fill= "yellow", alpha= 0.2)

#checking skewness
skewness(data$Target_Var)
boxplot(data$Target_Var)#data is normally distributed as their are  no outliers

# Variable Significance or Measure of Association.(step=5)
numeric <- data[sapply(data, is.numeric)]
View(desc) 
desc <- cor(numeric)
corrplot(desc)
corrplot.mixed(desc, lower.col = "black", number.cex = .7)
View(data)

#sampling(step=6)
#spliting dataset into test and train data
set.seed(20)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
View(train)
test=data[-s,]
dim(train)
dim(test)
colnames(test)
test1 <- test[-1]
colnames(test1)
dim(test)
dim(test1)
View(test1)

# model building applying logistic regression (step=7)  
model.logistic = glm(Target_Var~.,data=train,family=binomial("logit"))
predicted <- predict(model.logistic, test1, type="response")
summary(model.logistic )

# Accuracy, Confusion_matrix
View(predicted)
predicted1 = ifelse(predicted >=0.5 ,1,0)
View(predicted1)

#confusion matrix
table(predicted1)
table(test$Target_Var)
cf1 = table(test$Target_Var, predicted1)
cf1
library(caret)
sensitivity(cf1)
specificity(cf1)
confusionMatrix(cf1)

#stepwise slection:
nullModel<- glm(Target_Var~ 1, data=train,family=binomial("logit"))
summary(nullModel)

fullmodel <- glm(Target_Var ~., data=train,family=binomial("logit"))
summary(fullmodel)

fit <- step(nullModel, scope=list(lower=nullModel, upper=fullmodel), direction="both")

#revel the model
fit


#Final model
 final_model <-  glm(formula = Target_Var ~ Sex + Pclass + Age + SibSp, 
              family = binomial("logit"), data = train)

summary(final_model)


fitted.results = predict(final_model,newdata=test1, type='response')
View(fitted.results)

fitted.results1 = ifelse(fitted.results >=0.7 ,1,0)

#confusion matrix
table(fitted.results1)
table(test$Target_Var)
cf1 = table(test$Target_Var, fitted.results1)
cf1
confusionMatrix(cf1)

# Roc_Auc curve
#install.packages("pROC")
library(pROC)
roccurve=roc(test$Target_Var, predicted1)
plot(roccurve)
auc(roccurve)
#getting 80.76 accuracy by logistic regression 
#data set is too small for random forest thats why i am not using it 
#logistic regression is ideal for this data set 