library(tidyverse) # metapackage with lots of helpful functions
library(data.table)
library(e1071)
# install.packages(mice)
# install.packages(caret)
# install.packages(outliers)
# install.packages(e1071)
# install.packages(moderndive)
#install.packages(effects)

#Data preprocessing
#Function to get mode of categorical data
#usage of functions
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}#Function for removing outliers
getmode
out.rem<-function(x) {
  x[which(x==outlier(x))]=NA
  x
}
#DATA PREPROCESSING-------------------------------------
set.seed(1023)
weather_data <- read.csv("weatherAUS.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
is.numeric(weather_data$WindGustDir)
weather_data2 <- subset(weather_data, select = -c(Date, Location, RISK_MM, Rainfall, RainToday))
colnames(weather_data2)
weather_data3 <- weather_data2[complete.cases(weather_data2),]
weather_data3
summary(weather_data2)
weather_data2


#plot the data for better understanding
##1. Exploratory Data Analysis(EDA)
#get the boxplots
library(ggplot2)
gp <- invisible(lapply(weather_data3, function(x) { 
  ggplot(data=weather_data3, aes(x= RainTomorrow, y=eval(parse(text=x)), col = RainTomorrow)) + geom_boxplot() + xlab("RainTomorrow") + ylab(x) + ggtitle("") + theme(legend.position="none")}))
gp[[1]]
gp[[2]]
gp[[3]]
gp[[4]]
gp[[5]]
gp[[6]]
gp[[7]]
gp[[8]]
gp[[9]]
gp[[10]]
gp[[11]]
gp[[12]]
gp[[13]]
gp[[14]]
#histograms
#Check the skewness of data
hist(Mintemp=weather_data5$MinTemp)
hist(weather_data5$MaxTemp)
hist(weather_data5$Evaporation)
hist(weather_data5$Sunshine)
hist(weather_data5$WindGustSpeed)
hist(weather_data5$WindSpeed9am)
hist(weather_data5$WindSpeed3pm)
hist(weather_data5$Humidity9am)
hist(weather_data5$Humidity3pm)
hist(weather_data5$Temp9am)
hist(weather_data5$Temp3pm)
hist(weather_data5$Pressure9am)
hist(weather_data5$Pressure3pm)

#2.Feature Extraction
#Get the categorical variables
#2.1Chi-Square to check whether the variables are dependent on RainTomorrow
factor_vars1 <- names(which(sapply(weather_data3, class) == "factor"))
factor_vars1
factor_vars1 <- setdiff(factor_vars1, "RainTomorrow")
factor_vars1
chisq_test_res <- lapply(factor_vars1, function(x) { 
  chisq.test(weather_data3[,x], weather_data3[, "RainTomorrow"], simulate.p.value = TRUE)
})
names(chisq_test_res) <- factor_vars1
chisq_test_res
#Baed on the chisquare values including categorical variables WindGustDir,WindDir9am and WindDir3pm

#Feature Extraction for numberic variables
#2.2Method:Correlation

#Remove Categorical variables from dataset
weather_data4 <- subset(weather_data2, select = -c(WindDir9am, WindDir3pm))
colnames(weather_data4)
weather_data5 <- weather_data4[complete.cases(weather_data4),]
numeric_vars <- setdiff(colnames(weather_data5), factor_vars1)
numeric_vars <- setdiff(numeric_vars, "RainTomorrow")
numeric_vars_mat <- as.matrix(weather_data5[, numeric_vars, drop=FALSE])
numeric_vars_cor <- cor(numeric_vars_mat)

#Get the correlation between the numeric variables
library(caret)
fndCorrelation = findCorrelation(numeric_vars_cor, cutoff=0.6) # putt any value as a "cutoff"
fndCorrelation = sort(fndCorrelation)
reduced_Data = numeric_vars_mat[,c(fndCorrelation)]
cols=colnames(reduced_Data)
cols
summary (reduced_Data)
#Based on the result including variables "MaxTemp"  , "Sunshine"  , "WindGustSpeed" ,"Humidity9am" ,"Pressure3pm",   "Cloud3pm"      "Temp9am"      
"Temp3pm"

# Get the numeric and categorical variables
library(dplyr)
weather_data7= weather_data2[c("WindGustDir","WindDir9am","WindDir3pm","RainTomorrow")]
weather_data9= weather_data2[c(cols)]

#remove outliers
library(outliers)
apply(weather_data9,2,out.rem)
colnames(weather_data9)

#merge numeric anf factor columns
weather_data10=cbind(weather_data9,weather_data7)
summary(weather_data10)
dim(weather_data10$Pressure3pm)
dim(weather_data10$Cloud3pm)

#3.Data normalisation/Cleaning 
#Replace NA values with mean,mode
library(dplyr)
weather_data10=weather_data10 %>% mutate_if(is.numeric, funs(replace(.,is.na(.), mean(., na.rm = TRUE)))) %>%
  mutate_if(is.factor, funs(replace(.,is.na(.), getmode(na.omit(.)))))
summary(weather_data10)

#plot to verify results of Data Preprocessing
hist(weather_data10$MaxTemp)
hist(weather_data10$Sunshine)
hist(weather_data10$WindGustSpeed)
hist(weather_data10$Humidity9am)
hist(weather_data10$Pressure3pm)
hist(weather_data10$Cloud3pm)#issues in values
hist(weather_data10$Temp9am)
hist(weather_data10$Temp3pm)
WindGustDirnum=as.numeric(weather_data10$WindGustDir)
WindGustDirnum
WindDir9amnum=as.numeric(weather_data10$WindDir9am)
WindDir9amnum
WindDir3pmnum=as.numeric(weather_data10$WindDir3pm)
WindDir3pmnum
hist(WindGustDirnum)
hist(WindDir9amnum)
hist(WindDir3pmnum)

# 4.Data Modeling  
weather_data10$WindGustDir=as.numeric(weather_data10$WindGustDir)
weather_data10$WindDir9am=as.numeric(weather_data10$WindDir9am)
weather_data10$WindDir3pm=as.numeric(weather_data10$WindDir3pm)

#Convert Raintomrrow data to numeric
library(plyr)
weather_data10
weather_data10$RainTomorrow <- revalue(weather_data10$RainTomorrow, c("Yes"=1))
weather_data10$RainTomorrow <- revalue(weather_data10$RainTomorrow, c("No"=0))

#Data us split to test and train data in the ratio 75:25
weather_data10
library(caTools)
set.seed(123)
split = sample.split(weather_data10$RainTomorrow, SplitRatio = 0.75)
training_set = subset(weather_data10, split == TRUE)
test_set = subset(weather_data10, split == FALSE)
training_set$RainTomorrow
# Feature Scaling
training_set[-12] = scale(training_set[-12])
test_set[-12] = scale(test_set[-12])

#MODELLING--------------------------------------------------------------------------------------------------

#Multiple Linear Regression----------------------
# Fitting Logistic Regression to the Training set
classifier = glm(formula = RainTomorrow ~ .,
                 family = binomial,
                 data = training_set)
summary(classifier)
#Predict using test set
prob_pred = predict(classifier, type = 'response')
prob_prd_glm=predict(classifier, type = 'response', newdata = test_set[-12])
y_pred = ifelse(prob_prd_glm > 0.5, 1, 0)
#
library(ROCR)
ROCRpred <- prediction(prob_pred, training_set$RainTomorrow)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
#
cm3 = table(test_set[,12], y_pred)
cm3
#
#Plot the predictin against all independednt variables
install.packages("effects")
library(effects)
plot(allEffects(classifier))
install.packages("sjp")
library(sjPlot)
sjp.glm(classifier, type = "eff", show.ci = T)
#
summary(training_set)
training_set$RainTomorrow=as.numeric(training_set$RainTomorrow)
is.numeric(training_set$RainTomorrow)

#KNN---------------------------------------------------------------------------------------------
set.seed(200)
library(class)
training_set$RainTomorrow<-ifelse(training_set$RainTomorrow == "yes",1,0)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(  RainTomorrow ~ MaxTemp+Sunshine+WindGustSpeed+Humidity9am+Pressure3pm+Cloud3pm+Temp9am+Temp3pm+WindGustDir+WindDir9am+WindDir3pm+RainTomorrow, data = training_set, method = "knn",
 trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
y_predknn = knn(train = training_set[, -12],
                test = test_set[, -12],
                cl = training_set[, 12],
                k = 5,
                prob = TRUE)

y_predknn

attributes(y_predknn)$prob

library(pROC)

roc(test_set$RainTomorrow, attributes(y_predknn)$prob)

plot(roc(test_set$RainTomorrow, attributes(y_predknn)$prob),
     print.thres = T,
     print.auc=T)
# Making the Confusion Matrix
cm_knn = table(test_set[, 12], y_predknn)
cm_knn



#SVM--------------------------------------------------------------------------------------
library(e1071)
svmfit = svm(formula = RainTomorrow ~ .,
             data = training_set,
             type = 'C-classification',
             kernel = 'linear')

# Predicting the Test set results
y_pred_svm = predict(svmfit, newdata = test_set[-12])

# Making the Confusion Matrix
cm_svm = table(test_set[, 12], y_pred_svm)

#plot the output of svm
plot(svmfit, test_set[-12])


#From the output of the algorithms 
#multiplr linear regression
#25512  4603
#2067   3366

#knn output
#25968  4606
#1611  3363

#svm output
#26344  5043
#1235   2926

#Accuracy
#Linear Regression=81.2
#Svm=82
#knn=82.5
# randomForest :86

#Also checked Random forest algorithm and found that random forest is the most suitable model compared to the rest of the models
#randomforest
install.packages("randomForest")
library(randomForest)
set.seed(100)
model2 <- randomForest( RainTomorrow ~ ., data = training_set, ntree = 500, mtry = 6, 
                       importance = TRUE)
model2
plot(model2, main = "Error rate of random forest")
varImpPlot(model2, pch = 20, main = "Importance of Variables")
predTrain <- predict(model2, newdata = test_set[-12], type = "prob")
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(RainTomorrow ~ ., data = training_set, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, newdata = test_set[-12], type = "class")
  a[i-2] = mean(predValid == test_set$RainTomorrow)
}

a

plot(3:8,a)
require(pROC)
roc(test_set$RainTomorrow, attributes(predTrain)$prob)

plot(roc(test_set$RainTomorrow, attributes(y_predknn)$prob),
     print.thres = T,
     print.auc=T)
predTrain
y_pred_rf = ifelse(predTrain > 0.5, 1, 0)
plot(roc(test_set$RainTomorrow,y_pred_rf[,2]),
     print.thres = T,
     print.auc=T)

cm_rf1 = table(test_set[,-12], predTrain)
cm_rf1



# Making the Confusion Matrix
# View the forest results.
plot(model2)
library(class)
library(e1071)
library(ROCR)
nb_rain<-naiveBayes(RainTomorrow~.,data=training_set)
nb_pred<-predict(nb_rain,test_set)

nb_pred
library(pROC)


plot(roc(test_set$RainTomorrow,nb_pred[,2]),
     print.thres = T,
     print.auc=T)
#Checking the data in comfusion matrix
x<-table(nb_pred,test_set$RainTomorrow)
x
acc<-((sum(diag(x)))/sum(x))
acc

log_Rain<-glm(RainTomorrow~.,family=binomial(link="logit"),training_set)
glm_pred <- predict(log_Rain, test_set, type="response")
summary(log_Rain)
install.packages("pscl")
library(pscl)
pR2(log_Rain)

glm_pred

