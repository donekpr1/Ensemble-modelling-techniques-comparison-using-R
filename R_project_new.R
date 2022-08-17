
library(dplyr)
library(tidyverse)
library(gridExtra)
library(caret)
library(caretEnsemble)
library(ROCR)
library(randomForest)
library(caretEnsemble)
library(ggplot2)
library(ggcorrplot)
library(skimr)
library(yardstick)
library(ROSE)
emp_data=read_csv("https://raw.githubusercontent.com/IBM/employee-attrition-aif360/master/data/emp_attrition.csv")
skim(emp_data)
table(emp_data$Attrition)

attach(emp_data)
data.frame(colnames(emp_data)) #Returns column index numbers in table format,df=DataFrame name

#Exploratory data analysis
#Checking the attrition %
ggplot(emp_data,aes(Attrition,fill=Attrition))+geom_bar()
prop.table(table(emp_data$Attrition)) 

#2
library(grid)
library(gridExtra)
agePlot <- ggplot(emp_data,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
travelPlot <- ggplot(emp_data,aes(BusinessTravel,fill=Attrition))+geom_bar()
ratePlot <- ggplot(emp_data,aes(DailyRate,Attrition))+geom_point(size=4,alpha = 0.05)
depPlot <- ggplot(emp_data,aes(Department,fill = Attrition))+geom_bar()
grid.arrange(agePlot,travelPlot,ratePlot,depPlot,ncol=2,top = "Fig 1")

#There are 9 character variables which needs to be converted to numeric according to their column numbers 
sapply(emp_data,class)
nearZeroVar(emp_data)

#There are few variables which have no variability,hence dropping them

emp_data$Over18 <- NULL
emp_data$EmployeeCount <- NULL
emp_data$StandardHours <- NULL
emp_data$EmployeeNumber <- NULL
dim(emp_data)
head(emp_data)
names(emp_data)
#columns with missing values
colnames(emp_data)[colSums((is.na(emp_data)))>0]


#Checking for duplicates
nrow(emp_data[!duplicated(emp_data),])
class(Attrition)
emp_data$Age
#binning of continous variables
emp_data$AgeGroup <- with(emp_data,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) #Creating Age Groups

emp_data$DistanceGroup <- with(emp_data,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1)))))) #Creating Distance Groups


emp_data$YearsWithManagerGroup <- with(emp_data,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1))))) #Creating YearsWithManager Groups


#Training1_os$AvgSatisGroup <- with(Training1_os,ifelse(AvgSatis<2.5,1,2)) # Create Average Satisfaction Groups

emp_data$WorkYearGroup <- with(emp_data,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

emp_data$NumCompGroup <- with(emp_data,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) #Creating Number of Companies Worked

#Converting dependent variable to 0 s and 1s
emp_data$Attrition[emp_data$Attrition=="Yes"]=1
emp_data$Attrition[emp_data$Attrition=="No"]=0
emp_data$OverTime[emp_data$OverTime=="Yes"]=1
emp_data$OverTime[emp_data$OverTime=="No"]=0
emp_data$OverTime=as.numeric(emp_data$OverTime)
emp_data$Attrition=as.numeric(emp_data$Attrition)
emp_data$Gender[emp_data$Gender=="Male"]=1
emp_data$Gender[emp_data$Gender=="Female"]=0
emp_data$Gender=as.numeric(emp_data$Gender)

class(emp_data$AgeGroup)
#Converting chacters to factors
sapply(emp_data, is.character)->char_var
lapply(emp_data[,char_var],as.factor)-> emp_data[,char_var]
#Converting integer to numeric
sapply(emp_data,is.integer)->int_var
emp_data[,int_var]<-lapply(emp_data[,int_var], as.numeric)

sapply(emp_data,class)



#Checking the skewness in the data
p1 <- ggplot(emp_data) + geom_histogram(aes(MonthlyIncome), binwidth = 1000, fill = "blue",col = "black")
p2 <- ggplot(emp_data) + geom_histogram(aes(PercentSalaryHike), binwidth = 1, fill = "blue",col = "black")
p3 <- ggplot(emp_data) + geom_histogram(aes(YearsAtCompany), binwidth = 2, fill = "blue",col = "black")
p4 <- ggplot(emp_data) + geom_histogram(aes(YearsInCurrentRole), binwidth = 2, fill = "blue",col = "black")
p5 <- ggplot(emp_data) + geom_histogram(aes(YearsSinceLastPromotion), binwidth = 2, fill = "blue",col = "black")
p6 <- ggplot(emp_data) + geom_histogram(aes(YearsWithCurrManager), binwidth = 2, fill = "blue",col = "black")

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)

#Applying log transformation to reduce skewness on the resulted plot features

emp_data <- emp_data %>%
  mutate(AgeGroup = log(AgeGroup + 1)
         ,DailyRate = log(DailyRate + 1)
         ,DistanceGroup = log(DistanceGroup + 1)
         ,HourlyRate = log(HourlyRate + 1)
         ,MonthlyIncome = log(MonthlyIncome + 1)
         ,MonthlyRate = log(MonthlyRate + 1)
         ,NumCompGroup = log(NumCompGroup + 1)
         ,PercentSalaryHike = log(PercentSalaryHike + 1)
         ,WorkYearGroup = log(WorkYearGroup + 1)
         ,TrainingTimesLastYear = log(TrainingTimesLastYear + 1)
         ,YearsAtCompany = log(YearsAtCompany +1)
         ,YearsInCurrentRole = log(YearsInCurrentRole + 1)
         ,YearsSinceLastPromotion = log(YearsSinceLastPromotion + 1)
         ,YearsWithManagerGroup = log(YearsWithManagerGroup + 1)
          )
head(emp_data)
class(emp_data$Gender)
as.numeric(emp_data$Gender)
emp_data$OverTime
corr_data  <- emp_data %>%
select(AgeGroup,Gender,DistanceGroup,WorkYearGroup,YearsWithManagerGroup,NumCompGroup,OverTime,PerformanceRating,JobLevel,Attrition,YearsAtCompany,PercentSalaryHike,JobSatisfaction,OverTime,WorkLifeBalance,Education,NumCompaniesWorked,EnvironmentSatisfaction,HourlyRate)

#Correlation analysis to check if any multicollinearity exists and also any less correlation between predictor vs target
ggcorrplot(cor(corr_data), hc.order = TRUE, lab = TRUE, lab_size = 2) +
  labs(title = "Correlation Between Variables and Attrition",
       subtitle = "Netural and Positive Correlation",
       caption = "Source: IBM HR Analytics") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Standardization

prep_num = preProcess(emp_data[-2], method=c("center", "scale"))
final_dataset = predict(prep_num, emp_data)

#Removing correlated independent variables to avoid multicollinearity
high_corr <- findCorrelation(cor(corr_data), cutoff = 0.7)
names(emp_data)[high_corr]
#names(emp_data)
#emp_data$AgeGroup
#final_dataset <- cbind(trsf, emp_data[2])

names(final_dataset)

final_dataset <- emp_data %>%
select(-JobLevel,-Age,-PerformanceRating,-HourlyRate,-YearsAtCompany,-MonthlyIncome,-TotalWorkingYears,-YearsWithCurrManager,-YearsInCurrentRole,-TotalWorkingYears,-YearsSinceLastPromotion,-Education,-Gender,-OverTime)
#Train test split
dim(final_dataset)
set.seed(42)
Train <- createDataPartition(final_dataset$Attrition, p=0.7, list=FALSE)
training <- final_dataset[ Train, ]
testing <- final_dataset[ -Train, ]
dim(testing)
prop.table(table(emp_data$Attrition))
prop.table(table(training$Attrition))
prop.table(table(testing$Attrition))

#Applying oversampling to to treat the imbalance as minority class 0 is less

train_balanced <- ovun.sample(Attrition ~ ., data = training, method = "over",
                              N = 996*2, seed = 1)$data
table(train_balanced$Attrition)

#Modelling
#Logistic Regression
#final_dataset$Attrition
#logmodel <- glm(training$Attrition ~., family=binomial(link="logit"), data = training)
#summary(logmodel)
control <- trainControl(method="repeatedcv", number=10,repeats = 5)
set.seed(42)
model_lr <- train(as.factor(Attrition)~., data=train_balanced, method="glm", trControl=control)
print(model_lr)
pred_lr <- predict(model_lr, newdata=testing)

confusionMatrix(pred_lr, as.factor(testing$Attrition))

ROCRpred <- prediction(as.numeric(pred_lr), as.factor(testing$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_lr, digits=5, scientific=FALSE)))
#F_meas(data=test,estimate=pred_lr,truth=as.factor(testing$Attrition))
#precision <- posPredValue(as.factor(testing$Attrition),pred_lr, positive="1")
#recall <- sensitivity(as.factor(testing$Attrition),pred_lr, positive="1")
#f1_lr <- (2*precision*recall)/(precision+recall)
#print(f1_lr)
library(MLmetrics)
F1_score_lr = F1_Score(pred_lr,as.factor(testing$Attrition))
print(F1_score_lr)

#Randomforest
control <- trainControl(method="repeatedcv", number=10,repeats=5,search='grid')
set.seed(42)
mtry<-sqrt(ncol(train_balanced))
tunegrid<-expand.grid(.mtry=mtry)
model_rf <- train(as.factor(Attrition)~., data=train_balanced, method="rf",metric='Accuracy',tuneGrid=tunegrid,trControl=control)
print(model_rf)
#plot(model_rf)
pred_rf <- predict(model_rf, newdata=testing)
head(pred_rf)
head(train_balanced$Attrition)
varImp(model_rf, type=2)
print(model_rf)
confusionMatrix(pred_rf, as.factor(testing$Attrition))
ROCRpred <- prediction(as.numeric(pred_rf), as.factor(testing$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_rf <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_rf, digits=5, scientific=FALSE)))
F1_score_rf = F1_Score(pred_rf,as.factor(testing$Attrition))
print(F1_score_rf)

#Bagging
# Bagging Decision tree
set.seed(42)

control1 <- trainControl(method="repeatedcv", number=10, repeats=5)

bagCART_model <- train(as.factor(Attrition) ~ ., dat=train_balanced, method="treebag", metric="Accuracy", trControl=control1)

#Predictions on the test set
predictTest1 = predict(bagCART_model, newdat = testing)
head(predictTest1)
# Confusion matrix on test set
confusionMatrix(predictTest1,as.factor(testing$Attrition))

ROCRpred <- prediction(as.numeric(predictTest1), as.factor(testing$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_bag <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_bag, digits=5, scientific=FALSE)))
F1_score_bag = F1_Score(predictTest1,as.factor(testing$Attrition))
print(F1_score_bag)
#Boosting
library('xgboost')
set.seed(42)
control2 <- trainControl(method="repeatedcv", number=5, repeats=3)

xgb_model <- train(as.factor(Attrition) ~., dat=train_balanced, method="xgbTree", metric="Roc", trControl=control2,verbose=FALSE)

predictTest3 = predict(xgb_model, newdat = testing)

head(predictTest3)
# Confusion matrix on test set
confusionMatrix(predictTest3,as.factor(testing$Attrition))
ROCRpred <- prediction(as.numeric(predictTest3), as.factor(testing$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_xboost <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_xboost, digits=5, scientific=FALSE)))
F1_score_xboost = F1_Score(predictTest3,as.factor(testing$Attrition))
print(F1_score_xboost)
#Gradient Boosting
set.seed(42)
control2 <- trainControl(method="repeatedcv", number=5, repeats=3)

gbm_model <- train(as.factor(Attrition) ~., dat=train_balanced, method="gbm", metric="Roc", trControl=control2,verbose=FALSE)

predictTest4 = predict(gbm_model, newdat = testing)

head(predictTest3)
# Confusion matrix on test set
confusionMatrix(predictTest4,as.factor(testing$Attrition))
ROCRpred <- prediction(as.numeric(predictTest3), as.factor(testing$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_gboost <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_gboost, digits=5, scientific=FALSE)))
F1_score_gboost = F1_Score(predictTest4,as.factor(testing$Attrition))
print(F1_score_gboost)
#Stacking

train_balanced$Attrition<-as.factor(make.names(train_balanced$Attrition))
testing$Attrition<-as.factor(make.names(testing$Attrition))
set.seed(42)
control_stacking <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions="all",classProbs=TRUE,summaryFunction=twoClassSummary,index=createFolds((train_balanced$Attrition), 5))

algorithms_to_use <- c( 'svmRadial','glm','knn','rpart')

levels(train_balanced$Attrition)<-c("No","Yes")
levels(testing$Attrition)<-c("No","Yes")
levels(testing$Attrition)
stacked_models <- caretList(Attrition ~., dat=train_balanced, trControl=control_stacking, methodList=algorithms_to_use)

stacking_results <- resamples(stacked_models)

summary(stacking_results)
dotplot(stacking_results)
modelCor(stacking_results)
splom(stacking_results)
stackControl <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE)
set.seed(42)

glm_stack <- caretStack(stacked_models, method="glm",metric="ROC", trControl=stackControl)

print(glm_stack)
summary(glm_stack)
testing_var=subset(testing,select=-c(Attrition))

pred = predict(glm_stack,testing_var)

cm = confusionMatrix(as.factor(testing$Attrition), pred)
print(cm)
F1_score_stack = F1_Score(pred,as.factor(testing$Attrition))
print(F1_score_stack)
print(data.frame(F1_score_rf,F1_score_bag,F1_score_gboost,F1_score_xboost,F1_score_stack))
