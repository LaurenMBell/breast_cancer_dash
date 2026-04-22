# survival calculator for the breast cancer transcriptomics dashboard! 
#naive bayes model, 1=survived, 0=died

#next goal: implement naive bayes without a library import

library(e1071)
library(caTools)
library(caret)
library(dplyr)

data <- read.csv('data/METABRIC_RNA_Mutation.csv')
data <- data[,-1] #drop patient ID
data %>% relocate(overall_survival)

### map all text features to discrete 

as.factor(data$type_of_breast_surgery)


set.seed(123)
split <- sample.split(data, SplitRatio=0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

train_scale <- scale(train[,-1])
test_scale <- scale(test[,-1])

s_classifier <- naiveBayes(overall_survival ~ ., data = data)
classifier

y_pred <- predict(s_classifier, newdata=test)

#confusion matrix
cm <- table(test$overall_survival, y_pred)
confusionMatrix(cm)



