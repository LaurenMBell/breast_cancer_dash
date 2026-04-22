# survival calculator for the breast cancer transcriptomics dashboard! 
#naive bayes model, 1=survived, 0=died

#next goal: implement naive bayes without a library import

library(e1071)
library(caTools)
library(caret)
library(dplyr)

data <- read.csv('data/METABRIC_RNA_Mutation.csv')
data <- data[,-1] #drop patient ID
data$oncotree_code <- NULL
data$primary_tumor_laterality <- NULL
data$integrative_cluster <- NULL

data <- data %>% relocate(overall_survival)
data <- na.omit(data)

### map all text features to discrete 
data <- data %>%
  mutate(across(c(cancer_type, cancer_type_detailed, cellularity, 
                  type_of_breast_surgery, pam50_._claudin.low_subtype, 
                  er_status_measured_by_ihc, er_status, 
                  her2_status_measured_by_snp6, her2_status,
                  tumor_other_histologic_subtype, inferred_menopausal_state, 
                  pr_status, X3.gene_classifier_subtype, 
                  death_from_cancer), as.factor))


set.seed(123)
split <- sample.split(data, SplitRatio=0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

s_classifier <- naiveBayes(overall_survival ~ ., data = train)

y_pred <- predict(s_classifier, newdata=test)

#confusion matrix
cm <- table(test$overall_survival, y_pred)
confusionMatrix(cm)



