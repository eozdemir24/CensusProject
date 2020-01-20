#installing libraries

library(ggplot2)
library(tidyverse)
library(moments)
library(caret)
library(ROCR)
library(corrplot)
library(randomForest)




#install datasets
train <- read.csv('C:/Users/eozde/Desktop/data123/train.csv', stringsAsFactors = F, header = TRUE, na.strings = '' )
test <- read.csv('C:/Users/eozde/Desktop/data123/test.csv', stringsAsFactors = F, header = TRUE, na.strings = '')
sample.sub <- read.csv('C:/Users/eozde/Desktop/data123/sample_submission.csv', stringsAsFactors = F, header = TRUE, na.strings = '')




#checking missing values

is.na(train$Workclass)
table(is.na(train$Workclass))

is.na(test$Workclass)
table(is.na(test$Workclass))

is.na(train$Occupation)
table(is.na(train$Occupation))

is.na(test$Occupation)
table(is.na(test$Occupation))

is.na(train$Country)
table(is.na(train$Country))

is.na(test$Country)
table(is.na(test$Country))

#complete empty cell with missing

train$Workclass[is.na(train$Workclass)] <- 'missing'
train$Occupation[is.na(train$Occupation)] <- 'missing'
train$Country[is.na(train$Country)] <- 'missing'


test$Workclass[is.na(test$Workclass)] <- 'missing'
test$Occupation[is.na(test$Occupation)] <- 'missing'
test$Country[is.na(test$Country)] <- 'missing'


#transformation factor to integer in train dataset.

train$Workclass <- as.integer(as.factor(train$Workclass))
train$Occupation <- as.integer(as.factor(train$Occupation))
train$Country <- as.integer(as.factor(train$Country))
train$Education_Num <- as.integer(as.factor(train$Education_Num))
train$Hours_per_week <- as.integer(as.factor(train$Hours_per_week))

#adjusted zero and one Target column in train dataset
train$Target <- as.integer(as.factor(train$Target)) - 1
train$Target <- ifelse(train$Target == 0, 'zero', 'one')
train$Target <- factor(train$Target, levels = c('zero', 'one'))


#transformation factor to integer in test dataset.
test$Workclass <- as.integer(as.factor(test$Workclass))
test$Occupation <- as.integer(as.factor(test$Occupation))
test$Country <- as.integer(as.factor(test$Country))
test$Education_Num <- as.integer(as.factor(test$Education_Num))
test$Hours_per_week <- as.integer(as.factor(test$Hours_per_week))

#drawing correlation matrix
numeric.feats <- names(train)[sapply(train, is.numeric)]
corrplot(cor(train[, numeric.feats]))


#make a validation dataset
sample = createDataPartition(train$Target, p=0.8, list=F)


#ProcessingPart
processed.train = train[sample, ]
processed.validationset  = train[-sample, ]


processed.train$Id <- NULL
processed.validationset$Id <- NULL
test$Id <- NULL
train$id <- NULL


numFolds = trainControl( method = "cv", number = 7, classProbs = TRUE, summaryFunction=twoClassSummary)


model <- caret::train(
  Target ~.,
  data = processed.train,
  verbose = FALSE,
  method = 'rf',
  preProcess = c('pca'),
  trControl = numFolds,
  metric = "ROC",
  tuneLenght = 1
)
preds_prob <- predict(model,processed.validationset, type = 'prob')
preds <- predict(model,processed.validationset)


#confusionmatrix

xtab <- table(preds, processed.validationset$Target)
confusionMatrix(xtab)


confusion_matrix <- as.data.frame(table(predicted_class, actual_class))

#prediction of probability.

all_probs <- predict(model, test , type = "prob")
sample.sub$Target <- all_probs[,2]


write.csv(sample.sub, 'submission.csv', row.names = FALSE)
