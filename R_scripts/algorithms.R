library(ggplot2)
library(OneR)
library(ROCR)
library("e1071")

setwd("~/Desktop/tcd_study/Semester_2/Text_Analytics/Pratical/Data")
df <- read.csv("data_punctuation1.csv",sep = ',')
#LIWC resluts
data <- read.csv("temp.csv")
data <- merge(data, df, by="X")
data <- data[,-c(1,2)]
cor(data, y = data['label'])
data['label'] <- as.factor(data$label)
#split data
data_Train <- subset(data, data$type == 0)[,-65]
data_Test <- subset(data, data$type == 1)[,-65]


#logistic regression
confunsion_matrix <- function(model,test){
  prediction <- round(predict(model, type = 'response', test))
  pred <- prediction(prediction, test$label)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  plot(perf, col=rainbow(10))
  eval_model(prediction, test$label) 
}

glm <- glm(label ~ ., data = data_Train, family = "binomial")
glm_baseline <- glm(label ~ ., data = data_Train[,-c(65:67)], family = "binomial")
glm_question_mark <- glm(label ~. , data = data_Train[,-c(66,67)], family = "binomial")
glm_exclamation_mark <- glm(label ~. , data = data_Train[,-c(65,67)], family = "binomial")
glm_sentence_length <- glm(label ~ . ,data = data_Train[,-c(65,66)], family = "binomial")
confunsion_matrix(glm, data_Test)
confunsion_matrix(glm_baseline, data_Test[,-c(65:67)]) #0.7741
confunsion_matrix(glm_question_mark, data_Test[,-c(66,67)])
confunsion_matrix(glm_exclamation_mark, data_Test[,-c(65,67)])
confunsion_matrix(glm_sentence_length, data_Test[,-c(65,66)])

#Support Vector Machine
result <- function(model,test){
  prediction <- predict(model, type = 'response', test)
  pred <- prediction(as.numeric(prediction), as.numeric(test$label))
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  plot(perf, col=rainbow(10))
  eval_model(prediction, test$label) 
}
SVM <- svm(label ~ ., data = data_Train)
result(SVM, data_Test)
SVM_baseline <- svm(label ~ ., data = data_Train[,-c(65:67)])
result(SVM_baseline, data_Test[,-c(65:67)])

SVM_question_mark <- svm(label ~ ., data = data_Train[,-c(66,67)])
result(SVM_question_mark, data_Test[,-c(66,67)])

SVM_exclamation_mark <- svm(label ~ ., data = data_Train[,-c(65,67)])
result(SVM_exclamation_mark, data_Test[,-c(65,67)])

SVM_sentence_length <- svm(label ~ ., data = data_Train[,-c(65,66)])
result(SVM_sentence_length, data_Test[,-c(65,66)])
