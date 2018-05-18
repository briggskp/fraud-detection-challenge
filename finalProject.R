test.data<- read.delim(file.choose(), sep=",")
train.data<- read.delim(file.choose(), sep= ",")

library(caret)
library(ROCR)
Class<- train.data$is_attributed



train.data$Class= ifelse(train.data$is_attributed==1, 'Y','N')
f <- formula(Class~as.factor(app)+as.factor(device))

ctr <- trainControl(
  p=0.75,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

ctr = trainControl(p<-.75)
fit_rf = train(f,
               data = train.data,
               method = 'rf',
               trControl = ctr,
               metric = 'ROC',
               num_trees = 2)

fit_rpart = train(f,
                  data = train.data,
                  method = 'rpart',
                  trControl = ctr
)

fit_glm = train(f,
                data = train.data,
                method = 'glm',
                preProcess = c('zv','nzv','pca'),
                trControl = ctr,
                metric = 'ROC')

#ggplot for curve

basicplot <- ggplot(test.data,aes(Class,device)) + geom_roc() 
basicplot

#prediction

labels_rf = ifelse(test.data$Class %in% 'Bad',1,0)
preds_rf = predict(fit_rf, test.data, type = 'prob')[, 'Bad']
preds_rpart = predict(fit_rpart, test.data, type = 'prob')[, 'Bad']
preds_glm = predict(fit_glm, test.data, type = 'prob')[, 'Bad']


