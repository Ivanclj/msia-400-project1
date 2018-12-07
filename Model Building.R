#Model Building

# library
library(readr)
library(tidyverse)
library(ggplot2)

# read in data
data_full <- read_csv('/nfs/home/lcu1428/z/400_proj/data_full.csv')
data_engineer <- read_csv('/nfs/home/lcu1428/z/400_proj/data_engineer.csv')
data_driver <- read_csv('/nfs/home/lcu1428/z/400_proj/data_driver.csv')


##ENGINEER

# Data Preparation
# response variable
data_engineer$filled <- 0
data_engineer$filled[data_engineer$time_to_fill <= 35] <- '< 1 month'
data_engineer$filled[data_engineer$time_to_fill > 35 & data_engineer$time_to_fill <= 65] <- '< 2 month'
data_engineer$filled[data_engineer$time_to_fill > 65] <- '> 2 month'
table(data_engineer$filled)

# get month
library(lubridate)
data_engineer$month <- month(data_engineer$post_date) 
# adjust for over exploded jobs
data_engineer <-data_engineer[data_engineer$post_date > as.Date('2017-07-31'),]

# make factors
data_engineer$filled <- as.factor(data_engineer$filled)
data_engineer$month <- as.factor(data_engineer$month)
data_engineer$state[is.na(data_engineer$state)] <- '-1'
data_engineer$state <- as.factor(data_engineer$state)

# make dummy for tags


# make prediction table
eng.predict <- data_engineer[,c('filled','state','month','salary')]

# train test split
set.seed(123)
test.idx<-sample(1:nrow(eng.predict),size = 0.2*nrow(eng.predict))
eng.test<-eng.predict[test.idx,]
eng.train<-eng.predict[-test.idx,]


#Randomforest
library(randomForest)
library(pROC)

#model
raw.rf<-randomForest(filled~state+month+salary,data=eng.train,importance=T,ntree=400)
Rank.Variable.Importance<-raw.rf
plot(raw.rf)
importance(raw.rf)->zz
varImpPlot(Rank.Variable.Importance,cex=0.9)

#predict

pred<-predict(raw.rf,eng.test,type = 'class')
t<-table(pred=pred,actual=eng.test$filled)
t
sum(diag(t)/sum(t))

library(pROC)
pred.p<-predict(raw.rf,eng.test,type = 'prob')
pred.p %>% head
auc<-auc(eng.test$filled,pred.p[,2])
auc
plot(roc(eng.test$filled,pred.p[,2]))

#find best mtry
best.mtry<-tuneRF(eng.train,eng.train$filled,ntreeTry = 100,stepFactor = 1,improve = 0.05,trace = T,
                  plot = T)
#Cross validation 
k_fold_x_valid<-function(d,k){
  set.seed(123)
  size<-nrow(d)/k
  loss.vec<-1:k
  loss.plot<-1:k
  model.vec<-1:k
  for(ii in 1:k){
    #train-test split
    test.idx<-((ii-1)*size+1):(ii*size)
    print(length(test.idx))
    test<-d[test.idx,]
    train<-d[-test.idx,]
    print(nrow(train))
    
    #model
    raw.rf<-randomForest(relevel(filled,ref = '< 1 month')~.,data=train,importance=T,ntree=400)
    model.vec[ii]<-raw.rf
    loss.plot[ii]<-plot(raw.rf)
    raw.pred<-predict(raw.rf,test)
    print(table(raw.pred,test$filled))
    err.rf<-sum((as.numeric(test$filled)-as.numeric(raw.pred))^2)/nrow(test)
    loss.vec[ii]<-err.rf
    print(paste('error:',err.rf))
  }
  print('ada')
  rst<-list(model.vec,loss.plot,loss.vec)
  return(rst)
  
}

rst<-k_fold_x_valid(eng.predict,10)
rst
## SVM
library(e1071)
eng.svm <- svm(relevel(filled,ref = '< 1 month') ~ ., data = eng.train)
summary(eng.svm)

plot(eng.svm,eng.train)

svm.pred <- predict(eng.svm,eng.test,type='response')

table(pred = svm.pred,actual = eng.test$filled)# 0.58

       