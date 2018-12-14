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
#data_engineer$filled[data_engineer$time_to_fill > 35 & data_engineer$time_to_fill <= 65] <- '< 2 month'
#data_engineer$filled[data_engineer$time_to_fill > 65] <- '> 2 month'
data_engineer$filled[data_engineer$time_to_fill > 35] <- '> 1 month'

table(data_engineer$filled)

data_driver$filled <-0
data_driver$filled[data_driver$time_to_fill <= 35] <- '< 1 month'
#data_driver$filled[data_driver$time_to_fill > 35 & data_driver$time_to_fill <= 65] <- '< 2 month'
#data_driver$filled[data_driver$time_to_fill > 65] <- '> 2 month'
data_driver$filled[data_driver$time_to_fill > 35] <- '> 1 month'

table(data_driver$filled)

# merge two dfs
data.predict <- rbind(data_engineer,data_driver)

# Check if tags are good for predict time to fill 
temp <- data_engineer
a <- rep(0,nrow(temp))
for(i in 1:nrow(temp)){
  tags <- strsplit(temp$tags[i],',')[[1]]
  a[i] <- sum(perc_fill_eng[names(perc_fill_eng)%in%tags])
}
cor(temp$time_to_fill,a)#0.009
sum(abs(temp$time_to_fill-a))/nrow(temp)# on avg off by 30.

# b <- ifelse(a <=35,'< 1 month',ifelse(a<=65, '< 2 month','> 2 month'))
# conf_matx <- table(b,temp$filled)
# conf_matx
# sum(diag(conf_matx))/sum(conf_matx) #0.38

b <- ifelse(a <=35,'< 1 month','> 1 month')
conf_matx <- table(b,temp$filled)
conf_matx
sum(diag(conf_matx))/sum(conf_matx) #0.506
# look at distribution of time to fill for min occurence tags, max occurences tags,
# as well as hardest to fill tags and easierst to fill tags.
get_tags <- function(tags){return(strsplit(tags,',')[[1]])}
min_tags <- sort(top_eng_skills)[1:10]
max_tags <- sort(top_eng_skills,decreasing = T)[1:10]
min_perc_tags <- sort(perc_fill_eng)[1:10]
max_perc_tags <- sort(perc_fill_eng,decreasing = T)[1:10]

min_idxes <- c()
max_idxes <- c()
min_perc <- c()
max_perc <- c()
for(i in 1:nrow(temp)){
  if(any(get_tags(temp$tags[i])%in%names(min_tags))){
    min_idxes <- c(min_idxes,i)
  }
  if(any(get_tags(temp$tags[i])%in%names(max_tags))){
    max_idxes <- c(max_idxes,i)
  }
  if(any(get_tags(temp$tags[i])%in%names(min_perc_tags))){
    min_perc <- c(min_perc,i)
  }
  if(any(get_tags(temp$tags[i])%in%names(max_perc_tags))){
    max_perc <- c(max_perc,i)
  }
  
}

min_df <-temp[min_idxes,]
max_df <- temp[max_idxes,]
min_p_df <-temp[min_perc,]
max_p_df <- temp[max_perc,]

summary(min_df$time_to_fill)
summary(max_df$time_to_fill)
summary(min_p_df$time_to_fill)
summary(max_p_df$time_to_fill)

ddf <- rbind(data.frame(tof = min_df$time_to_fill, status ='1'),
             data.frame(tof = max_df$time_to_fill, status ='2'),
             data.frame(tof = min_p_df$time_to_fill, status ='3'),
             data.frame(tof = max_p_df$time_to_fill, status ='4'))
ddf$status <- as.factor(ddf$status)
ggboxplot(ddf, x = "status", y = "tof", fill = 'status',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))# all look the same

min_idxes%in%max_idxes %>%  sum() # high overlapping
min_perc%in%max_perc %>% sum() # high overlapping

# check with non overlapping
min_idxes_filted <- min_idxes[!min_idxes%in%(intersect(min_idxes,max_idxes))]
max_idxes_filted <- max_idxes[!max_idxes%in%(intersect(min_idxes,max_idxes))]
min_perc_filted <- min_perc[!min_perc%in%(intersect(min_perc, max_perc))]
max_perc_filted <- max_perc[!max_perc%in%(intersect(min_perc, max_perc))]

# check again
min_df <-temp[min_idxes_filted,]
max_df <- temp[max_idxes_filted,]
min_p_df <-temp[min_perc_filted,]
max_p_df <- temp[max_perc_filted,]

summary(min_df$time_to_fill)
summary(max_df$time_to_fill)
summary(min_p_df$time_to_fill)
summary(max_p_df$time_to_fill)

ddf <- rbind(data.frame(tof = min_df$time_to_fill, status ='1'),
             data.frame(tof = max_df$time_to_fill, status ='2'),
             data.frame(tof = min_p_df$time_to_fill, status ='3'),
             data.frame(tof = max_p_df$time_to_fill, status ='4'))
ddf$status <- as.factor(ddf$status)
ggboxplot(ddf, x = "status", y = "tof", fill = 'status',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))# all look the same

# tags does not affect that much

# get month
library(lubridate)
data.predict$month <- month(data.predict$post_date) 
# adjust for over exploded jobs
data.predict <-data.predict[data.predict$post_date > as.Date('2017-07-31'),]

# make factors
data.predict$filled <- as.factor(data.predict$filled)
data.predict$month <- as.factor(data.predict$month)
data.predict$state[is.na(data.predict$state)] <- '-1'
data.predict$state <- as.factor(data.predict$state)


# make prediction table
data.predict <- data.predict[,c('filled','state','month','salary','role')]

# train test split
set.seed(123)
test.idx<-sample(1:nrow(data.predict),size = 0.2*nrow(data.predict))
data.test<-data.predict[test.idx,]
data.train<-data.predict[-test.idx,]


#Randomforest
library(randomForest)
library(pROC)

#model
raw.rf<-randomForest(filled~state+month+salary,data=data.train,importance=T,ntree=400)
Rank.Variable.Importance<-raw.rf
plot(raw.rf)
importance(raw.rf)->zz
varImpPlot(Rank.Variable.Importance,cex=0.9)

#predict

pred<-predict(raw.rf,data.test,type = 'class')
t<-table(pred=pred,actual=data.test$filled)
t
# ccr
sum(diag(t)/sum(t)) 0.66
# f score
recall <- t[2,2]/(t[2,2]+t[2,1])
precision <- t[2,2]/(t[2,2]+t[1,2])
2*recall*precision/(recall+precision)#0.30

pred.p<-predict(raw.rf,data.test,type = 'prob')
pred.p %>% head
auc<-auc(data.test$filled,pred.p[,2])
auc
plot(roc(data.test$filled,pred.p[,2]))

#find best mtry
best.mtry<-tuneRF(data.train,data.train$filled,ntreeTry = 100,stepFactor = 1,improve = 0.05,trace = T,
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

#rst<-k_fold_x_valid(data.predict,10)
#rst
## SVM
library(e1071)
data.svm <- svm(relevel(filled,ref = '< 1 month') ~ ., data = data.train)
summary(data.svm)

#plot(data.svm,data.train)

svm.pred <- predict(data.svm,data.test,type='response')

conf_matx <- table(pred = svm.pred,actual = data.test$filled)
conf_matx
sum(diag(conf_matx))/sum(conf_matx) #0.64
recall <- conf_matx[2,2]/(conf_matx[2,2]+conf_matx[2,1])
precision <- conf_matx[2,2]/(conf_matx[2,2]+conf_matx[1,2])
2*recall*precision/(recall+precision)#0.24
