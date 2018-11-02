

# library
library(readr)
library(tidyverse)
library(ggplot2)

# read data
test <- read_csv('Manu_data_200000.csv')
data_full<-data_full[,-c(16,18)]
#summary
str(data_full)
summary(data_full)
# data cleaning
data_full$post_date<-as.Date(data_full$post_date,"%Y-%m-%d")
data_full$fill_date<-as.Date(data_full$fill_date,"%Y-%m-%d")

#treat NA
sapply(data_full, function(x){sum(is.na(x))})

tmp <- data_full %>% select(post_date,fill_date,salary,location,company,role,tags) %>% 
  filter(is.na(fill_date) == T) %>% head(.,50)

# most popular 10 jobs
top_jobs <- sort(table(data_full$role),decreasing = T)
top_jobs[0:10]
# most wanted 50 skills 
top_skills <- sort(table(unlist(strsplit(data_full$tags,','))),decreasing = T)
top_skills[0:50]
# top skills group by top 10 jobs 
data_full %>% select(role,tags) %>% filter(role %in% names(top_jobs[0:10])) %>% 
  separate_rows(.,tags,sep = ',') %>% filter(!is.na(tags) & tags != '') %>% 
  group_by(role,tags) %>% summarise(num = n()) %>% top_n(.,10,num) %>% 
  arrange(role,desc(num))-> tmp

# top skills repeated in top jobs
sort(table(tmp$tags),decreasing = T)

