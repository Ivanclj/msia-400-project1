

# library
library(readr)
library(tidyverse)
library(ggplot2)

# read data
data_full <- read_csv('Manu_data_200000.csv')
data_full<-data_full[,-c(1,17,19)]
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

# plot trend
tmp <- data_full %>% mutate('mydate' = format(as.Date(post_date),'%Y-%m')) %>% 
  select(mydate) %>% group_by(mydate) %>% summarise(num = n())

for(ii in 1:nrow(tmp)){
  tmp$mydate[ii] <- paste(tmp$mydate[ii],'-01',sep = '')
}
tmp$mydate <- as.Date(tmp$mydate,'%Y-%m-%d')

ggplot(data = tmp, aes(x = mydate, y = num)) + geom_line()

# time to fill and salary distribution
summary(data_full$time_to_fill)
summary(data_full$salary)

# region state
usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

states <- map_data('state')
ggplot(data = states ) + 
  geom_polygon(aes(x = long, y = lat, group = group),fill = 'blue', color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

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


# check if engineers pay and time to fill differ in groups
unique_engineer <- unique(data_full$role)[grep('Engineer',unique(data_full$role))]
all_engineers <- data_full[data_full$role %in% unique_engineer,]
sum_engineer <- all_engineers %>% group_by(role) %>% summarise(num = n(), 
                                              min_sal = min(salary,na.rm = T),
                                               mean_sal = mean(salary,na.rm = T), 
                                               median_sal = median(salary,na.rm = T),
                                               max_sal = max(salary,na.rm = T), 
                                               std_val = sd(salary,na.rm = T),
                                               min_fill = min(time_to_fill,na.rm = T),
                                               mean_fill = mean(time_to_fill,na.rm = T), 
                                               median_fill = median(time_to_fill,na.rm = T),
                                               max_fill = max(time_to_fill,na.rm = T), 
                                               std_fill = sd(time_to_fill,na.rm = T)) %>% 
  arrange(desc(num))

## check if all enginnering/drivers role following similar skill distribution and total count

all_engineers <- data_full %>% select(post_date,time_to_fill,region_state,role,tags) %>% 
  filter(role %in% unique_engineer) %>% separate_rows(.,tags,sep = ',') %>% filter(!is.na(tags) & tags != '') %>% 
  group_by(role,tags) %>% summarise(num = n()) %>% top_n(5,num) %>% 
  arrange(role,desc(num))
sort(table(all_engineers$tags),decreasing = T)

#number of job postings in each engineers
num_engineers <- data_full %>% select(post_date,time_to_fill,region_state,role,tags) %>% 
  filter(role %in% unique_engineer) %>% group_by(role) %>% summarise(num = n()) %>% arrange(desc(num))
# look at skill distribution in top 7 engineers
all_engineers<-all_engineers %>% filter(role %in% num_engineers$role[1:7]) # safe to combine engineers together

# drivers
unique_drivers <- unique(data_full$role)[grep('Driver',unique(data_full$role))]
all_drivers <- data_full %>% select(post_date,time_to_fill,region_state,role,tags) %>% 
  filter(role %in% unique_drivers) %>% separate_rows(.,tags,sep = ',') %>% filter(!is.na(tags) & tags != '') %>% 
  group_by(role,tags) %>% summarise(num = n()) %>% top_n(5,num) %>% 
  arrange(role,desc(num))
sort(table(all_drivers$tags),decreasing = T) # safe to combine

#combine roles 
unique_engineer <- unique_engineer[unique_engineer!= 'Engineer']
unique_drivers <- unique_drivers[unique_drivers!= 'Driver']

data_full$role[data_full$role %in% unique_engineer] <- 'Engineer'
data_full$role[data_full$role %in% unique_drivers] <- 'Driver'

#Engineering/driver job trend
tmp <- data_full %>% mutate('mydate' = format(as.Date(post_date),'%Y-%m')) %>% 
  select(mydate,role) %>%  
  group_by(mydate,role) %>% summarise(num = n())
x <- (tmp %>% filter(role %in% c('Engineer','Driver')) %>% group_by(mydate,role)%>%
           summarise(num = sum(num)))
y <- (tmp %>% group_by(mydate) %>% summarise(num = sum(num)))
tmp <- merge(x,y, by = 'mydate')
tmp$rate <- tmp$num.x/tmp$num.y
for(ii in 1:nrow(tmp)){
  tmp$mydate[ii] <- paste(tmp$mydate[ii],'-01',sep = '')
}
tmp$mydate <- as.Date(tmp$mydate,'%Y-%m-%d')
ggplot(data = tmp, aes(x = mydate, y = rate, color = role)) + geom_line()

#summraise rates
summary(tmp[tmp$role == 'Engineer','rate'])
summary(tmp[tmp$role == 'Driver','rate'])

#check time to fill and salary distribution
data_engineer <- data_full[data_full$role =='Engineer',]
data_driver <- data_full[data_full$role == 'Driver',]

summary(data_engineer$time_to_fill)
summary(data_engineer$salary)
summary(data_driver$time_to_fill)
summary(data_driver$salary)

ggplot(data = data_engineer)+geom_histogram(aes(time_to_fill))
ggplot(data = data_engineer)+geom_histogram(aes(salary))


sort(table(data_engineer$time_to_fill),decreasing = T) 
#suprisingly high number of 1 day,range focus on 3 - 123
sum(data_engineer$salary<25000)
sum(data_engineer$salary>160000)
sum(data_engineer$time_to_fill<3,na.rm = T)
sum(data_engineer$time_to_fill>123,na.rm = T)
data_engineer <- data_engineer %>% filter(salary < 160000, salary > 25000,
                                          time_to_fill >3,time_to_fill< 123) #much more normal

# distribution of median salary and time fill over time
tmp <- data_full %>% mutate('mydate' = format(as.Date(post_date),'%Y-%m')) %>% 
  select(mydate,role,salary) %>% filter(role %in% c('Engineer','Driver')) %>% 
  group_by(mydate,role) %>% summarise(med_salary = median(salary,na.rm = T))

for(ii in 1:nrow(tmp)){
  tmp$mydate[ii] <- paste(tmp$mydate[ii],'-01',sep = '')
}
tmp$mydate <- as.Date(tmp$mydate,'%Y-%m-%d')
ggplot(data = tmp, aes(x = mydate, y = med_salary, color = role)) + geom_line()

tmp_2 <- tmp
tmp_2$med_salary[tmp_2$role == 'Driver'] <-tmp_2$med_salary[tmp_2$role == 'Driver'] + 
  (median(data_engineer$salary,na.rm = T)-median(data_driver$salary,na.rm = T))
ggplot(data = tmp_2, aes(x = mydate, y = med_salary, color = role)) + geom_line()

# most wanted 50 skills 
top_eng_skills <- sort(table(unlist(strsplit(data_engineer$tags,','))),decreasing = T)
length(top_eng_skills)# 629
top_eng_skills[1:30]

# avg num of skills required
count_tag <- function(x){length(strsplit(x,',')[[1]])}
num_tags <- as.numeric(sapply(data_engineer$tags, count_tag))
summary(num_tags)
data_engineer[which(num_tags ==0),c(16,17)]
# what skills earn most money? what about pay per occurence
top_eng_skills <- top_eng_skills[as.numeric(top_eng_skills) > 360] # skill appeared more than 1%
length(top_eng_skills) #171

skill_money <- rep(0,171)

for(ii in 1:nrow(data_engineer)){
  tags <- strsplit(data_engineer$tags[ii],',')[[1]]
  money_tag <- data_engineer$salary[ii]/length(tags) # amount of money avg for tags
  tag_index <- which(names(top_eng_skills) %in% tags)
  skill_money[tag_index] <- skill_money[tag_index] + money_tag
}

names(skill_money)<-names(top_eng_skills)
sort(skill_money,decreasing = T)[1:10] # top 10 most earning skills in total

perc_money <- skill_money/top_eng_skills
sort(perc_money,decreasing = T)[1:20] #top 10 most earning skills per occur
top_eng_skills[1:20]
top_eng_skills[which(names(top_eng_skills) == 'Pipeline')]
skill_money[which(names(skill_money)=='Pipeline')]

# what skills gets filled way quicker
skill_fill <- rep(0,171)

for(ii in 1:nrow(data_engineer)){
  tags <- strsplit(data_engineer$tags[ii],',')[[1]]
  fill_tag <- data_engineer$time_to_fill[ii]/length(tags) # amount of money avg for tags
  tag_index <- which(names(top_eng_skills) %in% tags)
  skill_fill[tag_index] <- skill_fill[tag_index] + fill_tag
}

names(skill_fill)<-names(top_eng_skills)
sort(skill_fill,decreasing = T)[1:10] # top 10 most earning skills in total

perc_fill <- skill_fill/top_eng_skills
sort(perc_fill,decreasing = T)[1:20] #top 10 most earning skills per occur

# what about if we broke down by state

# what skills earn most money? what about pay per occurence
unique_states <- unique(data_engineer$state)
unique_states <- unique_states[-7]
zeros <- rep(0,length(unique_states))
skill_money_df <- data.frame(state = unique_states,tag1 = zeros,
                             tag2 = zeros,tag3 = zeros,
                             tag4 = zeros,tag5 = zeros,
                             num1 = zeros,num2 = zeros,
                             num3 = zeros,num4 = zeros,
                             num5 = zeros,sal1 = zeros,
                             sal2 = zeros,sal3 = zeros,
                             sal4 = zeros,sal5 = zeros)
# get skill distribution from each state
for(jj in 1:length(unique_states)){
  this_df <- data_engineer[data_engineer$state == unique_states[jj],]
  this_df<- this_df[!is.na(this_df$job_id),]
  top_eng_skills_state <- sort(table(unlist(strsplit(this_df$tags,','))),decreasing = T)
  top_eng_skills_state <- top_eng_skills_state[as.numeric(top_eng_skills_state) > (nrow(this_df)/100)]
  skill_money_state <- rep(0,length(top_eng_skills_state))
  for(ii in 1:nrow(this_df)){
    tags <- strsplit(this_df$tags[ii],',')[[1]]
    money_tag <- this_df$salary[ii]/length(tags) # amount of money avg for tags
    tag_index <- which(names(top_eng_skills_state) %in% tags)
    skill_money_state[tag_index] <- skill_money_state[tag_index] + money_tag
  }
  names(skill_money_state)<-names(top_eng_skills_state)
  skill_money_state <- sort(skill_money_state/top_eng_skills_state,decreasing = T)[1:5]
  num_skills <- top_eng_skills_state[names(top_eng_skills_state) %in% names(skill_money_state)]
  skill_money_df[jj,2:ncol(skill_money_df)]<- c(names(skill_money_state)[1:5],
                                                as.numeric(num_skills),
                                                as.numeric(skill_money_state)
                                                )
}
write_csv(skill_money_df,'/nfs/home/lcu1428/z/400_proj/skill_money_state.csv')

# what skills fills quicker?
skill_fill_df <- data.frame(state = unique_states,tag1 = zeros,
                             tag2 = zeros,tag3 = zeros,
                             tag4 = zeros,tag5 = zeros,
                             num1 = zeros,num2 = zeros,
                             num3 = zeros,num4 = zeros,
                             num5 = zeros,fill1 = zeros,
                             fill2 = zeros,fill3 = zeros,
                             fill4 = zeros,fill5 = zeros)
# get skill distribution from each state
for(jj in 1:length(unique_states)){
  this_df <- data_engineer[data_engineer$state == unique_states[jj],]
  this_df<- this_df[!is.na(this_df$job_id),]
  top_eng_skills_state <- sort(table(unlist(strsplit(this_df$tags,','))),decreasing = T)
  top_eng_skills_state <- top_eng_skills_state[as.numeric(top_eng_skills_state) > (nrow(this_df)/100)]
  skill_fill_state <- rep(0,length(top_eng_skills_state))
  for(ii in 1:nrow(this_df)){
    tags <- strsplit(this_df$tags[ii],',')[[1]]
    money_tag <- this_df$time_to_fill[ii]/length(tags) # amount of money avg for tags
    tag_index <- which(names(top_eng_skills_state) %in% tags)
    skill_fill_state[tag_index] <- skill_fill_state[tag_index] + money_tag
  }
  names(skill_fill_state)<-names(top_eng_skills_state)
  skill_fill_state <- sort(skill_fill_state/top_eng_skills_state,decreasing = T)[1:5]
  num_skills <- top_eng_skills_state[names(top_eng_skills_state) %in% names(skill_fill_state)]
  skill_fill_df[jj,2:ncol(skill_money_df)]<- c(names(skill_fill_state)[1:5],
                                                as.numeric(num_skills),
                                                as.numeric(skill_fill_state)
  )
}
write_csv(skill_fill_df,'/nfs/home/lcu1428/z/400_proj/skill_fill_state.csv')
