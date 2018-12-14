

# library
library(readr)
library(tidyverse)
library(ggplot2)

# read data
data_full <- read_csv('../../Manu_data_200000.csv')

# get rid of repeated job id hash
data_full<-data_full[,-c(1,17,19)]
#summary
str(data_full)
summary(data_full) # time to fill NA 4477, salary NA 20, state 17482

# make date date object
data_full$post_date<-as.Date(data_full$post_date,"%Y-%m-%d")
data_full$fill_date<-as.Date(data_full$fill_date,"%Y-%m-%d")


# # #treat NA, tof and salary are minor numbers, removed. Mainly impute for State
# data_full <- data_full[!is.na(data_full$salary) & !is.na(data_full$time_to_fill),] 
# library(mice)
# library(VIM)
# mice_plot <- aggr(data_full, col=c('navyblue','yellow'),
#                   numbers=TRUE, sortVars=TRUE,
#                   labels=names(data_full), cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))
# temp <- data_full[,c('post_date','fill_date','time_to_fill','salary','role','state')]
# temp$state <- as.factor(temp$state)
# tempData <- mice(temp,m=5,maxit=50,meth='pmm',seed=123)
# summary(tempData)
# 
# tempData$imp$state
# plot trend
tmp <- data_full %>% mutate('mydate' = format(as.Date(post_date),'%Y-%m'))%>% 
  group_by(mydate) %>% summarise(num = n())

for(ii in 1:nrow(tmp)){
  tmp$mydate[ii] <- paste(tmp$mydate[ii],'-01',sep = '')
}
tmp$mydate <- as.Date(tmp$mydate,'%Y-%m-%d')

ggplot(data = tmp, aes(x = mydate, y = num)) + geom_line() # It appears that after 2017-07 the number of jobs exploded


# time to fill and salary distribution
summary(data_full$time_to_fill) 
summary(data_full$salary)

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

## Boxplot for this
# boxplots of ten groups salary
library(ggpubr)
ggboxplot(all_engineers, x = "role", y = "salary", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups salary
ggline(all_engineers, x = "role", y = "salary", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# Filter out the extreme groups and values
eng.large.role <- sum_engineer$role[sum_engineer$num>(nrow(all_engineers)*0.01)] 
all_engineers.filterd <- all_engineers[all_engineers$role %in% eng.large.role, ]

summary(all_engineers.filterd$salary)
## get the 80% tiles of salary
eng.tiles <- ntile(all_engineers.filterd$salary,10)
all_engineers.filterd <- all_engineers.filterd[eng.tiles>=2 & eng.tiles <=8,]
table(all_engineers.filterd$role) %>% sort(decreasing = T)

ggboxplot(all_engineers.filterd, x = "role", y = "salary", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups salary
ggline(all_engineers.filterd, x = "role", y = "salary", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# boxplots of ten groups time to fill
library(ggpubr)
ggboxplot(all_engineers, x = "role", y = "time_to_fill", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups salary
ggline(all_engineers, x = "role", y = "time_to_fill", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# Filter out the extreme groups and values
eng.large.role <- sum_engineer$role[sum_engineer$num>(nrow(all_engineers)*0.01)] 
all_engineers.filterd <- all_engineers[all_engineers$role %in% eng.large.role, ]

summary(all_engineers.filterd$time_to_fill)
## get the 80% tiles of salary
eng.tiles <- ntile(all_engineers.filterd$time_to_fill,10)
all_engineers.filterd <- all_engineers.filterd[eng.tiles>=2 & eng.tiles <=9,]
table(all_engineers.filterd$role) %>% sort(decreasing = T)

## plot again
ggboxplot(all_engineers.filterd, x = "role", y = "time_to_fill", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups
ggline(all_engineers.filterd, x = "role", y = "time_to_fill", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7)) # distribution looked similar


## check if all enginnering/drivers role following similar skill distribution and total count

# get data frame contains each role and its top 5 tags
eng.tags <- data_full %>% filter(role %in% unique_engineer) %>% 
  separate_rows(.,tags,sep = ',') %>% filter(!is.na(tags) & tags != '') %>% 
  group_by(role,tags) %>% summarise(num = n()) %>% top_n(10,num) %>% 
  arrange(role,desc(num)) %>% ungroup()
sort(table(eng.tags$tags),decreasing = T)
eng.tags.max.num <- eng.tags %>% group_by(role) %>% summarise(max_num = max(num)) 
eng.tags.max.num <- eng.tags.max.num[eng.tags.max.num$max_num > 100,]

eng.tags <- inner_join(eng.tags,eng.tags.max.num, by = 'role')
eng.tags$perc <- eng.tags$num/eng.tags$max_num

# reshape it to calculate similarity
library(reshape2)
wide.eng.tags <- dcast(eng.tags,role~tags,value.var = 'perc' )
wide.eng.tags[is.na(wide.eng.tags)] <- 0
# quick clustering
library(stylo)
d <- dist(wide.eng.tags,method = 'euclidean') # distance matrix
d # distance similar, safe to combine engineers together

## DRIVERS

unique_drivers <- unique(data_full$role)[grep('Driver',unique(data_full$role))]
all_drivers <- data_full[data_full$role %in% unique_drivers,]
sum_drivers <- all_drivers %>% group_by(role) %>% summarise(num = n(), 
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
## Boxplot for this
# boxplots of ten groups salary
library(ggpubr)
ggboxplot(all_drivers, x = "role", y = "salary", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups salary
ggline(all_drivers, x = "role", y = "salary", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# Filter out the extreme groups and values
dri.large.role <- sum_drivers$role[sum_drivers$num>(nrow(all_drivers)*0.01)] 
all_drivers.filterd <- all_drivers[all_drivers$role %in% dri.large.role, ]

summary(all_drivers.filterd$salary)
## get the 80% tiles of salary
dri.tiles <- ntile(all_drivers.filterd$salary,10)
all_drivers.filterd <- all_drivers.filterd[dri.tiles>=2 & dri.tiles <=9,]
table(all_drivers.filterd$role) %>% sort(decreasing = T)

ggboxplot(all_drivers.filterd, x = "role", y = "salary", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups salary
ggline(all_drivers.filterd, x = "role", y = "salary", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))


# boxplots of ten groups time to fill

ggboxplot(all_drivers, x = "role", y = "time_to_fill", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups salary
ggline(all_drivers, x = "role", y = "time_to_fill", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# Filter out the extreme groups and values
dri.large.role <- sum_drivers$role[sum_drivers$num>(nrow(all_drivers)*0.01)] 
all_drivers.filterd <- all_drivers[all_drivers$role %in% dri.large.role, ]

summary(all_drivers.filterd$time_to_fill)
## get the 80% tiles of salary
dri.tiles <- ntile(all_drivers.filterd$time_to_fill,10)
all_drivers.filterd <- all_drivers.filterd[dri.tiles>=2 & dri.tiles <=8,]
table(all_drivers.filterd$role) %>% sort(decreasing = T)

## plot again
ggboxplot(all_drivers.filterd, x = "role", y = "time_to_fill", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7))

# trend of the ten groups
ggline(all_drivers.filterd, x = "role", y = "time_to_fill", color = "role",
       add = c("mean_se",'violin'),
       palette ='magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7)) # distribution looked similar


## check if all enginnering/drivers role following similar skill distribution and total count

# get data frame contains each role and its top 5 tags
dri.tags <- data_full %>% filter(role %in% unique_drivers) %>% 
  separate_rows(.,tags,sep = ',') %>% filter(!is.na(tags) & tags != '') %>% 
  group_by(role,tags) %>% summarise(num = n()) %>% top_n(10,num) %>% 
  arrange(role,desc(num)) %>% ungroup()
sort(table(dri.tags$tags),decreasing = T)
dri.tags.max.num <- dri.tags %>% group_by(role) %>% summarise(max_num = max(num)) 
dri.tags.max.num <- dri.tags.max.num[dri.tags.max.num$max_num > 100,]

dri.tags <- inner_join(dri.tags,dri.tags.max.num, by = 'role')
dri.tags$perc <- dri.tags$num/dri.tags$max_num

# reshape it to calculate similarity
wide.dri.tags <- dcast(dri.tags,role~tags,value.var = 'perc' )
wide.dri.tags[is.na(wide.dri.tags)] <- 0
# quick clustering
d <- dist(wide.dri.tags,method = 'euclidean') # distance matrix
d # distance similar, safe to combine drivers together


#combine roles 
unique_engineer <- unique_engineer[unique_engineer!= 'Engineer']
unique_drivers <- unique_drivers[unique_drivers!= 'Driver']

data_full$role[data_full$role %in% unique_engineer] <- 'Engineer'
data_full$role[data_full$role %in% unique_drivers] <- 'Driver'

#Engineering/driver job demand trend (number of engineer/driver job over total jobs that month)
tmp <- data_full %>% mutate('mydate' = format(as.Date(post_date),'%Y-%m')) %>% 
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
ggplot(data = tmp, aes(x = mydate, y = rate, color = role)) + geom_line() # after 2017 07 much stabler.

#summraise rates
summary(tmp[tmp$role == 'Engineer','rate'])
summary(tmp[tmp$role == 'Driver','rate'])


## Split to Drivers and Engineers
#check time to fill and salary distribution
data_engineer <- data_full[data_full$role =='Engineer',]
data_driver <- data_full[data_full$role == 'Driver',]

summary(data_engineer$time_to_fill)
summary(data_engineer$salary)
summary(data_driver$time_to_fill)
summary(data_driver$salary)

ggboxplot(rbind(data_engineer,data_driver), x = "role", y = "time_to_fill", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7)) # although mean close, engineers seems to take longer to fill

ggboxplot(rbind(data_engineer,data_driver), x = "role", y = "salary", fill = 'role',
          palette = 'magma')+theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 7)) # engineers salary higher on average

# kept only 80% inner quantile of all salaries
eng.tiles <- ntile(data_engineer$salary,10)
data_engineer <- data_engineer[eng.tiles>=2 & eng.tiles <=9,] #much more normal

dri.tiles <- ntile(data_driver$salary,10)
data_driver <- data_driver[dri.tiles>=2 & dri.tiles <=9,] #much more normal


## look at time_to_fill, keep inner 80%. 
sort(table(data_engineer$time_to_fill),decreasing = T) 
eng.time.tiles <- ntile(data_engineer$time_to_fill,10)
data_engineer$time_to_fill[eng.time.tiles>=2 & eng.time.tiles <=9] %>% summary() # 4 -115
data_engineer <- data_engineer[eng.time.tiles>=2 & eng.time.tiles <=9,] # 26148 rows

sort(table(data_driver$time_to_fill),decreasing = T) 
dri.time.tiles <- ntile(data_driver$time_to_fill,10)
data_driver$time_to_fill[dri.time.tiles>=2 & dri.time.tiles <=9] %>% summary() # 3 -94
data_driver <- data_driver[dri.time.tiles>=2 & dri.time.tiles <=9,] # 21130 row

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


## write the csvs 
write_csv(data_full,'/nfs/home/lcu1428/z/400_proj/data_full.csv')
write_csv(data_engineer,'/nfs/home/lcu1428/z/400_proj/data_engineer.csv')
write_csv(data_driver,'/nfs/home/lcu1428/z/400_proj/data_driver.csv')



