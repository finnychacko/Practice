## classification

hr <- read.csv("E:/Term 2/ML/datasets-master/HR Analytics.csv")

hr_train<-hr[1:(0.7*nrow(hr)),]
hr_test<-hr[(0.7*nrow(hr)+1):nrow(hr),]
View(hr_train)
model <- tree::tree(Attrition~.,data = hr)
plot(model)+text(model)

install.packages("rattle")
library(rattle)
m1 <- rpart::rpart(Attrition~OverTime+Gender,data = hr_train)
names(hr_train)
plot(m1) + text(m1)
fancyRpartPlot(m1)

## Gini Impurity
### Input Variable : categorical with two classes

nrow(hr_train)
left_overtime <- hr_train %>% filter(OverTime=='Yes')
right_overtime <- hr_train %>% filter(OverTime=='No')
nrow(left_overtime)
nrow(right_overtime)
table(left_overtime$Attrition)
1-(96/303)^2-(207/303)^2

table(right_overtime$Attrition)
1-(76/726)^2-(650/726)^2

# based on gender
left_gender <- hr_train %>% filter(Gender=='Female')
right_gender <- hr_train %>% filter(Gender=='Male')
table(left_gender$Attrition)
gi_left <- 1-(364/nrow(left_gender))^2 - (67/nrow(left_gender))^2
table(right_gender$Attrition)
gi_right <- 1 - (105/nrow(right_gender))^2 - (493/nrow(right_gender))^2
gi_gender <- nrow(left_gender)/nrow(hr_train)*gi_left +
  nrow(right_gender)/nrow(hr_train)*gi_right
gi_gender


martial_status_uniq <- unique(hr_train$MaritalStatus)
for (status in martial_status_uniq) {
  samples_left <- hr_train  %>% filter(MaritalStatus==status)
  samples_right <- hr_train  %>% filter(MaritalStatus!= status)
  
  p0_left <- nrow(samples_left %>% filter(Attrition==0))/nrow(samples_left)
  p1_left <- nrow(samples_left %>% filter(Attrition==1))/nrow(samples_left)
  gi_left <- 1- p0_left^2 - p1_left^2
  
  p0_right <- nrow(samples_right %>% filter(Attrition==0))/nrow(samples_right)
  p1_right <- nrow(samples_right %>% filter(Attrition==1))/nrow(samples_right)
  gi_right <- 1- p0_right^2 - p1_right^2
  
  gi_status <- nrow(samples_left)/nrow(hr_train)*gi_left +
    nrow(samples_right)/nrow(hr_train) * gi_right
  temp <- martial_status_uniq[martial_status_uniq!=status]
  print('Left node')
  print(status)
  print("Right node")
  print(temp)
  print(gi_status)
  print("-----------------------------------")
}

x <- c("a","b","c","d")
combn(x,2,simplify = F)



combinations_left <- c()
combinations_right <- c()
gi_all <- c()
jobs_uniq <- unique(hr_train$JobRole)
n=2
comb_n <- combn(jobs_uniq,n,simplify = F)
result <- data.frame()
for (i in seq(1,length(comb_n))) {
  comb_left <- comb_n[[i]]
  comb_right <- jobs_uniq[!jobs_uniq %in% comb_left]
  
  samples_left <- hr_train  %>% filter(JobRole %in% comb_left)
  samples_right <- hr_train  %>% filter(JobRole %in% comb_right)
  
  p0_left <- nrow(samples_left %>% filter(Attrition==0))/nrow(samples_left)
  p1_left <- nrow(samples_left %>% filter(Attrition==1))/nrow(samples_left)
  gi_left <- 1- p0_left^2 - p1_left^2
  
  p0_right <- nrow(samples_right %>% filter(Attrition==0))/nrow(samples_right)
  p1_right <- nrow(samples_right %>% filter(Attrition==1))/nrow(samples_right)
  gi_right <- 1- p0_right^2 - p1_right^2
  
  gi_status <- nrow(samples_left)/nrow(hr_train)*gi_left +
    nrow(samples_right)/nrow(hr_train) * gi_right
  
  combinations_left <- c(combinations_left,paste0(comb_left,collapse = ','))
  combinations_right <- c(combinations_right,paste0(comb_right,collapse = ','))
  gi_all <- c(gi_all,gi_status)
}
result <- data.frame(left=combinations_left,right=combinations_right,gi=gi_all)
View(result)


jobs_uniq <- unique(hr_train$JobRole)
combinations_left <- c()
combinations_right <- c()
gi_all <- c()
for (n in c(1,2,3,4)) {
  comb_n <- combn(jobs_uniq,n,simplify = F)
  for (i in seq(1,length(comb_n))) {
    comb_left <- comb_n[[i]]
    comb_right <- jobs_uniq[!jobs_uniq %in% comb_left]
    
    samples_left <- hr_train  %>% filter(JobRole %in% comb_left)
    samples_right <- hr_train  %>% filter(JobRole %in% comb_right)
    
    p0_left <- nrow(samples_left %>% filter(Attrition==0))/nrow(samples_left)
    p1_left <- nrow(samples_left %>% filter(Attrition==1))/nrow(samples_left)
    gi_left <- 1- p0_left^2 - p1_left^2
    
    p0_right <- nrow(samples_right %>% filter(Attrition==0))/nrow(samples_right)
    p1_right <- nrow(samples_right %>% filter(Attrition==1))/nrow(samples_right)
    gi_right <- 1- p0_right^2 - p1_right^2
    
    gi_status <- nrow(samples_left)/nrow(hr_train)*gi_left +
      nrow(samples_right)/nrow(hr_train) * gi_right
    
    combinations_left <- c(combinations_left,paste0(comb_left,collapse = ','))
    combinations_right <- c(combinations_right,paste0(comb_right,collapse = ','))
    gi_all <- c(gi_all,gi_status)
  } 
}
result <- data.frame(left=combinations_left,right=combinations_right,gi=gi_all)
View(result)
result %>% arrange(gi) %>% head(3) 

model <- rpart::rpart(Attrition~JobRole,data = hr_train)
fancyRpartPlot(model)
levels(hr_train$JobRole)
plot(model)+text(model)
                      

month<-sort(unique(hr_train$MonthlyIncome))
split<-(month[1:length(month)-1]+month[2:length(month)])/2
splits<-c()
gi<-c()
for (cut in split) {
  left<-hr_train%>%filter(MonthlyIncome<cut)
  right<-hr_train%>%filter(MonthlyIncome>cut)
  
  p0_left<-nrow(left%>%filter(Attrition==0))/nrow(left)
  p1_left<-nrow(left%>%filter(Attrition==1))/nrow(left)
  gi_left<-1-p0_left^2-p1_left^2
  
  p0_right<-nrow(right%>%filter(Attrition==0))/nrow(right)
  p1_right<-nrow(right%>%filter(Attrition==1))/nrow(right)
  gi_right<-1-p0_right^2-p1_right^2
  
  gi_status=nrow(left)/nrow(hr_train)*gi_left+nrow(right)/nrow(hr_train) *gi_right
  splits<-c(splits,cut)
  gi<-c(gi,gi_status)
}
result<-data.frame(splits,gi)
result
result%>%arrange(gi)%>%head(1)

m2<-rpart::rpart(Attrition~MonthlyIncome,data = hr_train)
plot(m2)+text(m2)
fancyRpartPlot(m2)

model <- rpart::rpart(Attrition~OverTime,data = hr_train)
plot(model) + text(model)
fancyRpartPlot(model)

table(hr_train$Attrition)
samples_left <- hr_train %>% filter(OverTime=='No')
samples_right <- hr_train %>% filter(OverTime=='Yes')
nrow(samples_left)
nrow(samples_left) / nrow(hr_train)


nrow(samples_left %>% filter(Attrition==1))/nrow(samples_left)
nrow(samples_right %>% filter(Attrition==1))/nrow(samples_right)
