

adv <- read.csv("E:/Term 2/ML/Datasets/Advertising.csv")
model <- tree::tree(sales~.,data = adv)
plot(model) + text(model)

# to find no of cuts for each column
tv_uniqs = sort(unique(adv$TV))
length(tv_uniqs)
tv_uniqs[1:10]
cuts_tv <- (tv_uniqs[1:length(tv_uniqs)-1] +
    tv_uniqs[2:length(tv_uniqs)]) / 2
length(cuts_tv)

radio_uniqs = sort(unique(adv$radio))
length(radio_uniqs)
radio_uniqs[1:10]
cuts_radio <- (radio_uniqs[1:length(radio_uniqs)-1] +
              radio_uniqs[2:length(radio_uniqs)]) / 2
length(cuts_radio)

news_uniqs = sort(unique(adv$newspaper))
length(news_uniqs)
news_uniqs[1:10]
cuts_news <- (news_uniqs[1:length(news_uniqs)-1] +
                news_uniqs[2:length(news_uniqs)]) / 2
length(cuts_news)

cuts <- c(cuts_tv,cuts_radio,cuts_news)
predictors <- c(rep('TV',length(cuts_tv)),rep('radio',length(cuts_radio)),
                rep('newspaper',length(cuts_news)))
result <- data.frame(cut=cuts,predicor=predictors)
View(result)

#method 1
# to find the MSE for each cut
library(dplyr)
tv_cuts_mse <- c()
temp=adv
for (cut in cuts_tv) {
  samples_left = adv %>% filter(TV<cut)
  samples_right = adv %>% filter(TV>cut)
  pred_left = mean(samples_left$sales)
  pred_right = mean(samples_right$sales)
  temp$pred = ifelse(temp$TV<cut,pred_left,pred_right)
  curr_mse = sum((temp$sales-temp$pred)^2) / nrow(temp)
  tv_cuts_mse = c(tv_cuts_mse,curr_mse)
}
View(tv_cuts_mse)

radio_cuts_mse <- c()
temp=adv
for (cut in cuts_radio) {
  samples_left1 = adv %>% filter(radio<cut)
  samples_right1 = adv %>% filter(radio>cut)
  pred_left1 = mean(samples_left1$sales)
  pred_right1 = mean(samples_right1$sales)
  temp$pred = ifelse(temp$radio<cut,pred_left1,pred_right1)
  curr_mse1 = sum((temp$sales-temp$pred)^2) / nrow(temp)
  radio_cuts_mse = c(radio_cuts_mse,curr_mse1)
}
View(radio_cuts_mse)

news_cuts_mse <- c()
temp=adv
for (cut in cuts_news) {
  samples_left2 = adv %>% filter(newspaper<cut)
  samples_right2 = adv %>% filter(newspaper>cut)
  pred_left2 = mean(samples_left2$sales)
  pred_right2 = mean(samples_right2$sales)
  temp$pred = ifelse(temp$news<cut,pred_left2,pred_right2)
  curr_mse2 = sum((temp$sales-temp$pred)^2) / nrow(temp)
  news_cuts_mse = c(news_cuts_mse,curr_mse2)
}
View(news_cuts_mse)

result_tv = data.frame(column=rep('TV',length(cuts_tv)),
                       cut=cuts_tv,
                       mse=tv_cuts_mse)
result_radio = data.frame(column=rep('radio',length(cuts_radio)),
                       cut=cuts_radio,
                       mse=radio_cuts_mse)
result_news = data.frame(column=rep('Newspaper',length(cuts_news)),
                       cut=cuts_news,
                       mse=news_cuts_mse)
result = rbind(result_tv,result_radio,result_news)
View(result)

cuts <- c(cuts_tv,cuts_radio,cuts_news)
predictors <- c(rep('TV',length(cuts_tv)),rep('radio',length(cuts_radio)),
                rep('newspaper',length(cuts_news)))
result <- data.frame(cut=cuts,predicor=predictors)

temp <- adv %>% filter(TV>122.05 & radio >26.85 & TV >194.55)
for (i in seq(1,length(cuts))) {
  cut = cuts[i]
  curr_col = predictors[i]
  samples_left = temp[temp[,curr_col]<cut,]
  samples_right = temp[temp[,curr_col]>cut,]
  pred_left = mean(samples_left2$sales)
  pred_right = mean(samples_right2$sales)
  var_temp = var(temp$sales)
  var_left = var(samples_left$sales)
  var_right = var(samples_right$sales)
  var_dev = var_temp - (nrow(samples_left)/nrow(temp)*var_left) - 
    (nrow(samples_right)/nrow(temp)*var_right)
  temp$pred = ifelse(temp[,curr_col]<cut,pred_left,pred_right)
  curr_mse = sum((temp$sales-temp$pred)^2) / nrow(temp)
  cuts_mse = c(news_cuts_mse,curr_mse)
}
result$mse = cuts_mse
result$var_dev = var_dev
View(result)
