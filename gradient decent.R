# gradient decent

x = rnorm(100)
y = 0.05 * x
df_xy = data.frame(x=x,y=y)
plot(x,y)
cor(x,y)

m = 0
df_xy = df_xy %>% mutate(xy=x * y)
df_xy = df_xy %>% mutate(mx_square=m * (x^2))

# to find optimal values (part of the derivation)
m=1000
n_interactions =1000
alpha = 0.01
errors = c()
m_vals = c()
for (i in seq(1,n_interactions)) {
  m_vals = c(m_vals,m)
  curr_err = sum((y - (m*x))^2) / length(x)
  errors = c(errors,curr_err)
  df_xy = df_xy %>% dplyr::mutate(xy=x * y)
  df_xy = df_xy %>% dplyr::mutate(mx_square=m * (x^2))
  df_xy = df_xy %>% dplyr::mutate(xy_mx2=xy - mx_square)
  sigma_xy_mx2 = sum(df_xy$xy_mx2)
  m_gradident = -2 / length(x) * sigma_xy_mx2
  m = m -alpha * m_gradident
}
print(m)
plot(m_vals,errors) + lines(m_vals,errors)


adv <- read.csv("E:/Term 2/ML/Datasets/Advertising.csv")
adv_training <- adv[sample(seq(1,nrow(adv)),162),] 
adv_testing <- adv[sample(seq(1,nrow(adv)),38),]
lm(sales~TV,data = adv_training)



library(dplyr)
alpha <- 0.01
m = 100
c = 100
n_interactions <- 1000
ad_x <- scale(adv_training$TV)
ad_y <- adv_training$sales
df_xy <- data.frame()
d_xy <- data.frame()
for (i in seq(1,n_interactions)) {
  df_xy = df_xy %>% dplyr::mutate(xy = ad_x * ad_y)
  df_xy = df_xy %>% dplyr::mutate(mx_sq = m * (ad_x^2))
  df_xy = df_xy %>% dplyr::mutate(cx = c * ad_x)
  df_xy = df_xy %>% dplyr::mutate(xy_mx2 = xy - mx_sq - cx)
  sigma_xy_mx2 = sum(df_xy$xy_mx2)
  m_gradident = -2 / nrow(ad_x) * sigma_xy_mx2
  
  d_xy = d_xy %>% dplyr::mutate(mx = m * ad_x)
  d_xy = d_xy %>% dplyr::mutate(y_mx_c = ad_y - mx - c)
  sig_y_mx_c = sum(d_xy$y_mx_c)
  c_gradient = -2 / nrow(ad_x) * sig_y_mx_c
  m = m - alpha * m_gradident
  c = c - alpha * c_gradient
}
m
c

adv <- read.csv("E:/Term 2/ML/Datasets/Advertising.csv")
adv_training <- adv[sample(seq(1,nrow(adv)),162),] 
adv_testing <- adv[sample(seq(1,nrow(adv)),38),]
x <- adv_training$TV
y <- adv_training$sales
m = -10
c1 = 0
a= 0.01
error_vals = c(); m_vals = c(); c1_vals = c()
test=data.frame(x=scale(x),y=y)
for (i in seq(1,1000)) {
  test <- mutate(test,yhat2 = (y - m * x - c1)^2)
  error_vals = c(error_vals, 1 / length(x) * sum(test$yhat2))
  m_vals = c()
  c_vals = c()
  test = mutate(test, xy=x*y)
  test = mutate(test,mx2 = m*(x^2))
  test = mutate(test,m_gradient_sum = (x*y) - mx2 - (c1*x))
  test = mutate(test,c_gradient_sum = (y)-(m*x)-(c1))
  m_gradient = -2 / length(x) * sum(test$m_gradient_sum)
  c_gradient = -2 / length(y) * sum(test$c_gradient_sum)
  m=m-alpha * m_gradient
  c1=c1-alpha * c_gradient
}
m
c

