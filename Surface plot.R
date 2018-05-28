View(iris)
iris <- datasets::'iris'

library(dplyr)
iris_new <- iris %>% select(-Species)
View(iris_new)
iris_model <- kmeans(iris_new,centers=3)
iris_model$cluster
length(iris_model$cluster)
table(iris_model$cluster)

library(ggplot2)
library(plotly)
adv <- read.csv("E:/Term 2/ML/Datasets/Advertising.csv")
adv_training <- adv[sample(seq(1,nrow(adv)),162),]
adv_testing <- adv[sample(seq(1,nrow(adv)),38),]

#fit the model
adv_model <- lm(sales~TV,data = adv_training)
adv_model 


m=0.08
c=1
sales_predicted <- m * adv_training$TV + c
error <- sum((adv_training$sales -  sales_predicted) ^2 ) / nrow(adv_training)
error

#using for loop to give the m values for 100 values
m <- seq(0,1,length.out = 100)
for (i in m) {
  sales_predicted <- i * adv_training$TV + c
  error <- sum((adv_training$sales -  sales_predicted) ^2 ) / nrow(adv_training)
  print(error)
  }

plot(adv_training$TV,adv_training$sales) + lines(adv_training$TV,sales_predicted)

m <- seq(-1,1,length.out = 100)
e=c()
for (i in m) {
  sales_predicted <- i * adv_training$TV + c
  error <- sum((adv_training$sales -  sales_predicted) ^2 ) / nrow(adv_training)
  print(error)
  e=c(e,error)
}
plot(e)

which(e==min(e))
m[54]


e <- c()
m_rep <- c()
c_rep <- c()
m <- seq(-1,1,length.out = 200)
c <- seq(-10,10,length.out = 200)
for (i in m) {
  for (j in c) {
    sales_predicted <- i * adv_training$TV + j
    error <- sum((adv_training$sales -  sales_predicted) ^2 ) / nrow(adv_training)
    m_rep <- c(m_rep,i)
    c_rep <- c(c_rep,j)
    e=c(e,error) 
  }
}
plot(e)
library(plotly)
mspace = m
cspace = c
zspace = matrix(e, nrow = length(m),ncol = length(c))

plot_ly(x=mspace,y=cspace,z=zspace) %>% add_surface()


e <- c()
m1_rep <- c()
m2_rep <- c()
c_rep <- c()
m <- seq(-1,1,length.out = 200)
c <- seq(-10,10,length.out = 200)
for (i in m) {
  for (j in c) {
    for (k in c) {
      sales_predicted <- i * adv_training$TV + j*adv_training$radio + k
      error <- sum((adv_training$sales -  sales_predicted) ^2 ) / nrow(adv_training)
      m_rep <- c(m_rep,i)
      c_rep <- c(c_rep,j)
      c_rep <- c(c_rep,k)
      e=c(e,error)  
    }
  }
}
/plot(e)
