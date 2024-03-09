rm(list=ls())

#### Probability density function 
x <- seq(450, 550, by=0.01)
#dnorm:확률밀도함수
p <- dnorm(x, mean=500, sd=12)
plot(x, p, 
     col="red", 
     xlab="x", 
     ylab="Probability: P(X=x)", 
     type="l", 
     lwd=2, 
     cex=2, 
     main ="Probability Density Function")

#pnorm: 누적분포함수를 계산하는 함수
c<- pnorm(x, mean=500, sd=12)
plot(x, c, 
     col="red", 
     xlab="x", 
     ylab="Probability: P(X=x)", 
     type="l", 
     lwd=2, 
     cex=2, 
     main="Cumulative Distribution Function")




### Binomial distribution
n <- 3
p <- 1/3
x <- c(0:n)
#dbinom(): 이항분포의 확률질량함수를 계산하는 함수
px<- dbinom(x, size=n, prob=p)

plot(x, 
     px, 
     type="p", 
     col="red", 
     lwd=2, 
     cex=2,
     xlab="Number of success (x)", 
     ylab="Probability (P(X=x))")


### p is now 1/10
n <- 3
p <- 1/10
x <- c(0:n)
px<- dbinom(x, size=n, prob=p)

plot(x, 
     px, 
     type="p", 
     col="red", 
     lwd=2, 
     cex=2,
     xlab="Number of success (x)", 
     ylab="Probability (P(X=x))")



### n is now 5
n <- 5
p <- 1/3
x <- c(0:n)
px<- dbinom(x, size=n, prob=p)
plot(x, 
     px, 
     type="p", 
     col="red", 
     lwd=2, 
     cex=2,
     xlab="Number of success (x)", 
     ylab="Probability (P(X=x))")


#uniform distribution
plot(c(1:6), 
     rep(1/6, 6), 
     xlab="Number (x)", 
     ylab="Probability (P(X=x))")


#normal distribution
x <- seq(130, 190, by=0.0001)
p <- dnorm(x, mean=160, sd=10)
plot(x, 
     p, 
     col="red", 
     type="l", 
     xlab="x", 
     ylab="Probability: P(X=x)", 
     main ="Probability Density Function (mean=170, s.d.=4)")







#p-value
#신뢰구간
#유의수준


v<-c(1, 4, 5) 

for (i in v) {
  print(i)
}

for (i in 1:3) {
  print(v[i])
}

for (i in 1:length(v)) {
  print(v[i])
}


#rnorm:정규분표에서 난수 생성 
r.n <- rnorm(10, mean = 10, sd = 1)
r.n
sum <- 0

for (i in 3:5) {
  sum <- sum + r.n[i]
  print(sum)
}

#paste:문자열을 결합하는 함수
dan <- 2

for (i in 2:9){
  outcome <- dan * i
  print(paste(dan, "x", i, "=", outcome))
}


for (j in 2:9){
  for (i in 2:9){
    outcome <- j * i
    print(paste(j, "x", i, "=", outcome))
  }
}







setwd("/Users/yangjinmo/Desktop/r특강/수업자료")
library(readxl)
mydata <- read_excel("kor_data_20160024.xlsx")


mydata$test <- NA

for (i in 1:nrow(mydata)){
  mydata$test[i] <- mydata$dem03[i] + 100
}

summary(mydata$dem03)
summary(mydata$test)


mydata$test <- NA

for (i in 1:nrow(mydata)){
  mydata$test[i] [ mydata$dem03[i] == median(mydata$dem03) ] <- mydata$dem03[i]
}

summary(mydata$test)

median(mydata$dem03)
table(mydata$dem03)

mydata$test <- NA
mydata$test [ mydata$dem03 == median(mydata$dem03) ] <- mydata$dem03
summary(mydata$test)

mydata$test <- NA
mydata$test [ mydata$dem03 == median(mydata$dem03) ] <- 8
summary(mydata$test)




#유의수준 =< p-value : 영가설을 채택
#유의수준 > p-value : 대안가설을 채택


#hypothesis test
rm(list=ls())
k.data <- read_excel("kor_data_20160024.xlsx")

## female
table(k.data$dem01)
k.data$dem01 <- k.data$dem01 - 1
summary(k.data$dem01)
t.test(k.data$dem01, mu=0.5)


##codes for the plot in the slide 
par(mar=c(0,1,0,1))
x <- seq(-3, 3, by=0.01)
y <- dt(x, 4)
plot(x, y, axes=F, type="l", ylim=c(-0.1, 0.5), xlab="", ylab="")
abline(h=0)
ul <- qt(1-(0.1/2), df=4)
ll <- -ul
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), density=20)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), density=20, angle=135)

text(0, 0.2, expression(0.95))
text(ll, -0.02, expression(0.464173), cex=0.8)
text(ul, -0.02, expression(0.520827), cex=0.8)



##나이
summary(k.data$dem14_01)
t.test(k.data$dem14_01, mu=45.04)

v <- seq(44, 46, 0.2)

for (i in v){
  print(t.test(k.data$dem14_01, mu=i))
}









### if
x <- 2
if (x == 1){
  print("x is equal to 1")
} else{
  print("x is not equal to 1")
}


#else를 쓸 때 주의. 
#else는 바로 앞의 if의 여집합에 해당하는 모든 조건에 적용됨. 
#따라서 조건의 개수가 3개 이상일 경우에는 else를 안 쓰는 것이 일반적으로 적절


k.data$test <- NA

for (i in 1:nrow(k.data)){
  if (k.data$dem01[i] == 1){
    k.data$test[i] <- "x is equal to 1"
  }
  else {
    k.data$test[i] <- "x is not equal to 1"
  }
}

table(k.data$dem01, k.data$test)


#2와 3을 비교

k.data$test2 <- NA

for (i in 1:nrow(k.data)){
  if (k.data$dem01[i] == 1){
    if (k.data$dem14_01[i] < 40) {
      k.data$test2[i] <- "male, below 40"
    }
    if (k.data$dem14_01[i] >= 40 & k.data$dem14_01[i] <= 60) {
      k.data$test2[i] <- "male, between 40 and 60"
    }
    if (k.data$dem14_01[i] > 60) {
      k.data$test2[i] <- "male, above 60"
    }
  }
  
  else {
    if (k.data$dem14_01[i] < 40) {
      k.data$test2[i] <- "female, below 40"
    }
    if (k.data$dem14_01[i] >= 40 & k.data$dem14_01[i] <= 60) {
      k.data$test2[i] <- "female, between 40 and 60"
    }
    if (k.data$dem14_01[i] > 60) {
      k.data$test2[i] <- "female, above 60"
    }
  }
}

table(k.data$dem14_01, k.data$test2)



k.data$test3 <- NA

for (i in 1:nrow(k.data)){
  if (k.data$dem01[i] == 1){
    if (k.data$dem14_01[i] < 40) {
      k.data$test3[i] <- "male, below 40"
    }
    if (k.data$dem14_01[i] >= 40 & k.data$dem14_01[i] <= 60) {
      k.data$test3[i] <- "male, between 40 and 60"
    }
    else {
      k.data$test3[i] <- "male, above 60"
    }
  }
  
  else {
    if (k.data$dem14_01[i] < 40) {
      k.data$test3[i] <- "female, below 40"
    }
    if (k.data$dem14_01[i] >= 40 & k.data$dem14_01[i] <= 60) {
      k.data$test3[i] <- "female, between 40 and 60"
    }
    else {
      k.data$test3[i] <- "female, above 60"
    }
  }
}

table(k.data$dem14_01, k.data$test3)


## ifelse 
k.data$test4 <- NA
k.data$test4 <- ifelse(k.data$dem01 == 1, "male", "female")
table(k.data$dem01, k.data$test4)












# function
var.function <- function(x){
  n <- length(x)
  m <- mean(x)
  calculated.var <- (sum((x-m)^2))/n
  return(calculated.var)
}

var.function

a<-c(234, 234, 234, 233, 233)
var(a)
var.function(a)

var(k.data$dem14_01)
var.function(k.data$dem14_01)

test.function <-function(alpha, pop.mean, sample.size, sample.mean, sample.var){
  num <- sample.mean - pop.mean
  denom <- sqrt(sample.var)/sqrt(sample.size)
  t <- num/denom
  p <- 1 - pt(t, df=sample.size - 1)
  if (p < alpha){
    result <- "null hypothesis rejected"
  }
  else {
    result <- "null hypothesis accepted"
  }
  return(c(p, result))
}

test.function(0.05, 10, 16, 11, 4)

