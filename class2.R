
install.packages("readxl")

library(readxl)

k.data <- read_excel("kor_data_20160024.xlsx")

View(k.data)
str(k.data)

str(cars)  # R??? 기본????????? ????????? ???????????? 경??? 
           # ?????? environment??? ????????? ????????? ??????
plot(cars$speed, cars$dist, 
     main="Speed and distance", xlab="Speed(mph)", ylab="distance(ft)", 
     pch=1, col="red")

plot(k.data$dem14_01, 
     k.data$uni01, 
     main="Age and Unification", 
     xlab="Age", 
     ylab="Unification", 
     pch=2, 
     col="blue")

table(k.data$uni01)
table(k.data$dem14_01)
table(k.data$uni01, k.data$dem14_01)

a<-table(k.data$uni01, k.data$dem14_01)
#막대그래프
barplot(a,
        main="Age and Unification", 
        xlab="Age", 
        ylab="Unification (frequency)", 
        beside=F, 
        col=c("orange", "green", "red", "blue", "grey"))

barplot(table(k.data$uni01, k.data$dem14_01),
        main="Age and Unification", 
        xlab="Age", 
        ylab="Unification (frequency)", 
        beside=F)
#히스토그램
hist(k.data$dem14_01, 
     main="Distribution across age", 
     xlab="Age", 
     ylab="Frequency", 
     breaks= c(seq(0, 100, 5)))

#c는 column의 약자

pie(k.data$dem14_01, main="Distribution across age")

#원그래프
b<-table(k.data$dem14_01) 
pie(b, main="Distribution across age")
pie(table(k.data$dem14_01), main="Distribution across age")





k.data$dem14_01

k.data[,5]

table(k.data$dem14_01)

sort(k.data$dem14_01)
#r은 파이썬과 다르게 1부터 시작
sort(k.data$dem14_01)[1]
sort(k.data$dem14_01)[1200]
k.data$dem14_01[1]
k.data$dem14_01[1200]

sort(k.data$dem14_01, decreasing=TRUE)
sort(k.data$dem14_01, decreasing=TRUE)[1]
sort(k.data$dem14_01, decreasing=TRUE)[1200]
View(read_excel)


min(k.data$dem14_01)
max(k.data$dem14_01)

barplot(table(k.data$dem14_01))
table(k.data$dem14_01)
#min 함수나 max함수처럼 최빈값을 바로 알려주는 기본 함수는 없음.
max(table(k.data$dem14_01))

sum((k.data$dem14_01* 1/1200))
mean(k.data$dem14_01)

k.data$dem14_01[1]

#[1]번쨰 자리 숫자 새로 지정
k.data$dem14_01[1] <- 48

mean(k.data$dem14_01)
k.data$dem14_01[length(k.data$dem14_01)]

k.data$dem14_01[length(k.data$dem14_01)] <-NA 
#여러 값 중 하나라도 결측값이 있으면 mean() 함수는 결과로 NA를 반환
mean(k.data$dem14_01)
#na.rm=TRUE: 이는 옵션으로, 결측값을 제거하도록 지정
mean(k.data$dem14_01, na.rm=TRUE)

# 벡터 c1을 생성하고, 이 벡터에 1, 1, 1을 할당
c1<-c(1, 1, 1)
#벡터 c1을 정렬:오름차순
sort(c1)[2]
#중앙값(중간값)을 계산
median(c1)


median(1)
median(1000)
median(k.data) #error message will appear


c2<-c(1, 2, 1)
sort(c2)[2]
median(c2)

c3<-c(1, 2, 3, 1)
c3.1<-sort(c3)[2]
c3.2<-sort(c3)[3]
(c3.1+c3.2)/2
median(c3)

c4<-c(1, 1, 2, 1)
c4.1<-sort(c4)[2]
c4.2<-sort(c4)[3]
(c4.1+c4.2)/2
median(c4)

k<-k.data$dem14_01[1:100]
k
sort(k)
median(k)

median(k.data$dem14_01)
median(k.data$dem14_01, na.rm = TRUE)




##we now use kor_welfare_2014.xls 
rm(list=ls())
w_data<-read.csv("kor_welfare_2014.csv")

colnames(w_data)
colnames(w_data)[2:5]

w_data$age
table(w_data$age)
mean.age <- mean(w_data$age)

deviation.age <-  w_data$age - mean.age  
w_data$deviation.age <-  w_data$age - mean.age 

sum(deviation.age)
sum(w_data$deviation.age)



example<-c(1,2,3,4,5)
#평균
mean.example<-mean(example)
mean.example
#편차
deviation.example<-example-mean.example
deviation.example
#편차의 제곱
deviation.example.2<-deviation.example^2
deviation.example.2
#분산
variance<-mean(deviation.example.2)
variance
#표준편차
standard.deviation<-sqrt(variance)
standard.deviation

#분산과 표준편차 구하는 툴
#위에서 우리가 일일이 한 것과 다른 값을 보여줌. 
#그 이유는 편차의 제 곱의 평균을 구하는 과정에서 분모값으로 n-1을 사용하기 때문.
var(example)
sd(example)


# w_data$deviation.age was already made above
w_data$deviation.age2 <- (w_data$deviation.age)^2
w_data$deviation.age2[100:200]

w_data$variance <- mean(w_data$deviation.age2)
#nrow(w_data)는 데이터 프레임 w_data의 행의 개수
w_data$variance[1000:nrow(w_data)]

table(w_data$variance)

sd<-sqrt(mean(w_data$deviation.age2))
sd

var(w_data$age)

sd(w_data$age)

## relationship between mean and standard deviation (and variance)
example2<-c(1:21)
mean(example2)
sd(example2)
hist(example2, breaks=(c(seq(0, 21, 1))), 
     main="Mean:11, standard deviation: 6.205",
     ylim=c(0, 10)) 



example3 <- c(11, 11, 11, 11, 11, 
              10, 10, 10, 
              9, 9,
              8, 8, 
              7,
              12, 12, 12,
              13, 13,
              14, 14,
              15)
mean(example3)
sd(example3)
hist(example3, breaks=(c(seq(0, 21, 1))), 
     main="Mean:11, standard deviation: 2.121",
     ylim=c(0, 10))               




example4<-c(11,11,11,11,11,11,11,11,11, 
            10, 10, 10, 10, 
            9, 9, 
            12, 12, 12, 12,
            13, 13)
mean(example4)
sd(example4)
hist(example4, breaks=(c(seq(0, 21, 1))), 
     main="Mean:11, standard deviation: 1.095",
     ylim=c(0, 10))  



## Quantile
# 분위수를 계산하는 함수
quantile(w_data$inc)

boxplot(w_data$inc, main="Montly household income")

summary(w_data$inc)

w_data$inc_copy <- w_data$inc
w_data$inc_copy[ 900 : nrow(w_data) ] <- NA
summary(w_data$inc_copy)

#새로운 줄 생성. 텅빈 값에 범위에 맞게 수를 지정 
#첫 수를 NA로 지정하면 코딩실수를 줄일 수 있음
w_data$new_variable <-NA
w_data$new_variable[ w_data$inc <=3 ] <-1
w_data$new_variable[ w_data$inc > 4 & w_data$inc <=8 ] <-2
w_data$new_variable[ w_data$inc > 8 ] <-3

quantile(w_data$new_variable)

summary(w_data$new_variable)
quantile(w_data$new_variable, na.rm = TRUE)



w_data$new_variable.2 <-NA
w_data$new_variable.2[ w_data$inc <=4 ] <-1
w_data$new_variable.2[ w_data$inc > 4 & w_data$inc <=8 ] <-2
w_data$new_variable.2[ w_data$inc > 8 ] <-3

summary(w_data$new_variable.2)
quantile(w_data$new_variable.2)
boxplot(w_data$new_variable.2, 
        main="Montly household income (new variable)")




m.data <- read_excel("March_first_movements.xlsx")

summary(m.data$Q9)
table(m.data$Q9)

m.data$japan<-NA
m.data$japan[m.data$Q9 == "① 완전히 청산되었다"] <-1 
m.data$japan[m.data$Q9 == "② 대체로 청산되었다"] <-2
m.data$japan[m.data$Q9 == "별로 청산되지 않았다"] <-3
m.data$japan[m.data$Q9 == "④ 전혀 청산되지 않았다"] <-4
m.data$japan[m.data$Q9 == "9999"] <-NA

summary(m.data$japan)
table(m.data$japan)

barplot(table(m.data$japan))


## csv files do not need any package. No need to install or library any package. 
install.packages("foreign")
library("foreign")
spss.data <- read.spss("March_first_movements.sav")
#r의 경우 뒤에 to.data.frame=TRUE를 붙여야 정형화된 데이터 형식이 나온다.
spss.data2 <- read.spss("March_first_movements.sav", to.data.frame=TRUE)  

View(spss.data)
View(spss.data2)
