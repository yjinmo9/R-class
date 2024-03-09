rm(list=ls())

## Independent sample
baby <- read.table("baby_weight.txt", header=T)
str(baby)

var.test(baby$weight ~ baby$gender)

t.test(baby$weight ~ baby$gender, mu=0, alternative="two.sided", var.equal=TRUE)


baby$weight[nrow(baby)]<-6000
var.test(baby$weight ~ baby$gender)
t.test(baby$weight ~ baby$gender, mu=0, alternative="two.sided", var.equal=FALSE)



#### ANOVA Figure
set.seed(2)
grp1 <- rnorm(20, mean=-1)
grp2 <- rnorm(20, mean=0.5)
grp3 <- rnorm(20, mean=1)

d <- data.frame(grp=rep(1:3, each=20), val=c(grp1, grp2, grp3))
par(mar=c(1, 1, 1, 1))

plot(val~grp, data=d, pch=20, col="red", xlim=c(1, 3.5), ylim=c(-4.2, 4), axes=F, xlab="", ylab="")

text(1.2, -4, "Group 1")
text(2.2, -3, "Group 2")
text(3.2, -2, "Group 3")

abline(h=mean(d$val), lwd=2, col="red")
arrows(1.5, 1.9, 1.5, mean(d$val), length=0.1)
text(1.5, 2.2, expression(bar(y[..]) ), cex=1.5)

lines(c(1, 1), c(-4, 2), lty=2, col="blue")
lines(c(2, 2), c(-2.5, 3.5), lty=2, col="blue")
lines(c(3, 3), c(-2, 4), lty=2, col="blue")

y1 <- seq(-4, 2, by=0.01)
x1 <- 1+dnorm(y1, mean=-1)
lines(x1, y1, lty=3, lwd=1.5)
lines(c(1, max(x1)), c(-1, -1), lty=2)
arrows(1.5, -3, 1.3, -2, length=0.05)
text(1.5, -3.2, expression(N(mu[1], sigma^2) ))

y2 <- seq(-2.5, 3.5, by=0.01)
x2 <- 2+dnorm(y2, mean=0.5)
lines(x2, y2, lty=3, lwd=1.5)
lines(c(2, max(x2)), c(0.5, 0.5), lty=2)
arrows(2.4, 1.8, 2.3, 1.3, length=0.05)
text(2.5, 2.2, expression(N(mu[2], sigma^2) ))

y3 <- seq(-2, 4, by=0.01)
x3 <- 3+dnorm(y3, mean=1)
lines(x3, y3, lty=3, lwd=1.5)
lines(c(3, max(x3)), c(1, 1), lty=2)
arrows(3.4, 2.8, 3.3, 2, length=0.05)
text(3.4, 3.2, expression(N(mu[3], sigma^2) ))


## ANOVA test
age<- read.csv("age_data.csv")
str(age)
test <-lm(age ~ scale, data = age)
anova(test)


## chi squared test
# Figure
x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)

alpha <- 0.05
tol <- qchisq(0.95, df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
text(0, -0.03, "0", cex=0.8)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
text(tol, -0.03, expression(chi[0.05]^{2}==7.81), cex=0.8)

#적합도 검정
##goodness of fit test
x<-c(315, 101, 108, 32)
chisq.test(x, p=c(9/16, 3/16, 3/16, 1/16))

chisq.test(c(2550, 2450), p=c(1/2, 1/2))


#동질성 검정
## homogeneity test
sns <- read.csv("snsbyage.csv", header=T)
str(sns)

sns.table<-table(sns$age, sns$service)
sns.table

chisq.test(sns.table)
chisq.test(table(sns$age, sns$service))





## regression slide
w.data<-readRDS("WV6.rds")
summary(w.data$V131)
table(w.data$V131)
summary(w.data$V248)
table(w.data$V248)


w.data$welfare_attitude <- w.data$V131
w.data$welfare_attitude[w.data$V131 < 0 ] <- NA
summary(w.data$welfare_attitude)
table(w.data$welfare_attitude)

w.data$education <- w.data$V248
w.data$education[ w.data$V248 < 0 ] <- NA
summary(w.data$education)
table(w.data$education)

lm(welfare_attitude ~ education, data = w.data)

r.1 <- lm(welfare_attitude ~ education, data = w.data)
summary(r.1)


# controls 
summary(w.data$V55)
table(w.data$V55)
w.data$control.my.life <- w.data$V55
w.data$control.my.life [ w.data$V55 < 0 ] <- NA
table(w.data$control.my.life)
summary(w.data$control.my.life)

summary(w.data$V57)
table(w.data$V57)
w.data$marital.status <- w.data$V57
w.data$marital.status[ w.data$V57 < 0 ] <- NA
w.data$marital.status[ w.data$V57 == 1 | w.data$V57 == 2 ] <- 1
w.data$marital.status[ w.data$V57 == 3 | w.data$V57 == 4 | w.data$V57 == 5 ] <- 2
w.data$marital.status[ w.data$V57 == 6 ] <- 3
table(w.data$marital.status)
summary(w.data$marital.status)

summary(w.data$V84)
table(w.data$V84)
w.data$political.interest <- NA
for (i in 1:nrow(w.data)){
  if (w.data$V84[i] < 0 ){
    w.data$political.interest[i] <- NA
  }
  else {
    w.data$political.interest[i] <- 4 - w.data$V84[i]
  }
}
table(w.data$political.interest)
summary(w.data$political.interest)




summary(w.data$V239)
table(w.data$V239) 
w.data$income.level <- w.data$V239 
w.data$income.level [ w.data$V239 < 0] <- NA
table(w.data$income.level)
summary(w.data$income.level)


summary(w.data$V240)
table(w.data$V240)
w.data$female <- NA
w.data$female [ w.data$V240 == 1] <- 0
w.data$female [ w.data$V240 == 2] <- 1
table(w.data$female)
summary(w.data$female)





summary(w.data$V242)
table(w.data$V242)
w.data$age.group <- NA
w.data$age.group [w.data$V242 >= 16 & w.data$V242 <= 19 ] <- 1
w.data$age.group [w.data$V242 >= 20 & w.data$V242 <= 29 ] <- 2
w.data$age.group [w.data$V242 >= 30 & w.data$V242 <= 39 ] <- 3
w.data$age.group [w.data$V242 >= 40 & w.data$V242 <= 49 ] <- 4
w.data$age.group [w.data$V242 >= 50 & w.data$V242 <= 59 ] <- 5
w.data$age.group [w.data$V242 >= 60 & w.data$V242 <= 69 ] <- 6
w.data$age.group [w.data$V242 >= 70 ] <- 7
table(w.data$age.group)
summary(w.data$age.group)



#회귀분석 결과

r.2 <- lm(welfare_attitude ~ education + 
            female + 
            age.group + 
            income.level + 
            political.interest +
            control.my.life + 
            marital.status
          , data = w.data)
summary(r.2)


r.3 <- lm(welfare_attitude ~ education + 
            female + 
            age.group + 
            income.level + 
            political.interest +
            control.my.life + 
            as.factor(marital.status)
          , data = w.data)
summary(r.3)


r.4 <- lm(welfare_attitude ~ education + 
            female + 
            age.group + 
            income.level + 
            political.interest +
            control.my.life + 
            as.factor(marital.status) +
            as.factor(V2)
          , data = w.data)
summary(r.4)

r.5 <- lm(welfare_attitude ~ education + 
            female + 
            age.group + 
            income.level + 
            political.interest +
            control.my.life + 
            as.factor(marital.status)
          , data = subset(w.data, V2== 410))
summary(r.5)




#Regression table


install.packages("stargazer")
library(stargazer)

stargazer(r.1, r.2, r.3, r.4, r.5, 
          keep = c("education", "female", 
                   "age.group", "income.level", 
                   "political.interest", "control.my.life", 
                   "marital.status", 
                   "as.factor(marital.status)2", 
                   "as.factor(marital.status)3"),
          type="html", 
          title="Results", 
          align=TRUE,  
          out="regression_table.doc")



#Coefficient plot

install.packages(c("modelsummary", "ggplot2"))
library(modelsummary) ## for "modelplot"
library(ggplot2) ## for "geom_vline"

modelplot(list("model 1"= r.1, 
               "model 2"= r.2, 
               "model 3"= r.3, 
               "model 4"= r.4, 
               "model 5"= r.5), 
          conf_level = 0.95, 
          coef_map = c("as.factor(marital.status)3",
                       "as.factor(marital.status)2", 
                       "marital.status",
                       "political.interest", "control.my.life",
                       "age.group", "income.level",
                       "female", "education"), 
          size = 0.1) +
  geom_vline(xintercept = 0, linetype="longdash", color = 'black')




### Public data ###
#공공데이터
install.packages("readxl")
install.packages("dplyr")
rm(list=ls())
library(readxl)

setwd("/Users/yangjinmo/Desktop/r특강/공공데이터")
data.pop <- read.csv("pop_2021.csv")
data.f.ind <- read.csv("financial_ind_2022.csv")
data.econ.act <-read.csv("econ_act_2022.csv")
data.admin.dist <- read_excel("KIKcd_H.20220901.xlsx")


library(dplyr)
name.list <- distinct(data.admin.dist, 시도명, 시군구명)
name.list <- na.omit(name.list)
#name.list에서 "출장소"가 포함된 행을 출장소에 저장하고 해당 행을 name.list에서 제거합니다.
출장소 <- name.list[grepl("출장소", name.list$시군구명), ]
name.list <- name.list[!grepl("출장소", name.list$시군구명), ]

data.f.ind$지역명[ data.f.ind$지역명 == "서울"] <- "서울특별시"
data.f.ind$지역명[ data.f.ind$지역명 == "부산"] <- "부산광역시"
data.f.ind$지역명[ data.f.ind$지역명 == "대구"] <- "대구광역시"
data.f.ind$지역명[ data.f.ind$지역명 == "인천"] <- "인천광역시"
data.f.ind$지역명[ data.f.ind$지역명 == "광주"] <- "광주광역시"
data.f.ind$지역명[ data.f.ind$지역명 == "대전"] <- "대전광역시"
data.f.ind$지역명[ data.f.ind$지역명 == "울산"] <- "울산광역시"
data.f.ind$지역명[ data.f.ind$지역명 == "경기"] <- "경기도"
data.f.ind$지역명[ data.f.ind$지역명 == "경북"] <- "경상북도"
data.f.ind$지역명[ data.f.ind$지역명 == "경남"] <- "경상남도"
data.f.ind$지역명[ data.f.ind$지역명 == "강원"] <- "강원도"
data.f.ind$지역명[ data.f.ind$지역명 == "전북"] <- "전라북도"
data.f.ind$지역명[ data.f.ind$지역명 == "전남"] <- "전라남도"
data.f.ind$지역명[ data.f.ind$지역명 == "충북"] <- "충청북도"
data.f.ind$지역명[ data.f.ind$지역명 == "충남"] <- "충청남도"
data.f.ind$지역명[ data.f.ind$지역명 == "제주"] <- "제주특별자치도"
## 세종은 name.list에 포함되어 있지 않음


#data.f.ind$자치단체명에서 처음 두 글자를 제거합니다.
data.f.ind$자치단체명 <- sub('..', '', data.f.ind$자치단체명) ## 처음 두 글자 제거


#combined.data를 초기화하고 data.f.ind와 name.list를 사용하여 financial.ind 열을 병합합니다.
combined.data <- name.list
combined.data$financial.ind <- NA
data.f.ind$merged <- 0

for (i in 1:nrow(combined.data)){
  for (j in 1:nrow(data.f.ind)){
    if (combined.data$시도명[i] == data.f.ind$지역명[j] & combined.data$시군구명[i] == data.f.ind$자치단체명[j]){
      combined.data$financial.ind[i] <- data.f.ind$"재정자립도.결산..개편후."[j]
      data.f.ind$merged[j] <- 1
    }
  }
}

summary(combined.data$financial.ind)
table(data.f.ind$merged)








#data.pop 데이터프레임의 열 이름을 의미 있는 이름으로 변경합니다.
colnames(data.pop) <- c("admin.district", "total.pop", "total.male", "total.female")
colnames(data.pop)
#data.pop 데이터프레임의 첫 번째 행을 제거합니다.
data.pop <- data.pop[-1, ]

data.pop$시도명 <- NA
data.pop$시군구명 <- NA

#stringr 라이브러리를 로드하고, "admin.district" 열에서 고정된(" ") 공백을 모두 제거합니다.
library(stringr)
data.pop$admin.district <- str_replace_all(data.pop$admin.district, fixed(" "), "")

#"admin.district" 열을 ">"를 구분자로 사용하여 두 부분(시도명 및 시군구명)으로 분할하고, 새로운 열에 할당합니다.
for (i in 1:nrow(data.pop)){
  temp <- str_split(data.pop$admin.district[i], ">", simplify = TRUE)
  data.pop$시도명[i] <- temp[, 1]
  data.pop$시군구명[i] <- temp[, 2]
}

#"시도명" 및 "시군구명" 열의 일치 여부를 기반으로 data.pop에서 combined.data로 데이터를 병합하려고 합니다. 
#일치하는 경우, combined.data$total.pop 열에 총 인구 값을 할당하고, data.pop의 "merged" 열을 1로 업데이트합니다.
combined.data.2 <- merge(combined.data, data.pop, by = c("시도명", "시군구명"))
combined.data$total.pop <- NA
data.pop$merged <- 0

for (i in 1:nrow(combined.data)){
  for (j in 1:nrow(data.pop)){
    if (combined.data$시도명[i] == data.pop$시도명[j] & combined.data$시군구명[i] == data.pop$시군구명[j]){
      combined.data$total.pop[i] <- data.pop$total.pop[j]
      data.pop$merged[j] <- 1
    }
  }
}








#combined.data의 "total.pop" 열을 숫자형으로 변환합니다.
summary(combined.data$total.pop) ## it shows that we have to store "total.pop" as numeric.
combined.data$total.pop <- as.numeric(combined.data$total.pop)

summary(combined.data$total.pop)
table(data.pop$merged)



colnames(data.econ.act) <- c("admin.district", "econ.prop.total", "econ.prop.male", "econ.prop.female")
colnames(data.econ.act)
data.econ.act <- data.econ.act[-1, ]

data.econ.act$시도명 <- NA
data.econ.act$시군구명 <- NA

for (i in 1:nrow(data.econ.act)){
  if (i <= 74 ){
    temp <- str_split(data.econ.act$admin.district[i], " ", simplify = TRUE)
    data.econ.act$시도명[i] <- temp[, 1]
    data.econ.act$시군구명[i] <- temp[, 2]
  }
  else {
    data.econ.act$시군구명[i] <- data.econ.act$admin.district[i]
  }
}

combined.data$econ.act <-NA
data.econ.act$merged <- 0

for (i in 1:nrow(combined.data)){
  for (j in 1:nrow(data.econ.act)){
    if (!is.na(data.econ.act$시도명[j])){
      if (combined.data$시도명[i] == data.pop$시도명[j] & combined.data$시군구명[i] == data.econ.act$시군구명[j]){
        combined.data$econ.act[i] <-data.econ.act$econ.prop.total[j]
        data.econ.act$merged[j] <- 1 + data.econ.act$merged[j]   ## why? 
      }
    }
    else {
      if (combined.data$시군구명[i] == data.econ.act$시군구명[j]){
        combined.data$econ.act[i] <-data.econ.act$econ.prop.total[j] 
        data.econ.act$merged[j] <- 1 + data.econ.act$merged[j]  ## why?
      }
    }
  }
}

summary(combined.data$econ.act) ## it shows that we have to store "total.pop" as numeric.
combined.data$econ.act <- as.numeric(combined.data$econ.act)
summary(combined.data$econ.act) 
table(data.econ.act$merged) ## two obs have value 2. What does this mean? 



#회귀분석
result <- lm(financial.ind ~ total.pop + econ.act, 
             data = combined.data)

summary(result)



