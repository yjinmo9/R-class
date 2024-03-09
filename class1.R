
4+4
4-4
4*4
4/4
4^4
4(4+2*3)


4<4
4<=4 
4>4
4>=4
4=4
4==4


a <- 1
a
b <- 2
b
a+b
c <- a + b
c
rm(list=ls())


a <- cars
View(a)
colnames(a)


c <- 5
d <- "5"
e <- "I hate"
f <- "statistics"

c+d
e+f

is.numeric(c)
is.numeric(d)
is.character(c)
is.character(d)
is.vector(c)
is.vector(d)

paste(e, f)
g.hanil <- paste(e, f)
help(paste)
help("paste")


data.co2 <- CO2
str(data.co2)
plot(data.co2$conc, data.co2$uptake)
plot(data.co2$Type, data.co2$Treatment)


help(View)
