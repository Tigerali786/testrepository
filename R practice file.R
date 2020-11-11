mydata <- read.csv('titanic.csv')
View(mydata)
install.packages(dplyr)
library(dplyr)
head(mydata,5)
mydata <- mydata[duplicated(mydata),]
head(mydata,5)
mydata <- distinct(mydata)
head(mydata,5)

#column Names

names(mydata)

#select Function

myselect1 <- select(mydata,Sex:Age)
head(myselect1,6)

# Lib for Excel
library('readxl')
install.packages(ggplot2)

library(ggplot2)

data() # datasets in R

ggplot(data=mpg)
head(mpg)
unique(mpg$cyl)


ggplot(data=mpg,mapping = aes(x=cyl))
ggplot(data=mpg,mapping = aes(x=cyl))+geom_bar()
ggplot(data=sleep)      
head(msleep)
ggplot(data=msleep,mapping = aes(x=order))+geom_bar()

#scatter plot
ggplot(data=msleep,mapping = aes(x=order,y=sleep_total))+geom_violin()
ggplot(data=mpg,mapping = aes(x=displ,y=cty))+geom_smooth(method = 'lm')
ggplot(data=mpg,mapping = aes(x=displ,y=cty))+geom_smooth()
ggplot(data=mpg,mapping = aes(x=displ,y=cty,colour=cyl))+geom_jitter()+geom_smooth()
ggplot(data=mpg,mapping = aes(x=displ,y=cty,colour=cyl))+geom_jitter()+geom_smooth(span=0.5)

Video <- c(5.2,4.5,6.5,3.2,4.6,5.2,7.2)

# Arithmetic Mean
gm <- prod(Video)^(1/length(Video))
gm


stock <- c(110,80,130)

gm1 <- prod(stock)^(1/length(stock))
gm1


hm <- 1/mean(1/Video)
hm
summary(mydata)

median(Video)

mode(Video)

s <- c(1,2,3,4,4,5,6,7,8)
mode(s)

sort(table(s))

quantile(s,c(0.25,0.75))

# Standard Deviation

var(s)

sd(s)

library(dplyr)

#var(as.vector(dataset$column name))
#sd(as.vector(dataset$column name))

x <- c(10,20,30,40,10)
y <- c(10,20,30,40,10)

cor(x,y)

x <- c(10,20,30,40,50)
y <- c(50,40,30,20,10)

cor(x,y)

x <- c(10,30,19,35,60)
y <- c(30,25,54,35,24)

cor(x,y)

View(mydata)

cor(mydata$Survived)

#Lower Tail Hypothesis with NUll and alternative hypothesis

xbar <- 9900
mu0 <- 10000
sigma <- 120
n <- 30
z <- (xbar-mu0)/(sigma/sqrt(n))
z

alpha <- 0.05
z.alpha <- qnorm(1-alpha)
z.alpha

# T Test hypothesis

plantA <-c(239,244,256,268,279)
plantB <- c(232,234,236,238,239)
mean(plantA)
mean(plantB)

t.test(plantA,plantB,alternative = 'greater')

View(mydata)
t.test(mydata$Pclass~mydata$Sex,alternative='two.sided')
t.test(mydata$Fare~mydata$Sex,alternative='two.sided')

t.test(mydata$Pclass~mydata$Name,alternative='two.sided')
anovapclass <- aov(Pclass~Name,data=mydata)
summary(anovapclass)

#Chi Test

chisq.test(mydata$Pclass,mydata$Survived)

chisq.test(mydata$Sex,mydata$Name)

chisq.test(mydata$Age,mydata$Sex)