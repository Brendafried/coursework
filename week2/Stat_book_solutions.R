#Statistics
library(tidyverse)
# Question 7.1

#read in data
pop2 <- read.csv("pop2.csv")

#1
mean(pop2$bmi)

#2
sd(pop2$bmi)

#3
x.bar <- rep(0, 10^5)
for(i in 1:10^5) {
  x.samp <- sample(pop2$bmi, 150)
  x.bar[i] <- mean(x.samp)
}
mean(x.bar)
#4
sd(x.bar)
sd(pop2$bmi)

#5 - between these values are 80% of the sampling distribution
quantile(x.bar, c(0.1, 0.9))

#6
qnorm(c(0.1, 0.9), mean(x.bar), sd(x.bar))

#Chapter 9
#1
cars <- read_csv("cars.csv")
summary(cars)

X.bar <- rep(0, 10^5)
for(i in 1:10^5) {
  X <- rexp(201, 1/2000)
  X.bar[i] <- mean(X)
}
mean(abs(X.bar-12000) <= 1.96*0.0705*12000)

mid.range <- rep(0, 10^5)
for(i in 1:10^5) {
  X <- runif(100, 3, 7)
  mid.range[i] <- (max(X)+min(X))/2
}

quantile(mid.range, c(0.025, 0.975))
mean(mid.range)
sd(mid.range)

magnets <- read_csv("magnets.csv")
summary(magnets)
 
mean(magnets$change[1:29])
mean(magnets$change[30:50])

magnets %>%
  group_by(active) %>%
  summarize(avg = mean(change))

sd(magnets$change[1:29])
sd(magnets$change[30:50])

magnets %>%
  group_by(active)%>%
  summarize(sd = sd(change))

boxplot(magnets$change[1:29])
boxplot(magnets$change[30:50])

table(magnets$change[30:50])
table(magnets$change[1:29])

#9.2
mu1 <- 3.5
sig1 <- 3
mu2 <- 3.5
sig2 <- 1.5

test.stat <- rep(0, 10^5)
for(i in 1:10^5)
{
  X1 <- rnorm(29, mu1, sig1)
  X2 <- rnorm(21, mu2, sig2)
  X1.bar <- mean(X1)
  X2.bar <- mean(X2)
  X1.var <- var(X1)
  X2.var <- var(X2)
  test.stat[i] <- (X1.bar-X2.bar)/sqrt(X1.var/29 + X2.var/21)
}
quantile(test.stat, c(0.025,0.975))

# Chapter 4 - Probability

ex1 <- read.csv("ex1.csv")
summary(ex1)

summary(pop.1)

sample(pop.1$height, 1)
X <- pop.1$height
mean(abs(X-170) <= 10)
Y <- c(6.3, 6.9, 6.6, 3.4, 5.5, 4.3, 6.5, 4.7, 6.1, 5.3)
abs(Y - 5) <= 1
mean(abs(Y - 5) <= 1)

#4.1.6
Y.val <- c(0,1,2,3,4,5)
P.val <- c(1,2,3,4,5,6)/21

#Chapter 6
pnorm((5-2)/3) - pnorm((0-2)/3)
1-pnorm(650, 560, 57)
1 - pnorm(650, 630, 61)
qnorm(0.1, 560, 57)
qnorm(0.9, 560, 57)

qnorm(0.1, 630, 61)
qnorm(0.9, 630, 61)

# example 5 in 8.3.5
#HELP!

#10.1
mu <- 3
sd <- sqrt(2)
X.bar <- rep(0, 10^5)
X.med <- rep(0, 10^5)
for(i in 1:10^5) {
  X <- rnorm(100, mu, sd)
  X.bar[i] <- mean(X)
  X.med[i] <- median(X)
}
ex2 <-read_csv("ex2.csv")
mean(pop2$group == "HIGH")
mean <- mean(ex2$group == "HIGH")
mean
P.hat <- rep(0, 10^5)
for(i in 1:10^5) {
  X <- sample(pop2$group, 150)
  P.hat[i] <- mean(X == "HIGH")
}
mean(P.hat)
