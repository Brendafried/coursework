library(dplyr)
library(stargazer)
library(caret)
library(tidyverse)
library(modelr)

loan <- read.csv("https://www.dropbox.com/s/89g1yyhwpcqwjn9/lending_club_cleaned.csv?raw=1")
summary(loan)

logit1 <- glm(good ~ fico, data = loan, family = "binomial")
summary(logit1)
exp(coef(logit1))

test <- data.frame(fico = c(750, 800))
test$pred = predict(logit1, test, type = "response")
test

logit2 <- glm(good ~ fico + loan_amnt, data= loan, family = "binomial")
summary(logit2)
exp(coef(logit2))

logit3 <- glm(good~ fico + loan_amnt + purpose, data = loan, family = "binomial")
summary(logit3)

round(exp(coef(logit3)),3)

loan <- loan %>% 
  group_by(purpose) %>%
  mutate(num_obs = n())
loan$purpose <- reorder(loan$purpose, -loan$num_obs)
levels(loan$purpose)

logit4 <- glm(good ~ fico + loan_amnt + income + purpose, data = loan, family = "binomial")
summary(logit4)

set.seed(364)
sample <- sample(nrow(loan), floor(nrow(loan)* 0.8))
train <- loan[sample,]
test <- loan[-sample,]
logit4 <- glm(good ~ fico + dti + loan_amnt + purpose, data= train, family = "binomial")
test$pred <- predict(logit4, test, type = "response")
test$good_pred <- ifelse(test$pred > 0.80, "good", "bad")
table(test$good_pred, test$good)

#accuracy
mean(test$good_pred == test$good)

library(pROC)
roc <- roc(test$good, test$pred)
test$sens <- roc$sensitivities[2:8508]
test$spec <- roc$specificities[2:8508]

ggplot(test, aes(x = spec, y=sens)) + geom_line()

#compare naive bayes to logistic regression

library(e1071)
loan$ficocat <- cut(loan$fico, breaks=c(0,687, 742,1000),
                    labels = c("bottom 25%", "middle 50%", "top 25%"))
loan$dticat <- cut(loan$dti, breaks=c(0, 8.2,18.68,100),
                   labels=c("bottom 25%", "middle 50%", "top 25%"))

train$ficocat <- loan$ficocat[sample]
test$ficocat <- loan$ficocat[-sample]
train$dticat <- loan$dticat[sample]
test$dticat <- loan$dticat[-sample]

NB <- naiveBayes(good ~ ficocat + dticat + purpose, train)
test$predNB <- predict(NB, select(test, ficocat, dticat, purpose), type = "raw")[,"good"]
rocNB <- roc(test$good, test$predNB)

test$sensNB <- rocNB$sensitivities[2:8508]
test$specNB <- rocNB$specificities[2:8508]

ggplot(test) + geom_line(aes(x = spec, y = sens), color = "blue") + geom_line(aes(x=specNB, y=sensNB), color="red")
