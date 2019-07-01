library(modelr)
library(tidyverse)

babydata <- read.table("babyweights.txt")
View(babydata)

options(na.action = na.warn)
model <- lm(bwt ~ smoke, babydata)
summary(model)

model <- lm(bwt ~ parity, babydata)
summary(model)

model <- lm(bwt ~ gestation + parity + age + height + weight + smoke, babydata)
preds <- babydata %>% add_predictions(model)
head(preds$pred,1)- 120

summary(model)
