library(scales)
library(broom)
library(glmnet)
library(tidyverse)

theme_set(theme_bw())

options(repr.plot.height = 3, repr.plot.width = 4)

set.seed(42)

parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

load('trips.RData')

trips_per_day <- trips %>%
  group_by(ymd) %>%
  summarize(num_trips = n())

weather <- mutate(weather,
                  ymd = as.Date(date))

model_data <- inner_join(trips_per_day, weather, by = "ymd")

#Create model matrix

K <- 10
form <- formula(~ poly(tmin, K, raw = T))

X <- model.matrix(form, data = model_data)
y <- model_data$num_trips

model <- glmnet(X, y, alpha = 0)
coef(model, s = 100)

model <- glmnet(X, y, alpha = 1)
coef(model, s = 100)

#Cross validation for Ridge

cvfit <- cv.glmnet(X, y, alpha = 0)
coef(cvfit, s = "lambda.min")
plot(cvfit)
sqrt(min(cvfit$cvm))

grid <- 2^seq(0.1, 10, length = 100)
cvfit <- cv.glmnet(X, y, alpha = 0, lambda = grid)
coef(cvfit, s = "lambda.min")
plot(cvfit)
sqrt(min(cvfit$cvm))

model_data$pred <- as.vector(predict(cvfit, X, s = "lambda.min"))
#plot

ggplot(model_data, aes(x = tmin)) +
  geom_point(aes(y = num_trips)) +
  geom_line(aes(y = pred))

#cv for lasso
cvfit <- cv.glmnet(X, y, alpha = 1)
coef(cvfit, s = "lambda.min")
plot(cvfit)
sqrt(min(cvfit$cvm))

grid <- 2^seq(1, 7, length = 100)
cvfit <- cv.glmnet(X, y, alpha = 1, lambda = grid)
coef(cvfit, s = "lambda.min")
plot(cvfit)
sqrt(min(cvfit$cvm))

model_data$pred <- as.vector(predict(cvfit, X, s = "lambda.min"))
ggplot(model_data, aes(x = tmin)) +
  geom_point(aes(y = num_trips)) +
  geom_line(aes(y = pred))
