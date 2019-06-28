library(dplyr)
library(MASS)

fix(Boston)
names(Boston)

lm.fit = lm(medv~lstat, data=Boston)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

predict(lm.fit, data.frame(lstat=c(5,10,15)),
        interval="prediction")
plot(lstat, medv)
abline(lm.fit)

abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat,medv,pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) #which observation has the largest leverage statistic


# body data problem
body_data <- read_table('http://jse.amstat.org/datasets/body.dat.txt', col_names=(c("Biacromial_diameter", "Biiliac_diameter", "Bitrochanteric_diameter", "chest_depth", "chest_diameter", "elbow_diameter", "wrist_diameter", "knee_diameter", "ankle_diameter", "shoulder_girth", "chest_girth", "waist_girth", "navel_girth", "hip_girth", "thigh_girth", "bicep_girth", "forearm_girth", "knee_girth", "calf_max_girth", "ankle_min_girth", "wrist_min_girth", "age", "weight", "height", "gender"
)))

lm.fit = lm(weight~ height, data=body_data)
abline(lm.fit)
summary(lm.fit)

body_data %>%
  ggplot(aes(x=height, y=weight, color= as.factor(gender), shape=as.factor(gender))) +
  geom_point()
           