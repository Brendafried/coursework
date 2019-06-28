#coffee question

#Null Hypothesis
#Coffee does not cause cancer

#p1 is the probability of non coffee drinkers getting cancer
p1 <- .4
#p2 is the probability of coffee drinkers getting cancer
p2 <- .40001

#sig level is the probability of rejecting the null hypothesis when it's false
sig_level <- .1

#find the sample size needed
power.prop.test(p1=p1, p2=p2, sig.level = sig_level, power=.8, alternative = "two.sided")



#Comparing 2 proportions!
library(tidyverse)
library(dplyr)

#true is yawn, false is not yawn
cards <- c(rep("yawn", 14), rep("not_yawn", 36))

#1 sample
experiment <- function(cards) {
  shuffled_cards <- sample(cards)
  p_trtmt <- mean(shuffled_cards[1:34] == "yawn")
  p_ctrl <- mean(shuffled_cards[35:50] == "yawn")
  return (p_trtmt - p_ctrl)
}

p_difference <- replicate(1e4, experiment(cards))

#part b
observed_difference <- (10/34) - (4/16)

# put the data into a data frame so it's easier to filter
values <- data.frame(p_difference)

#find how many values are greater than or equal to observed_difference
p_values<- values %>%
  filter(p_difference >= observed_difference) %>%
  summarise(count=n())

#divide by total num of values to get percentage
all_values <- values %>%
  summarise(count=n())

p_value <- p_values/all_values

power.prop.test(n=25, p1=10/34, p2=4/16, sig.level=.05, alternative = "one.sided")

power.prop.test(p1=10/34, p2=4/16, sig.level=.05, power=.8, alternative="one.sided")

#hist(p_difference)
ggplot(data.frame(p_difference), aes(x=p_difference)) +
  geom_histogram(binwidth = .01) +
  geom_vline(xintercept=observed_difference)
  



