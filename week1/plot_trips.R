########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
trips %>%
  filter(tripduration/60 < 1000) %>%
  ggplot(aes(x = tripduration/60)) +
  scale_x_log10() +
  geom_histogram()

# plot the distribution of trip times by rider type
trips %>%
  ggplot() +
  scale_x_log10(label = comma) +
  geom_histogram(position = 'identity',
                 alpha = 0.2,
                 bins = 50,
                 aes( x = tripduration,
                          color = usertype, 
                          fill = usertype))

# plot the total number of trips over each day
trips %>% 
  group_by(ymd) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_line(aes(x = ymd, y = count))
# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
trips %>%
  mutate(age = year(ymd) - birth_year) %>%
  group_by(age, gender) %>%
  summarize(count = n()) %>%
  ggplot() +
  scale_y_continuous(label = comma, limits = c(0, 300000)) +
  geom_point(aes(x = age, y = count, color = gender)) +
  scale_color_brewer(palette="Set1", direction = -1)
# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio
trips %>%
  mutate(age = year(ymd) - birth_year) %>%
  group_by(age, gender) %>%
  summarize(count = n()) %>%
  spread(gender, count) %>%
  ggplot() +
  geom_point(aes(x = age, y = Male/Female),color = "blue")
########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
weather %>%
  ggplot() +
  geom_line(aes(x = ymd, y = tmin))

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting
weather %>%
  gather("min_max", "temp", tmin, tmax) %>%
  ggplot() +
  geom_point(aes(x = ymd, y = temp, color = min_max))
########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>%
  group_by(ymd, tmin) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_point(aes(x = tmin, y = count))
# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
weather %>%
  group_by(prcp) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_histogram(aes(x = prcp))

trips_with_weather %>%
  group_by(ymd, tmin, prcp) %>%
  summarize(count = n()) %>%
  ungroup %>%
  mutate(subs_prcp = ifelse(prcp >= 0.7, 'T', 'F')) %>%
  ggplot(aes(x = tmin, y = count, color = subs_prcp)) +
  geom_line()

  # add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weather %>%
  group_by(ymd, tmin, prcp) %>%
  summarize(count = n()) %>%
  ungroup %>%
  mutate(subs_prcp = ifelse(prcp >= 0.7, 'T', 'F')) %>%
  ggplot(aes(x = tmin, y = count, color = subs_prcp)) +
  geom_point() +
  geom_smooth() + 
  labs(x = "Min temp", y = "Trip Count")

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
trips %>%
  mutate(hour = hour(starttime)) %>%
  group_by(hour, ymd) %>%
  summarize(count = n()) %>%
  group_by(hour) %>%
  summarize(avg = mean(count), sd = sd(count)) %>%
  ggplot() +
  geom_line(aes(x = hour, y = avg), color = "blue") +
  geom_ribbon(aes(x = hour,
                  ymin = (avg - sd),
                  ymax = avg + sd),
                  alpha = 0.2)
# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package

trips %>%
  mutate(hour = hour(starttime), weekday = wday(ymd, label = TRUE)) %>%
  group_by(hour, weekday, ymd) %>%
  summarize(count = n()) %>%
  group_by(weekday, hour) %>%
  summarize(avg = mean(count), sd = sd(count)) %>%
  ggplot() +
  geom_line(aes(x = hour, y = avg), color = "blue") +
  geom_ribbon(aes(x = hour,
                  ymin = (avg - sd),
                  ymax = avg + sd),
                  alpha = 0.2) +
  facet_wrap( ~ weekday)
