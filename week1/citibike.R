library(tidyverse)
library(lubridate)

########################################
# READ AND TRANSFORM THE DATA
########################################

# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
# trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown","Male","Female")))

View(trips)
########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
nrow(trips)
  
# find the earliest and latest birth years (see help for max and min to deal with NAs)
trips %>%
  mutate(birth_year = as.numeric(birth_year)) %>%
  summarize(max = max(birth_year, na.rm = TRUE), min = min(birth_year, na.rm = TRUE))

# use filter and grepl to find all trips that either start or end on broadway
trips %>%
  filter(grepl('.*Broadway.*', start_station_name) | grepl('.*Broadway.*', end_station_name))%>%
  View
# do the same, but find all trips that both start and end on broadway
trips %>%
  filter(grepl('.*Broadway.*', start_station_name) , grepl('.*Broadway.*', end_station_name))%>%
  View
# find all unique station names
trips %>%
  distinct(start_station_name) %>%
  arrange(start_station_name) %>%
  View
# count the number of trips by gender, the average trip time by gender, and the standard deviation in trip time by gender
# do this all at once, by using summarize() with multiple arguments

trips %>%
  group_by(gender) %>%
  summarize(count = n(), mean_duration = mean(tripduration)/60, sd_duration = sd(tripduration)/60)
  ungroup

# find the 10 most frequent station-to-station trips
  
trips %>%
  group_by(start_station_name, end_station_name) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  View
  
# find the top 3 end stations for trips starting from each start station
trips %>%
  group_by(start_station_name, end_station_name) %>%
  summarize(count = n()) %>% 
  group_by(start_station_name) %>%
  arrange(start_station_name, desc(count)) %>% 
  top_n(n = 3, wt = count) %>%
  View
# find the top 3 most common station-to-station trips by gender
trips %>%
  group_by(start_station_name, end_station_name, gender) %>%
  summarize(count = n()) %>%
  arrange(gender, desc(count)) %>%
  group_by(gender) %>%
  top_n( n = 3, wt = count) %>%
  View
  
# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)
trips %>%
  mutate(ymd = as.Date(starttime)) %>%
  group_by(ymd) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(1) %>%
  View
# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?
trips %>%
  mutate(day = as.Date(starttime), hour = hour(starttime)) %>%
  group_by(day, hour) %>%
  summarize(count = n()) %>%
  group_by(hour) %>%
  summarize(avg = mean(count)) %>%
  arrange(desc(avg)) %>%
  View
