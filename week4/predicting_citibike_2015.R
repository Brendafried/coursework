library(dplyr)
library(readr)
library(modelr)
library(lubridate)

trips_2015 <- all_trips
trips_2015 <- trips_2015 %>% group_by(ymd) %>%
  summarize(num_trips = n()) %>%
  ungroup()
weather <- read_csv("weather_2015.csv")
weather <- dplyr::select(weather, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)

names(weather) <- tolower(names(weather))

holidays <- read_csv('https://gist.githubusercontent.com/shivaas/4758439/raw/b0d3ddec380af69930d0d67a9e0519c047047ff8/US%2520Bank%2520holidays', col_names=c('n', 'date', 'holiday')) %>%
    mutate(date = as.Date(date))

weather <- left_join(weather, holidays, by = c("date" = "date"))

weather <- weather %>%
  mutate(isholiday = (!(is.na(holiday))), wday = wday(date)) %>%
  mutate(isweekend = (wday == 1) | (wday == 7) )


View(trips_2015)
trips_2015 <- left_join(trips_2015, weather, by = c('ymd' = 'date'))

trips_2015 <- trips_2015 %>% 
  mutate(isholiday = (!(is.na(holiday))), wday = wday(ymd), tmin = tmin/10) %>%
  mutate(isweekend = (wday == 1) | (wday == 7) ) 

View(trips_2015)



## 
bestModel <- load("bestModel.RData")
trips_2015 <- add_predictions(trips_2015, model, var = "pred")
View(trips_2015)


ggplot() +
  geom_point(data = trips_2015, aes(x = ymd, y = num_trips)) +
  geom_line(data = trips_2015, aes(x = ymd, y = pred))

ggplot() +
  geom_point(data= trips_2015, aes(x = pred, y =num_trips)) +
  geom_abline(slope = 1, intercept = 0)

rmse(model, validation_data)
rmse(model, trips_2015)
rsquare(model, validation_data)
rsquare(model, trips_2015)
