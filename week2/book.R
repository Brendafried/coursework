library(tidyverse)

table2 %>%
  spread(type, count) %>%
  mutate(rate = cases/population * 1e4) %>%
  View()

joined <- inner_join(table4a, table4b, by = c('country'), suffix=c("_cases" ,"_population"))
joined %>%
  mutate(rate_1999 = `1999_cases`/ `1999_population` * 1e4, rate_2000 = `2000_cases` / `2000_population` * 1e4) %>% View()

stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stocks %>%
  spread(year, return) %>%
  gather("year", "return", `2015`: `2016`)


