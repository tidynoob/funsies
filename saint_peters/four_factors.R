library(tidyverse)
library(lubridate)
library(ggthemr)

ggthemr('fresh')

`%!in%` <- Negate(`%in%`)

mm_stats <- readxl::read_excel('alt_stats.xlsx')
team_data <- read_csv("team_data.csv")

mm_stats_2 <- mm_stats |> 
  mutate(date = ymd(date)) |> 
  pivot_longer(3:8,"stat",values_to = "mm_rating") |> 
  group_by(school,stat) |> 
  summarise(mm_rating = mean(mm_rating))

teams <- c("Saint Peter's", "Kentucky", "Purdue", "Murray State")

ff_avg <- team_data |> 
  filter(school %in% teams) |>
  pivot_longer(4:9,"stat",values_to = "rating") |> 
  select(school,stat,rating) |> 
  group_by(stat) |> 
  summarise(avg_rating = mean(rating))

team_data |> 
  filter(school %in% teams) |> 
  pivot_longer(4:9,"stat",values_to = "rating") |> 
  select(school,stat,rating) |> 
  left_join(ff_avg, by = c('stat')) |> 
  mutate(diff_from_avg = rating - avg_rating)

team_data |> 
  # mutate(sp = if_else(school == "Saint Peter's", "Saint Peter's", "Other")) |> 
  filter(school %in% teams) |>
  pivot_longer(4:9,"stat",values_to = "rating") |> 
  select(school,stat,rating) |> 
  filter(stat %!in% c('ortg','pace')) |> 
  left_join(mm_stats_2, by = c('school','stat')) |> 
  group_by(stat) |> 
  mutate(std_rating = (rating - mean(rating)) / sd(rating),
         mm_std_rating = (mm_rating - mean(rating)) / sd(rating)) |> 
  select(school,stat,std_rating,mm_std_rating) |> 
  pivot_longer(c('std_rating','mm_std_rating'),names_to = "period",values_to = "rating") |> 
  ggplot(aes(x = school, y = rating, fill = school, alpha = period)) +
  geom_col(color = 'black', position ='dodge', width = 0.6) +
  facet_wrap(vars(stat)) +
  scale_alpha_manual(values = c(0.8, .25))
  