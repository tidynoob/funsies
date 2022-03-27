library(tidyverse)
library(lubridate)
library(ggthemr)
library(ggimage)

ggthemr('fresh')

`%!in%` <- Negate(`%in%`)

sp_image <- "images/saint_peter.png"
ky_image <- "images/kentucky.png"
ms_image <- "images/murray_state.png"
pd_image <- "images/purdue.png"

mm_stats <- readxl::read_excel('alt_stats.xlsx')
team_data <- read_csv("team_data.csv")

mm_stats_2 <- mm_stats |> 
  mutate(date = ymd(date)) |> 
  pivot_longer(3:8,"stat",values_to = "mm_rating") |> 
  group_by(school,stat) |> 
  summarise(mm_rating = mean(mm_rating))

teams <- c("Saint Peter's", "Kentucky", "Purdue", "Murray State")

team_data |> 
  # mutate(sp = if_else(school == "Saint Peter's", "Saint Peter's", "Other")) |> 
  filter(school %in% teams) |>
  pivot_longer(4:9,"stat",values_to = "rating") |> 
  select(school,stat,rating) |> 
  filter(stat %in% c('ortg')) |> 
  left_join(mm_stats_2, by = c('school','stat')) |> 
  mutate(image = case_when(
    school == "Saint Peter's" ~ sp_image,
    school == "Kentucky" ~ ky_image,
    school == "Murray State" ~ ms_image,
    school == "Purdue" ~ pd_image)) |> 
  group_by(stat) |> 
  # mutate(std_rating = (rating - mean(rating)) / sd(rating),
  #        mm_std_rating = (mm_rating - mean(rating)) / sd(rating)) |> 
  mutate(ctr_rating = (rating - mean(rating)),
         mm_ctr_rating = (mm_rating - mean(rating))) |>
  select(school,stat,ctr_rating,mm_ctr_rating, rating, mm_rating,image) |>
  # pivot_longer(c('ctr_rating','mm_ctr_rating'),names_to = "period",values_to = "rating") |> 
  # pivot_longer(c('rating','mm_rating'),names_to = "period",values_to = "rating") |>
  ggplot(aes(x = rating, y = mm_rating)) +
  geom_point(size = 2) +
  geom_image(aes(image = image)) +
  geom_abline(slope = 1,intercept = 0, alpha = 0.8, linetype = 'dashed') +
  xlim(c(90,120)) +
  ylim(c(90,120))
  # facet_wrap(vars(period)) +
  # scale_alpha_manual(values = c(0.8, .25))