library(tidyverse)
library(lubridate)
library(ggthemr)
library(ggimage)

ggthemr('fresh')

`%!in%` <- Negate(`%in%`)

mm_stats <- readxl::read_excel('off_rating.xlsx')
team_data <- read_csv("team_data.csv")

mm_stats_2 <- mm_stats |> 
  mutate(date = ymd(date)) |> 
  group_by(school,image) |> 
  summarise(mm_rating = mean(ortg))

teams <- c("Saint Peter's", "North Carolina", "Kansas", "Miami (FL)", "Houston", "Villanova", "Arkansas", "Duke")

team_data |> 
  filter(school %in% teams) |>
  pivot_longer(4:9,"stat",values_to = "rating") |> 
  select(school,stat,rating) |> 
  filter(stat %in% c('ortg')) |> 
  left_join(mm_stats_2, by = c('school')) |> 
  ggplot(aes(x = rating, y = mm_rating)) +
  geom_abline(slope = 1,intercept = 0, alpha = .9, linetype = 'dashed', size = 1.05) +
  geom_image(aes(image = image), size = 0.065) +
  annotate("segment", x = 121, xend = 120, y = 110, yend = 120,
           arrow = arrow(), size = 1, colour = "grey20") +
  annotate("label", x = 120, y = 110, vjust = 1, size = 3, hjust = 0.5,
           label = "Teams above this line are overperforming\ncompared to their season average while\nteams below are underpeforming") +
  xlim(c(90,125)) +
  ylim(c(90,125)) +
  labs(
    title = "Have the Elite Eight Performed Better in March Madness?",
    subtitle = "Elite Eight March Madness Offensive Ratings vs. Season Average",
    x = "Season Offensive Rating",
    y = "March Madness Offensive Rating",
    caption = "Data: basketball-reference.com | Chart: Mitch Griffin"
  ) +
  theme(
    plot.caption = element_text(size = 6, face = 'italic', color = 'grey30'),
  )

ggsave("elite_eight_offensive_ratings.png")
  