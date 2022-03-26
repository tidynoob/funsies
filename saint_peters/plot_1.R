library(tidyverse)
library(lubridate)

stats <- readxl::read_excel('stats.xlsx') |> 
  rename(ft_per_fga = `ft/fga`) |> 
  mutate(date = ymd(date),
         pre_mm = if_else(date <= '2022-03-17','Pre March Madness', 'March Madness'))

plot_stat <- function(stat) {
  
  stats |>
    ggplot(aes(x = date, y = {{stat}})) +
    geom_line() +
    theme_classic()
}

plot_stat(ortg)

stats |> 
  group_by(pre_mm) |> 
  summarise(across(.cols = where(is.numeric),mean))

stats |> 
  ggplot(aes(x = date, y = ortg, color = pre_mm)) +
  geom_point() +
  theme_classic()
