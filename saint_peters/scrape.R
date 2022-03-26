library(tidyverse)
library(rvest)
library(xml2)

html <- read_html("https://www.sports-reference.com/cbb/boxscores/2022-03-17-19-kentucky.html")

html_body <- html |> html_node('body')

html |> html_nodes('div.table_container') |> html_text2()
div <- html |> html_elements('div')

html_children <- html_body |> html_children()

test <- html |> html_text()

html_children |> 
  xml2::xml_find_all("//div[contains(@id,'content')]") |> 
  xml2::xml_find_all(".//div[contains(@class,'content_grid')]") |> 
  xml2::xml_find_all(".//div[contains(@id,'all_four-factors')]") |>
  # xml2::xml_find_all(".//div[contains(@class, 'table_container is_setup')]")
  x

html_children |> 
  xml_find_all

rank <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__rank__number')]") %>% 
  rvest::html_text()
artist <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__artist')]") %>% 
  rvest::html_text()
title <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__song')]") %>% 
  rvest::html_text()
  
  