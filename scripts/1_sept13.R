rm(list=ls())
gc()

library(tidyverse)
library(sf)

data <- readxl::read_excel("data/1_Sep13/Tybur_data.xlsx")
t1 <- read_csv("data/1_Sep13/table1.csv", col_select = c(country, n))
geo <- read_sf("data/1_Sep13/countries.geojson") |> 
  mutate(ADMIN = case_match(
    ADMIN, 
    "Republic of Serbia" ~ "Serbia",
    "United States of America" ~ "United States",
    "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
    "South Korea" ~ "Republic of Korea",
    .default = ADMIN
  )) |> 
  select(ADMIN, geometry)

countries <- data |> 
  count(nation) |> 
  left_join(t1, by = "n") |> 
  mutate(country = str_squish(str_remove(country, "\\(.*?\\)")))

p1 <- data |> 
  left_join(countries, by = "nation") |> 
  select(-n, -sd01:-trad6r, -nation) |> 
  summarize(HistPath = mean(HistPath), .by = country) |> 
  right_join(geo, join_by(country == ADMIN)) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = HistPath)) +
  theme_minimal()

p2 <- data |> 
  left_join(countries, by = "nation") |> 
  select(-n, -sd01:-trad6r, -nation) |> 
  summarize(ZoonoticParasitePrevalence = mean(ZoonoticParasitePrevalence), .by = country) |> 
  right_join(geo, join_by(country == ADMIN)) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = ZoonoticParasitePrevalence)) +
  theme_minimal()

p3 <- data |> 
  left_join(countries, by = "nation") |> 
  select(-n, -sd01:-trad6r, -nation) |> 
  summarize(NonzoonoticParasitePrevalence = mean(NonzoonoticParasitePrevalence), .by = country) |> 
  right_join(geo, join_by(country == ADMIN)) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = NonzoonoticParasitePrevalence)) +
  theme_minimal()

p4 <- data |> 
  left_join(countries, by = "nation") |> 
  select(-n, -sd01:-trad6r, -nation) |> 
  summarize(Parasite_PCA = mean(Parasite_PCA), .by = country) |> 
  right_join(geo, join_by(country == ADMIN)) |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = Parasite_PCA)) +
  theme_minimal()

ggsave("data/1_Sep13/p1.jpg", p1, width = 16, height = 9, units = "in")
ggsave("data/1_Sep13/p2.jpg", p2,  width = 16, height = 9, units = "in")
ggsave("data/1_Sep13/p3.jpg", p3, width = 16, height = 9, units = "in")
ggsave("data/1_Sep13/p4.jpg", p4, width = 16, height = 9, units = "in")