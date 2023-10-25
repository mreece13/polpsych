rm(list=ls())
gc()

library(tidyverse)
library(haven)
library(brms)
library(bayesplot)
library(tidybayes)
library(marginaleffects)

data <- read_sav("data/7_Oct25/afrobarometer_release-dataset_merge-34ctry_r8_en_2023-03-01.sav") |> 
  zap_missing() |> 
  mutate(across(where(is.labelled), as_factor)) |> 
  select(COUNTRY, Q21, Q51A) |> 
  mutate(Q21 = case_match(
    as.character(Q21),
    "Refused" ~ NA,
    "Don't know" ~ NA,
    .default = Q21
  ),
  Q51A = case_match(
    as.character(Q51A), 
    "Refused" ~ NA,
    "Missing" ~ NA,
    "Don’t know or haven't heard enough to say" ~ NA,
    .default = Q51A
  )) |> 
  mutate(Q51A = fct_drop(Q51A),
         Q21 = fct_drop(Q21),
         Q51A = factor(Q51A, ordered = TRUE),
         Q51A_num = as.numeric(Q51A)) |> 
  drop_na(Q21, Q51A) |> 
  left_join(freedom_house, join_by(COUNTRY))

freedom_house <- read_csv("data/7_Oct25/All_data_FIW_2013-2023.csv",
                          col_select = c("COUNTRY" = `Country/Territory`, "Edition", "Total", "Region")
                          ) |> 
  filter(Edition == 2022, Region == "Africa") |>  
  mutate(COUNTRY = case_match(
    COUNTRY,
    "Cote d'Ivoire" ~ "Côte d'Ivoire",
    "The Gambia" ~ "Gambia",
    .default = COUNTRY
  ))

# Q21 = Which of these three statements is closest to your own opinion?
# Statement 1: Democracy is preferable to any other kind of government.
# Statement 2: In some circumstances, a non-democratic government can be preferable.
# Statement 3: For someone like me, it doesn't matter what kind of government we have.

# Q51A
# Do you approve or disapprove of the way the following people have performed their jobs over
# the past 12 months, or haven't you heard enough about them to say: President Keis Saied?

fit <- brm(
  formula = Q51A_num ~ Q21 + (1 | COUNTRY) + (0 + Q21 | COUNTRY),
  data = data,
  chains = 4, 
  cores = 8,
  backend = "cmdstanr"
)

# Extract posterior samples
posterior_samples <- as_draws_df(fit)

# Filter only the terms related to interaction between x and country
interaction_samples <- select(posterior_samples, starts_with("r_COUNTRY"))

# Reshape to long format and assign x and country labels
interaction_long <- interaction_samples |>
  pivot_longer(everything(), names_to = "parameter", values_to = "value") |>
  mutate(
    country = sub("^r_COUNTRY\\[(.*),.*\\]$", "\\1", parameter),
    x = sub("^r_COUNTRY\\[.*,(.*)\\]$", "\\1", parameter)
  ) |> 
  mutate(x = case_match(
    x,
    "Q21STATEMENT3:Forsomeonelikemeitdoesn’tmatterwhatkindofgovernmentwehave." ~ "3 - Neutral",
    "Q21STATEMENT2:InsomecircumstancesanonMdemocraticgovernmentcanbepreferable." ~ "2 - Non-Democracy",
    "Q21STATEMENT1:Democracyispreferabletoanyotherkindofgovernment." ~ "1 - Pro-Democracy",
  )) |> 
  drop_na(x)

ggplot(interaction_long, aes(x = value, y = country)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
  facet_wrap(~ x, scales = "free") +
  labs(
    title = "Interaction Effects of Q21 by Country",
    x = "Effect Size",
    y = "Density"
  ) +
  theme_bw()

freedom_house |> 
  left_join(data, join_by(COUNTRY)) |>  
  drop_na(Q21) |> 
  distinct(COUNTRY, Total.x) |> 
  arrange(desc(Total.x))
