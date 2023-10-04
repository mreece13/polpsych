rm(list=ls())
gc()

library(tidyverse)
library(brms)
library(haven)

m3_formula <- can_trust ~ mo(birth_weight) + sex + age + 
  mo(total_household_income_past) + mo(highest_edu)

d <- readRDS("data/4_Oct4/petersen_data_study3-websample.rds") |> 
  rename(birth_weight_exact = Q18_n1) |> 
  mutate(across(where(is.labelled), ~ as_factor(.x, ordered = TRUE))) |> 
  mutate(across(where(is.factor), ~ case_when(
    .x %in% c("Have no idea at all", "Do not know", "Not relevant", "Other") ~ NA,
    .default = .x
  ))) |> 
  mutate(treatment_type = case_match(
    treatment_type,
    "block 2" ~ "0",
    "block 1" ~ "1",
  ),
    sex = as.character(sex)) |> 
  mutate(across(where(is.factor), fct_drop))

brm_cat <- brm(
  formula = m3_formula,
  data = d,
  family = cumulative(probit),
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  refresh = 0,
  normalize = FALSE,
  control = list(adapt_delta = 0.95)
)

summary(brm_cat)

plot(conditional_effects(brm_cat, effects = "birth_weight", categorical = TRUE, plot = FALSE))[[1]] + 
  scale_color_grey() +
  scale_fill_grey() +
  theme_bw()
