rm(list = ls())
gc()

library(tidyverse)
library(tidymodels)
library(ranger)
set.seed(02139)

sc <- read_delim("data/10_Nov15/Dietrich et al. (2019)/tables2/tables/data/justice_results.tab")

vars <- c(
  "petitioner_vote", "pitch_diff", "law_type", "lc_disposition", "issue", "issue_area",
  "month_argument", "month_decision", "petitioner", "petitioner_dk",
  "respondent", "respondent_dk", "cert_reason", "court_direction_issue_mean",
  "court_direction_mean_10", "lower_court_direction_issue_mean",
  "current_court_direction_issue_mean", "current_court_direction_circuit_origin_std",
  "current_court_direction_issue_std", "justice_direction_mean",
  "justice_direction_mean_10", "justice_direction_mean_z",
  "diff_court_lc_direction", "diff_court_lc_direction_abs",
  "diff_court_lc_direction_abs_z", "diff_justice_court_direction",
  "diff_justice_court_direction_issue", "diff_justice_court_direction_z",
  "justice_agree_mean_10", "justiceName"
)

justice_outcome <- read_csv("data/10_Nov15/Dietrich et al. (2019)/tables2/tables/data/justice_outcome_data.csv") |>
  filter(docket %in% sc$docketId) |>
  mutate(across(where(is.character), ~ replace_na(.x, "-99"))) |>
  left_join(select(sc, docketId, justice, pitch_diff, petitioner_vote, justiceName), join_by(docket == docketId, justice)) |>
  select(vars) |> 
  drop_na(petitioner_vote) |> 
  mutate(petitioner_vote = as.factor(petitioner_vote))

splits <- initial_validation_split(justice_outcome, strata = petitioner_vote)
test <- testing(splits)
train <- training(splits)
valid <- validation(splits)
valid_set <- validation_set(splits)

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |> 
  set_engine("ranger", num.threads = 6) |>  
  set_mode("classification")

rf_recipe <- 
  recipe(petitioner_vote ~ ., data = justice_outcome)

rf_workflow <- 
  workflow() |>  
  add_model(rf_mod) |> 
  add_recipe(rf_recipe)

rf_res <- 
  rf_workflow |>  
  tune_grid(valid_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res |> 
  show_best(metric = "roc_auc")

# the last model
last_rf_mod <- 
  rand_forest(mtry = 1, min_n = 7, trees = 1000) |> 
  set_engine("ranger", num.threads = 6, importance = "impurity") |> 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow |> 
  update_model(last_rf_mod)

# the last fit
last_rf_fit <- 
  last_rf_workflow |> 
  last_fit(splits)

last_rf_fit |> 
  extract_fit_parsnip() |>  
  vip::vip(num_features = 20)


#### 
# Bayesian extension
library(brms)

options(future = FALSE)

fit <- brm(
  formula = petitioner_vote ~ pitch_diff + law_type + lc_disposition + issue + issue_area + month_argument + 
    month_decision + petitioner + petitioner_dk + respondent + respondent_dk + cert_reason + 
    court_direction_issue_mean + court_direction_mean_10 + lower_court_direction_issue_mean + 
    current_court_direction_issue_mean + current_court_direction_circuit_origin_std + 
    current_court_direction_issue_std + justice_direction_mean + justice_direction_mean_10 + 
    justice_direction_mean_z + diff_court_lc_direction + diff_court_lc_direction_abs + 
    diff_court_lc_direction_abs_z + diff_justice_court_direction + diff_justice_court_direction_issue + 
    diff_justice_court_direction_z + justice_agree_mean_10 + (1 | justiceName),
  data = justice_outcome,
  family = bernoulli(),
  cores = 4
)

