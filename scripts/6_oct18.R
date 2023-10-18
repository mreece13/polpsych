rm(list=ls())
gc()

library(tidyverse)
library(modelsummary)
library(brms)

data <- read_csv("data/6_Oct18/lilla_data_lucid.csv") |> 
  filter(treat_vignette2 != 2, respondent_pid %in% c("Dem", "Rep")) |> 
  select(treat_vignette2, vignette_warmth_1, infer_agree2, gender, age) |> 
  mutate(gender = ifelse(gender == 1, "Male", "Female"),
         infer_agree2 = case_match(
           infer_agree2,
           "Always agree" ~ 1, 
           "Usually agree" ~ 3/4, 
           "Sometimes agree, sometimes disagree" ~ 2/4,
           "Usually disagree" ~ 1/4, 
           "Always disagree" ~ 0,
           .default = NA
         ),
         infer_agree2 = factor(infer_agree2, 
                               levels = c("0", "0.25", "0.5", "0.75", "1"),
                               labels = c("Always disagree", "Usually disagree", "Sometimes agree, sometimes disagree", "Usually agree", "Always agree"),
                               ordered = TRUE))

brm_cat <- brm(
  formula = infer_agree2 ~ treat_vignette2 + gender + age,
  data = data,
  family = cumulative(logit),
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  refresh = 0,
  normalize = FALSE
)

plot(conditional_effects(brm_cat, effects = "treat_vignette2", categorical = TRUE, plot = FALSE))[[1]] + 
  scale_color_brewer(type = "div", palette = "RdYlGn") +
  theme_bw() + 
  labs(x = "", color = "", fill = "") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  scale_x_discrete(labels = c("Weak", "Strong + Moderate Inactive", "Strong + Extreme Active", "Strong", "Base")) +
  coord_flip()

lm_ft <- lm(vignette_warmth_1 ~ treat_vignette2 + gender + age, data = data)
lm_ag <- lm(as.numeric(infer_agree2) ~ treat_vignette2 + gender + age, data = data)

modelplot(list("Warmth" = lm_ft, "Agree" = lm_ag), 
          coef_map = c(
            "genderMale" = "Male",
            "age" = "Age",
            "treat_vignette2pid_strong_mod_inactive" = "Strong + Moderate Inactive",
            "treat_vignette2pid_strong_extreme_active" = "Strong + Extreme Active",
            "treat_vignette2pid_strong" = "Strong",
            "treat_vignette2pid_weak" = "Weak"
          ),
          vcov = "HC2",
          standardize = "basic") +
  geom_vline(xintercept = 0) +
  labs(x = "ATE relative to generic out-partisan", y = "Treatment") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  scale_color_discrete(type = c("grey", "black"))
  