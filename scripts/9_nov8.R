rm(list = ls())
gc()

library(tidyverse)
library(modelsummary)
library(fixest)
library(lavaan)
library(blavaan)
library(future)
library(tidybayes)

theme_set(theme_bw())

data <- read_csv("data/9_Nov8/bullock_data_experiment1.csv",
  name_repair = janitor::make_clean_names
) |>
  mutate(
    initial_like = factor(initial_like, levels = c(
      "approve strongly", "approve somewhat",
      "approve slightly", "neither approve nor disapprove",
      "disapprove slightly", "disapprove somewhat",
      "disapprove strongly"
    ), ordered = TRUE),
    initial_like = fct_rev(initial_like),
    initial_like_num = as.numeric(initial_like),
    ncog1 = factor(ncog1, ordered = TRUE, levels = c("greatly prefer simple problems", "somewhat prefer simple problem", "slightly prefer simple problem", "no preference", "slightly prefer complex proble", "somewhat prefer complex proble", "greatly prefer complex problem")),
    ncog3 = factor(ncog3, ordered = TRUE, levels = c("none", "a little", "a moderate amount", "a lot", "a great deal")),
    ncog4 = factor(ncog4, ordered = TRUE, levels = c("greatly prefer small, daily pr", "somewhat prefer small, daily p", "slightly prefer small, daily p", "no preference", "slightly prefer big, long-term", "somewhat prefer big, long-term")),
    ncog5 = factor(ncog5, ordered = TRUE, levels = c("dislike a lot", "dislike somewhat", "dislike a little", "neither like nor dislike", "like a little", "like somewhat", "like a lot")),
    ncog6 = factor(ncog6, ordered = TRUE, levels = c("dislike a lot", "dislike somewhat", "dislike a little", "neither like nor dislike", "like a little", "like somewhat", "like a lot")),
    ncog7 = factor(ncog7, ordered = TRUE, levels = c("much more relieved than satisf", "somewhat more relieved than sa", "slightly more relieved than sa", "relief and satisfaction to the", "slightly more satisfied than r", "somewhat more satisfied than r", "much more satisfied than relie")),
    lib_policy = if_else(str_detect(article_type, "^lib"), 1, 0),
    demsupp = as.numeric(demsupp),
    demopp = as.numeric(demopp)
  ) |> 
  mutate(across(where(is.ordered), as.numeric, .names = "{.col}_num")) |> 
  mutate(across(ends_with("_num"), ~ (.x - min(.x, na.rm = TRUE))/(max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)))) |> 
  select(initial_like, initial_like_num, demsupp, demopp, lib_policy, starts_with("ncog"), pid_pre)

cfa <- cfa(
  model = "factor1 =~ ncog1 + ncog3 + ncog4 + ncog5 + ncog6 + ncog7",
  data = select(data, starts_with("ncog")),
  ordered = c("ncog1", "ncog3", "ncog4", "ncog5", "ncog6", "ncog7"),
  std.lv = TRUE
)

# plot factor loadings
filter(parameterEstimates(cfa), op == "=~") |>           # factor loadings
  ggplot(aes(x = rhs, y = est, ymin = ci.lower, ymax = ci.upper)) +
  geom_pointrange() +
  labs(
    title = "Factor Loadings",
    x = "Question",
    y = "Estimate"
  ) +
  coord_flip()

# plot thresholds for each variables
filter(parameterEstimates(cfa), op == "|") |> # get just thresholds
  mutate(
    t = factor(as.integer(str_remove(rhs, "t")))
  ) |>
  ggplot() +
  aes(
    x = lhs,
    y = est,
    ymin = ci.lower,
    ymax = ci.upper,
    color = t,
    label = t
  ) +
  geom_linerange(alpha = .5) +
  geom_label() +
  labs(
    title = "Thresholds",
    x = "Question",
    y = "Estimate",
    color = "Threshold"
  ) +
  guides(color = "none", label = "none") +
  coord_flip()

data_blavaan <- semTools::indProd(data,
  var1 = c("demsupp", "demopp", "lib_policy"),
  var2 = c("ncog1_num", "ncog3_num", "ncog4_num", "ncog5_num", "ncog6_num", "ncog7_num"),
  match = FALSE
) |> 
  filter(pid_pre == "Rep", !row_number() %in% c(414, 2397))

model <- "need_cog =~ ncog1_num + ncog3_num + ncog4_num + ncog5_num + ncog6_num + ncog7_num"

plan(multisession, workers = 4)

bsem <- bcfa(model,
            data = data_blavaan,
            save.lvs = TRUE,
            std.lv = TRUE,
            mcmcfile = TRUE)

plan(sequential)

fitted <- blavInspect(bsem, 'lvs') |> 
  tidy_draws() |> 
  pivot_longer(cols = starts_with("eta"), values_to = "cognition_factor") |> 
  mutate(demopp = rep(data_blavaan$demopp, times = 1000),
         demsupp = rep(data_blavaan$demsupp, times = 1000),
         lib_policy = rep(data_blavaan$lib_policy, times = 1000),
         initial_like = rep(data_blavaan$initial_like, times = 1000),
         .by = ".chain") |> 
  mutate(initial_like_num = as.numeric(initial_like))

fit_dem <- feols(initial_like_num ~ cognition_factor*(demsupp + demopp + lib_policy),
                           data = fitted,
                           cluster = ~ name)

fit_rep <- feols(initial_like_num ~ cognition_factor*(demsupp + demopp + lib_policy),
                 data = fitted,
                 cluster = ~ name)

modelplot(list("Democrat" = fit_dem, "Republican" = fit_rep)) +
  scale_color_manual(values = c("blue", "red")) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Replication of Table 2")
