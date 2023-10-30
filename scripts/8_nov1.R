rm(list=ls())
gc()

library(tidyverse)
library(estimatr)

mega <- read_delim("data/8_Nov1/Rand et al. (2014)/Rand_et_al_2014_Nature_Comm_megaanalysis_data.txt") |> 
  mutate(across(c(fast, intime, round, gender, ed, india, otherNonUS, pgg, mturk), as.character)) |> 
  mutate(ed = if_else(ed == "7", NA, ed))

fit1 <- fixest::feglm(failedcomp ~ age + gender + india + otherNonUS + ed | study,
                      vcov = cluster ~ ip,
                      data = filter(mega, mturk == "1"))

fit2 <- fixest::feglm(failedcomp ~ age + gender + india + otherNonUS + ed | study,
                      vcov = cluster ~ ip,
                      data = filter(mega, mturk == "0"))

modelsummary::modelsummary(list(fit1, fit2), 
                           stars = TRUE,
                           output = "nov1_tbl.png",
                           gof_omit = "BIC|AIC|RMSE")
