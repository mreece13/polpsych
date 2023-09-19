rm(list=ls())
gc()

library(tidyverse)
library(haven)
library(estimatr)

d_country <- read_dta("data/2_Sep20/CountryData.dta")
d_ea_contiguous <- read_dta("data/2_Sep20/EA_contiguous.dta")
d_ea_sccs <- read_dta("data/2_Sep20/EAShort.dta")
d_gtrends <- read_dta("data/2_Sep20/GTrends.dta")

est <- fixest::feols(c(locality, 1-nuclear, 1-bilateral, clan, kinship_score) ~ s_malariaindex + small_scale | ..("cont_.*?$") + ln_time_obs_ea,
                     cluster = ~cluster,
                     data = d_ea_sccs)

df <- data.frame("FE (Continent and Years)", t(rep("Yes", 5)))

modelsummary::modelsummary(est, 
                           coef_rename = c(s_malariaindex = "Malaria Index (Std.)",
                                           small_scale = "Dependence on hunting and gathering"),
                           gof_map = c("nobs", "vcov.type"),
                           add_rows = df,
                           stars = TRUE,
                           output = "data/2_Sep20/table3.html",
                           title = "Table III, Column 3, with separate DVs")
