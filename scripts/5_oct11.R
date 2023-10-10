rm(list=ls())
gc()

library(ccesMRPprep)
library(tidyverse)
library(labelled)

# https://www.shirokuriwaki.com/ccesMRPprep/articles/acs.html
# https://walker-data.com/tidycensus/articles/basic-usage.html
# https://walker-data.com/census-r/an-introduction-to-tidycensus.html

ces <- read_rds("~/Dropbox (MIT)/Class/Behavior I/data/input/cces/cces_cumulative.rds")
ces_participation <- read_delim("~/Dropbox (MIT)/Class/Behavior I/data/input/cces/ces_participation.tab", delim = "\t") |> 
  select(caseid, year, pid3, weight, ends_with("_recode"))

merged <- ces_participation |> 
  mutate(case_id = as.character(caseid)) |> 
  select(-caseid) |> 
  left_join(ces, by = "case_id") |> 
  mutate(educ = fct_relevel(factor(educ), "4-Year", "Post-Grad", "Some College", "2-Year", "High School Graduate", "No HS"),
         race = fct_relevel(factor(race), "White"),
         # approval_pres = fct_relevel(factor(approval_pres), "Neither Approve nor Disapprove"),
         approval_pres = case_match(
           approval_pres,
           "Strongly Approve" ~ 5,
           "Approve / Somewhat Approve" ~ 4,
           "Neither Approve nor Disapprove" ~ 3,
           "Disapprove / Somewhat Disapprove" ~ 2,
           "Strongly Disapprove" ~ 1,
           .default = NA,
         ),
         democrat = case_match(
           pid3.y,
           "Democrat" ~ 1,
           "Republican" ~ 0,
           .default = NA
         ),
         voted_turnout_self = case_match(
           voted_turnout_self,
           "Yes" ~ 1,
           "No" ~ 0,
           .default = NA
         ))

fit <- fixest::feglm(c(voted_turnout_self, pol_meet_recode, put_sign_recode, work_candidate_recode, donate_candidate_recode) ~ 
                       educ*approval_pres + age + race + gender + democrat + faminc | st + dist,
                     data = merged,
                     family = binomial())

summary(fit)

modelsummary::modelsummary(fit,
                           stars = TRUE,
                           output = "oct11_table.png",
                           coef_omit = "^race|^democrat|^gender|^age|^faminc")