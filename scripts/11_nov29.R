rm(list = ls())
gc()

library(tidyverse)
library(fixest)
library(marginaleffects)

int_midpoint <- function(interval) {
  int_start(interval) + (int_end(interval) - int_start(interval))/2
}

data <- haven::read_dta("data/11_Nov29/bk_data_survey.dta") |> 
  rename(Q147_would_get_covid_vaccine_yes_binary   = Q147_would_get_covid_vaccine_yb,
         Q146_flu_vaccine_2020_yes_binary          = Q146_flu_vaccine_2020_yes_binar,
         zip_hh_inc_2018_median_range_middle       = zip_hh_inc_2018_median_range_mi, 
         zip_educ_2018_high_school_or_less         = zip_educ_2018_high_school_or_le, 
         demo_household_income_range_middle        = demo_household_income_range_mid,
         demo_race_ethnicity_binned_hispanic       = demo_race_ethnicity_binned_hisp,
         demo_household_income_top_quartile_binary = demo_household_income_top_quart,
         zip_hh_inc_2018_prop_top_quartile         = zip_hh_inc_2018_prop_top_quarti,
         Q10x_plan_tried_vaccinated_binary         = Q10x_plan_tried_vaccinated_bina
  ) |> 
  mutate(time = case_match(
    wave,
    1 ~ interval(ymd("2020-5-11"), ymd("2020-5-24")) |> int_midpoint(),
    2 ~ interval(ymd("2020-7-9"), ymd("2020-7-22")) |> int_midpoint(),
    3 ~ interval(ymd("2020-10-1"), ymd("2020-10-17")) |> int_midpoint(),
    4 ~ interval(ymd("2020-12-4"), ymd("2020-12-16")) |> int_midpoint(),
    5 ~ interval(ymd("2021-3-25"), ymd("2021-4-13")) |> int_midpoint(),
    6 ~ interval(ymd("2021-6-17"), ymd("2021-7-6")) |> int_midpoint(),
    7 ~ interval(ymd("2021-9-3"), ymd("2021-10-4")) |> int_midpoint(),
  ))

fit <- feglm(mask_policy_active ~ prop_r + zip_white + zip_educ_2018_college_and_above + 
               zip_hh_inc_2018_median_range_middle + zip_pop_density_2018_over_2010 + log(deaths_14_days_county_pc_1k + 0.1) | time,
             data = data,
             cluster = ~ demo_zip_scrambled,
             family = binomial())

ddf_display_names_regressions <- tribble(
  ~packed_rows, ~term, ~display_name,
  "Primary Variables", "prop_r", "Share GOP",
  "Primary Variables", "republican", "Republican",
  "Primary Variables", "republican:prop_r", "Republican * Share GOP",
  "Primary Variables", "republican:zip_white", "Republican * Share White ('18 Zip)",
  "Primary Variables", "republican:zip_educ_2018_college_and_above", "Republican * Share College and Above ('18 ZIP)",
  "Primary Variables", "republican:zip_hh_inc_2018_median_range_middle","Republican * Median HH Income, Thousands ('18 ZIP)",
  # # WHITE INTERACTIONS VERSION
  "Primary Variables", "white",                                     "White",
  "Primary Variables", "white:zip_white",                           "White * Share White ('18 ZIP)",
  "Primary Variables", "white:prop_r",                              "White * Share GOP",
  "Primary Variables", "white:zip_educ_2018_college_and_above",     "White * Share College and Above ('18 ZIP)",
  "Primary Variables", "white:zip_hh_inc_2018_median_range_middle", "White * Median HH Income, Thousands ('18 ZIP)",
  # # COLLEGE AND ABOVE INTERACTIONS VERSION
  "Primary Variables", "college_and_above",                                     "College and Above",
  "Primary Variables", "college_and_above:zip_educ_2018_college_and_above",     "College and Above * Share College and Above ('18 ZIP)",
  "Primary Variables", "college_and_above:prop_r",                              "College and Above * Share GOP",
  "Primary Variables", "college_and_above:zip_white",                           "College and Above * Share White ('18 ZIP)",
  "Primary Variables", "college_and_above:zip_hh_inc_2018_median_range_middle", "College and Above * Median HH Income, Thousands ('18 ZIP)",
  # HOUSEHOLD INCOME INTERACTIONS VERSION
  "Primary Variables", "demo_household_income_top_quartile_binary",                                     "High Income",
  "Primary Variables", "demo_household_income_top_quartile_binary:zip_hh_inc_2018_prop_top_quartile",   "High Income * Share High Income ('18 ZIP)",
  "Primary Variables", "demo_household_income_top_quartile_binary:prop_r",                              "High Income * Share GOP",
  "Primary Variables", "demo_household_income_top_quartile_binary:zip_white",                           "High Income * Share White ('18 ZIP)",
  "Primary Variables", "demo_household_income_top_quartile_binary:zip_educ_2018_college_and_above",     "High Income * Share College and Above ('18 ZIP)",
  # MASK POLICY INTERACTIONS
  "Primary Variables", "mask_policy_active",                   "Mask Policy",
  "Primary Variables", "republican:mask_policy_active",        "Republican * Mask Policy",
  "Primary Variables", "prop_r:mask_policy_active",            "Share GOP * Mask Policy",
  "Primary Variables", "republican:prop_r:mask_policy_active", "Republican * Share GOP * Mask Policy",
  # LOCAL COVID-19 DEATHS
  "COVID Deaths", "log(deaths_14_days_county_pc_1k + 0.1)", "Log(Deaths per 1K People)",
  # INDIVIDUAL CONTROLS
  "Individual Controls", "factor(demo_gender)2", "Female", 
  "Individual Controls", "demo_household_income_range_middle", "Household Income",
  "Individual Controls", "demo_household_income_missing", "Household Income Missing",
  "Individual Controls", "demo_age", "Age",
  "Individual Controls", "as_factor(demo_race_ethnicity_binned_hispanic)White", "White",
  "Individual Controls", "as_factor(demo_race_ethnicity_binned_hispanic)Black, or African American", "Black", 
  "Individual Controls", "as_factor(demo_race_ethnicity_binned_hispanic)Hispanic", "Hispanic",
  "Individual Controls", "as_factor(demo_race_ethnicity_binned_hispanic)Asian or Pacific Islander", "Asian or Pacific Islander",
  "Individual Controls", "as_factor(demo_race_ethnicity_binned_hispanic)Some other race", "Some other race",
  "Individual Controls", "as_factor(demo_education_binned)Some College", "Some College",
  "Individual Controls", "as_factor(demo_education_binned)College and Above", "College and Above",
  "Individual Controls", "Q7_health_dx_sum", "Health Diagnoses (0-6)",
  "ZIP Code Controls", "zip_educ_2018_college_and_above", "Share College and Above ('18 ZIP)",
  "ZIP Code Controls", "zip_hh_inc_2018_median_range_middle", "Median HH Income ('18 ZIP)", 
  "ZIP Code Controls", "zip_pop_density_2018_over_2010", "Pop. Density ('18 ZIP)",
  "ZIP Code Controls", "zip_white", "Share White ('18 ZIP)",
  "Ideology", "demo_ideology_fLibertarian",       "Libertarian",
  "Ideology", "demo_ideology_fVery Conservative", "Very Conservative",
  "Ideology", "demo_ideology_fConservative",      "Conservative",
  "Ideology", "demo_ideology_fLiberal",           "Liberal",
  "Ideology", "demo_ideology_fVery Liberal",      "Very Liberal",
  "Ideology", "demo_ideology_fI don't know",      "I don't know",
  "Time Fixed Effects", "as.factor(wave)2", "Wave 2",
  "Time Fixed Effects", "as.factor(wave)3", "Wave 3",
  "Time Fixed Effects", "as.factor(wave)4", "Wave 4",
  "Time Fixed Effects", "as.factor(wave)5", "Wave 5",
  "Time Fixed Effects", "as.factor(wave)6", "Wave 6",
  "Time Fixed Effects", "as.factor(wave)7", "Wave 7",
) |> 
  select(-packed_rows) |> 
  deframe()

modelsummary::modelsummary(fit,
                           output = "nov29_tbl.jpg",
                           stars = TRUE,
                           gof_map = c("nobs", "adj.r.squared", "vcov.type", "FE: wave"),
                           coef_rename = df_display_names_regressions)

modelsummary::modelplot(fit, coef_rename = df_display_names_regressions) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Predicting Mask Policies")

ggsave("nov29_plot.jpg", width = 8, height = 8, units = "in")
