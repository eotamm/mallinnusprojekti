#### Aineistojen alustus ####

library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(splines)

#set filepath
#dir.path <- file.path("")

pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

library(lme4)
library(lmerTest)

# Uudet steps-muuttujat

pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(steps_lag1 = lag(steps, n = 1)) %>%  # previous day's steps
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(steps_lag1 = lag(steps, n = 1)) %>%  # previous day's steps
  ungroup()

library(zoo)
pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    steps_mean3 = rollapply(
      steps,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    steps_mean3 = rollapply(
      steps,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

pregnancy$steps = scale(pregnancy$steps)
postpartum$steps = scale(postpartum$steps)
pregnancy$steps_lag1 = scale(pregnancy$steps_lag1)
postpartum$steps_lag1 = scale(postpartum$steps_lag1)
pregnancy$steps_mean3 = scale(pregnancy$steps_mean3)
postpartum$steps_mean3 = scale(postpartum$steps_mean3)

# Uudet average_met-muuttujat

pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(average_met_lag1 = lag(average_met, n = 1)) %>%  # previous day's average_met
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(average_met_lag1 = lag(average_met, n = 1)) %>%  # previous day's average_met
  ungroup()

library(zoo)
pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    average_met_mean3 = rollapply(
      average_met,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    average_met_mean3 = rollapply(
      average_met,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

pregnancy$average_met = scale(pregnancy$average_met)
postpartum$average_met = scale(postpartum$average_met)
pregnancy$average_met_lag1 = scale(pregnancy$average_met_lag1)
postpartum$average_met_lag1 = scale(postpartum$average_met_lag1)
pregnancy$average_met_mean3 = scale(pregnancy$average_met_mean3)
postpartum$average_met_mean3 = scale(postpartum$average_met_mean3)

# Uudet met_min_high-muuttujat

pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_high_lag1 = lag(met_min_high, n = 1)) %>%  # previous day's average_met
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_high_lag1 = lag(met_min_high, n = 1)) %>%  # previous day's average_met
  ungroup()

library(zoo)
pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_high_mean3 = rollapply(
      met_min_high,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_high_mean3 = rollapply(
      met_min_high,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

# Uudet met_min_medium-muuttujat

pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_medium_lag1 = lag(met_min_medium, n = 1)) %>%  # previous day's average_met
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_medium_lag1 = lag(met_min_medium, n = 1)) %>%  # previous day's average_met
  ungroup()

library(zoo)
pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_medium_mean3 = rollapply(
      met_min_medium,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_medium_mean3 = rollapply(
      met_min_medium,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

# Uudet met_min_low-muuttujat

pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_low_lag1 = lag(met_min_low, n = 1)) %>%  # previous day's average_met
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_low_lag1 = lag(met_min_low, n = 1)) %>%  # previous day's average_met
  ungroup()

library(zoo)
pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_low_mean3 = rollapply(
      met_min_low,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_low_mean3 = rollapply(
      met_min_low,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

# Uudet met_min_inactive-muuttujat

pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_inactive_lag1 = lag(met_min_inactive, n = 1)) %>%  # previous day's average_met
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%  # ensure correct order
  group_by(id) %>%               # for each person
  mutate(met_min_inactive_lag1 = lag(met_min_inactive, n = 1)) %>%  # previous day's average_met
  ungroup()

library(zoo)
pregnancy <- pregnancy %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_inactive_mean3 = rollapply(
      met_min_inactive,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

postpartum <- postpartum %>%
  arrange(id, summary_date) %>%       # make sure data are in order
  group_by(id) %>%
  mutate(
    met_min_inactive_mean3 = rollapply(
      met_min_inactive,
      width = 3,                     # 3 days: today + 2 days before
      FUN = mean,
      align = "right",               # so each day gets mean of itself and 2 before
      fill = NA                      # first two days will be NA
    )
  ) %>%
  ungroup()

pregnancy$met_min_high = scale(pregnancy$met_min_high)
postpartum$met_min_high = scale(postpartum$met_min_high)
pregnancy$met_min_high_lag1 = scale(pregnancy$met_min_high_lag1)
postpartum$met_min_high_lag1 = scale(postpartum$met_min_high_lag1)
pregnancy$met_min_high_mean3 = scale(pregnancy$met_min_high_mean3)
postpartum$met_min_high_mean3 = scale(postpartum$met_min_high_mean3)

pregnancy$met_min_medium = scale(pregnancy$met_min_medium)
postpartum$met_min_medium = scale(postpartum$met_min_medium)
pregnancy$met_min_medium_lag1 = scale(pregnancy$met_min_medium_lag1)
postpartum$met_min_medium_lag1 = scale(postpartum$met_min_medium_lag1)
pregnancy$met_min_medium_mean3 = scale(pregnancy$met_min_medium_mean3)
postpartum$met_min_medium_mean3 = scale(postpartum$met_min_medium_mean3)

pregnancy$met_min_low = scale(pregnancy$met_min_low)
postpartum$met_min_low = scale(postpartum$met_min_low)
pregnancy$met_min_low_lag1 = scale(pregnancy$met_min_low_lag1)
postpartum$met_min_low_lag1 = scale(postpartum$met_min_low_lag1)
pregnancy$met_min_low_mean3 = scale(pregnancy$met_min_low_mean3)
postpartum$met_min_low_mean3 = scale(postpartum$met_min_low_mean3)

pregnancy$met_min_inactive = scale(pregnancy$met_min_inactive)
postpartum$met_min_inactive = scale(postpartum$met_min_inactive)
pregnancy$met_min_inactive_lag1 = scale(pregnancy$met_min_inactive_lag1)
postpartum$met_min_inactive_lag1 = scale(postpartum$met_min_inactive_lag1)
pregnancy$met_min_inactive_mean3 = scale(pregnancy$met_min_inactive_mean3)
postpartum$met_min_inactive_mean3 = scale(postpartum$met_min_inactive_mean3)

pregnancy$id = as.factor(pregnancy$id)
postpartum$id = as.factor(postpartum$id)

pregnancy_sync = pregnancy %>%
filter(!is.na(steps) & !is.na(steps_lag1) & !is.na(steps_mean3) &
         !is.na(average_met) & !is.na(average_met_lag1) & !is.na(average_met_mean3) &
         !is.na(met_min_high) & !is.na(met_min_high_lag1) & !is.na(met_min_high_mean3) &
         !is.na(met_min_medium) & !is.na(met_min_medium_lag1) & !is.na(met_min_medium_mean3) &
         !is.na(met_min_low) & !is.na(met_min_low_lag1) & !is.na(met_min_low_mean3) &
         !is.na(met_min_inactive) & !is.na(met_min_inactive_lag1) & !is.na(met_min_inactive_mean3))

postpartum_sync = postpartum %>%
  filter(!is.na(steps) & !is.na(steps_lag1) & !is.na(steps_mean3) &
           !is.na(average_met) & !is.na(average_met_lag1) & !is.na(average_met_mean3) &
           !is.na(met_min_high) & !is.na(met_min_high_lag1) & !is.na(met_min_high_mean3) &
           !is.na(met_min_medium) & !is.na(met_min_medium_lag1) & !is.na(met_min_medium_mean3) &
           !is.na(met_min_low) & !is.na(met_min_low_lag1) & !is.na(met_min_low_mean3) &
           !is.na(met_min_inactive) & !is.na(met_min_inactive_lag1) & !is.na(met_min_inactive_mean3))

#### Mallien sovitus ####

#Kun halutaan koota pregnancy-taulukko (stats_df) täytyy hypätä yli postpartum-kohdista, ja kun
#halutaan koota postpartum-taulukko täytyy hypätä yli pregnancy-kohdista.

#### Selittäjänä steps ####

stats_df <- data.frame(
  "Selittävä FA muuttuja" = character(),
  Regressiokerroin = numeric(),
  "P-arvo" = numeric(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)

#### pregnancy ####

model <- lmer(efficiency ~ steps + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ steps + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "steps"
reg <- round(fixef(model)["steps"],3)
p_steps <- round(summary(model)$coefficients["steps", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#steps_lag1

model <- lmer(efficiency ~ steps_lag1 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ steps_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "steps_lag1"
reg <- round(fixef(model)["steps_lag1"],3)
p_steps <- round(summary(model)$coefficients["steps_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#steps_mean3

model <- lmer(efficiency ~ steps_mean3 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ steps_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "steps_mean3"
reg <- round(fixef(model)["steps_mean3"],3)
p_steps <- round(summary(model)$coefficients["steps_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### postpartum ####

postpartum$id = as.factor(postpartum$id)


model <- lmer(efficiency ~ steps + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ steps + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "steps"
reg <- round(fixef(model)["steps"],3)
p_steps <- round(summary(model)$coefficients["steps", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# steps lag_1

model <- lmer(efficiency ~ steps_lag1 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ steps_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "steps_lag1"
reg <- round(fixef(model)["steps_lag1"],3)
p_steps <- round(summary(model)$coefficients["steps_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# steps mean3

model <- lmer(efficiency ~ steps_mean3 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ steps_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "steps_mean3"
reg <- round(fixef(model)["steps_mean3"],3)
p_steps <- round(summary(model)$coefficients["steps_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### Selittäjänä average_met ####

#### pregnancy ####

model <- lmer(efficiency ~ average_met + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ average_met + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "average_met"
reg <- round(fixef(model)["average_met"],3)
p_steps <- round(summary(model)$coefficients["average_met", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#average_met_lag1

model <- lmer(efficiency ~ average_met_lag1 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ average_met_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "average_met_lag1"
reg <- round(fixef(model)["average_met_lag1"],3)
p_steps <- round(summary(model)$coefficients["average_met_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#average_met_mean3

model <- lmer(efficiency ~ average_met_mean3 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ average_met_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "average_met_mean3"
reg <- round(fixef(model)["average_met_mean3"],3)
p_steps <- round(summary(model)$coefficients["average_met_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### postpartum ####

postpartum$id = as.factor(postpartum$id)


model <- lmer(efficiency ~ average_met + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ average_met + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "average_met"
reg <- round(fixef(model)["average_met"],3)
p_steps <- round(summary(model)$coefficients["average_met", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# average_met lag_1

model <- lmer(efficiency ~ average_met_lag1 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ average_met_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "average_met_lag1"
reg <- round(fixef(model)["average_met_lag1"],3)
p_steps <- round(summary(model)$coefficients["average_met_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# average_met mean3

model <- lmer(efficiency ~ average_met_mean3 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ average_met_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "average_met_mean3"
reg <- round(fixef(model)["average_met_mean3"],3)
p_steps <- round(summary(model)$coefficients["average_met_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### Selittäjänä met_min_high ####

#### pregnancy ####

model <- lmer(efficiency ~ met_min_high + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_high + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_high"
reg <- round(fixef(model)["met_min_high"],3)
p_steps <- round(summary(model)$coefficients["met_min_high", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_high_lag1

model <- lmer(efficiency ~ met_min_high_lag1 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ met_min_high_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_high_lag1"
reg <- round(fixef(model)["met_min_high_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_high_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_high_mean3

model <- lmer(efficiency ~ met_min_high_mean3 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_high_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_high_mean3"
reg <- round(fixef(model)["met_min_high_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_high_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### postpartum ####

postpartum$id = as.factor(postpartum$id)


model <- lmer(efficiency ~ met_min_high + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(efficiency ~ met_min_high + ns(week, df=3) + (1 + ns(week, df=3) | id), data = postpartum_sync)
# summary(model)

model <- lmer(efficiency ~ met_min_high + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_high"
reg <- round(fixef(model)["met_min_high"],3)
p_steps <- round(summary(model)$coefficients["met_min_high", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_high lag_1

model <- lmer(efficiency ~ met_min_high_lag1 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_high_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_high_lag1"
reg <- round(fixef(model)["met_min_high_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_high_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_high mean3

model <- lmer(efficiency ~ met_min_high_mean3 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_high_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_high_mean3"
reg <- round(fixef(model)["met_min_high_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_high_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### Selittäjänä met_min_medium ####

####pregnancy####

#met_min_medium

model <- lmer(efficiency ~ met_min_medium + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ met_min_medium + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_medium"
reg <- round(fixef(model)["met_min_medium"],3)
p_steps <- round(summary(model)$coefficients["met_min_medium", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_medium_lag1

model <- lmer(efficiency ~ met_min_medium_lag1 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_medium_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_medium_lag1"
reg <- round(fixef(model)["met_min_medium_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_medium_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_medium_mean3

model <- lmer(efficiency ~ met_min_medium_mean3 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_medium_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_medium_mean3"
reg <- round(fixef(model)["met_min_medium_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_medium_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### postpartum ####

#met_min_medium

model <- lmer(efficiency ~ met_min_medium + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(efficiency ~ steps + ns(week, df=3) + (1 + ns(week, df=3) | id), data = postpartum_sync)
# summary(model)

model <- lmer(efficiency ~ met_min_medium + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_medium"
reg <- round(fixef(model)["met_min_medium"],3)
p_steps <- round(summary(model)$coefficients["met_min_medium", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_medium lag_1

model <- lmer(efficiency ~ met_min_medium_lag1 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_medium_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_medium_lag1"
reg <- round(fixef(model)["met_min_medium_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_medium_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_medium mean3

model <- lmer(efficiency ~ met_min_medium_mean3 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_medium_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_medium_mean3"
reg <- round(fixef(model)["met_min_medium_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_medium_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### Selittäjänä met_min_low ####

####pregnancy####

#met_min_low

model <- lmer(efficiency ~ met_min_low + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ met_min_low + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_low"
reg <- round(fixef(model)["met_min_low"],3)
p_steps <- round(summary(model)$coefficients["met_min_low", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_low_lag1

model <- lmer(efficiency ~ met_min_low_lag1 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_low_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_low_lag1"
reg <- round(fixef(model)["met_min_low_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_low_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_low_mean3

model <- lmer(efficiency ~ met_min_low_mean3 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_low_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_low_mean3"
reg <- round(fixef(model)["met_min_low_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_low_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### postpartum ####

#met_min_low

model <- lmer(efficiency ~ met_min_low + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(efficiency ~ steps + ns(week, df=3) + (1 + ns(week, df=3) | id), data = postpartum_sync)
# summary(model)

model <- lmer(efficiency ~ met_min_low + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_low"
reg <- round(fixef(model)["met_min_low"],3)
p_steps <- round(summary(model)$coefficients["met_min_low", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_low lag_1

model <- lmer(efficiency ~ met_min_low_lag1 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_low_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_low_lag1"
reg <- round(fixef(model)["met_min_low_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_low_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_low mean3

model <- lmer(efficiency ~ met_min_low_mean3 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_low_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_low_mean3"
reg <- round(fixef(model)["met_min_low_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_low_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#### Selittäjänä met_min_inactive ####

####pregnancy####

#met_min_inactive

model <- lmer(efficiency ~ met_min_inactive + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)


model <- lmer(efficiency ~ met_min_inactive + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_inactive"
reg <- round(fixef(model)["met_min_inactive"],3)
p_steps <- round(summary(model)$coefficients["met_min_inactive", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_inactive_lag1

model <- lmer(efficiency ~ met_min_inactive_lag1 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_inactive_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_inactive_lag1"
reg <- round(fixef(model)["met_min_inactive_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_inactive_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

#met_min_inactive_mean3

model <- lmer(efficiency ~ met_min_inactive_mean3 + ns(week, df=3) + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_inactive_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_inactive_mean3"
reg <- round(fixef(model)["met_min_inactive_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_inactive_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### postpartum ####

#met_min_inactive

model <- lmer(efficiency ~ met_min_inactive + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(efficiency ~ steps + ns(week, df=3) + (1 + ns(week, df=3) | id), data = postpartum_sync)
# summary(model)

model <- lmer(efficiency ~ met_min_inactive + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_inactive"
reg <- round(fixef(model)["met_min_inactive"],3)
p_steps <- round(summary(model)$coefficients["met_min_inactive", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_inactive lag_1

model <- lmer(efficiency ~ met_min_inactive_lag1 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_inactive_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_inactive_lag1"
reg <- round(fixef(model)["met_min_inactive_lag1"],3)
p_steps <- round(summary(model)$coefficients["met_min_inactive_lag1", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

# met_min_inactive mean3

model <- lmer(efficiency ~ met_min_inactive_mean3 + ns(week, df=3) + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

model <- lmer(efficiency ~ met_min_inactive_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = postpartum_sync, REML = FALSE)
summary(model)
AIC(model)
BIC(model)

muuttuja = "met_min_inactive_mean3"
reg <- round(fixef(model)["met_min_inactive_mean3"],3)
p_steps <- round(summary(model)$coefficients["met_min_inactive_mean3", "Pr(>|t|)"], 3)
AIC_kerroin = round(AIC(model), 1)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä FA muuttuja" = muuttuja,
  Regressiokerroin = reg,
  "P-arvo" = p_steps,
  AIC = AIC_kerroin,
  stringsAsFactors = FALSE,
  row.names = NULL
))

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

