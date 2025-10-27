library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(splines)

dir.path <- file.path("C:/Users/Esato/Seafile/Projektikurssi/") # symlink 

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

pregnancy$average_met_lag1 = scale(pregnancy$average_met_lag1)
postpartum$average_met_lag1 = scale(postpartum$average_met_lag1)
pregnancy$average_met_mean3 = scale(pregnancy$average_met_mean3)
postpartum$average_met_mean3 = scale(postpartum$average_met_mean3)


#### Unen kesto ####

#### Selittäjänä steps ####

#### pregnancy ####

pregnancy$id = as.factor(pregnancy$id)

#step skaalaus
pregnancy$steps = scale(pregnancy$steps)
# vai per 1000 askelta?
# pregnancy$steps = pregnancy$steps/1000

# duration tunneiksi
pregnancy$duration = pregnancy$duration/(60*60)

#steps

model <- lmer(duration ~ steps + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ steps + week + (1 + week | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps + week + (1  | id) + (week | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#steps_lag1

model <- lmer(duration ~ steps_lag1 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ steps_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#steps_mean3

model <- lmer(duration ~ steps_mean3 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ steps_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# time epälineaarinen

model <- lmer(duration ~ steps_mean3 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#step skaalaus
postpartum$steps = scale(postpartum$steps)
# vai per 1000 askelta?
# postpartum$steps = postpartum$steps/1000

# duration tunneiksi
postpartum$duration = postpartum$duration/(60*60)

#steps

model <- lmer(duration ~ steps + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(duration ~ steps + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(duration ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# steps lag_1

model <- lmer(duration ~ steps_lag1 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ steps_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# steps mean3

model <- lmer(duration ~ steps_mean3 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ steps_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# epälineaarinen aika

model <- lmer(duration ~ steps_mean3 + 
                ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)


#### Selittäjänä average_met ####

#### pregnancy ####

pregnancy$id = as.factor(pregnancy$id)

#average_met skaalaus
pregnancy$average_met = scale(pregnancy$average_met)

# duration tunneiksi
pregnancy$duration = pregnancy$duration/(60*60)

# average_met

model <- lmer(duration ~ average_met + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(duration ~ average_met + week + (1 + week | id), data = pregnancy)
# summary(model)

model <- lmer(duration ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# average_met_lag1

model <- lmer(duration ~ average_met_lag1 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ average_met_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# average_met_mean3

model <- lmer(duration ~ average_met_mean3 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ average_met_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ average_met_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#average_met skaalaus
postpartum$average_met = scale(postpartum$average_met)

# duration tunneiksi
postpartum$duration = postpartum$duration/(60*60)

# average_met

model <- lmer(duration ~ average_met + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(duration ~ average_met + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(duration ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# average_met lag1

model <- lmer(duration ~ average_met_lag1 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ average_met_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# average_met mean3

model <- lmer(duration ~ average_met_mean3 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(duration ~ average_met_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

#### Unen laatu ####

#### Selittäjänä steps ####

#### pregnancy ####

pregnancy$id = as.factor(pregnancy$id)

#step skaalaus
pregnancy$steps = scale(pregnancy$steps)
# vai per 1000 askelta?
pregnancy$steps = pregnancy$steps/1000

# steps

model <- lmer(score ~ steps + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(score ~ steps + week + (1 + week | id), data = pregnancy)
# summary(model)

model <- lmer(score ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# steps lag1

model <- lmer(score ~ steps_lag1 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ steps_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# steps mean3

model <- lmer(score ~ steps_mean3 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ steps_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#step skaalaus
postpartum$steps = scale(postpartum$steps)
# vai per 1000 askelta?
# postpartum$steps = postpartum$steps/1000

# steps

model <- lmer(score ~ steps + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(score ~ steps + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(score ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# steps lag1

model <- lmer(score ~ steps_lag1 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ steps_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# steps mean3

model <- lmer(score ~ steps_mean3 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ steps_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

#### Selittäjänä average_met ####

#### pregnancy ####

pregnancy$id = as.factor(pregnancy$id)

#average_met skaalaus
pregnancy$average_met = scale(pregnancy$average_met)

#average_met

model <- lmer(score ~ average_met + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(score ~ average_met + week + (1 + week | id), data = pregnancy)
# summary(model)

model <- lmer(score ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#average_met lag1

model <- lmer(score ~ average_met_lag1 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ average_met_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#average_met mean3

model <- lmer(score ~ average_met_mean3 + week + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ average_met_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)
AIC(model)
BIC(model)

#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#average_met skaalaus
postpartum$average_met = scale(postpartum$average_met)

#average_met

model <- lmer(score ~ average_met + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

# model <- lmer(score ~ average_met + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(score ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

#average_met lag1

model <- lmer(score ~ average_met_lag1 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ average_met_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

#average_met mean3

model <- lmer(score ~ average_met_mean3 + week + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

model <- lmer(score ~ average_met_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
AIC(model)
BIC(model)

#### Uudet aktiivisuusmuuttujat ####

#### steps ####

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

pregnancy$steps_lag1 = scale(pregnancy$steps_lag1)
postpartum$steps_lag1 = scale(postpartum$steps_lag1)
pregnancy$steps_mean3 = scale(pregnancy$steps_mean3)
postpartum$steps_mean3 = scale(postpartum$steps_mean3)


pregnancy$id = as.factor(pregnancy$id)


# duration tunneiksi
pregnancy$duration = pregnancy$duration/(60*60)

model <- lmer(duration ~ steps_lag1 + week + (1 | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps_lag1 + week + (1 + week | id), data = pregnancy)
summary(model)


model <- lmer(duration ~ steps_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)


#### postpartum ####

postpartum$id = as.factor(postpartum$id)

# duration tunneiksi
postpartum$duration = postpartum$duration/(60*60)

model <- lmer(duration ~ steps_lag1 + week + (1 | id), data = postpartum)
summary(model)

# model <- lmer(duration ~ steps + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(duration ~ steps_lag1 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)

# mean_3

model <- lmer(duration ~ steps_mean3 + week + (1 | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps_mean3 + week + (1 + week | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)


#### postpartum ####


model <- lmer(duration ~ steps_mean3 + week + (1 | id), data = postpartum)
summary(model)

# model <- lmer(duration ~ steps + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(duration ~ steps_mean3 + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
