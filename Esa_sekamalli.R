library(tidyverse)
library(haven)
library(janitor)
library(dplyr)

dir.path <- file.path("C:/Users/Esato/Seafile/Projektikurssi/") # symlink 

pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

library(lme4)
library(lmerTest)

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

model <- lmer(duration ~ steps + week + (1 | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps + week + (1 + week | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps + week + (1  | id) + (week | id), data = pregnancy)
summary(model)

model <- lmer(duration ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)


#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#step skaalaus
postpartum$steps = scale(postpartum$steps)
# vai per 1000 askelta?
# postpartum$steps = postpartum$steps/1000

# duration tunneiksi
postpartum$duration = postpartum$duration/(60*60)

model <- lmer(duration ~ steps + week + (1 | id), data = postpartum)
summary(model)

# model <- lmer(duration ~ steps + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(duration ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)

#### Selittäjänä average_met ####

#### pregnancy ####

pregnancy$id = as.factor(pregnancy$id)

#average_met skaalaus
pregnancy$average_met = scale(pregnancy$average_met)


# duration tunneiksi
pregnancy$duration = pregnancy$duration/(60*60)

model <- lmer(duration ~ average_met + week + (1 | id), data = pregnancy)
summary(model)

# model <- lmer(duration ~ average_met + week + (1 + week | id), data = pregnancy)
# summary(model)

model <- lmer(duration ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)


#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#average_met skaalaus
postpartum$average_met = scale(postpartum$average_met)


# duration tunneiksi
postpartum$duration = postpartum$duration/(60*60)

model <- lmer(duration ~ average_met + week + (1 | id), data = postpartum)
summary(model)

# model <- lmer(duration ~ average_met + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(duration ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)

#### Unen laatu ####

#### Selittäjänä steps ####

#### pregnancy ####

pregnancy$id = as.factor(pregnancy$id)

#step skaalaus
pregnancy$steps = scale(pregnancy$steps)
# vai per 1000 askelta?
pregnancy$steps = pregnancy$steps/1000


model <- lmer(score ~ steps + week + (1 | id), data = pregnancy)
summary(model)

# model <- lmer(score ~ steps + week + (1 + week | id), data = pregnancy)
# summary(model)

model <- lmer(score ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)


#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#step skaalaus
postpartum$steps = scale(postpartum$steps)
# vai per 1000 askelta?
# postpartum$steps = postpartum$steps/1000


model <- lmer(score ~ steps + week + (1 | id), data = postpartum)
summary(model)

# model <- lmer(score ~ steps + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(score ~ steps + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)

#### Selittäjänä average_met ####

#### pregnancy ####

pregnancy$id = as.factor(pregnancy$id)

#average_met skaalaus
pregnancy$average_met = scale(pregnancy$average_met)


model <- lmer(score ~ average_met + week + (1 | id), data = pregnancy)
summary(model)

# model <- lmer(score ~ average_met + week + (1 + week | id), data = pregnancy)
# summary(model)

model <- lmer(score ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)


#### postpartum ####

postpartum$id = as.factor(postpartum$id)

#average_met skaalaus
postpartum$average_met = scale(postpartum$average_met)


model <- lmer(score ~ average_met + week + (1 | id), data = postpartum)
summary(model)

# model <- lmer(score ~ average_met + week + (1 + week | id), data = postpartum)
# summary(model)

model <- lmer(score ~ average_met + week + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)
