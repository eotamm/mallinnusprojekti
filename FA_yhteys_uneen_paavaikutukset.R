#### Aineistojen alustus ####

library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(splines)

#set filepath
#dir.path <- file.path("") # symlink 

pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

library(lme4)
library(lmerTest)
library(nlme)

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


pregnancy$id = as.factor(pregnancy$id)
postpartum$id = as.factor(postpartum$id)

pregnancy$duration = pregnancy$duration/(60*60)
postpartum$duration = postpartum$duration/(60*60)


#### Mallien sovitus ####

#### Vastemuuttuja duration ####

#### pregnancy ####

model <- lmer(duration ~ average_met + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + gt_weight_gain + (1 | id), data = pregnancy)
summary(model)

model2<- lme(fixed = duration ~ average_met + ns(week, df = 3) + age_category +
                   education + previous_children + epds_category + bmi_bl2 + gt_weight_gain,
                 random = ~ 1 | id,
                 correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
                 data = pregnancy,
                 na.action = na.omit,
                 method = "REML")
summary(model2)
intervals(model2)

stats_df <- data.frame(
  "Selittävä muuttuja" = character(),
  Regressiokerroin = numeric(),
  "luottamusväli alaraja" = numeric(),
  "luottamusväli yläraja" = numeric(),
  "P-arvo" = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "Vakiokerroin",
  Regressiokerroin = round(fixef(model2)["(Intercept)"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["(Intercept)", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "average_met",
  Regressiokerroin = round(fixef(model2)["average_met"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["average_met", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["average_met", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["average_met", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "age_category30 or more",
  Regressiokerroin = round(fixef(model2)["age_category30 or more"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["age_category30 or more", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "education2",
  Regressiokerroin = round(fixef(model2)["education2"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["education2", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["education2", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["education2", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "previous_children1",
  Regressiokerroin = round(fixef(model2)["previous_children1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["previous_children1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["previous_children1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["previous_children1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "epds_categoryPossible depr",
  Regressiokerroin = round(fixef(model2)["epds_categoryPossible depr"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["epds_categoryPossible depr", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "bmi_bl22",
  Regressiokerroin = round(fixef(model2)["bmi_bl22"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["bmi_bl22", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "gt_weight_gainmore",
  Regressiokerroin = round(fixef(model2)["gt_weight_gainmore than recommendat"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["gt_weight_gainmore than recommendat", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

colnames(stats_df) = c("Selittävä muuttuja",
                       "Reg. kerroin",
                       "CI alaraja 95%",
                       "CI yläraja 95%",
                       "P-arvo")

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


#### postpartum ####

model <- lmer(duration ~ average_met + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)

model2<- lme(fixed = duration ~ average_met + ns(week, df = 3) + age_category +
               education + previous_children + epds_category + bmi_bl2 +gt_weight_gain,
             random = ~ 1 | id,
             correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
             data = postpartum,
             na.action = na.omit,
             method = "REML")
summary(model2)
intervals(model2)

stats_df <- data.frame(
  "Selittävä muuttuja" = character(),
  Regressiokerroin = numeric(),
  "luottamusväli alaraja" = numeric(),
  "luottamusväli yläraja" = numeric(),
  "P-arvo" = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "Vakiokerroin",
  Regressiokerroin = round(fixef(model2)["(Intercept)"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["(Intercept)", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "average_met",
  Regressiokerroin = round(fixef(model2)["average_met"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["average_met", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["average_met", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["average_met", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "age_category30 or more",
  Regressiokerroin = round(fixef(model2)["age_category30 or more"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["age_category30 or more", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "education2",
  Regressiokerroin = round(fixef(model2)["education2"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["education2", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["education2", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["education2", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "previous_children1",
  Regressiokerroin = round(fixef(model2)["previous_children1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["previous_children1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["previous_children1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["previous_children1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "epds_categoryPossible depr",
  Regressiokerroin = round(fixef(model2)["epds_categoryPossible depr"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["epds_categoryPossible depr", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "bmi_bl22",
  Regressiokerroin = round(fixef(model2)["bmi_bl22"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["bmi_bl22", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "gt_weight_gainmore",
  Regressiokerroin = round(fixef(model2)["gt_weight_gainmore than recommendat"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["gt_weight_gainmore than recommendat", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

colnames(stats_df) = c("Selittävä muuttuja",
                       "Reg. kerroin",
                       "CI alaraja 95%",
                       "CI yläraja 95%",
                       "P-arvo")

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


#### Vastemuuttuja efficiency ####

#### pregnancy ####

library(nlme)

model <- lmer(efficiency ~ average_met + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)




model2<- lme(fixed = efficiency ~ average_met + ns(week, df = 3) + age_category +
               education + previous_children + epds_category + bmi_bl2+ gt_weight_gain,
             random = ~ 1 | id,
             correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
             data = pregnancy,
             na.action = na.omit,
             method = "REML")
summary(model2)
intervals(model2)



stats_df <- data.frame(
  "Selittävä muuttuja" = character(),
  Regressiokerroin = numeric(),
  "luottamusväli alaraja" = numeric(),
  "luottamusväli yläraja" = numeric(),
  "P-arvo" = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "Vakiokerroin",
  Regressiokerroin = round(fixef(model2)["(Intercept)"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["(Intercept)", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "average_met",
  Regressiokerroin = round(fixef(model2)["average_met"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["average_met", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["average_met", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["average_met", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "age_category30 or more",
  Regressiokerroin = round(fixef(model2)["age_category30 or more"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["age_category30 or more", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "education2",
  Regressiokerroin = round(fixef(model2)["education2"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["education2", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["education2", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["education2", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "previous_children1",
  Regressiokerroin = round(fixef(model2)["previous_children1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["previous_children1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["previous_children1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["previous_children1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "epds_categoryPossible depr",
  Regressiokerroin = round(fixef(model2)["epds_categoryPossible depr"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["epds_categoryPossible depr", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "bmi_bl22",
  Regressiokerroin = round(fixef(model2)["bmi_bl22"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["bmi_bl22", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "gt_weight_gainmore",
  Regressiokerroin = round(fixef(model2)["gt_weight_gainmore than recommendat"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["gt_weight_gainmore than recommendat", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

colnames(stats_df) = c("Selittävä muuttuja",
                       "Reg. kerroin",
                       "CI alaraja 95%",
                       "CI yläraja 95%",
                       "P-arvo")

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### postpartum ####

model <- lmer(efficiency ~ average_met + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)

model2<- lme(fixed = efficiency ~ average_met + ns(week, df = 3) + age_category +
               education + previous_children + epds_category + bmi_bl2 + gt_weight_gain,
             random = ~ 1 | id,
             correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
             data = postpartum,
             na.action = na.omit,
             method = "REML")
summary(model2)
intervals(model2)

stats_df <- data.frame(
  "Selittävä muuttuja" = character(),
  Regressiokerroin = numeric(),
  "luottamusväli alaraja" = numeric(),
  "luottamusväli yläraja" = numeric(),
  "P-arvo" = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "Vakiokerroin",
  Regressiokerroin = round(fixef(model2)["(Intercept)"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["(Intercept)", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "average_met",
  Regressiokerroin = round(fixef(model2)["average_met"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["average_met", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["average_met", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["average_met", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "age_category30 or more",
  Regressiokerroin = round(fixef(model2)["age_category30 or more"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["age_category30 or more", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "education2",
  Regressiokerroin = round(fixef(model2)["education2"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["education2", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["education2", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["education2", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "previous_children1",
  Regressiokerroin = round(fixef(model2)["previous_children1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["previous_children1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["previous_children1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["previous_children1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "epds_categoryPossible depr",
  Regressiokerroin = round(fixef(model2)["epds_categoryPossible depr"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["epds_categoryPossible depr", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "bmi_bl22",
  Regressiokerroin = round(fixef(model2)["bmi_bl22"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["bmi_bl22", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "gt_weight_gainmore",
  Regressiokerroin = round(fixef(model2)["gt_weight_gainmore than recommendat"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["gt_weight_gainmore than recommendat", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

colnames(stats_df) = c("Selittävä muuttuja",
                       "Reg. kerroin",
                       "CI alaraja 95%",
                       "CI yläraja 95%",
                       "P-arvo")

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### Vastemuuttuja score ####

#### pregnancy ####

library(nlme)

model <- lmer(score ~ average_met_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = pregnancy)
summary(model)

model2<- lme(fixed = score ~ average_met_lag1 + ns(week, df = 3) + age_category +
               education + previous_children + epds_category + bmi_bl2 + gt_weight_gain,
             random = ~ 1 | id,
             correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
             data = pregnancy,
             na.action = na.omit,
             method = "REML")
summary(model2)
intervals(model2)

stats_df <- data.frame(
  "Selittävä muuttuja" = character(),
  Regressiokerroin = numeric(),
  "luottamusväli alaraja" = numeric(),
  "luottamusväli yläraja" = numeric(),
  "P-arvo" = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "Vakiokerroin",
  Regressiokerroin = round(fixef(model2)["(Intercept)"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["(Intercept)", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "average_met_lag1",
  Regressiokerroin = round(fixef(model2)["average_met_lag1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["average_met_lag1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["average_met_lag1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["average_met_lag1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "age_category30 or more",
  Regressiokerroin = round(fixef(model2)["age_category30 or more"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["age_category30 or more", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "education2",
  Regressiokerroin = round(fixef(model2)["education2"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["education2", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["education2", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["education2", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "previous_children1",
  Regressiokerroin = round(fixef(model2)["previous_children1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["previous_children1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["previous_children1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["previous_children1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "epds_categoryPossible depr",
  Regressiokerroin = round(fixef(model2)["epds_categoryPossible depr"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["epds_categoryPossible depr", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "bmi_bl22",
  Regressiokerroin = round(fixef(model2)["bmi_bl22"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["bmi_bl22", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "gt_weight_gainmore",
  Regressiokerroin = round(fixef(model2)["gt_weight_gainmore than recommendat"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["gt_weight_gainmore than recommendat", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

colnames(stats_df) = c("Selittävä muuttuja",
                       "Reg. kerroin",
                       "CI alaraja 95%",
                       "CI yläraja 95%",
                       "P-arvo")

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


#### postpartum ####

model <- lmer(score ~ average_met_lag1 + ns(week, df=3) + age_category + education + previous_children + 
                epds_category + bmi_bl2 + (1 | id), data = postpartum)
summary(model)

model2<- lme(fixed = score ~ average_met_lag1 + ns(week, df = 3) + age_category +
               education + previous_children + epds_category + bmi_bl2 + gt_weight_gain,
             random = ~ 1 | id,
             correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
             data = postpartum,
             na.action = na.omit,
             method = "REML")
summary(model2)
intervals(model2)

stats_df <- data.frame(
  "Selittävä muuttuja" = character(),
  Regressiokerroin = numeric(),
  "luottamusväli alaraja" = numeric(),
  "luottamusväli yläraja" = numeric(),
  "P-arvo" = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "Vakiokerroin",
  Regressiokerroin = round(fixef(model2)["(Intercept)"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["(Intercept)", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["(Intercept)", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "average_met_lag1",
  Regressiokerroin = round(fixef(model2)["average_met_lag1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["average_met_lag1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["average_met_lag1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["average_met_lag1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "age_category30 or more",
  Regressiokerroin = round(fixef(model2)["age_category30 or more"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["age_category30 or more", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["age_category30 or more", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "education2",
  Regressiokerroin = round(fixef(model2)["education2"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["education2", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["education2", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["education2", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "previous_children1",
  Regressiokerroin = round(fixef(model2)["previous_children1"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["previous_children1", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["previous_children1", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["previous_children1", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "epds_categoryPossible depr",
  Regressiokerroin = round(fixef(model2)["epds_categoryPossible depr"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["epds_categoryPossible depr", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["epds_categoryPossible depr", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "bmi_bl22",
  Regressiokerroin = round(fixef(model2)["bmi_bl22"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["bmi_bl22", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["bmi_bl22", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

stats_df <- rbind(stats_df, data.frame(
  "Selittävä muuttuja" = "gt_weight_gainmore",
  Regressiokerroin = round(fixef(model2)["gt_weight_gainmore than recommendat"],3),
  "luottamusväli alaraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][1]),3),
  "luottamusväli yläraja" = round(unname(intervals(model2)$fixed["gt_weight_gainmore than recommendat", ][3]),3),
  "P-arvo" = round(summary(model2)$tTable["gt_weight_gainmore than recommendat", "p-value"], 3),
  stringsAsFactors = FALSE,
  row.names = NULL
))

colnames(stats_df) = c("Selittävä muuttuja",
                       "Reg. kerroin",
                       "CI alaraja 95%",
                       "CI yläraja 95%",
                       "P-arvo")

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


