library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tibble)
library(lme4)
library(lmerTest)
library(DT)


dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 


# Haetaan puhdisteut aineistot
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()


# Lisää keskiarvoistaminen muuttujille
pm_center <- function(dat, id = "id", vars = c("steps", "average_met")) {
  stopifnot(id %in% names(dat), all(vars %in% names(dat)))
  dat %>%
    group_by(.data[[id]]) %>%
    mutate(
      # Päivittäinen poikkeama oman keskiarvon ympäriltä (within-person)
      across(all_of(vars), ~ . - mean(., na.rm = TRUE), .names = "{.col}_dev"),
      # Henkilön oma keskiarvo (between-person)
      across(all_of(vars), ~ mean(., na.rm = TRUE), .names = "{.col}_mean_id")
    ) %>%
    ungroup()
}

# Lisää keskitykset molempiin datoihin
pregnancy  <- pm_center(pregnancy,  id = "id", vars = c("steps", "average_met"))
postpartum <- pm_center(postpartum, id = "id", vars = c("steps", "average_met"))

# duration tunneiksi
pregnancy$duration = pregnancy$duration/(60*60)
postpartum$duration = postpartum$duration/(60*60)

# Skaalaa vertailtavuuden vuoksi (1 SD)
pregnancy <- pregnancy %>%
  mutate(
    steps_dev_z = as.numeric(scale(steps_dev)),
    steps_mean_id_z = as.numeric(scale(steps_mean_id)),
    avgmet_dev_z = as.numeric(scale(average_met_dev)),
    avgmet_mean_id_z = as.numeric(scale(average_met_mean_id)),
    steps_z = as.numeric(scale(steps)),
    avgmet_z = as.numeric(scale(average_met))
  )
postpartum <- postpartum %>%
  mutate(
    steps_dev_z = as.numeric(scale(steps_dev)),
    steps_mean_id_z = as.numeric(scale(steps_mean_id)),
    avgmet_dev_z = as.numeric(scale(average_met_dev)),
    avgmet_mean_id_z = as.numeric(scale(average_met_mean_id)),
    steps_z = as.numeric(scale(steps)),
    avgmet_z = as.numeric(scale(average_met))
  )


## PRE: steps
pre_steps_raw <- lmer(duration ~ steps_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_steps_raw <- AIC(pre_steps_raw)
summary(pre_steps_raw)

pre_steps_dev <- lmer(duration ~ steps_dev_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_steps_dev <- AIC(pre_steps_dev)
summary(pre_steps_dev)

pre_steps_mean_dev <- lmer(duration ~ steps_dev_z + steps_mean_id_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_steps_mean_dev <- AIC(pre_steps_mean_dev)
summary(pre_steps_mean_dev)

# AIC-vertailu (pienempi parempi)
AIC(pre_steps_raw, pre_steps_dev, pre_steps_mean_dev)

# LRT (sisäkkäiset): tarvitaanko mean-termi
anova(pre_steps_dev, pre_steps_mean_dev)


## PRE: average_met
pre_met_raw <- lmer(duration ~ avgmet_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_met_raw <- AIC(pre_met_raw)
summary(pre_met_raw)

pre_met_dev <- lmer(duration ~ avgmet_dev_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_met_dev <- AIC(pre_met_dev)
summary(pre_met_dev)

pre_met_mean_dev <- lmer(duration ~ avgmet_dev_z + avgmet_mean_id_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_met_mean_dev <- AIC(pre_met_mean_dev)
summary(pre_met_mean_dev)

# AIC-vertailu
AIC(pre_met_raw, pre_met_dev, pre_met_mean_dev)

# LRT
anova(pre_met_dev, pre_met_mean_dev)


## POST: steps
post_steps_raw <- lmer(duration ~ steps_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_steps_raw <- AIC(post_steps_raw)
summary(post_steps_raw)

post_steps_dev <- lmer(duration ~ steps_dev_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_steps_dev <- AIC(post_steps_dev)
summary(post_steps_dev)

post_steps_mean_dev <- lmer(duration ~ steps_dev_z + steps_mean_id_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_steps_mean_dev <- AIC(post_steps_mean_dev)
summary(post_steps_mean_dev)

# AIC-vertailu
AIC(post_steps_raw, post_steps_dev, post_steps_mean_dev)

# LRT
anova(post_steps_dev, post_steps_mean_dev)


## POST: average_met
post_met_raw <- lmer(duration ~ avgmet_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_met_raw <- AIC(post_met_raw)
summary(post_met_raw)

post_met_dev <- lmer(duration ~ avgmet_dev_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_met_dev <- AIC(post_met_dev)
summary(post_met_dev)

post_met_mean_dev <- lmer(duration ~ avgmet_dev_z + avgmet_mean_id_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_met_mean_dev <- AIC(post_met_mean_dev)
summary(post_met_mean_dev)

# AIC-vertailu
AIC(post_met_raw, post_met_dev, post_met_mean_dev)

# LRT
anova(post_met_dev, post_met_mean_dev)



## SCORE, PRE: steps
pre_steps_raw_s <- lmer(score ~ steps_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_steps_raw_s <- AIC(pre_steps_raw_s)
summary(pre_steps_raw_s)

pre_steps_dev_s <- lmer(score ~ steps_dev_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_steps_dev_s <- AIC(pre_steps_dev_s)
summary(pre_steps_dev_s)

pre_steps_mean_dev_s <- lmer(score ~ steps_dev_z + steps_mean_id_z + week + (1 | id),
                             data = pregnancy, REML = FALSE)
aic_pre_steps_mean_dev_s <- AIC(pre_steps_mean_dev_s)
summary(pre_steps_mean_dev_s)

# AIC-vertailu (pienempi parempi)
AIC(pre_steps_raw_s, pre_steps_dev_s, pre_steps_mean_dev_s)

# LRT (sisäkkäiset): tarvitaanko mean-termi
anova(pre_steps_dev_s, pre_steps_mean_dev_s)



## SCORE, PRE: average_met
pre_met_raw_s <- lmer(score ~ avgmet_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_met_raw_s <- AIC(pre_met_raw_s)
summary(pre_met_raw_s)

pre_met_dev_s <- lmer(score ~ avgmet_dev_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_met_dev_s <- AIC(pre_met_dev_s)
summary(pre_met_dev_s)

pre_met_mean_dev_s <- lmer(score ~ avgmet_dev_z + avgmet_mean_id_z + week + (1 | id),
                           data = pregnancy, REML = FALSE)
aic_pre_met_mean_dev_s <- AIC(pre_met_mean_dev_s)
summary(pre_met_mean_dev_s)

# AIC-vertailu
AIC(pre_met_raw_s, pre_met_dev_s, pre_met_mean_dev_s)

# LRT
anova(pre_met_dev_s, pre_met_mean_dev_s)



## SCORE, POST: steps
post_steps_raw_s <- lmer(score ~ steps_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_steps_raw_s <- AIC(post_steps_raw_s)
summary(post_steps_raw_s)

post_steps_dev_s <- lmer(score ~ steps_dev_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_steps_dev_s <- AIC(post_steps_dev_s)
summary(post_steps_dev_s)

post_steps_mean_dev_s <- lmer(score ~ steps_dev_z + steps_mean_id_z + week + (1 | id),
                              data = postpartum, REML = FALSE)
aic_post_steps_mean_dev_s <- AIC(post_steps_mean_dev_s)
summary(post_steps_mean_dev_s)

# AIC-vertailu
AIC(post_steps_raw_s, post_steps_dev_s, post_steps_mean_dev_s)

# LRT
anova(post_steps_dev_s, post_steps_mean_dev_s)



## SCORE, POST: average_met
post_met_raw_s <- lmer(score ~ avgmet_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_met_raw_s <- AIC(post_met_raw_s)
summary(post_met_raw_s)

post_met_dev_s <- lmer(score ~ avgmet_dev_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_met_dev_s <- AIC(post_met_dev_s)
summary(post_met_dev_s)

post_met_mean_dev_s <- lmer(score ~ avgmet_dev_z + avgmet_mean_id_z + week + (1 | id),
                            data = postpartum, REML = FALSE)
aic_post_met_mean_dev_s <- AIC(post_met_mean_dev_s)
summary(post_met_mean_dev_s)

# AIC-vertailu
AIC(post_met_raw_s, post_met_dev_s, post_met_mean_dev_s)

# LRT
anova(post_met_dev_s, post_met_mean_dev_s)





## EFFICIENCY, PRE: steps
pre_steps_raw_e <- lmer(efficiency ~ steps_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_steps_raw_e <- AIC(pre_steps_raw_e)
summary(pre_steps_raw_e)

pre_steps_dev_e <- lmer(efficiency ~ steps_dev_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_steps_dev_e <- AIC(pre_steps_dev_e)
summary(pre_steps_dev_e)

pre_steps_mean_dev_e <- lmer(efficiency ~ steps_dev_z + steps_mean_id_z + week + (1 | id),
                             data = pregnancy, REML = FALSE)
aic_pre_steps_mean_dev_e <- AIC(pre_steps_mean_dev_e)
summary(pre_steps_mean_dev_e)

# AIC-vertailu
AIC(pre_steps_raw_e, pre_steps_dev_e, pre_steps_mean_dev_e)

# LRT
anova(pre_steps_dev_e, pre_steps_mean_dev_e)


## EFFICIENCY, PRE: average_met
pre_met_raw_e <- lmer(efficiency ~ avgmet_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_met_raw_e <- AIC(pre_met_raw_e)
summary(pre_met_raw_e)

pre_met_dev_e <- lmer(efficiency ~ avgmet_dev_z + week + (1 | id), data = pregnancy, REML = FALSE)
aic_pre_met_dev_e <- AIC(pre_met_dev_e)
summary(pre_met_dev_e)

pre_met_mean_dev_e <- lmer(efficiency ~ avgmet_dev_z + avgmet_mean_id_z + week + (1 | id),
                           data = pregnancy, REML = FALSE)
aic_pre_met_mean_dev_e <- AIC(pre_met_mean_dev_e)
summary(pre_met_mean_dev_e)

# AIC-vertailu
AIC(pre_met_raw_e, pre_met_dev_e, pre_met_mean_dev_e)

# LRT
anova(pre_met_dev_e, pre_met_mean_dev_e)


## EFFICIENCY, POST: steps
post_steps_raw_e <- lmer(efficiency ~ steps_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_steps_raw_e <- AIC(post_steps_raw_e)
summary(post_steps_raw_e)

post_steps_dev_e <- lmer(efficiency ~ steps_dev_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_steps_dev_e <- AIC(post_steps_dev_e)
summary(post_steps_dev_e)

post_steps_mean_dev_e <- lmer(efficiency ~ steps_dev_z + steps_mean_id_z + week + (1 | id),
                              data = postpartum, REML = FALSE)
aic_post_steps_mean_dev_e <- AIC(post_steps_mean_dev_e)
summary(post_steps_mean_dev_e)

# AIC-vertailu
AIC(post_steps_raw_e, post_steps_dev_e, post_steps_mean_dev_e)

# LRT
anova(post_steps_dev_e, post_steps_mean_dev_e)


## EFFICIENCY, POST: average_met
post_met_raw_e <- lmer(efficiency ~ avgmet_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_met_raw_e <- AIC(post_met_raw_e)
summary(post_met_raw_e)

post_met_dev_e <- lmer(efficiency ~ avgmet_dev_z + week + (1 | id), data = postpartum, REML = FALSE)
aic_post_met_dev_e <- AIC(post_met_dev_e)
summary(post_met_dev_e)

post_met_mean_dev_e <- lmer(efficiency ~ avgmet_dev_z + avgmet_mean_id_z + week + (1 | id),
                            data = postpartum, REML = FALSE)
aic_post_met_mean_dev_e <- AIC(post_met_mean_dev_e)
summary(post_met_mean_dev_e)

# AIC-vertailu
AIC(post_met_raw_e, post_met_dev_e, post_met_mean_dev_e)

# LRT
anova(post_met_dev_e, post_met_mean_dev_e)


## AIC-summary
# duration
aic_duration <- data.frame(
  Outcome = "duration",
  Set   = c("PRE: steps","PRE: steps","PRE: steps",
            "PRE: average_met","PRE: average_met","PRE: average_met",
            "POST: steps","POST: steps","POST: steps",
            "POST: average_met","POST: average_met","POST: average_met"),
  Model = c("raw","dev","dev+mean",
            "raw","dev","dev+mean",
            "raw","dev","dev+mean",
            "raw","dev","dev+mean"),
  AIC   = c(aic_pre_steps_raw, aic_pre_steps_dev, aic_pre_steps_mean_dev,
            aic_pre_met_raw,   aic_pre_met_dev,   aic_pre_met_mean_dev,
            aic_post_steps_raw, aic_post_steps_dev, aic_post_steps_mean_dev,
            aic_post_met_raw,   aic_post_met_dev,   aic_post_met_mean_dev)
)

# score
aic_score <- data.frame(
  Outcome = "score",
  Set   = c("PRE: steps","PRE: steps","PRE: steps",
            "PRE: average_met","PRE: average_met","PRE: average_met",
            "POST: steps","POST: steps","POST: steps",
            "POST: average_met","POST: average_met","POST: average_met"),
  Model = c("raw","dev","dev+mean",
            "raw","dev","dev+mean",
            "raw","dev","dev+mean",
            "raw","dev","dev+mean"),
  AIC   = c(aic_pre_steps_raw_s, aic_pre_steps_dev_s, aic_pre_steps_mean_dev_s,
            aic_pre_met_raw_s,   aic_pre_met_dev_s,   aic_pre_met_mean_dev_s,
            aic_post_steps_raw_s, aic_post_steps_dev_s, aic_post_steps_mean_dev_s,
            aic_post_met_raw_s,   aic_post_met_dev_s,   aic_post_met_mean_dev_s)
)

# efficiency
aic_efficiency <- data.frame(
  Outcome = "efficiency",
  Set   = c("PRE: steps","PRE: steps","PRE: steps",
            "PRE: average_met","PRE: average_met","PRE: average_met",
            "POST: steps","POST: steps","POST: steps",
            "POST: average_met","POST: average_met","POST: average_met"),
  Model = c("raw","dev","dev+mean",
            "raw","dev","dev+mean",
            "raw","dev","dev+mean",
            "raw","dev","dev+mean"),
  AIC   = c(aic_pre_steps_raw_e, aic_pre_steps_dev_e, aic_pre_steps_mean_dev_e,
            aic_pre_met_raw_e,   aic_pre_met_dev_e,   aic_pre_met_mean_dev_e,
            aic_post_steps_raw_e, aic_post_steps_dev_e, aic_post_steps_mean_dev_e,
            aic_post_met_raw_e,   aic_post_met_dev_e,   aic_post_met_mean_dev_e)
)

# yhdistä aiemmat duration + score + uusi efficiency
aic_all <- bind_rows(aic_duration, aic_score, aic_efficiency) %>%
  group_by(Outcome, Set) %>%
  mutate(DeltaAIC = AIC - min(AIC)) %>%
  arrange(Outcome, Set, AIC) %>%
  ungroup()

aic_all_dt <- aic_all
aic_all_dt$AIC      <- round(aic_all_dt$AIC, 1)
aic_all_dt$DeltaAIC <- round(aic_all_dt$DeltaAIC, 1)

datatable(
  aic_all_dt,
  rownames = FALSE,
  options = list(
    pageLength = 36,
    dom = "tip",
    order = list(list(0,'asc'), list(1,'asc'), list(3,'asc'))
  )
) %>%
  formatStyle(
    'DeltaAIC',
    target = 'row',
    backgroundColor = styleEqual(0, '#E3FCEF'),
    fontWeight      = styleEqual(0, 'bold')
  )

