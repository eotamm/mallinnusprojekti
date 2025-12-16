library(dplyr)
library(forcats)
library(ggplot2)
library(rlang)
library(patchwork)
library(zoo)
library(lme4)
library(lmerTest)
library(splines)
library(broom.mixed)
library(stringr)
library(ggeffects)
library(fs)
library(nlme)
library(ggh4x)
library(purrr)

source("Taustamuuttujafunktiot.R")

dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 


# Haetaan puhdisteut aineistot
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

# Muunna duration tunneiksi
pregnancy  <- pregnancy  %>% mutate(duration = duration / 3600)
postpartum <- postpartum %>% mutate(duration = duration / 3600)


set_ref_levels_safe <- function(df) {
  if ("age_category"   %in% names(df)) df$age_category   <- relevel(factor(df$age_category),   ref = "Under 30")
  if ("education"      %in% names(df)) df$education      <- relevel(factor(df$education),      ref = "1")
  if ("previous_children" %in% names(df)) df$previous_children <- relevel(factor(df$previous_children), ref = "0")
  if ("epds_category"  %in% names(df)) df$epds_category  <- relevel(factor(df$epds_category),  ref = "No depression")
  if ("bmi_bl2"        %in% names(df)) df$bmi_bl2        <- relevel(factor(df$bmi_bl2),        ref = "1")
  if ("gt_weight_gain" %in% names(df)) df$gt_weight_gain <- relevel(factor(df$gt_weight_gain), ref = "within or less")
  if ("pp_weight_lost" %in% names(df)) df$pp_weight_lost <- relevel(factor(df$pp_weight_lost), ref = "1")
  df
}

postpartum$delivery_method <- factor(postpartum$delivery_method,
                                    levels = c(1, 2, 3),
                                    labels = c("Vaginal", "Vacuum-assisted", "Caesarian section"))
postpartum$delivery_method <- relevel(postpartum$delivery_method, ref = "Vaginal")


pregnancy  <- set_ref_levels_safe(pregnancy)
postpartum <- set_ref_levels_safe(postpartum)

# Lag1 muuttuja
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

# Skaalaukset
pregnancy$average_met = scale(pregnancy$average_met)
postpartum$average_met = scale(postpartum$average_met)
pregnancy$average_met_lag1 = scale(pregnancy$average_met_lag1)
postpartum$average_met_lag1 = scale(postpartum$average_met_lag1)



# Tallennus kansio
out_dir <- file.path("figures", "taustamuuttujamallit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)



# Taustamuuttuja mallit
## PREGNANCY — DURATION
m_preg_dur_base <- lme(
  fixed = duration ~ average_met +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = pregnancy, method = "REML", na.action = na.omit
)
summary(m_preg_dur_base)

# Plot
coef_preg_dur_base <- plot_coef_forest_pretty(
  m_preg_dur_base,
  title = "Pregnancy: duration — kertoimet (ei interaktioita)"
)
print(coef_preg_dur_base)
ggsave(file.path(out_dir, "coef_preg_dur_base.png"),
       coef_preg_dur_base, width = 10, height = 6, dpi = 300)


# Interaktiot
m_preg_dur_int <- lme(
  fixed = duration ~ average_met * (age_category + education + previous_children +
                                              epds_category + bmi_bl2 + gt_weight_gain) +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = pregnancy, method = "REML", na.action = na.omit
)
summary(m_preg_dur_int)

# Plot
coef_preg_dur_int <- plot_coef_forest_pretty(
  m_preg_dur_int,
  title = "Pregnancy: duration — kertoimet"
)
print(coef_preg_dur_int)
ggsave(file.path(out_dir, "coef_preg_dur_int.png"),
       coef_preg_dur_int, width = 10, height = 6, dpi = 300)


## PREGNANCY — SCORE

m_preg_score_base <- lme(
  fixed = score ~ average_met_lag1 +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = pregnancy, method = "REML", na.action = na.omit
)
summary(m_preg_score_base)

# Plot
coef_preg_score_base <- plot_coef_forest_pretty(
  m_preg_score_base,
  title = "Pregnancy: score — kertoimet (ei interaktioita)"
)
print(coef_preg_score_base)
ggsave(file.path(out_dir, "coef_preg_score_base.png"),
       coef_preg_score_base, width = 10, height =6, dpi = 300)


# Interaktiot
m_preg_score_int <- lme(
  fixed = score ~ average_met_lag1 * (age_category + education + previous_children +
                                           epds_category + bmi_bl2 + gt_weight_gain) +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = pregnancy, method = "REML", na.action = na.omit
)
summary(m_preg_score_int)

# Plot
coef_preg_score_int <- plot_coef_forest_pretty(
  m_preg_score_int,
  title = "Pregnancy: score — kertoimet"
)
print(coef_preg_score_int)
ggsave(file.path(out_dir, "coef_preg_score_int.png"),
       coef_preg_score_int, width = 10, height = 6, dpi = 300)


## PREGNANCY — EFFICIENCY

m_preg_eff_base <- lme(
  fixed = efficiency ~ average_met +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = pregnancy, method = "REML", na.action = na.omit
)
summary(m_preg_eff_base)

# Plot
coef_preg_eff_base <- plot_coef_forest_pretty(
  m_preg_eff_base,
  title = "Pregnancy: efficiency — kertoimet (ei interaktioita)"
)
print(coef_preg_eff_base)
ggsave(file.path(out_dir, "coef_preg_eff_base.png"),
       coef_preg_eff_base, width = 10, height = 6, dpi = 300)

# Interaktiot
m_preg_eff_int <- lme(
  fixed = efficiency ~ average_met * (age_category + education + previous_children +
                                        epds_category + bmi_bl2 + gt_weight_gain) +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = pregnancy, method = "REML", na.action = na.omit
)
summary(m_preg_eff_int)

# Plot
coef_preg_eff_int <- plot_coef_forest_pretty(
  m_preg_eff_int,
  title = "Pregnancy: efficiency — kertoimet"
)
print(coef_preg_eff_int)
ggsave(file.path(out_dir, "coef_preg_eff_int.png"),
       coef_preg_eff_int, width = 10, height = 6, dpi = 300)





## POSTPARTUM — DURATION

m_post_dur_base <- lme(
  fixed = duration ~ average_met +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    pp_weight_lost + delivery_method +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = postpartum, method = "REML", na.action = na.omit
)
summary(m_post_dur_base)

# Plot
coef_post_dur_base <- plot_coef_forest_pretty(
  m_post_dur_base,
  title = "Postpartum: duration — kertoimet (ei interaktioita)"
)
print(coef_post_dur_base)
ggsave(file.path(out_dir, "coef_post_dur_base.png"),
       coef_post_dur_base, width = 10, height = 6, dpi = 300)


# Interaktiot
m_post_dur_int <- lme(
  fixed = duration ~ average_met * (age_category + education + previous_children +
                                              epds_category + bmi_bl2 + gt_weight_gain +
                                              pp_weight_lost + delivery_method) +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = postpartum, method = "REML", na.action = na.omit
)
summary(m_post_dur_int)

# Plot
coef_post_dur_int <- plot_coef_forest_pretty(
  m_post_dur_int,
  title = "Postpartum: duration — kertoimet"
)
print(coef_post_dur_int)
ggsave(file.path(out_dir, "coef_post_dur_int.png"),
       coef_post_dur_int, width = 10, height = 6, dpi = 300)



## POSTPARTUM — SCORE

m_post_score_base <- lme(
  fixed = score ~ average_met_lag1 +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    pp_weight_lost + delivery_method +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = postpartum, method = "REML", na.action = na.omit
)
summary(m_post_score_base)

# Plot
coef_post_score_base <- plot_coef_forest_pretty(
  m_post_score_base,
  title = "Postpartum: score — kertoimet (ei interaktioita)"
)
print(coef_post_score_base)
ggsave(file.path(out_dir, "coef_post_score_base.png"),
       coef_post_score_base, width = 10, height = 6, dpi = 300)


# Interaktiot
m_post_score_int <- lme(
  fixed = score ~ average_met_lag1 * (age_category + education + previous_children +
                                           epds_category + bmi_bl2 + gt_weight_gain +
                                           pp_weight_lost + delivery_method) +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = postpartum, method = "REML", na.action = na.omit
)
summary(m_post_score_int)

# Plot
coef_post_score_int <- plot_coef_forest_pretty(
  m_post_score_int,
  title = "Postpartum: score — kertoimet"
)
print(coef_post_score_int)
ggsave(file.path(out_dir, "coef_post_score_int.png"),
       coef_post_score_int, width = 10, height = 6, dpi = 300)



## POSTPARTUM — EFFICIENCY

m_post_eff_base <- lme(
  fixed = efficiency ~ average_met +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    pp_weight_lost + delivery_method +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = postpartum, method = "REML", na.action = na.omit
)
summary(m_post_eff_base)

# Plot
coef_post_eff_base <- plot_coef_forest_pretty(
  m_post_eff_base,
  title = "Postpartum: efficiency — kertoimet (ei interaktioita)"
)
print(coef_post_eff_base)
ggsave(file.path(out_dir, "coef_post_eff_base.png"),
       coef_post_eff_base, width = 10, height = 6, dpi = 300)

# Interaktiot
m_post_eff_int <- lme(
  fixed = efficiency ~ average_met * (age_category + education + previous_children +
                                        epds_category + bmi_bl2 + gt_weight_gain +
                                        pp_weight_lost + delivery_method) +
    ns(week, df = 3),
  random = ~ 1 | id,
  correlation = corCAR1(form = ~ as.numeric(summary_date) | id),
  data = postpartum, method = "REML", na.action = na.omit
)
summary(m_post_eff_int)

# Plot
coef_post_eff_int <- plot_coef_forest_pretty(
  m_post_eff_int,
  title = "Postpartum: efficiency — kertoimet"
)
print(coef_post_eff_int)
ggsave(file.path(out_dir, "coef_post_eff_int.png"),
       coef_post_eff_int, width = 10, height = 6, dpi = 300)






# EPDS-kategorian interaktio
plot_interaction_facets(by = "epds_category",
                        focal = c("average_met", "average_met_lag1"),
                        xlab  = "Aktiivisuus (z)")

# Previous-children interaktio
plot_interaction_facets(by = "previous_children",
                        focal = c("average_met", "average_met_lag1"),
                        xlab  = "Aktiivisuus (z)")

# Lähtö-BMI
plot_interaction_facets(by = "bmi_bl2",
                        focal = c("average_met", "average_met_lag1"),
                        xlab  = "Aktiivisuus (z)")

# Raskaudenaikainen painonnousu
plot_interaction_facets(by = "gt_weight_gain",
                        focal = c("average_met", "average_met_lag1"),
                        xlab  = "Aktiivisuus (z)")

# Synnytyksen jälkeinen painonlasku
plot_interaction_facets(by = "pp_weight_lost",
                        focal = c("average_met", "average_met_lag1"),
                        xlab  = "Aktiivisuus (z)")


# PREGNANCY, duration: average_met x epds_category (p = 0.0235)
plot_interaction_single(
  model      = m_preg_dur_int,
  by         = "epds_category",
  focal      = c("average_met"),
  model_name = "Pregnancy: duration",
  xlab       = "Aktiivisuus (z)",
  fname      = "interaction__Pregnancy_duration__average_met_x_epds_category.png"
)

# PREGNANCY, efficiency: average_met x bmi_bl2 (p = 0.0069)
plot_interaction_single(
  model      = m_preg_eff_int,
  by         = "bmi_bl2",
  focal      = c("average_met"),
  model_name = "Pregnancy: efficiency",
  xlab       = "Aktiivisuus (z)",
  fname      = "interaction__Pregnancy_efficiency__average_met_x_bmi_bl2.png"
)

# PREGNANCY, efficiency: average_met x EPDS (p = 0.079)
plot_interaction_single(
  model      = m_preg_eff_int,
  by         = "epds_category",
  focal      = c("average_met"),
  model_name = "Pregnancy: efficiency",
  xlab       = "Aktiivisuus (z)",
  fname      = "interaction__Pregnancy_efficiency__average_met_x_epds_category.png"
)

# PREGNANCY, score: average_met_lag1 x gt_weight_gain (p = 0.0003)
plot_interaction_single(
  model      = m_preg_score_int,
  by         = "gt_weight_gain",
  focal      = c("average_met_lag1"),
  model_name = "Pregnancy: score",
  xlab       = "Aktiivisuus (lag1, z)",
  fname      = "interaction__Pregnancy_score__average_met_lag1_x_gt_weight_gain.png"
)

# PREGNANCY, score: average_met_lag1 x age (p = 0.058)
plot_interaction_single(
  model      = m_preg_score_int,
  by         = "age_category",
  focal      = c("average_met_lag1"),
  model_name = "Pregnancy: score",
  xlab       = "Aktiivisuus (lag1, z)",
  fname      = "interaction__Pregnancy_score__average_met_lag1_x_age_category.png"
)

# POSTPARTUM, duration: average_met x previous_children (p = 0.0328)
plot_interaction_single(
  model      = m_post_dur_int,
  by         = "previous_children",
  focal      = c("average_met"),
  model_name = "Postpartum: duration",
  xlab       = "Aktiivisuus (z)",
  fname      = "interaction__Postpartum_duration__average_met_x_previous_children.png"
)

# POSTPARTUM, duration: average_met x pp_weight_lost (p = 0.0101)
plot_interaction_single(
  model      = m_post_dur_int,
  by         = "pp_weight_lost",
  focal      = c("average_met"),
  model_name = "Postpartum: duration",
  xlab       = "Aktiivisuus (z)",
  fname      = "interaction__Postpartum_duration__average_met_x_pp_weight_lost.png"
)

# POSTPARTUM, duration: average_met x delivery_method
plot_interaction_single(
  model      = m_post_dur_int,
  by         = "delivery_method",
  focal      = c("average_met"),
  model_name = "Postpartum: duration",
  xlab       = "Aktiivisuus (z)",
  fname      = "interaction__Postpartum_duration__average_met_x_delivery_method.png"
)

# POSTPARTUM, efficiency: average_met x pp_weight_lost (p = 0.067)
plot_interaction_single(
  model      = m_post_eff_int,
  by         = "pp_weight_lost",
  focal      = c("average_met"),
  model_name = "Postpartum: efficiency",
  xlab       = "Aktiivisuus (z)",
  fname      = "interaction__Postpartum_efficiency__average_met_x_pp_weight_lost.png"
)



# Latex taulukot
tab_preg_dur_full <- latex_main_inter_full_table(
  mod    = m_preg_dur_int,
  focal  = "average_met",
  caption = "Raskausaika: unen kesto — päävaikutukset, interaktiot ja aktiivisuuden ehdolliset vaikutukset",
  label   = "tab:preg_dur_full"
)
cat(tab_preg_dur_full)


tab_preg_score_full <- latex_main_inter_full_table(
  mod    = m_preg_score_int,
  focal  = "average_met_lag1",
  caption = "Raskausaika: unen laatu (Oura-score) — päävaikutukset, interaktiot ja aktiivisuuden ehdolliset vaikutukset",
  label   = "tab:preg_score_full"
)
cat(tab_preg_score_full)


tab_preg_eff_full <- latex_main_inter_full_table(
  mod    = m_preg_eff_int,
  focal  = "average_met",
  caption = "Raskausaika: unen tehokkuus — päävaikutukset, interaktiot ja aktiivisuuden ehdolliset vaikutukset",
  label   = "tab:preg_eff_full"
)
cat(tab_preg_eff_full)


tab_post_dur_full <- latex_main_inter_full_table(
  mod    = m_post_dur_int,
  focal  = "average_met",
  caption = "Synnytyksen jälkeen: unen kesto — päävaikutukset, interaktiot ja aktiivisuuden ehdolliset vaikutukset",
  label   = "tab:post_dur_full"
)
cat(tab_post_dur_full)


tab_post_score_full <- latex_main_inter_full_table(
  mod    = m_post_score_int,
  focal  = "average_met_lag1",
  caption = "Synnytyksen jälkeen: unen laatu (Oura-score) — päävaikutukset, interaktiot ja aktiivisuuden ehdolliset vaikutukset",
  label   = "tab:post_score_full"
)
cat(tab_post_score_full)


tab_post_eff_full <- latex_main_inter_full_table(
  mod    = m_post_eff_int,
  focal  = "average_met",
  caption = "Synnytyksen jälkeen: unen tehokkuus — päävaikutukset, interaktiot ja aktiivisuuden ehdolliset vaikutukset",
  label   = "tab:post_eff_full"
)
cat(tab_post_eff_full)




