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

dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 


# Haetaan puhdisteut aineistot
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

# Uusi muuttuja
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


pregnancy$average_met_mean3_z <- as.numeric(scale(pregnancy$average_met_mean3))
postpartum$average_met_mean3_z <- as.numeric(scale(postpartum$average_met_mean3))


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


pregnancy$delivery_method <- factor(pregnancy$delivery_method,
                             levels = c(1, 2, 3),
                             labels = c("Vaginal", "Vacuum-assisted", "Caesarian section"))
pregnancy$delivery_method <- relevel(pregnancy$delivery_method, ref = "Vaginal")


postpartum$delivery_method <- factor(postpartum$delivery_method,
                                    levels = c(1, 2, 3),
                                    labels = c("Vaginal", "Vacuum-assisted", "Caesarian section"))
postpartum$delivery_method <- relevel(postpartum$delivery_method, ref = "Vaginal")


pregnancy  <- set_ref_levels_safe(pregnancy)
postpartum <- set_ref_levels_safe(postpartum)


# Tallennus kansio
out_dir <- file.path("figures", "taustamuuttujamallit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)



# Taustamuuttuja mallit
## PREGNANCY — DURATION

m_preg_dur_base <- lmer(
  duration ~ average_met_mean3_z +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    ns(week, df = 3) +
    (1 | id),
  data = pregnancy, REML = TRUE
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
m_preg_dur_int <- lmer(
  duration ~ average_met_mean3_z * (age_category + education + previous_children +
                                      epds_category + bmi_bl2 + gt_weight_gain) +
    ns(week, df = 3) +
    (1 | id),
  data = pregnancy, REML = TRUE
)
summary(m_preg_dur_int)

# Plot
coef_preg_dur_int <- plot_coef_forest_pretty(
  m_preg_dur_int,
  title = "Pregnancy: duration — kertoimet (interaktiot mukana)"
)
print(coef_preg_dur_int)
ggsave(file.path(out_dir, "coef_preg_dur_int.png"),
       coef_preg_dur_int, width = 10, height = 6, dpi = 300)

anova(m_preg_dur_base, m_preg_dur_int)


## PREGNANCY — SCORE

m_preg_score_base <- lmer(
  score ~ average_met_mean3_z +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    ns(week, df = 3) +
    (1 | id),
  data = pregnancy, REML = TRUE
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
m_preg_score_int <- lmer(
  score ~ average_met_mean3_z * (age_category + education + previous_children +
                                   epds_category + bmi_bl2 + gt_weight_gain) +
    ns(week, df = 3) +
    (1 | id),
  data = pregnancy, REML = TRUE
)
summary(m_preg_score_int)

# Plot
coef_preg_score_int <- plot_coef_forest_pretty(
  m_preg_score_int,
  title = "Pregnancy: score — kertoimet (interaktiot mukana)"
)
print(coef_preg_score_int)
ggsave(file.path(out_dir, "coef_preg_score_int.png"),
       coef_preg_score_int, width = 10, height = 6, dpi = 300)

anova(m_preg_score_base, m_preg_score_int)



## POSTPARTUM — DURATION

m_post_dur_base <- lmer(
  duration ~ average_met_mean3_z +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    pp_weight_lost + delivery_method +
    ns(week, df = 3) +
    (1 | id),
  data = postpartum, REML = TRUE
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
m_post_dur_int <- lmer(
  duration ~ average_met_mean3_z * (age_category + education + previous_children +
                                      epds_category + bmi_bl2 + gt_weight_gain +
                                      pp_weight_lost + delivery_method) +
    ns(week, df = 3) +
    (1 | id),
  data = postpartum, REML = TRUE
)
summary(m_post_dur_int)

# Plot
coef_post_dur_int <- plot_coef_forest_pretty(
  m_post_dur_int,
  title = "Postpartum: duration — kertoimet (interaktiot mukana)"
)
print(coef_post_dur_int)
ggsave(file.path(out_dir, "coef_post_dur_int.png"),
       coef_post_dur_int, width = 10, height = 6, dpi = 300)

anova(m_post_dur_base, m_post_dur_int)



## POSTPARTUM — SCORE

m_post_score_base <- lmer(
  score ~ average_met_mean3_z +
    age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain +
    pp_weight_lost + delivery_method +
    ns(week, df = 3) +
    (1 | id),
  data = postpartum, REML = TRUE
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
m_post_score_int <- lmer(
  score ~ average_met_mean3_z * (age_category + education + previous_children +
                                   epds_category + bmi_bl2 + gt_weight_gain +
                                   pp_weight_lost + delivery_method) +
    ns(week, df = 3) +
    (1 | id),
  data = postpartum, REML = TRUE
)
summary(m_post_score_int)

# Plot
coef_post_score_int <- plot_coef_forest_pretty(
  m_post_score_int,
  title = "Postpartum: score — kertoimet (interaktiot mukana)"
)
print(coef_post_score_int)
ggsave(file.path(out_dir, "coef_post_score_int.png"),
       coef_post_score_int, width = 10, height = 6, dpi = 300)

anova(m_post_score_base, m_post_score_int)




# Merkitsevät interaktiot
# Pregnancy — duration
plot_all_sig_interactions(
  m_preg_dur_int,
  model_tag = "preg_duration",
  focal    = "average_met_mean3_z",
  outdir   = out_dir,
  alpha    = 0.05
)

# Pregnancy — score
plot_all_sig_interactions(
  m_preg_score_int,
  model_tag = "preg_score",
  focal    = "average_met_mean3_z",
  outdir   = out_dir,
  alpha    = 0.05
)

# Postpartum — duration
plot_all_sig_interactions(
  m_post_dur_int,
  model_tag = "post_duration",
  focal    = "average_met_mean3_z",
  outdir   = out_dir,
  alpha    = 0.05
)

# Postpartum — score
plot_all_sig_interactions(
  m_post_score_int,
  model_tag = "post_score",
  focal    = "average_met_mean3_z",
  outdir   = out_dir,
  alpha    = 0.05
)




# EPDS-kategorian interaktio kaikissa neljässä mallissa
plot_interaction_facets(by = "epds_category",
                        focal = "average_met_mean3_z",
                        xlab = "Aktiivisuus (z)")

# Education-kategorian interaktio kaikissa neljässä mallissa
plot_interaction_facets(by = "education",
                        focal = "average_met_mean3_z",
                        xlab = "Aktiivisuus (z)")

# Previous-kategorian interaktio kaikissa neljässä mallissa
plot_interaction_facets(by = "previous_children",
                        focal = "average_met_mean3_z",
                        xlab = "Aktiivisuus (z)")



