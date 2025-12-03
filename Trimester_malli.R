#' Tässä dokumentissa tutkitaan muttuuko liikunnan ja unen assosiaatio eri raskauskolmanneksilla (trimester).
#' Raskauskolmannes on otettu mukaan faktorina joka on koodattu T1, T2 ja T3 kolmanneksen mukaan. 
#' Malliin on valittu, mallin perusosa, jossa on mukana liikunta hyväksi aikaisemmin hyväksi todetussa muodossa. 
#' Tämä muoto on liukuva keskiarvo avarage_met muttujasta. 

{
library(tidyverse) # koodin kirjoittaminen 
library(dplyr) # koodin kirjoittaminen 
library(rlang) 
library(ggplot2) # visualisointi
library(patchwork) # visualisointi
library(lme4)
library(geepack)
library(splines) # ajan epälineaariselle mallintamiselle. 
library(nlme)
library(zoo) # liukuvalle keskiarvolle. 
library(lmerTest) # p-arvoille 
library(marginaleffects) # visualisoinneille.   
library(effects)
library(dotwhisker)
library(broom.mixed)
library(emmeans) # parittaisille vertailuille. 
library(car) # ANOVA. 
}

#' Ei vielä sovittuja kysymyksiä, jotka voivat vaikuttaa lopulliseen malliin:
#' 1. Otetaanko autokorrelaatio huomioon. 
#' Skaalataanko vastemuuttuja?
#' Millä tavalla p-arvot korjataan tai korjataanko? 

#Theme
golden_coast_colors <- c("#FFD700", "#1E90FF", "#FF6F20", "#FFBBC1", "#2082AA") 
theme_golden_coast <- theme_minimal(base_size = 13) +
  theme(
    text = element_text(color = golden_coast_colors[5]),
    plot.title = element_text(face = "bold", color = golden_coast_colors[1]), #golden_coast_colors[1]
    axis.title = element_text(color = golden_coast_colors[5]),
    axis.text  = element_text(color = golden_coast_colors[5]),
    panel.grid.major = element_line(color = scales::alpha(golden_coast_colors[5], 0.08)),
    panel.grid.minor = element_line(color = scales::alpha(golden_coast_colors[5], 0.04)),
    legend.title = element_blank()
  )


dir.path <- file.path("./data") # path
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% 
  as.data.frame() %>% 
  mutate(
    trimester = as.factor(case_when(
  week <= 12 ~ "T1",
  week >= 13 & week <= 27 ~ "T2",
  week >= 28 ~ "T3"
)),
average_met_z=as.numeric(scale(average_met)),
duration=duration/3600
) %>% 
  arrange(id, summary_date) %>% 
  group_by(id) %>% 
  mutate(avarage_met_lag1 = lag(average_met, n = 1)) %>% 
  mutate(average_met_lag1_z=scale(avarage_met_lag1)) %>% 
  ungroup()
  

# Asetetaa refrenssi trimesteri. 
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
pregnancy  <- set_ref_levels_safe(pregnancy)
pregnancy$delivery_method <- factor(pregnancy$delivery_method,
                                    levels = c(1, 2, 3),
                                    labels = c("Vaginal", "Vacuum-assisted", "Caesarian section"))
pregnancy$delivery_method <- relevel(pregnancy$delivery_method, ref = "Vaginal")
# Miten muuttuu suhteessa raskauden alkuun. 
pregnancy$trimester <- relevel(pregnancy$trimester, ref = "T1")
# Päätös on käyttää T1 refrenssinä ja katsoa kaikki vertailut eri trimesterien välillä emmeansin avulla. 

# MALLIT------------------------------------------------------------------------

# Duration 
lme_duration<- lme(
  fixed = duration ~ average_met_z*trimester+ns(week, df=3)+ age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain,
  random=~1|id,
  correlation=corCAR1(form=~as.numeric(summary_date)|id),
  data=pregnancy,
  method="REML", 
  na.action=na.omit
)
# Score
lme_score<- lme(
  fixed = score ~ average_met_lag1_z*trimester +ns(week, df=3)+age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain,
  random=~1|id,
  correlation=corCAR1(form=~as.numeric(summary_date)|id),
  data= pregnancy,
  method="REML", 
  na.action=na.omit
)

# Efficiency
lme_efficiency <- lme(
  fixed = efficiency ~ average_met_z*trimester+ns(week, df=3)+age_category + education + previous_children +
    epds_category + bmi_bl2 + gt_weight_gain,
  random=~1|id,
  correlation=corCAR1(form=~as.numeric(summary_date)|id),
  data= pregnancy,
  method="REML", 
  na.action=na.omit
)
#-------------------------------------------------------------------------------
# summaries ja interaktio testi
summary(lme_duration)
Anova(lme_duration, type = 3)
summary(lme_score)
Anova(lme_score, type = 3)
summary(lme_efficiency)
Anova(lme_efficiency, type = 3)

# Emmeans analyysi antaa kaikkien trimesterien vertailut. Käytetty korjaus on fdr. 
em_duration <- emtrends(lme_duration, 
                        pairwise ~ trimester, 
                        var = "average_met_z",
                        adjust = "fdr")
em_score <- emtrends(lme_score, 
                     pairwise ~ trimester, 
                     var = "average_met_lag1_z",
                     adjust = "fdr")
em_efficiency <- emtrends(lme_efficiency, 
                          pairwise ~ trimester, 
                          var = "average_met_z",
                          adjust = "fdr")

print(summary(em_duration$emtrends))
print(summary(em_duration$contrasts))

print(summary(em_score$emtrends))
print(summary(em_score$contrasts))

print(summary(em_efficiency$emtrends))
print(summary(em_efficiency$contrasts))

# Taulukko: 
duration_contrasts <- as.data.frame(em_duration$contrasts) %>%
  mutate(outcome = "Duration", .before = 1)

score_contrasts <- as.data.frame(em_score$contrasts) %>%
  mutate(outcome = "Score", .before = 1)

efficiency_contrasts <- as.data.frame(em_efficiency$contrasts) %>%
  mutate(outcome = "Efficiency", .before = 1)

all_contrasts <- bind_rows(duration_contrasts, score_contrasts, efficiency_contrasts) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    ),
    contrast = factor(contrast, levels = c("T1 - T2", "T1 - T3", "T2 - T3")),
    outcome = factor(outcome, levels = c("Duration", "Score", "Efficiency"))
  ) %>%
  select(outcome, contrast, estimate, SE, df ,t.ratio, p.value, sig)# lower.CL, upper.CL,
