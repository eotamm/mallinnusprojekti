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


# Teema
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
# Voitaisiin harkita myös että miten muuttuu kohti loppua T3 refrenssiksi. Ns. Palautuminen. 
# contrasts(pregnancy$trimester) <- contr.sum(3) Ei refrenssiä vaan. Onko erillainen keskiarovon nähden. 
# Hierarkinen vaihtoehto: contrasts(pregnancy$trimester) <- contr.helmert(3)
# Helmert-kontrastit?


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

# Emmeans analyysi antaa kaikkien trimesterien vertailut. 
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

# Taulukko:  #Lisää luottamusväli taulukkoon ja jätä SE ja df pois taulukosta. 
duration_contrasts <- as.data.frame(em_duration$contrasts) %>%
  mutate(outcome = "Duration", .before = 1)

score_contrasts <- as.data.frame(em_score$contrasts) %>%
  mutate(outcome = "Score", .before = 1)

efficiency_contrasts <- as.data.frame(em_efficiency$contrasts) %>%
  mutate(outcome = "Efficiency", .before = 1)

# Lisää luottamusväli ja poista SE ja df
all_contrasts <- bind_rows(duration_contrasts, score_contrasts, efficiency_contrasts) %>%
  mutate(
    t_crit = qt(0.975, df),
    lower_95 = estimate - t_crit * SE,
    upper_95 = estimate + t_crit * SE,
    contrast = factor(contrast, levels = c("T1 - T2", "T1 - T3", "T2 - T3")),
    outcome = factor(outcome, levels = c("Duration", "Score", "Efficiency"))
  ) %>%
  select(outcome, contrast, estimate, lower_95, upper_95, p.value)



df_to_latex <- function(x,
                        caption = NULL,
                        label   = NULL,
                        digits  = 3) {
  knitr::kable(
    x,
    format  = "latex",
    booktabs = TRUE,
    caption = caption,
    label   = label,
    digits  = digits
  )
}
latex_koodi <- df_to_latex(all_contrasts,
                           caption = "Taulukko 1.",
                           label   = "tab:contrasts")



cat(latex_koodi)
# New plots. -------------------------------------------------------------------

# Duration trends
duration_trends <- as.data.frame(em_duration$emtrends) %>%
  mutate(outcome = "Duration (h)")

# Score trends  
score_trends <- as.data.frame(em_score$emtrends) %>%
  mutate(outcome = "Score")

# Efficiency trends
efficiency_trends <- as.data.frame(em_efficiency$emtrends) %>%
  mutate(outcome = "Efficiency")
# Emmeans trendi plot

all_trends <- bind_rows(duration_trends, score_trends, efficiency_trends) %>%
  mutate(outcome = factor(outcome, levels = c("Duration (h)", "Score", "Efficiency")))

trimester_cols <- c("#1E90FF", "#FF6F20", "#FFBBC1")


p_trend_duration <- ggplot(duration_trends, 
                       aes(x = trimester, y = average_met_z.trend, 
                           color = trimester, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_point(size = 4) +
  geom_line(color = "gray40", linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1) +
  scale_color_manual(values = trimester_cols) +
  facet_wrap(~outcome, scales = "free_y", ncol = 3) +  
  labs(
    x = "Raskauskolmannes",
    y = "Kaltevuus",
    title = "Liikunnan vaikutus uneen eri raskauskolmanneksilla",
    subtitle = "Estimoidut kaltevuudet ± 95% CI"
  ) +
  theme_golden_coast +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12)
  )
p_trend_score <- ggplot(score_trends, 
                           aes(x = trimester, y = average_met_lag1_z.trend,  
                               color = trimester, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_point(size = 4) +
  geom_line(color = "gray40", linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1) +
  scale_color_manual(values = trimester_cols) +
  facet_wrap(~outcome, scales = "free_y", ncol = 3) + 
  labs(
    x = "Raskauskolmannes",
    y = "Kaltevuus",
    title = "Liikunnan vaikutus uneen eri raskauskolmanneksilla",
    subtitle = "Estimoidut kaltevuudet ± 95% CI"
  ) +
  theme_golden_coast +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12)
  )

p_trend_eff <- ggplot(efficiency_trends, 
                           aes(x = trimester, y = average_met_z.trend, 
                               color = trimester, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_point(size = 4) +
  geom_line(color = "gray40", linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1) +
  scale_color_manual(values = trimester_cols) +
  facet_wrap(~outcome, scales = "free_y", ncol = 3) +  # <- scales = "free_y"!
  labs(
    x = "Raskauskolmannes",
    y = "Kaltevuus",
    title = "Liikunnan vaikutus uneen eri raskauskolmanneksilla",
    subtitle = "Estimoidut kaltevuudet ± 95% CI"
  ) +
  theme_golden_coast +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12)
  )

#Contrast plot? 
all_contrasts_plot <- all_contrasts

p_contrasts <- ggplot(all_contrasts_plot, 
                      aes(x = contrast, y = outcome, fill = estimate)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(round(estimate, 3), "\n")), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(
    low = golden_coast_colors[2],    # sininen negatiivisille
    mid = "white",                    # valkoinen nollalle
    high = golden_coast_colors[3],   # oranssi positiivisille
    midpoint = 0,
    name = "Ero kaltevuuksissa"
  ) +
  labs(
    x = "Trimester vertailu",
    y = "",
    title = "Kontrastit"
  ) +
  theme_golden_coast +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right"
  )
p_contrasts


# Duration
xr_duration <- seq(
  from = min(pregnancy$average_met_z, na.rm = TRUE),
  to   = max(pregnancy$average_met_z, na.rm = TRUE),
  length.out = 100
)

p_duration_improved <- plot_predictions(
  lme_duration,
  condition = list(
    average_met_z = xr_duration,
    trimester     = levels(pregnancy$trimester)
  )
) +
  scale_color_manual(values = trimester_cols, name = "Trimester") +
  scale_fill_manual(values = trimester_cols, name = "Trimester") +  
  labs(
    x = "Liikunta (z-skaalattu MET)",
    y = "Unen kesto (h)",
    title = ""
  ) +
  theme_golden_coast

# Score  
xr_score <- seq(
  from = min(pregnancy$average_met_lag1_z, na.rm = TRUE),
  to   = max(pregnancy$average_met_lag1_z, na.rm = TRUE),
  length.out = 100
)

p_score_improved <- plot_predictions(
  lme_score,
  condition = list(
    average_met_lag1_z = xr_score,
    trimester     = levels(pregnancy$trimester)
  )
) +
  scale_color_manual(values = trimester_cols, name = "Trimester") +
  scale_fill_manual(values = trimester_cols, name = "Trimester") +  
  labs(
    x = "Liikunta lag1 (z-skaalattu MET)",
    y = "Unen laatu (score)",
    title = ""
  ) +
  theme_golden_coast

# Efficiency
xr_eff <- seq(
  from = min(pregnancy$average_met_z, na.rm = TRUE),
  to   = max(pregnancy$average_met_z, na.rm = TRUE),
  length.out = 100
)

p_efficiency_improved <- plot_predictions(
  lme_efficiency,
  condition = list(
    average_met_z = xr_eff,
    trimester     = levels(pregnancy$trimester)
  )
) +
  scale_color_manual(values = trimester_cols, name = "Trimester") +
  scale_fill_manual(values = trimester_cols, name = "Trimester") +  
  labs(
    x = "Liikunta (z-skaalattu MET)",
    y = "Unen tehokkuus",
    title = ""
  ) +
  theme_golden_coast

p_combined_marginal <- (p_duration_improved + p_score_improved + p_efficiency_improved) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Liikunnan ja unen yhteys eri raskauskolmanneksilla",
    subtitle = "Marginaaliset ennusteet (marginalized over covariates)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  ) &
  theme(legend.position = "bottom")

print(p_combined_marginal)
ggsave("trimester_marginal_effects.png", p_combined_marginal, width = 15, height = 5, dpi = 300)



# Lopulliset kuvat raporttia varten.
# Trimester trendit: 
library(patchwork)

p_dur  <- p_trend_duration + labs(title = NULL, subtitle = NULL, x=NULL)
p_score <- p_trend_score   + labs(title = NULL, subtitle = NULL, y=NULL)
p_eff  <- p_trend_eff      + labs(title = NULL, subtitle = NULL, x=NULL, y=NULL)

(p_dur + p_score + p_eff) 

# ANOVA taulukot. 
a_duration    <- car::Anova(lme_duration,   type = 3)
a_score       <- car::Anova(lme_score,      type = 3)
a_efficiency  <- car::Anova(lme_efficiency, type = 3)

library(dplyr)
library(tibble)

extract_interaction <- function(model, outcome_label) {
  car::Anova(model, type = 3) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    filter(grepl(":trimester", term)) %>%
    transmute(
      Vaste = outcome_label,
      `χ²`  = Chisq,
      df    = Df,
      `p-arvo` = `Pr(>Chisq)`
    )
}

anova_interaction_table <- bind_rows(
  extract_interaction(lme_duration,   "Unen kesto"),
  extract_interaction(lme_score,      "Unen laatu"),
  extract_interaction(lme_efficiency, "Unen tehokkuus")
)

anova_interaction_table
df_to_latex(anova_interaction_table, caption = "Type III -Wald-testit liikunnan ja raskauskolmanneksen väliselle interaktiolle erikseen sovitetuissa lineaarisissa sekamalleissa eri vastemuuttujille.",
            label   = "tab:ANOVA")

# Efekti taulukko emtrends.


get_trends <- function(mod, var, outcome){
  emtrends(mod, ~ trimester, var = var) |>
    summary(infer = TRUE) |>
    as.data.frame() |>
    transmute(
      Vaste = outcome,
      Raskauskolmannes = trimester,
      Kaltevuus = !!sym(paste0(var, ".trend")),
      lower_95 = lower.CL,
      upper_95 = upper.CL
    )
}

trends_table <- bind_rows(
  get_trends(lme_duration,   "average_met_z",      "Unen kesto"),
  get_trends(lme_score,      "average_met_lag1_z", "Unen laatu"),
  get_trends(lme_efficiency, "average_met_z",      "Unen tehokkuus")
)

trends_table
df_to_latex(trends_table, caption = "Liikunnan ja unen välinen yhteys eri raskauskolmanneksissa. Taulukossa esitetään liikunnan raskauskolmanneskohtaiset vaikutukset uneen kaltevuuksina sekä niiden 95 \\%:n luottamusvälit, estimoituna marginaalisina trendeinä (emtrends) erikseen sovitetuista lineaarisista sekamalleista. Altistemuuttujana on unen kestolle ja tehokkuudelle z-standardoitunut MET-arvo ja unen laadulle edellisen päivän z-standardoitunut MET-arvo.",
            label   = "tab:emtrends")

# VarCorr(lme_duration)
# VarCorr(lme_score)
# VarCorr(lme_efficiency)

