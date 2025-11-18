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
# Voitaisiin harkita myös että miten muuttuu kohti loppua T3 refrenssiksi. Ns. Palautuminen. 
# contrasts(pregnancy$trimester) <- contr.sum(3) Ei refrenssiä vaan. Onko erillainen keskiarovon nähden. 
# Hierarkinen vaihtoehto: contrasts(pregnancy$trimester) <- contr.helmert(3)

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

# summaries 
summary(lme_duration)
summary(lme_score)
summary(lme_efficiency)

# Plots
trimester_cols <- c(
  T1 = golden_coast_colors[1],   
  T2 = golden_coast_colors[2],   
  T3 = golden_coast_colors[3]    
)

# Duration
xr <- seq(
  from = min(pregnancy$average_met_z, na.rm = TRUE),
  to   = max(pregnancy$average_met_z, na.rm = TRUE),
  length.out = 100
)

p_duration <- plot_predictions(
  lme_duration,
  condition = list(
    average_met_z = xr,
    trimester     = levels(pregnancy$trimester)
  )
) +
  scale_color_manual(values = trimester_cols) +
  scale_fill_manual(values = trimester_cols) +  
  labs(
    x = "average_met (z-skaalattu)",
    y = "Duration (h)",
    title = "Interaktio: avgMET × trimester",
    #subtitle = "Marginalisoitu yli kovariaattien (ml. week) jakauman"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank()
  )

# Estimaattien suuruudet dotwhisker plot
coefs <- tidy(lme_duration, effects = "fixed") %>% 
  filter(!grepl("ns\\(week", term)) %>% 
  filter(!grepl("(Intercept)", term))

coefs <- coefs %>% 
  mutate(term = case_when(
    #term == "(Intercept)" ~ "Intercept",
    term == "average_met_z" ~ "Average MET",
    term == "trimesterT2" ~ "Trimester: 2 vs 1",
    term == "trimesterT3" ~ "Trimester: 3 vs 1",
    term == "average_met_z:trimesterT2" ~ "AV_MET × Trimester 2",
    term == "average_met_z:trimesterT3" ~ "AV_MET × Trimester 3",
    TRUE ~ term
  ),
  term = fct_rev(factor(term)),
  sig  = if_else(p.value < 0.05, "sig", "ns"),
  model = sig)

coefs <- coefs %>% 
  mutate(term = fct_rev(factor(term)))

dw_duration <- dwplot(coefs) +
  scale_color_manual(values = c(
    "ns"  = "#1f77b4",  
    "sig" = "#FF6F20"  
  )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Duration",
    y = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")

#Score
xr <- seq(
  from = min(pregnancy$average_met_lag1_z, na.rm = TRUE),
  to   = max(pregnancy$average_met_lag1_z, na.rm = TRUE),
  length.out = 100
)

p_score <- plot_predictions(
  lme_score,
  condition = list(
    average_met_lag1_z = xr,
    trimester     = levels(pregnancy$trimester)
  )
) +
  scale_color_manual(values = trimester_cols) +
  scale_fill_manual(values = trimester_cols) +  
  labs(
    x = "average_met_lag1_z (z-skaalattu)",
    y = "Score",
    title = "Interaktio: avgMET × trimester",
    #subtitle = "Marginalisoitu yli kovariaattien (ml. week) jakauman"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank()
  )

# Estimaattien suuruudet dotwhisker plot
coefs <- tidy(lme_score, effects = "fixed") %>% 
  filter(!grepl("ns\\(week", term)) %>% 
  filter(!grepl("(Intercept)", term))

coefs <- coefs %>% 
  mutate(term = case_when(
    #term == "(Intercept)" ~ "Intercept",
    term == "average_met_lag1_z" ~ "MET_lag",
    term == "trimesterT2" ~ "Trimester: 2 vs 1",
    term == "trimesterT3" ~ "Trimester: 3 vs 1",
    term == "average_met_z:trimesterT2" ~ "MET_lag × Trimester 2",
    term == "average_met_z:trimesterT3" ~ "MET_lag × Trimester 3",
    TRUE ~ term
  ),
  term = fct_rev(factor(term)),
  sig  = if_else(p.value < 0.05, "sig", "ns"),
  model = sig)

coefs <- coefs %>%
  mutate(term = fct_rev(factor(term)))

dw_score <- dwplot(coefs) +
  scale_color_manual(values = c(
    "ns"  = "#1f77b4",  
    "sig" = "#FF6F20"  
  )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Score",
    y = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")

#Efficiency
xr <- seq(
  from = min(pregnancy$average_met_z, na.rm = TRUE),
  to   = max(pregnancy$average_met_z, na.rm = TRUE),
  length.out = 100
)

p_efficiency<- plot_predictions(
  lme_efficiency,
  condition = list(
    average_met_z = xr,
    trimester     = levels(pregnancy$trimester)
  )
) +
  scale_color_manual(values = trimester_cols) +
  scale_fill_manual(values = trimester_cols) +  
  labs(
    x = "average_met (z-skaalattu)",
    y = "efficiency"#,
    #subtitle = "Efficiency",
    #subtitle = "Marginalisoitu yli kovariaattien (ml. week) jakauman"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank()
  )

# Estimaattien suuruudet dotwhisker plot
coefs <- tidy(lme_efficiency, effects = "fixed") %>% 
  filter(!grepl("ns\\(week", term)) %>% 
  filter(!grepl("(Intercept)", term))

coefs <- coefs %>% 
  mutate(term = case_when(
    #term == "(Intercept)" ~ "Intercept",
    term == "average_met_z" ~ "Average MET",
    term == "trimesterT2" ~ "Trimester: 2 vs 1",
    term == "trimesterT3" ~ "Trimester: 3 vs 1",
    term == "average_met_z:trimesterT2" ~ "AV_MET × Trimester 2",
    term == "average_met_z:trimesterT3" ~ "AV_MET × Trimester 3",
    TRUE ~ term
  ),
  term = fct_rev(factor(term)),
  sig  = if_else(p.value < 0.05, "sig", "ns"),
  model = sig)

coefs <- coefs %>%
  mutate(term = fct_rev(factor(term)))

dw_efficiency <- dwplot(coefs) +
  scale_color_manual(values = c(
    "ns"  = "#1f77b4",  
    "sig" = "#FF6F20"  
  )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Efficiency",
    y = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")

#Kuvien teot
int1 <- p_duration + theme_golden_coast+labs(title = NULL)
int2 <- p_score + theme_golden_coast+labs(title = NULL)
int3 <- p_efficiency + theme_golden_coast+labs(title = NULL)

library(patchwork)
(int1 + int2 + int3) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom" 
  ) &
  plot_annotation(
    title = "Interaktiokuvat",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  )

dw1 <- dw_duration + theme_golden_coast +labs(title = NULL)
dw2 <- dw_score + theme_golden_coast +labs(title = NULL)
dw3 <- dw_efficiency + theme_golden_coast+labs(title = NULL)

(dw1+dw2+dw3) +
  plot_annotation(
    title = "Estimaatit",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  )&
  theme(
    legend.position = "none"
  )


