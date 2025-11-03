#' Tässä dokumentissa tutkitaan muttuuko liikunnan ja unen assosiaatio eri raskauskolmanneksilla (trimester).
#' Raskauskolmannes on otettu mukaan faktorina joka on koodattu T1, T2 ja T3 kolmanneksen mukaan. 
#' Malliin on valittu, mallin perusosa, jossa on mukana liikunta hyväksi aikaisemmin hyväksi todetussa muodossa. 
#' Tämä muoto on liukuva keskiarvo avarage_met muttujasta. 
#' 

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

#' Ei vielä sovittuja kysymyksiä, jotka voivat vaikuttaa lopulliseen malliin:
#' 1. Otetaanko autokorrelaatio huomioon. 
#' Skaalataanko vastemuuttuja?
#' Millä tavalla p-arvot korjataan tai korjataanko? 
#' 

dir.path <- file.path("./data") # path
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% 
  as.data.frame() %>% 
  mutate(
    trimester = as.factor(case_when(
  week <= 12 ~ "T1",
  week >= 13 & week <= 27 ~ "T2",
  week >= 28 ~ "T3"
)), 
avgmet_z=as.numeric(scale(average_met)),
duration=duration/3600
) %>% 
  arrange(id, summary_date) %>%       
  group_by(id) %>%
  mutate(
    average_met_mean3 = zoo::rollapply(
      average_met,
      width = 3,                     
      FUN = mean,
      align = "right",               
      fill = NA                      
    )
  ) %>%
  ungroup() %>% 
  dplyr::mutate(avgmet_mean3_z=as.numeric(scale(average_met_mean3)))
  

# Asetetaa refrenssi trimesteri. 
pregnancy$trimester <- relevel(pregnancy$trimester, ref = "T1")

# Miten muuttuu suhteessa raskauden alkuun. 
# Voitaisiin harkita myös että miten muuttuu kohti loppua T3 refrenssiksi. Ns. Palautuminen. 
# contrasts(pregnancy$trimester) <- contr.sum(3) Ei refrenssiä vaan. Onko erillainen keskiarovon nähden. 
# Hierarkinen vaihtoehto: contrasts(pregnancy$trimester) <- contr.helmert(3)

# Malleihin pitää varmaan listätä vielä unen autokorrelaatio termi. 

# Duration 

# lmer1<- lmer(duration~ average_met_mean3 * trimester+(1|id), data=pregnancy, REML = TRUE ) # Ilman viikon vaikutusta. 
# summary(lmer1)

lmer_duration<- lmer(duration~ average_met_mean3 * trimester+ns(week, df = 3)+(1|id), data=pregnancy, REML = TRUE )
summary(lmer_duration)

# Viikon vaikutuksen kuvaaja. 
plot(effect("week", lmer_duration), main = "Raskausviikon vaikutus unen pituuteen")

# Kannattaako epälineaarinen aika pitää mukana? 
anova(
  update(lmer_duration, . ~ . - ns(week, df = 3)), 
  lmer_duration
)
# Trimesterin yksittäinen arvo... 
xr <- seq(
  quantile(pregnancy$average_met_mean3, .025, na.rm = TRUE),
  quantile(pregnancy$average_met_mean3, .975, na.rm = TRUE),
  length.out = 80
)

plot_predictions(
  lmer_duration,
  condition = list(average_met_mean3 = xr, trimester = levels(pregnancy$trimester)),
  re.form = NA
) +
  ggplot2::labs(
    x = "average_met_mean3 (z)",
    y = "duration (h)",
    title = "Interaktio: avgMET × trimester",
    subtitle = "Marginalisoitu yli havaittujen kovariaattien (ml. week) jakauman"
  ) +
  ggplot2::theme_minimal(base_size = 13)

# Estimaattien suuruudet dotwhisker plot
coefs <- tidy(lmer_duration, effects = "fixed") %>% 
  filter(!grepl("ns\\(week", term)) %>% 
  filter(!grepl("(Intercept)", term))

coefs <- coefs %>% 
  mutate(term = case_when(
    #term == "(Intercept)" ~ "Intercept",
    term == "average_met_mean3" ~ "Average MET",
    term == "trimesterT2" ~ "Trimester: 2 vs 1",
    term == "trimesterT3" ~ "Trimester: 3 vs 1",
    term == "average_met_mean3:trimesterT2" ~ "AV_MET × Trimester 2",
    term == "average_met_mean3:trimesterT3" ~ "AV_MET × Trimester 3",
    TRUE ~ term
  ))

coefs <- coefs |>
  mutate(term = fct_rev(factor(term)))

dwplot(coefs) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    title = "Mallin kiinteiden efektien estimaatit (duration (h))",
    x = "Estimaatti (tunteina)",
    y = ""
  ) +
  theme_bw()+ # pitäisikö muuttaa teemaa? 
  theme(legend.position = "none")



# Score
# lmer2<- lmer(score~ average_met_mean3 * trimester+(1|id), data=pregnancy, REML = TRUE )
# summary(lmer1)

lmer_score<- lmer(score~ average_met_mean3 * trimester+ns(week, df = 3)+(1|id), data=pregnancy, REML = TRUE )
summary(lmer_score)
plot(effect("ns(week, df = 3)", lmer_score), main = "Raskausviikon vaikutus unen laatuun")

anova(
  update(lmer_score, . ~ . - ns(week, df = 3)), 
  lmer_score
)
# Interaktio kuva
xr <- seq(
  quantile(pregnancy$average_met_mean3, .025, na.rm = TRUE),
  quantile(pregnancy$average_met_mean3, .975, na.rm = TRUE),
  length.out = 80
)

plot_predictions(
  lmer_score,
  condition = list(
    average_met_mean3 = xr,
    trimester = levels(pregnancy$trimester)
  ),
  re.form = NA
) +
  ggplot2::labs(
    x = "3 vrk:n keskiarvo MET",
    y = "Score",
    title = "Interaktio: avgMET × trimester (score)",
    subtitle = "" #Marginalisoitu yli havaittujen kovariaattien (ml. week) jakauman
  ) +
  ggplot2::theme_minimal(base_size = 13)# +
  #theme(legend.position = "none")   

# Estimaatti kuva: 
coefs_score <- tidy(lmer_score, effects = "fixed") |>
  dplyr::filter(!grepl("ns\\(week", term)) |>     
  dplyr::filter(!grepl("(Intercept)", term))      

coefs_score <- coefs_score |>
  dplyr::mutate(term = dplyr::case_when(
    term == "average_met_mean3" ~ "Average MET",
    term == "trimesterT2" ~ "Trimester: 2 vs 1",
    term == "trimesterT3" ~ "Trimester: 3 vs 1",
    term == "average_met_mean3:trimesterT2" ~ "AV_MET × Trimester 2",
    term == "average_met_mean3:trimesterT3" ~ "AV_MET × Trimester 3",
    TRUE ~ term
  )) |>
  dplyr::mutate(term = forcats::fct_rev(factor(term)))

dwplot(coefs_score) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    title = "Mallin kiinteiden efektien estimaatit (score)",
    x = "Estimaatti (score[0-100])",
    y = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")

