library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(splines)

#set filepath
#dir.path <- file.path("") # symlink 

pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### pregnancy ####

#### Duration all ####

df_temp = pregnancy %>% filter(!is.na(duration))
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ika_alle30 #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(age_category=="Under 30")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ika_30_tai_yli #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(age_category=="30 or more")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_koulutus_2aste #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(education=="1")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_koulutus_korkeakoulutettu #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(education=="2")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ei_aiempaa_lasta #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(previous_children=="0")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_on_aiempi_lapsi #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(previous_children=="1")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ei_masennusta #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(epds_category=="No depression")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_mahdollinen_masennus #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(epds_category=="Possible depr")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ylipainoiset #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(bmi_bl2 == "1")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_lihavat #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(bmi_bl2 == "2")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_painonnousu_suunnitelman_mukainen #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(gt_weight_gain == "within or less")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_painonnousu_suositus_ylittyi #### 

df_temp = pregnancy %>% filter(!is.na(duration)) %>% filter(gt_weight_gain == "more than recommendat")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### postpartum ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### Duration all ####

df_temp = postpartum %>% filter(!is.na(duration))
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ika_alle30 #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(age_category=="Under 30")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ika_30_tai_yli #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(age_category=="30 or more")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_koulutus_2aste #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(education=="1")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_koulutus_korkeakoulutettu #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(education=="2")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ei_aiempaa_lasta #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(previous_children=="0")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_on_aiempi_lapsi #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(previous_children=="1")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ei_masennusta #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(epds_category=="No depression")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_mahdollinen_masennus #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(epds_category=="Possible depr")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_ylipainoiset #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(bmi_bl2 == "1")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_lihavat #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(bmi_bl2 == "2")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_painonnousu_suunnitelman_mukainen #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(gt_weight_gain == "within or less")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

#### Duration_painonnousu_suositus_ylittyi #### 

df_temp = postpartum %>% filter(!is.na(duration)) %>% filter(gt_weight_gain == "more than recommendat")
#duration tunneiksi
df_temp$duration = df_temp$duration/(60*60)

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$duration),2),
  keskihajonta = round(sd(df_temp$duration),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


#### Efficiency ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### pregnancy ####

#### Efficiency all ####

df_temp = pregnancy %>% filter(!is.na(efficiency))

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ika_alle30 #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(age_category=="Under 30")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ika_30_tai_yli #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(age_category=="30 or more")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_koulutus_2aste #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(education=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_koulutus_korkeakoulutettu #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(education=="2")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ei_aiempaa_lasta #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(previous_children=="0")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_on_aiempi_lapsi #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(previous_children=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ei_masennusta #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(epds_category=="No depression")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_mahdollinen_masennus #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(epds_category=="Possible depr")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ylipainoiset #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(bmi_bl2 == "1")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_lihavat #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(bmi_bl2 == "2")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_painonnousu_suunnitelman_mukainen #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(gt_weight_gain == "within or less")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_painonnousu_suositus_ylittyi #### 

df_temp = pregnancy %>% filter(!is.na(efficiency)) %>% filter(gt_weight_gain == "more than recommendat")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### postpartum ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### Efficiency all ####

df_temp = postpartum %>% filter(!is.na(efficiency))



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ika_alle30 #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(age_category=="Under 30")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ika_30_tai_yli #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(age_category=="30 or more")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_koulutus_2aste #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(education=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_koulutus_korkeakoulutettu #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(education=="2")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ei_aiempaa_lasta #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(previous_children=="0")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_on_aiempi_lapsi #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(previous_children=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ei_masennusta #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(epds_category=="No depression")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_mahdollinen_masennus #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(epds_category=="Possible depr")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_ylipainoiset #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(bmi_bl2 == "1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_lihavat #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(bmi_bl2 == "2")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_painonnousu_suunnitelman_mukainen #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(gt_weight_gain == "within or less")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

#### Efficiency_painonnousu_suositus_ylittyi #### 

df_temp = postpartum %>% filter(!is.na(efficiency)) %>% filter(gt_weight_gain == "more than recommendat")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$efficiency),2),
  keskihajonta = round(sd(df_temp$efficiency),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


#### Score ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### pregnancy ####

#### Score all ####

df_temp = pregnancy %>% filter(!is.na(score))

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ika_alle30 #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(age_category=="Under 30")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ika_30_tai_yli #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(age_category=="30 or more")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_koulutus_2aste #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(education=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_koulutus_korkeakoulutettu #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(education=="2")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ei_aiempaa_lasta #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(previous_children=="0")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_on_aiempi_lapsi #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(previous_children=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ei_masennusta #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(epds_category=="No depression")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_mahdollinen_masennus #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(epds_category=="Possible depr")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ylipainoiset #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(bmi_bl2 == "1")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_lihavat #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(bmi_bl2 == "2")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_painonnousu_suunnitelman_mukainen #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(gt_weight_gain == "within or less")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_painonnousu_suositus_ylittyi #### 

df_temp = pregnancy %>% filter(!is.na(score)) %>% filter(gt_weight_gain == "more than recommendat")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### postpartum ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### Score all ####

df_temp = postpartum %>% filter(!is.na(score))



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ika_alle30 #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(age_category=="Under 30")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ika_30_tai_yli #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(age_category=="30 or more")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_koulutus_2aste #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(education=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_koulutus_korkeakoulutettu #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(education=="2")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ei_aiempaa_lasta #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(previous_children=="0")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_on_aiempi_lapsi #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(previous_children=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ei_masennusta #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(epds_category=="No depression")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_mahdollinen_masennus #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(epds_category=="Possible depr")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_ylipainoiset #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(bmi_bl2 == "1")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_lihavat #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(bmi_bl2 == "2")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_painonnousu_suunnitelman_mukainen #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(gt_weight_gain == "within or less")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

#### Score_painonnousu_suositus_ylittyi #### 

df_temp = postpartum %>% filter(!is.na(score)) %>% filter(gt_weight_gain == "more than recommendat")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$score),2),
  keskihajonta = round(sd(df_temp$score),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### Steps ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### pregnancy ####

#### Steps all ####

df_temp = pregnancy %>% filter(!is.na(steps))
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ika_alle30 #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(age_category=="Under 30")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ika_30_tai_yli #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(age_category=="30 or more")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_koulutus_2aste #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(education=="1")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_koulutus_korkeakoulutettu #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(education=="2")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ei_aiempaa_lasta #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(previous_children=="0")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_on_aiempi_lapsi #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(previous_children=="1")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ei_masennusta #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(epds_category=="No depression")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_mahdollinen_masennus #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(epds_category=="Possible depr")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ylipainoiset #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(bmi_bl2 == "1")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_lihavat #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(bmi_bl2 == "2")
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_painonnousu_suunnitelman_mukainen #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(gt_weight_gain == "within or less")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_painonnousu_suositus_ylittyi #### 

df_temp = pregnancy %>% filter(!is.na(steps)) %>% filter(gt_weight_gain == "more than recommendat")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### postpartum ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### Steps all ####

df_temp = postpartum %>% filter(!is.na(steps))
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ika_alle30 #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(age_category=="Under 30")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ika_30_tai_yli #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(age_category=="30 or more")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_koulutus_2aste #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(education=="1")
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_koulutus_korkeakoulutettu #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(education=="2")
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ei_aiempaa_lasta #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(previous_children=="0")
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_on_aiempi_lapsi #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(previous_children=="1")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ei_masennusta #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(epds_category=="No depression")
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_mahdollinen_masennus #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(epds_category=="Possible depr")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_ylipainoiset #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(bmi_bl2 == "1")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_lihavat #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(bmi_bl2 == "2")
df_temp$steps = df_temp$steps/1000


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_painonnousu_suunnitelman_mukainen #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(gt_weight_gain == "within or less")
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

#### Steps_painonnousu_suositus_ylittyi #### 

df_temp = postpartum %>% filter(!is.na(steps)) %>% filter(gt_weight_gain == "more than recommendat")
df_temp$steps = df_temp$steps/1000

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$steps),2),
  keskihajonta = round(sd(df_temp$steps),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


#### Average\_met ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### pregnancy ####

#### Average\_met all ####

df_temp = pregnancy %>% filter(!is.na(average_met))


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ika_alle30 #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(age_category=="Under 30")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ika_30_tai_yli #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(age_category=="30 or more")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_koulutus_2aste #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(education=="1")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_koulutus_korkeakoulutettu #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(education=="2")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ei_aiempaa_lasta #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(previous_children=="0")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_on_aiempi_lapsi #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(previous_children=="1")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ei_masennusta #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(epds_category=="No depression")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_mahdollinen_masennus #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(epds_category=="Possible depr")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ylipainoiset #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(bmi_bl2 == "1")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_lihavat #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(bmi_bl2 == "2")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_painonnousu_suunnitelman_mukainen #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(gt_weight_gain == "within or less")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_painonnousu_suositus_ylittyi #### 

df_temp = pregnancy %>% filter(!is.na(average_met)) %>% filter(gt_weight_gain == "more than recommendat")

stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)

#### postpartum ####

stats_df <- data.frame(
  Ryhmä = character(),
  n = numeric(),
  keskiarvo = numeric(),
  keskihajonta = numeric(),
  stringsAsFactors = FALSE
)

#### Average\_met all ####

df_temp = postpartum %>% filter(!is.na(average_met))


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Kaikki",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ika_alle30 #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(age_category=="Under 30")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_alle30",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ika_30_tai_yli #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(age_category=="30 or more")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ikä_30_tai_yli",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_koulutus_2aste #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(education=="1")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_2aste_tai_alle",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_koulutus_korkeakoulutettu #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(education=="2")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Koulutus_korkeakoulutus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ei_aiempaa_lasta #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(previous_children=="0")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_aiempaa_lasta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_on_aiempi_lapsi #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(previous_children=="1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "On_aiempi_lapsi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ei_masennusta #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(epds_category=="No depression")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ei_masennusta",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_mahdollinen_masennus #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(epds_category=="Possible depr")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Mahdollinen_masennus",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_ylipainoiset #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(bmi_bl2 == "1")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Ylipainoiset",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_lihavat #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(bmi_bl2 == "2")



stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Lihavat",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_painonnousu_suunnitelman_mukainen #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(gt_weight_gain == "within or less")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositusten_mukainen",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

#### Average\_met_painonnousu_suositus_ylittyi #### 

df_temp = postpartum %>% filter(!is.na(average_met)) %>% filter(gt_weight_gain == "more than recommendat")


stats_df <- rbind(stats_df, data.frame(
  Ryhmä = "Painonnousu_suositus_ylittyi",
  n = nrow(df_temp),
  keskiarvo = round(mean(df_temp$average_met),2),
  keskihajonta = round(sd(df_temp$average_met),2),
  stringsAsFactors = FALSE
))

# Latex table

library(knitr)
kable(stats_df, format = "latex", booktabs = TRUE)


