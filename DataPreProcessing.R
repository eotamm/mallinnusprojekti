# Datan tuonti ja alkukäsittely. Alkuperäisesti saatu aineisto sijaitsee seafile kansiossa projekti kurssi ja tiedostossa siihen osoitta symlinkki "data". 
# Alkuperäinen jaettu muoto on sas tiedosto.sas7bdat.

library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(readxl)
library(readr)
library(purrr)

#Apufunktioita.
source("PreProcessingFuncs.R")
dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 

stopifnot(dir.exists(dir.path)) # tarkasta

activity_pregnancy  <- read_sas(file.path(dir.path, "activity_pregnancy.sas7bdat")) %>% as.data.frame() %>% distinct()
activity_postpartum <- read_sas(file.path(dir.path, "activity_postpartum.sas7bdat")) %>% as.data.frame() %>% distinct()
sleep_pregnancy     <- read_sas(file.path(dir.path, "sleep_pregnancy.sas7bdat")) %>% as.data.frame() %>% distinct()
sleep_postpartum    <- read_sas(file.path(dir.path, "sleep_postpartum.sas7bdat")) %>% as.data.frame() %>% distinct()

uniikit_idt <- activity_pregnancy %>% dplyr::filter(id != 0) %>% dplyr::pull(id) %>% unique()


# Valitaan aluksi score 
unen_laatu <- map_dfr(uniikit_idt, function(id) {
  # Tiedoston nimi: 
  tiedoston_nimi <- paste0("user",id, "_sleep.csv")
  
  # Polku tiedostoon
  path_uni <- file.path(dir.path, "CSV_muokatut_nimet", id, tiedoston_nimi)
  
  # Lue tiedosto ja valitse tarvittavat sarakkeet
  
  # Jos tiedosto puuttuu 
  if (!file.exists(path_uni)) {
    message("Tiedostoa ei löydy: ", path_uni)
    return(NULL)
  }
  
  # Jos tiedosto löytyy
  read_csv(path_uni, show_col_types = FALSE) %>%
    select(summary_date, score) %>%
    mutate(id = id)
  
})

#113 ja 130 puuttuvat. 

# Sunnitelma puutuvien ID -arvoje ja viikkonumerojen korjaamiseen. Pregnancy aineisto. 
# Huom! Raskausviikkojen viikko ei ala välttämättä maanantai päivällä. Vaan esimerkiksi id(101) ensimmäinen päivä (2021-04-07) on keskiviikko. 
# weekdays(as.Date("2021-04-07")) 
# Tämä tiedosto vaatii tiedoston PreProcessingFuncs.R



#:______________________________________________________________________________
#Tarkistus: (Huom! Tässä päivä numerot menevät seuraavasti: 1: Maanantai, 2: Tiistai, 3: Keskiviikko, 4: Torstai, 5: Perjanta, 6: Lauantai ja 7: Sunnuntai.)
#Tallennetaan kaikki uniikit ID arvot.  
# Haetaan aineisto. 

data <- activity_pregnancy 
data1 <- NULL
for(i in uniikit_idt){
  data_filtered <- data %>% dplyr::mutate(
    summary_date=as.Date(summary_date),
    id=as.character(id),
    week=as.numeric(week)
  ) %>% 
    dplyr::filter(id==i) %>% 
    dplyr::arrange(summary_date)
  
  # Täydentää puuttuvat päivät keskeltä
  data_filtered<- taydenna_pv(data_filtered)
  
  # Täydentää päivät alkuun ja viikko numerot. 
  data_filtered<- taydenna_viikot(data_filtered)
  
  # Lisää rivi kerrallaan data1:een
  if (is.null(data1)) {
    data1 <- data_filtered
  } else {
    data1 <- bind_rows(data1, data_filtered)  # yhdistää rivit, ei sarakkeita
  }
  
}
#data1 %>% View()
activity_pregnancy <- data1


uniikit_idt <- activity_postpartum %>% dplyr::filter(id != 0) %>% dplyr::pull(id) %>% unique()
data <- activity_postpartum
data1 <- NULL
for(i in uniikit_idt){
  
  data_filtered <- data %>% dplyr::mutate(
    summary_date=as.Date(summary_date),
    id=as.character(id),
    week=as.numeric(week)
  ) %>% 
    dplyr::filter(id==i) %>% 
    dplyr::arrange(summary_date)
  
  # Täydentää puuttuvat päivät keskeltä
  data_filtered<- taydenna_pv(data_filtered)
  
  # Täydentää päivät alkuun ja viikko numerot. 
  data_filtered<- taydenna_viikot(data_filtered)
  
  # Lisää rivi kerrallaan data1:een
  if (is.null(data1)) {
    data1 <- data_filtered
  } else {
    data1 <- bind_rows(data1, data_filtered)  # yhdistää rivit, ei sarakkeita
  }
  
}
#data1 %>% View()
activity_postpartum <- data1

uniikit_idt <- sleep_pregnancy %>% dplyr::filter(id != 0) %>% dplyr::pull(id) %>% unique()
data <- sleep_pregnancy
data1 <- NULL
for(i in uniikit_idt){
  
  data_filtered <- data %>% dplyr::mutate(
    summary_date=as.Date(summary_date),
    id=as.character(id),
    week=as.numeric(week)
  ) %>% 
    dplyr::filter(id==i) %>% 
    dplyr::arrange(summary_date)
  
  # Täydentää puuttuvat päivät keskeltä
  data_filtered<- taydenna_pv(data_filtered)
  
  # Täydentää päivät alkuun ja viikko numerot. 
  data_filtered<- taydenna_viikot(data_filtered)
  
  # Lisää rivi kerrallaan data1:een
  if (is.null(data1)) {
    data1 <- data_filtered
  } else {
    data1 <- bind_rows(data1, data_filtered)  # yhdistää rivit, ei sarakkeita
  }
  
}
#data1 %>% View()
sleep_pregnancy <- data1

uniikit_idt <- sleep_postpartum %>% dplyr::filter(id != 0) %>% dplyr::pull(id) %>% unique()
data <- sleep_postpartum
data1 <- NULL
for(i in uniikit_idt){
  
  data_filtered <- data %>% dplyr::mutate(
    summary_date=as.Date(summary_date),
    id=as.character(id),
    week=as.numeric(week)
  ) %>% 
    dplyr::filter(id==i) %>% 
    dplyr::arrange(summary_date)
  
  # Täydentää puuttuvat päivät keskeltä
  data_filtered<- taydenna_pv(data_filtered)
  
  # Täydentää päivät alkuun ja viikko numerot. 
  data_filtered<- taydenna_viikot(data_filtered)
  
  # Lisää rivi kerrallaan data1:een
  if (is.null(data1)) {
    data1 <- data_filtered
  } else {
    data1 <- bind_rows(data1, data_filtered)  # yhdistää rivit, ei sarakkeita
  }
  
}
#data1 %>% View()
sleep_postpartum <- data1

#Aineistojen yhdistäminen 

pregnancy<- yhdistys(activity_pregnancy, sleep_pregnancy)
postpartum<- yhdistys(activity_postpartum, sleep_postpartum)

# Yhdistä puuttuva muuttuja niiltä osin kuin on
pregnancy <- pregnancy %>%
  dplyr::left_join(
    unen_laatu %>%
      dplyr::mutate(
        id = as.character(id),
        summary_date = as.Date(summary_date)
      ) %>%
      dplyr::select(id, summary_date, score),
    by = c("id", "summary_date")
  )

postpartum <- postpartum %>%
  dplyr::left_join(
    unen_laatu %>%
      dplyr::mutate(
        id = as.character(id),
        summary_date = as.Date(summary_date)
      ) %>%
      dplyr::select(id, summary_date, score),
    by = c("id", "summary_date")
  )

# Posita keskeyttäneet tai keskenmenon saaneet
poista <- c("107","110","124","148","139","152")

pregnancy  <- pregnancy  %>% filter(!(as.character(id) %in% poista))
postpartum <- postpartum %>% filter(!(as.character(id) %in% poista))

# Muuta non-wear na -> 1440, jos ei käyetty koko päivänä
pregnancy <- pregnancy %>%
  mutate(non_wear = ifelse(is.na(non_wear), 1440L, non_wear))

postpartum <- postpartum %>%
  mutate(non_wear = ifelse(is.na(non_wear), 1440L, non_wear))


# Metadata
metadata  <- read_sas(file.path(dir.path, "taustamuuttujat.sas7bdat")) %>% as.data.frame() %>% distinct()

# Muuttujat faktoreiksi + tasojärjestykset
metadata <- metadata %>% #tässä oli metadata1 mutta en tiedä mistä se tulee?
  mutate(
    id = as.character(id),
    
    # trim + tyhjät -> NA kaikissa merkkikentissä (paitsi id)
    across(where(is.character) & !matches("^id$"), ~ na_if(trimws(.), ""))
  ) %>%
  # yhtenäistä ikäluokat ennen faktorointia
  mutate(
    age_category = recode(age_category,
                          "Under 3" = "Under 30", # onko tarkoitus olla under 30
                          "Over 30" = "30 or more")
  ) %>%
  # kaikki paitsi id faktoreiksi
  mutate(
    across(-id, ~ factor(.))
  ) %>%
  # haluttu tasajärjestys (1. taso = 1)
  mutate(
    age_category   = factor(age_category,   levels = c("Under 30", "30 or more")),
    gt_weight_gain = factor(gt_weight_gain, levels = c("within or less", "more than recommendat")),
    epds_category  = factor(epds_category,  levels = c("No depression", "Possible depr"))
  )

# Liitetään meta-aineisto 
pregnancy <- pregnancy %>%
  dplyr::left_join(
    metadata,
    by = c("id")
  )
postpartum <- postpartum %>%
  dplyr::left_join(
    metadata,
    by = c("id")
  )
#Tallennetaan 

tanaan <- Sys.Date()#tänään

# Luo tiedostonimet
pregnancy_file <- paste0("data/pregnancy_", tanaan, ".rds")
postpartum_file <- paste0("data/postpartum_", tanaan, ".rds")

# Tallennus
saveRDS(pregnancy, file = pregnancy_file)
saveRDS(postpartum, file = postpartum_file)
