# Datan tuonti ja alkukäsittely. Alkuperäisesti saatu aineisto sijaitsee seafile kansiossa projekti kurssi ja tiedostossa siihen osoitta symlinkki "data". 
# Alkuperäinen jaettu muoto on sas tiedosto.sas7bdat.

library(tidyverse)
library(haven)
library(janitor)
library(dplyr)

dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 

stopifnot(dir.exists(dir.path)) # tarkasta

read_sas<- function(p) {
  if (!file.exists(p)) {
    stop("Tiedostoa ei löydy: ", p)
  }
  haven::read_sas(p) |> janitor::clean_names()
} # Apufunktio datan tuomiseen.  

# Haetaan aineisto. 
activity_pregnancy  <- read_sas(file.path(dir.path, "activity_pregnancy.sas7bdat")) %>% as.data.frame() %>% distinct()
activity_postpartum <- read_sas(file.path(dir.path, "activity_postpartum.sas7bdat")) %>% as.data.frame() %>% distinct()
sleep_pregnancy     <- read_sas(file.path(dir.path, "sleep_pregnancy.sas7bdat")) %>% as.data.frame() %>% distinct()
sleep_postpartum    <- read_sas(file.path(dir.path, "sleep_postpartum.sas7bdat")) %>% as.data.frame() %>% distinct()

# Sunnitelma puutuvien ID -arvoje ja viikkonumerojen korjaamiseen. Pregnancy aineisto. 
# Huom! Raskausviikkojen viikko ei ala välttämättä maanantai päivällä. Vaan esimerkiksi id(101) ensimmäinen päivä (2021-04-07) on keskiviikko. 
# weekdays(as.Date("2021-04-07")) 
source("PreProcessingFuncs.R")

#:______________________________________________________________________________
#Tarkistus: (Huom! Tässä päivä numerot menevät seuraavasti: 1: Maanantai, 2: Tiistai, 3: Keskiviikko, 4: Torstai, 5: Perjanta, 6: Lauantai ja 7: Sunnuntai.)
#Tallennetaan kaikki uniikit ID arvot.  
uniikit_idt <- activity_pregnancy %>% dplyr::filter(id != 0) %>% dplyr::pull(id) %>% unique()

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
data1 <- data1 %>% mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))
data1 %>% View()
#saveRDS(data1, file = "data/activity_pregnancy.rds")

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
data1 <- data1 %>% mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))
data1 %>% View()
#saveRDS(data1, file = "data/activity_postpartum.rds")


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
data1 <- data1 %>% mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))
data1 %>% View()
#saveRDS(data1, file = "data/sleep_pregnancy.rds")

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
data1 <- data1 %>% mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))
data1 %>% View()
#saveRDS(data1, file = "data/sleep_postpartum.rds")


