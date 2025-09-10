# Datan tuonti ja alkukäsittely. Alkuperäisesti saatu aineisto sijaitsee seafile kansiossa projekti kurssi ja tiedostossa siihen osoitta symlinkki "data". 
# Alkuperäinen jaettu muoto on sas tiedosto.sas7bdat.

library(tidyverse)
library(haven)
library(janitor)
library(dplyr)

# Tähän kohtaan jouduin tekemään itse hieman muutoksia seuraavasti

dir.path <- "C:/Users/Ari/Documents/Ari Oura Projekti/Oura"
stopifnot(dir.exists(dir.path))  

# Funktio pysyy ennallaan
read_sas <- function(p) {
  if (!file.exists(p)) {
    stop("Tiedostoa ei löydy: ", p)
  }
  haven::read_sas(p)
}

# Sitten alla koodissa kun id:t oli pinellä niin ei toiminut minulla, niin laitoin isolla ID ja toimi

# Haetaan aineisto. 
activity_pregnancy  <- read_sas(file.path(dir.path, "activity_pregnancy.sas7bdat"))
activity_postpartum <- read_sas(file.path(dir.path, "activity_postpartum.sas7bdat"))
sleep_pregnancy     <- read_sas(file.path(dir.path, "sleep_pregnancy.sas7bdat"))
sleep_postpartum    <- read_sas(file.path(dir.path, "sleep_postpartum.sas7bdat"))

# Sunnitelma puutuvien ID -arvoje ja viikkonumerojen korjaamiseen. Pregnancy aineisto. 
# Huom! Raskausviikkojen viikko ei ala välttämättä maanantai päivällä. Vaan esimerkiksi id(101) ensimmäinen päivä (2021-04-07) on keskiviikko. 
# weekdays(as.Date("2021-04-07"))
data <- activity_pregnancy
str(data)
#Muutetaan id char muotoon
data <- data %>% mutate(id=as.character(id))
tol <- 1e-8 #liukulukuja varten oleva toleranssi. 
var_nimet <- colnames(data)
#otetaan pois jokainen rivi jossa sekä id ja week saavat arvon nolla. 
data<- data %>% filter(!(id == "0" & week == 0)) 

ids <- data$id %>% unique()
lista <- list()

for(k in seq_along(ids)){
  i <- ids[k]

  # Valitaan id ja järjestetään havainnot oikeaan järjestykseen päivämäärän perusteella. 
  data_id <- data %>% dplyr::filter(id==i) %>% dplyr::arrange(summary_date)
  
  aloitus_pv <-min(data_id$summary_date)
  raskausviikko <- min(data_id$week)
  
  # Moi! Tässä minun ehdotus koodille
  # Lisätään päivät
  paiva_id <- tibble(id = i,
                      summary_date = seq(min(data_id$summary_date), max(data_id$summary_date), by = "day")) # Täydentää päivämäärät ja id. 
  
  # yhdistetään
  yhd<- paiva_id %>% dplyr::left_join(data_id, by = c("id","summary_date"))
  
  pv_alusta <- as.integer(yhd$summary_date - aloitus_pv)
  viikko_nyt <- raskausviikko + (pv_alusta %/% 7)
  
  yhd$week <- ifelse(is.na(yhd$week), viikko_nyt, yhd$week)
  
  # Lisätään nollat: 
  muut_muuttujat<- var_nimet[!(var_nimet %in% c("id", "week", "summary_date"))]
  for (col in muut_muuttujat) {
    if (col %in% names(yhd)) {
      yhd[[col]] <- ifelse(is.na(yhd[[col]]), 0, yhd[[col]])
    }
  }
  
  # Varmistetaan sarake järjestys 
  yhd <- yhd %>% dplyr::select(all_of(var_nimet))
  
  lista[[k]] <- yhd
}

data_filled <- dplyr::bind_rows(lista) %>% arrange(id, summary_date)
view(data_filled) # huom jokaista päivää ja id kohti on tuplaantunut rivi ? en tiedä mistä johtuu... oli ilmeisesti jo alkuperäisessä aineistossa?
