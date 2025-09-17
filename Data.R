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

# Apufunktioita:________________________________________________________________
#Onko viikko täysi: ottaa vektorin päivämääriä
viikko_taysi <- function(pvt) {
  pvt <- sort(unique(as.Date(pvt)))
  length(pvt) == 7 &&
    as.integer(max(pvt) - min(pvt)) == 6 &&
    all(diff(pvt) == 1)
}

#Täydennä puuttuvat päivämäärät
taydenna_pv <- function(df, lisaa_lippu=FALSE){
  stopifnot(all(c("id","summary_date") %in% names(df)))
  
  # Järjestää päivämäärät
  df <- df %>%
    dplyr::mutate(
      summary_date = as.Date(summary_date), #
      id =as.character(id)
    ) %>%
    dplyr::arrange(summary_date)
  
  id_vec <- unique(df$id)
  
  if (length(id_vec) != 1L) {
    stop("Tämä funktio täydentää tietyn henkilön (id) aikasarjan päivät, jotka puuttuvat keskeltä. ",
         "Anna funktiolle aineisto filtteröitynä yhdelle id:lle.")
  }
  id_val<- id_vec[[1]]
  # Lisätään päivät
  paiva_id <- tibble(id = id_val,
                     summary_date = seq(min(df$summary_date), max(df$summary_date), by = "day")) # Täydentää päivämäärät ja id. 
  
 
  #Luetaan olemassa oleva viikko tieto oikeille päiville. 
  out <- paiva_id %>%
    dplyr::left_join(df, by = c("id","summary_date")) %>%
    dplyr::arrange(summary_date)
  
  if (lisaa_lippu) {
    out <- out %>%
      dplyr::mutate(imputed_row = !summary_date %in% df$summary_date)
  }
  return(as.data.frame(out))
}



#Lisää rivejä alkuun ja testaa mikä viikko numerointi on ok
taydenna_viikot <- function(df) {
  #Varmistus
  stopifnot(all(c("id","summary_date", "week") %in% names(df))) 
  
  df <- df %>%
    dplyr::mutate(
      summary_date = as.Date(summary_date), #päivämäärä muoto
      id =as.character(id) #char
    ) %>%
    dplyr::arrange(summary_date)
  
  #Ensimmäisen viikon pituus
  viikko1 <- min(df$week, na.rm = TRUE) #Aloitus viikkon numero
  pituus <- df %>% dplyr::filter(week==viikko1) %>% nrow() # aloitus viikon pituus. 
  pv1<- df %>%  dplyr::filter(week==viikko1) %>% dplyr::arrange(summary_date) %>% dplyr::pull(summary_date) %>% min() # Ensimmäisen viikon ensimmäinen päivä. 
  
  names_col<- setdiff(colnames(df),c("summary_date","id","week"))
  
  if(pituus>7){
    stop("Liikaa päiviä viikossa")
  }else if (pituus==7){
    counter <- 1
  
    # Ensimmäinen viikko on täysi: täydennä vain puuttuvat week-arvot laskennalla
    aloitus_pv   <- pv1
    raskausviikko <- viikko1
    
    yhd <- df
    pv_alusta <- as.integer(yhd$summary_date - aloitus_pv)
    viikko_nyt <- raskausviikko + (pv_alusta %/% 7)
    yhd$week <- ifelse(is.na(yhd$week), viikko_nyt, yhd$week)
    out <- yhd
    
  }else if(pituus<7){
    
    pv_num <- 7-pituus
    pv1 <- as.Date(pv1)
    
    id_vec <- unique(df$id)
    id_val<- id_vec[[1]]
    
    #Aloituksen päivät
    paivat <- tibble(id = id_val,
                       summary_date = seq(pv1 - pv_num, max(df$summary_date), by = "day"))
    
    #Lisätään muut kolumnit. 
    out <- paivat %>%
      dplyr::left_join(df, by = c("id","summary_date")) %>%
      dplyr::arrange(summary_date)
    
    best_n <- NA_integer_
    last_col <- NULL
    for (n in 0:pv_num) {
      aloitus_pv_n <- (pv1 - pv_num) + n
      coln <- paste0("testi_viikko_", n)
      # ennen oletettua aloituspäivää NA, muuten viikko1 + floor((date - aloitus)/7)
      cand <- ifelse(out$summary_date < aloitus_pv_n,
                     NA_integer_,
                     viikko1 + ((as.integer(out$summary_date - aloitus_pv_n)) %/% 7))
      out[[coln]] <- cand
      last_col <- coln
      
      # vertaillaan vain tiedettyihin week-arvoihin (ei NA/0)
      mask <- !is.na(out$week) & out$week != 0
      if (any(mask) && all(out[[coln]][mask] == out$week[mask])) {
        best_n <- n
        break
      }
    }
    
    if (!is.na(best_n)) {
      # löytyi täydellinen match → käytä sitä uutena weekinä ja tiputa alku ennen aloitusta
      best_col <- paste0("testi_viikko_", best_n)
      aloitus_pv_paras <- (pv1 - pv_num) + best_n
      out$testi_viikko <- out[[best_col]]
      out$week <- out$testi_viikko
      
      # poista alun ylimääräiset rivit (ennen valitun aloituksen päivää)
      out <- out %>% dplyr::filter(summary_date >= aloitus_pv_paras)
      # jätä vain lopullinen testi_viikko
      out <- out %>% dplyr::select(all_of(c("summary_date","id", names_col, "week")))
      
    } else {
      # ei täydellistä matchia → älä koske weekiin; jätä viimeisin laskettu testi_viikko vertailua varten
      out$testi_viikko <- out[[last_col]]
      out <- out %>% dplyr::select(all_of(c("summary_date","id", names_col, "week", "testi_viikko")))
    }
    
  }
 
  return(out)
}


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
#Lisää tähän 
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
data1 %>% View()
#saveRDS(data1, file = "data/sleep_postpartum.rds")


