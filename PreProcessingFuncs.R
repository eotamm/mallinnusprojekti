# Apufunktioita:________________________________________________________________

#Lukee SAS tiedoston ja yhtenäistää kolumnien nimet. 
read_sas<- function(p) {
  if (!file.exists(p)) {
    stop("Tiedostoa ei löydy: ", p)
  }
  haven::read_sas(p) |> janitor::clean_names()
}

# Yhdistää sleep ja activity datat id, summary_date, week ja time sarakkeiden perusteella. 
yhdistys <- function(df_activity, df_sleep){
  n_row_activity<- nrow(df_activity)
  n_row_sleep <-   nrow(df_sleep)
  
  if (n_row_sleep != n_row_activity) {
    warning(paste0(
      "Activity- ja Sleep-aineistot sisältävät eri määrän rivejä: ",
      n_row_activity, " (activity) vs ", n_row_sleep, " (sleep)."
    ))
  }
  
  merged_df <- merge(df_activity, df_sleep, 
                     by = c("id", "summary_date", "week"), 
                     all = TRUE) 
  
  n_row_merged<- nrow(merged_df)
  
  if(n_row_merged!=n_row_activity){
    warning("Lopullisessa aineistossa on vain",n_row_merged,"riviä \n
            Kaikille riveille ei ole löydetty paria.",)
  }
  return(merged_df)
}

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


# Muuttujat sekunneista minuuteiksi
sekunnit_minuteiksi <- function(x, digits = NULL) {
  stopifnot(is.numeric(x))
  m <- x / 60
  if (!is.null(digits)) m <- round(m, digits)
  m
}

