# Visualisaatiot
#
# Tässä dokumentissa toteutetaan aineiston ensisijaiset visualisoinnit 
# ennen varsinaisia analyysejä. Tarkoitus on saada yleiskuva aineistosta ja sen 
# laadusta, sekä tunnistaa mahdollisia piirteitä, jotka voivat vaikuttaa myöhempiin malleihin.

# Kirjastot
library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(rlang)
library(ggplot2)
library(patchwork)
#Aineiston tuonti
dir.path <- file.path("./data") # symlink 

# Haetaan puhdisteut aineistot
activity_pregnancy  <- readRDS(file.path(dir.path, "activity_pregnancy.rds"))  %>% as.data.frame()
activity_postpartum <- readRDS(file.path(dir.path, "activity_postpartum.rds")) %>% as.data.frame()

sleep_pregnancy     <- readRDS(file.path(dir.path, "sleep_pregnancy.rds"))     %>% as.data.frame()
sleep_postpartum    <- readRDS(file.path(dir.path, "sleep_postpartum.rds"))    %>% as.data.frame()
#Activity ja sleep aineistojen yhdistäminen.
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

pregnancy_df <- yhdistys(activity_pregnancy,sleep_pregnancy)
postpartum_df <- yhdistys(activity_postpartum,sleep_postpartum)

# Histogrammit keskeisistä muuttujista. 
plot_hist_overall <- function(df, var, bins = 30, title = NULL) {
  var_sym <- rlang::ensym(var)       
  n_na <- sum(is.na(dplyr::pull(df, !!var_sym)))
  ggplot(df, aes(x = !!var_sym)) +
    geom_histogram(bins = bins, na.rm = TRUE) +
    labs(
      title = title %||% paste0("Päivittäisten arvojen jakauma: ", rlang::as_name(var_sym)),
      subtitle = if (n_na > 0) paste0("Huom: pudotettu ", n_na, " NA-arvoa") else NULL,
      x = rlang::as_name(var_sym),
      y = "Havaintojen määrä"
    )
} #Yleinen arvojen jakauma, annetulle muuttujalle. 

p1_preg<- plot_hist_overall(pregnancy_df,  "duration")
p2_preg<- plot_hist_overall(pregnancy_df,  "steps")

p1_post<- plot_hist_overall(postpartum_df,  "duration")
p2_post<- plot_hist_overall(postpartum_df,  "steps")

wrap_plots(p1_preg,p1_post)
wrap_plots(p2_preg,p2_post)

# Muuttuko viikottainen keskiarvo

summarise_weekly_by_gw <- function(df,
                                   y, gw, id = id,
                                   min_obs_per_week = 1,
                                   compute_ci = FALSE,
                                   conf_level = 0.95) {
  y  <- ensym(y); gw <- ensym(gw); id <- ensym(id)
  
  out <- df %>%
    group_by(!!id, !!gw) %>%
    summarise(
      n  = sum(!is.na(!!y)),
      mean_y = if (n > 0) mean(!!y, na.rm = TRUE) else NA_real_,
      sd_y   = if (n > 1) sd(!!y,  na.rm = TRUE) else NA_real_,
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    filter(n >= min_obs_per_week)
  
  if (compute_ci) {
    alpha <- 1 - conf_level
    out <- out %>%
      mutate(
        se = ifelse(n > 1, sd_y / sqrt(n), NA_real_),
        tcrit = ifelse(n > 1, qt(1 - alpha/2, df = n - 1), NA_real_),
        lwr = ifelse(n > 1, mean_y - tcrit * se, NA_real_),
        upr = ifelse(n > 1, mean_y + tcrit * se, NA_real_)
      )
  }
  
  out
}

#Line plot jokaiselle henkilölle