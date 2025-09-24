library(tidyverse)
library(haven)
library(janitor)
library(dplyr)

dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 


# Haetaan puhdisteut aineistot
activity_pregnancy  <- readRDS(file.path(dir.path, "activity_pregnancy.rds"))  %>% as.data.frame()
activity_postpartum <- readRDS(file.path(dir.path, "activity_postpartum.rds")) %>% as.data.frame()
sleep_pregnancy     <- readRDS(file.path(dir.path, "sleep_pregnancy.rds"))     %>% as.data.frame()
sleep_postpartum    <- readRDS(file.path(dir.path, "sleep_postpartum.rds"))    %>% as.data.frame()


