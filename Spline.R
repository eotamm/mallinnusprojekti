# Onko aikaa järkevää mallintaa epälineaarisesti splinien avulla. Ajan epälineaarinen mallintaminen voi olla hyödyllistä, 
# sillä fyysinen aktiivisuus tai hyvin vointi saattaa muuttua eritavalla raskauden erivaiheissa. 
#
library(nlme)
library(lme4)
library(splines)
library(DT)


dir.path <- file.path("./data") # symlink. 
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

# Funktiot:
taulukoi_mallit <- function(..., nimet = NULL, taulukon_nimi = "Mallien vertailutaulukko", spline_mallit = NULL) {
  mallit <- list(...)
  if (is.null(nimet)) {
    nimet <- paste0("Aika_", seq_along(mallit))
  }
  
  tulokset <- data.frame(
    Malli = nimet,
    AIC = round(sapply(mallit, AIC),2),
    BIC = round(sapply(mallit, BIC),2)
  )

  min_aic_index <- which.min(tulokset$AIC)
  
  DT::datatable(tulokset,
                rownames = FALSE,
                options=list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                pageLenght=8),
                caption = taulukon_nimi)
}

tarkista_jaannokset <- function(malli, malli_nimi = NULL) {
  if (is.null(malli_nimi)) malli_nimi <- deparse(substitute(malli))
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  

  par(mfrow = c(1, 2),
      mar = c(4, 4, 3.5, 1),   
      oma = c(0, 0, 3.2, 0))   
  
  res <- resid(malli, type = "pearson")
  fit <- fitted(malli)
  
  # Sovite vs jäännös
  plot(fit, res, pch = 16, cex = 0.7,
       xlab = "Sovite (fitted)", ylab = "Jäännös",
       main = "Sovitteet vs. jäännökset")
  abline(h = 0, col = "red", lty = 2)
  
  # QQ-kuva
  qqnorm(res, main = "QQ-kuvaaja jäännöksille")
  qqline(res, col = "red", lwd = 2)
  
  # Otsikko
  mtext(paste("Jäännöstarkastelu:", malli_nimi),
        outer = TRUE, cex = 1.1, font = 2, line = 1.2)
}


# Scaling 

pregnancy <- pregnancy %>%
  mutate(
    steps_z = as.numeric(scale(steps)),
    avgmet_z = as.numeric(scale(average_met))
  )
postpartum <- postpartum %>%
  mutate(
    steps_z = as.numeric(scale(steps)),
    avgmet_z = as.numeric(scale(average_met))
  )

pregnancy$duration = pregnancy$duration/(60*60)

#______
dat <- pregnancy
dat <- dat %>%
  filter(!is.na(duration), !is.na(steps_z), !is.na(id))

#______
# Duration
#______
# Malli 1: Ilman aikaa
m0 <- lme(duration ~ steps_z, random = ~1 | id, data = dat)
summary(m0)
# Malli 2: Aika lineaarisena
m1 <- lme(duration ~ steps_z + week, random = ~1 | id, data = dat)
summary(m1)
# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
summary(m2)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
# m3<- lme(duration ~ steps_z * ns(week, df = 4), random = ~1 | id, data = dat)
# summary(m3)

#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

#m_df3 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(duration ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(duration ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(duration ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat)

AIC(m0, m1,m2, m_df4, m_df5, m_df6)
BIC(m0, m1,m2, m_df4, m_df5, m_df6)

taulukoi_mallit(m0, m1,m_df3, m_df4, m_df5, m_df6,
                nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
                spline_mallit = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
                taulukon_nimi = "Splinien vaikutus raskauden aikana unen kestoon. (duration ~ steps_z)")


tarkista_jaannokset(m_df3, "df = 3 spline malli")
tarkista_jaannokset(m_df6, "df = 6 spline malli")

# Jäännöksissä ei eroa kuvien välillä

# Malli 1: Ilman aikaa
m0 <- lme(duration ~ avgmet_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(duration ~ avgmet_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(duration ~ avgmet_z + ns(week, df = 4), random = ~1 | id, data = dat)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
m3<- lme(duration ~ avgmet_z * ns(week, df = 4), random = ~1 | id, data = dat)


#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

m_df3 <- lme(duration ~ avgmet_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(duration ~ avgmet_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(duration ~ avgmet_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(duration ~ avgmet_z + ns(week, df = 6), random = ~1 | id, data = dat)

#AIC(m2, m_df3,m_df4, m_df5, m_df6)
#BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukoi_mallit(m0, m1,m_df3, m_df4, m_df5, m_df6,
                nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
                spline_mallit = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
                taulukon_nimi = "Splinien vaikutus raskauden aikana unen kestoon. (duration ~ avgmet_z)")

#_____
# SCORE
#______
dat <- pregnancy
dat <- dat %>%
  filter(!is.na(score), !is.na(steps_z), !is.na(id))

# Malli 1: Ilman aikaa
m0 <- lme(score ~ steps_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(score ~ steps_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(score ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
m3<- lme(score ~ steps_z * ns(week, df = 4), random = ~1 | id, data = dat)


#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

m_df3 <- lme(score ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(score ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)
#m_df5 <- lme(score ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat) ongelma
m_df6 <- lme(score ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat)

AIC(m2, m_df3,m_df4, m_df5, m_df6)
#BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukoi_mallit(m0, m1,m_df3, m_df4, m_df5, m_df6,
                nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
                spline_mallit = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
                taulukon_nimi = "Splinien vaikutus raskauden aikana")

dat <- pregnancy
dat <- dat %>%
  filter(!is.na(score), !is.na(avgmet_z), !is.na(id))

# Malli 1: Ilman aikaa
m0 <- lme(score ~ avgmet_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(score ~ avgmet_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(score ~ avgmet_z + ns(week, df = 4), random = ~1 | id, data = dat)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
m3<- lme(score ~ avgmet_z * ns(week, df = 4), random = ~1 | id, data = dat)


#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

m_df3 <- lme(score ~ avgmet_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(score ~ avgmet_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(score ~ avgmet_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(score ~ avgmet_z + ns(week, df = 6), random = ~1 | id, data = dat)

#AIC(m2, m_df3,m_df4, m_df5, m_df6)
#BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukoi_mallit(m0, m1,m_df3, m_df4, m_df5, m_df6,
                nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
                spline_mallit = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
                taulukon_nimi = "Splinien vaikutus raskauden aikana (Score~avgmet_z)")
#_____
dat <- postpartum
dat <- dat %>%
  filter(!is.na(duration), !is.na(steps_z), !is.na(id))

# Malli 1: Ilman aikaa
m0 <- lme(duration ~ steps_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(duration ~ steps_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(duration ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
m3<- lme(duration ~ steps_z * ns(week, df = 4), random = ~1 | id, data = dat)


AIC(m0, m1, m2, m3)
BIC(m0, m1, m2, m3)

m_df3 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(duration ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(duration ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(duration ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat)

AIC(m2, m_df3,m_df4, m_df5, m_df6)
BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukoi_mallit(m0, m1,m_df3, m_df4, m_df5, m_df6,
                nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
                spline_mallit = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
                taulukon_nimi = "Splinien vaikutus raskauden raskauden jälekeen")

#Muuta: 
# Ennustedata
week_seq <- seq(min(dat$week), max(dat$week), length.out = 100)
newdata <- data.frame(
  week = week_seq,
  steps_z = mean(dat$steps_z, na.rm = TRUE)
)
newdata$pred_df3 <- predict(m_df3, newdata, level=0)
newdata$pred_df4 <- predict(m_df4, newdata, level=0)
newdata$pred_df5 <- predict(m_df5, newdata, level=0)
newdata$pred_df6 <- predict(m_df6, newdata, level=0)

ggplot(newdata, aes(x = week)) +
  geom_line(aes(y = pred_df3, color = "df = 3"), size = 1) +
  geom_line(aes(y = pred_df4, color = "df = 4"), size = 1) +
  geom_line(aes(y = pred_df5, color = "df = 5"), size = 1) +
  geom_line(aes(y = pred_df6, color = "df = 6"), size = 1) +
  labs(title = "Spline-mallien ennustekäyrät eri vapausasteilla",
       x = "Viikko", y = "Ennustettu duration",
       color = "Spline df") +
  theme_minimal()

