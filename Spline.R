# Onko aikaa järkevää mallintaa epälineaarisesti splinien avulla. Ajan epälineaarinen mallintaminen voi olla hyödyllistä, 
# sillä fyysinen aktiivisuus tai hyvin vointi saattaa muuttua eritavalla raskauden erivaiheissa. 

# Kirjastot
library(nlme)
library(lme4)
library(splines)
library(DT)
library(kableExtra)
library(knitr)
library(effects)
library(dplyr)
dir.path <- file.path("./data") # symlink. 
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

# Funktiot:
taulukoi_mallit_latex <- function(...,
                                  nimet = NULL,
                                  caption = "Ajan mallinnuksen vertailu (ML)",
                                  file = NULL,
                                  lihavoi_bic = FALSE) {
  mallit <- list(...)
  if (is.null(nimet)) nimet <- paste0("M", seq_along(mallit))
  
  AICv <- sapply(mallit, AIC)
  BICv <- sapply(mallit, BIC)
  best_aic <- which.min(AICv)
  best_bic <- which.min(BICv)
  
  df <- data.frame(
    Malli = nimet,
    AIC = round(AICv, 2),
    BIC = round(BICv, 2),
    check.names = FALSE
  )
  
  # Lihavoi paras AIC (ja haluttaessa BIC)
  df$AIC <- ifelse(seq_len(nrow(df)) == best_aic,
                   cell_spec(sprintf("%.2f", AICv), format = "latex", bold = TRUE),
                   sprintf("%.2f", AICv))
  if (lihavoi_bic) {
    df$BIC <- ifelse(seq_len(nrow(df)) == best_bic,
                     cell_spec(sprintf("%.2f", BICv), format = "latex", bold = TRUE),
                     sprintf("%.2f", BICv))
  } else {
    df$BIC <- sprintf("%.2f", BICv)
  }
  
  kb <- kbl(df, format = "latex", booktabs = TRUE, escape = FALSE, caption = caption) |>
    kable_styling(latex_options = c("hold_position"))
  
  if (!is.null(file)) {
    save_kable(kb, file)
  }
  return(kb)
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
postpartum$duration = postpartum$duration/(60*60)

#______Lets use the same data for every var. 
dat <- pregnancy
dat <- dat %>%
  filter(!is.na(duration), !is.na(score), !is.na(efficiency),!is.na(steps_z), !is.na(id))

#______
# Duration
#______
# Malli 1: Ilman aikaa
m0 <- lme(duration ~ steps_z, random = ~1 | id, data = dat,method = "ML")
summary(m0)
# Malli 2: Aika lineaarisena
m1 <- lme(duration ~ steps_z + week, random = ~1 | id, data = dat, method = "ML")
summary(m1)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat, method = "ML")
summary(m2)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 

#m_df3 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(duration ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat, method = "ML")
m_df5 <- lme(duration ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat, method = "ML")
m_df6 <- lme(duration ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat, method = "ML")

tarkista_jaannokset(m2, "df = 3 spline malli")
tarkista_jaannokset(m_df6, "df = 6 spline malli")

taulukko <- taulukoi_mallit_latex(
  m0, m1, m2, m_df4, m_df5, m_df6,
  nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
  caption = "Splinien vaikutus raskauden aikana unen kestoon ($duration \\sim steps_z$; ML)",
  file = "taulukko_duration_stepsz.tex",
  lihavoi_bic = FALSE
)
taulukko # latex koodi.

# Lasketaan efektit
eff_df3 <- effect("ns(week, df = 3)", m2)
eff_df4 <- effect("ns(week, df = 4)", m_df4)

eff_df6 <- effect("ns(week, df = 6)", m_df6)

# Piirretään molemmat samaan kuvaan
library(effects)

# Efektit dataksi
eff3 <- as.data.frame(effect("ns(week, df = 3)", m2,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))
eff6 <- as.data.frame(effect("ns(week, df = 6)", m_df6,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))

# Piirrä df=3 ensin
plot(eff3$week, eff3$fit, type="l", lwd=2, col="blue",
     xlab="Raskausviikko", ylab="Ennustettu unen kesto (h)",
     main="Raskausviikon vaikutus unen kestoon",
     ylim=range(eff3$lower, eff3$upper, eff6$lower, eff6$upper))

# Lisää df=6 päälle
lines(eff6$week, eff6$fit, lwd=2, col="red")

# (valinnainen) luottamusvälit
polygon(c(eff3$week, rev(eff3$week)),
        c(eff3$lower, rev(eff3$upper)),
        border=NA, col=adjustcolor("blue", 0.15))
polygon(c(eff6$week, rev(eff6$week)),
        c(eff6$lower, rev(eff6$upper)),
        border=NA, col=adjustcolor("red", 0.15))

legend("topright", legend=c("df = 3", "df = 6"),
       col=c("blue", "red"), lwd=2, bty="n")

#_____
# SCORE
#______

# Malli 1: Ilman aikaa
m0 <- lme(score ~ steps_z, random = ~1 | id, data = dat, method = "ML")

# Malli 2: Aika lineaarisena
m1 <- lme(score ~ steps_z + week, random = ~1 | id, data = dat, method = "ML")

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(score ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat, method = "ML")

#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

#m_df3 <- lme(score ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(score ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat, method = "ML")
m_df5 <- lme(score ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat, method = "ML") #ongelma?
m_df6 <- lme(score ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat, method = "ML")

#AIC(m0, m1, m2, m3, m_df4, m_df5, m_df6)
#BIC(m0, m1, m2, m3, m_df4, m_df5, m_df6)

taulukko <- taulukoi_mallit_latex(
  m0, m1, m2, m_df4, m_df5, m_df6,
  nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
  caption = "Splinien vaikutus raskauden aikana unen kestoon ($duration \\sim steps_z$; ML)",
  file = "taulukko_duration_stepsz.tex",
  lihavoi_bic = FALSE
)
taulukko # latex koodi.

# Lasketaan efektit
eff_df3 <- effect("ns(week, df = 3)", m2)
eff_df6 <- effect("ns(week, df = 6)", m_df6)

# Piirretään molemmat samaan kuvaan
library(effects)

# Efektit dataksi
eff3 <- as.data.frame(effect("ns(week, df = 3)", m2,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))
# eff6 <- as.data.frame(effect("ns(week, df = 6)", m_df6,
#                              xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
#                                                        max(dat$week, na.rm=TRUE), length.out=200))))

# Piirrä df=3 ensin
plot(eff3$week, eff3$fit, type="l", lwd=2, col="blue",
     xlab="Raskausviikko", ylab="Ennustettu unen kesto (h)",
     main="Raskausviikon vaikutus unen laatuun",
     ylim=range(eff3$lower, eff3$upper))

# Lisää df=6 päälle
# lines(eff6$week, eff6$fit, lwd=2, col="red")

# (valinnainen) luottamusvälit
polygon(c(eff3$week, rev(eff3$week)),
        c(eff3$lower, rev(eff3$upper)),
        border=NA, col=adjustcolor("blue", 0.15))
polygon(c(eff6$week, rev(eff6$week)),
        c(eff6$lower, rev(eff6$upper)),
        border=NA, col=adjustcolor("red", 0.15))

legend("topright", legend=c("df = 3", "df = 6"),
       col=c("blue", "red"), lwd=2, bty="n")

#_____
# Efficience
#______
dat <- pregnancy
dat <- dat %>%
  filter(!is.na(efficiency), !is.na(avgmet_z), !is.na(id))

# Malli 1: Ilman aikaa
m0 <- lme(efficiency ~ avgmet_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(efficiency ~ avgmet_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(efficiency ~ avgmet_z + ns(week, df = 3), random = ~1 | id, data = dat)

#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

#m_df3 <- lme(efficiency ~ avgmet_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(efficiency ~ avgmet_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(efficiency ~ avgmet_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(efficiency ~ avgmet_z + ns(week, df = 6), random = ~1 | id, data = dat)

#AIC(m2, m_df3,m_df4, m_df5, m_df6)
#BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukko <- taulukoi_mallit_latex(
  m0, m1, m2, m_df4, m_df5, m_df6,
  nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
  caption = "Splinien vaikutus raskauden aikana unen tehokkuuteen($efficiency \\sim steps_z$; ML)",
  file = "taulukko_duration_stepsz.tex",
  lihavoi_bic = FALSE
)
taulukko # latex koodi.

# Piirretään molemmat samaan kuvaan

# Efektit dataksi
eff3 <- as.data.frame(effect("ns(week, df = 3)", m2,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))
eff6 <- as.data.frame(effect("ns(week, df = 6)", m_df6,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))

# Piirrä df=3 ensin
plot(eff3$week, eff3$fit, type="l", lwd=2, col="blue",
     xlab="Raskausviikko", ylab="Ennustettu unen tehokkuuteen",
     main="Raskausviikon vaikutus unen tehokkuuteen",
     ylim=range(eff3$lower, eff3$upper, eff6$lower, eff6$upper))

# Lisää df=6 päälle
lines(eff6$week, eff6$fit, lwd=2, col="red")

# (valinnainen) luottamusvälit
polygon(c(eff3$week, rev(eff3$week)),
        c(eff3$lower, rev(eff3$upper)),
        border=NA, col=adjustcolor("blue", 0.15))
polygon(c(eff6$week, rev(eff6$week)),
        c(eff6$lower, rev(eff6$upper)),
        border=NA, col=adjustcolor("red", 0.15))

legend("topright", legend=c("df = 3", "df = 6"),
       col=c("blue", "red"), lwd=2, bty="n")
#_____
dat <- postpartum
dat <- dat %>%
  filter(!is.na(duration), !is.na(score), !is.na(efficiency),!is.na(steps_z), !is.na(id))

#______
# Duration
#______

# Malli 1: Ilman aikaa
m0 <- lme(duration ~ steps_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(duration ~ steps_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
#m3<- lme(duration ~ steps_z * ns(week, df = 4), random = ~1 | id, data = dat)


#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

#m_df3 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(duration ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(duration ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(duration ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat)

#AIC(m2, m_df3,m_df4, m_df5, m_df6)
#BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukko <- taulukoi_mallit_latex(
  m0, m1, m2, m_df4, m_df5, m_df6,
  nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
  caption = "Splinien vaikutus raskauden aikana unen kestoon ($duration \\sim steps_z$; ML, (Postpartum))",
  file = "taulukko_duration_stepsz.tex",
  lihavoi_bic = FALSE
)
taulukko # latex koodi.

# Lasketaan efektit
eff_df3 <- effect("ns(week, df = 3)", m2)
eff_df4 <- effect("ns(week, df = 4)", m_df4)

eff_df6 <- effect("ns(week, df = 6)", m_df6)

# Piirretään molemmat samaan kuvaan

# Efektit dataksi
eff3 <- as.data.frame(effect("ns(week, df = 3)", m2,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))
eff6 <- as.data.frame(effect("ns(week, df = 6)", m_df6,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))

# Piirrä df=3 ensin
plot(eff3$week, eff3$fit, type="l", lwd=2, col="blue",
     xlab="Raskausviikko", ylab="Ennustettu unen kesto (h)",
     main="Raskausviikon vaikutus unen kestoon",
     ylim=range(eff3$lower, eff3$upper, eff6$lower, eff6$upper))

# Lisää df=6 päälle
lines(eff6$week, eff6$fit, lwd=2, col="red")

# (valinnainen) luottamusvälit
# polygon(c(eff3$week, rev(eff3$week)),
#         c(eff3$lower, rev(eff3$upper)),
#         border=NA, col=adjustcolor("blue", 0.15))
# polygon(c(eff6$week, rev(eff6$week)),
#         c(eff6$lower, rev(eff6$upper)),
#         border=NA, col=adjustcolor("red", 0.15))

legend("topright", legend=c("df = 3", "df = 6"),
       col=c("blue", "red"), lwd=2, bty="n")

#______
# Score
#______

# Malli 1: Ilman aikaa
m0 <- lme(score ~ steps_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(score ~ steps_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(score ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
#m3<- lme(score ~ steps_z * ns(week, df = 4), random = ~1 | id, data = dat)


#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

#m_df3 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(score ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(score ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(score ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat)

#AIC(m2, m_df3,m_df4, m_df5, m_df6)
#BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukko <- taulukoi_mallit_latex(
  m0, m1, m2, m_df4, m_df5, m_df6,
  nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
  caption = "Splinien vaikutus raskauden aikana unen laatuun ($score \\sim steps_z$; ML, (Postpartum))",
  file = "taulukko_duration_stepsz.tex",
  lihavoi_bic = FALSE
)
taulukko # latex koodi.

# Lasketaan efektit
eff_df3 <- effect("ns(week, df = 3)", m2)
eff_df4 <- effect("ns(week, df = 4)", m_df4)

eff_df6 <- effect("ns(week, df = 6)", m_df6)

# Piirretään molemmat samaan kuvaan
library(effects)

# Efektit dataksi
eff3 <- as.data.frame(effect("ns(week, df = 3)", m2,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))
eff6 <- as.data.frame(effect("ns(week, df = 6)", m_df6,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))

# Piirrä df=3 ensin
plot(eff3$week, eff3$fit, type="l", lwd=2, col="blue",
     xlab="Raskausviikko", ylab="Ennustettu unen laatuun (0-100)",
     main="Raskausviikon vaikutus unen laatu",
     ylim=range(eff3$lower, eff3$upper, eff6$lower, eff6$upper))

# Lisää df=6 päälle
lines(eff6$week, eff6$fit, lwd=2, col="red")

# luottamusvälit
# polygon(c(eff3$week, rev(eff3$week)),
#         c(eff3$lower, rev(eff3$upper)),
#         border=NA, col=adjustcolor("blue", 0.15))
# polygon(c(eff6$week, rev(eff6$week)),
#         c(eff6$lower, rev(eff6$upper)),
#         border=NA, col=adjustcolor("red", 0.15))

legend("topright", legend=c("df = 3", "df = 6"),
       col=c("blue", "red"), lwd=2, bty="n")

#______
# Effiency
#______

# Malli 1: Ilman aikaa
m0 <- lme(efficiency ~ steps_z, random = ~1 | id, data = dat)

# Malli 2: Aika lineaarisena
m1 <- lme(efficiency ~ steps_z + week, random = ~1 | id, data = dat)

# Malli 3: Aika splinillä (epälineaarinen)

m2 <- lme(efficiency ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)

# Malli 4: Aktiivisuus muuttuu ajanmyötä 
#m3<- lme(efficiency ~ steps_z * ns(week, df = 4), random = ~1 | id, data = dat)


#AIC(m0, m1, m2, m3)
#BIC(m0, m1, m2, m3)

#m_df3 <- lme(duration ~ steps_z + ns(week, df = 3), random = ~1 | id, data = dat)
m_df4 <- lme(efficiency ~ steps_z + ns(week, df = 4), random = ~1 | id, data = dat)
m_df5 <- lme(efficiency ~ steps_z + ns(week, df = 5), random = ~1 | id, data = dat)
m_df6 <- lme(efficiency ~ steps_z + ns(week, df = 6), random = ~1 | id, data = dat)

#AIC(m2, m_df3,m_df4, m_df5, m_df6)
#BIC(m2, m_df3,m_df4, m_df5, m_df6)

taulukko <- taulukoi_mallit_latex(
  m0, m1, m2, m_df4, m_df5, m_df6,
  nimet = c("Ilman aikaa", "Aika lineaarisena", "df = 3", "df = 4", "df = 5", "df = 6"),
  caption = "Splinien vaikutus raskauden aikana unen tehokkuuteen ($efficiency \\sim steps_z$; ML, (Postpartum))",
  file = "taulukko_duration_stepsz.tex",
  lihavoi_bic = FALSE
)
taulukko # latex koodi.

# Lasketaan efektit
eff_df3 <- effect("ns(week, df = 3)", m2)
eff_df4 <- effect("ns(week, df = 4)", m_df4)

eff_df6 <- effect("ns(week, df = 6)", m_df6)

# Piirretään molemmat samaan kuvaan
# Efektit dataksi
eff3 <- as.data.frame(effect("ns(week, df = 3)", m2,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))
eff6 <- as.data.frame(effect("ns(week, df = 6)", m_df6,
                             xlevels = list(week = seq(min(dat$week, na.rm=TRUE),
                                                       max(dat$week, na.rm=TRUE), length.out=200))))

# Piirrä df=3 ensin
plot(eff3$week, eff3$fit, type="l", lwd=2, col="blue",
     xlab="Raskausviikko", ylab="Ennustettu unen tehokkuuteen",
     main="Raskausviikon vaikutus unen tehokkuuteen",
     ylim=range(eff3$lower, eff3$upper, eff6$lower, eff6$upper))

# Lisää df=6 päälle
lines(eff6$week, eff6$fit, lwd=2, col="red")

# luottamusvälit
# polygon(c(eff3$week, rev(eff3$week)),
#         c(eff3$lower, rev(eff3$upper)),
#         border=NA, col=adjustcolor("blue", 0.15))
# polygon(c(eff6$week, rev(eff6$week)),
#         c(eff6$lower, rev(eff6$upper)),
#         border=NA, col=adjustcolor("red", 0.15))

legend("topright", legend=c("df = 3", "df = 6"),
       col=c("blue", "red"), lwd=2, bty="n")









