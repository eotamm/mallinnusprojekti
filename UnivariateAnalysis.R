library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tibble)

dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 


# Haetaan puhdisteut aineistot
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()


compute_trimester_post_means <- function(preg_df, post_df, vars,
                                         id_col = "id", week_col = "week",
                                         return_wide = TRUE) {
  # Pregnancy: T1/T2/T3
  preg_long <- preg_df %>%
    mutate(
      period = cut(.data[[week_col]],
                   breaks = c(0, 12, 27, Inf),
                   labels = c("T1", "T2", "T3"),
                   right = TRUE, include.lowest = TRUE)
    ) %>%
    filter(!is.na(period)) %>%
    group_by(.data[[id_col]], period) %>%
    summarise(
      across(all_of(vars), list(mean = ~ mean(., na.rm = TRUE)),
             .names = "{.col}__{.fn}"),
      .groups = "drop"
    )
  
  # Postpartum: PP
  post_long <- post_df %>%
    mutate(period = factor("PP", levels = c("T1","T2","T3","PP"))) %>%
    group_by(.data[[id_col]], period) %>%
    summarise(
      across(all_of(vars), list(mean = ~ mean(., na.rm = TRUE)),
             .names = "{.col}__{.fn}"),
      .groups = "drop"
    )
  
  # Yhdistä
  res <- bind_rows(preg_long, post_long) %>%
    rename(id = all_of(id_col)) %>%
    pivot_longer(
      cols = -c(id, period),
      names_to = c("variable", ".value"),
      names_sep = "__"
    ) %>%
    select(id, period, variable, mean)
  
  if (!return_wide) return(res)
  
  # Wide-muoto
  res %>%
    pivot_wider(
      id_cols = id,
      names_from = c(variable, period),
      values_from = mean,
      names_glue = "{variable}_{period}_mean"
    ) %>%
    arrange(id)
}

vars <- c("steps", "average_met", "score", "duration")
means_wide <- compute_trimester_post_means(pregnancy, postpartum, vars)

periods <- c("T1","T2","T3","PP")

# Poimi skaalattavat mean-sarakkeet
mean_cols <- unlist(lapply(vars, function(v) paste0(v, "_", periods, "_mean")))

# Skaalaa per sarake yli id:ien (z-score) ja tee uudet _z-sarakkeet
means_wide <- means_wide %>%
  mutate(across(all_of(mean_cols), ~ as.numeric(scale(.)), .names = "{.col}_z"))


# Taustamuuttujat
bg_vars <- c(
  "age_category","education","previous_children","epds_category",
  "bmi_bl2","gt_weight_gain","gt_weight_gain_within_or_less",
  "pp_weight_lost","delivery_method"
)

first_non_na <- function(x) x[match(TRUE, !is.na(x), nomatch = NA_integer_)]

bg_from_preg <- pregnancy %>%
  mutate(id = as.character(id)) %>%
  group_by(id) %>%
  summarise(across(all_of(bg_vars), first_non_na), .groups = "drop")

# Liitä taustamuttujat means_wide-dataan
means_wide <- means_wide %>%
  mutate(id = as.character(id)) %>%
  left_join(bg_from_preg, by = "id")


# Selittäjät
predictors <- c(
  "age_category","education","previous_children","epds_category",
  "bmi_bl2","gt_weight_gain",
  "pp_weight_lost","delivery_method","steps","average_met"
)

periods   <- c("T1","T2","T3","PP")
responses <- c("score","duration")

# Siistit nimet ja tasotekstit
nice_name <- c(
  age_category                  = "Age category",
  education                     = "Education",
  previous_children             = "Parity",
  epds_category                 = "EPDS category",
  bmi_bl2                       = "BMI at baseline",
  gt_weight_gain                = "Gestational weight gain",
  pp_weight_lost                = "Postpartum weight lost",
  delivery_method               = "Delivery method",
  steps                         = "Steps",
  average_met                   = "Average MET"
)
nice_levels <- list(
  age_category = c("Under 30"="Under 30","30 or more"="30 or more"),
  education = c("1"="secondary education or below","2"="college or university"),
  epds_category = c("No depression"="No depression","Possible depr"="Possible depression"),
  bmi_bl2 = c("1"="Overweight","2"="Obesity"),
  gt_weight_gain = c("within or less"="within or less","more than recommendation"="more than recommendation"),
  pp_weight_lost = c("0"="No","1"="Yes"),
  previous_children = c("0"="primipara","1"="multipara"),
  delivery_method = c("1"="Vaginal","2"="Vacuum-assisted","3"="Caesarian section")
)
nice_var <- function(var) if (!is.null(nice_name[[var]])) nice_name[[var]] else gsub("_"," ", tools::toTitleCase(var))
nice_lvl <- function(var, lvl) {
  m <- nice_levels[[var]]; key <- as.character(lvl)
  if (!is.null(m) && key %in% names(m)) m[[key]] else as.character(lvl)
}

rows <- list()

for (per in periods) {
  for (resp in responses) {
    y_col <- paste0(resp, "_", per, "_mean")
    if (!y_col %in% names(means_wide)) next
    Y <- as.numeric(scale(means_wide[[y_col]]))
    
    for (pred in predictors) {
      
      # pp_weight_lost vain PP:ssä
      if (pred == "pp_weight_lost" && per != "PP") next
      
      # Aktiivisuus: käytä periodikohtaista z-saraketta jos löytyy
      x_per_col <- paste0(pred, "_", per, "_mean_z")
      if (x_per_col %in% names(means_wide)) {
        X <- means_wide[[x_per_col]]
        if (all(is.na(X)) || sd(X, na.rm = TRUE) == 0) next
        lab <- nice_var(pred)
        
        fit <- lm(Y ~ X)
        b <- coef(summary(fit))["X", c("Estimate","Std. Error")]
        rows[[length(rows)+1]] <- tibble(
          Predictor = lab,
          Estimate  = b[1],
          Q2_5      = b[1] - 1.96*b[2],
          Q97_5     = b[1] + 1.96*b[2],
          Response  = ifelse(resp=="score","Score","Duration"),
          Period    = per
        )
        next
      }
      
      # PP:ssä delivery_method → kaksi kontrastia vs Vaginal
      if (pred == "delivery_method" && per == "PP" && "delivery_method" %in% names(means_wide)) {
        dm <- factor(as.character(means_wide$delivery_method), levels = c("1","2","3"))
        
        # Vacuum vs Vaginal
        X <- as.numeric(dm == "2")
        if (sd(X, na.rm = TRUE) > 0) {
          fit <- lm(Y ~ X); b <- coef(summary(fit))["X", c("Estimate","Std. Error")]
          rows[[length(rows)+1]] <- tibble(
            Predictor = sprintf("%s (%s vs %s)", nice_var("delivery_method"),
                                nice_lvl("delivery_method","2"), nice_lvl("delivery_method","1")),
            Estimate  = b[1], Q2_5 = b[1]-1.96*b[2], Q97_5 = b[1]+1.96*b[2],
            Response  = ifelse(resp=="score","Score","Duration"),
            Period    = per
          )
        }
        # Section vs Vaginal
        X <- as.numeric(dm == "3")
        if (sd(X, na.rm = TRUE) > 0) {
          fit <- lm(Y ~ X); b <- coef(summary(fit))["X", c("Estimate","Std. Error")]
          rows[[length(rows)+1]] <- tibble(
            Predictor = sprintf("%s (%s vs %s)", nice_var("delivery_method"),
                                nice_lvl("delivery_method","3"), nice_lvl("delivery_method","1")),
            Estimate  = b[1], Q2_5 = b[1]-1.96*b[2], Q97_5 = b[1]+1.96*b[2],
            Response  = ifelse(resp=="score","Score","Duration"),
            Period    = per
          )
        }
        next
      }
      
      # Muut taustamuuttujat
      if (!(pred %in% names(means_wide))) next
      f <- factor(means_wide[[pred]])
      if (nlevels(f) != 2) next
      
      X <- as.numeric(f == levels(f)[2])
      if (all(is.na(X)) || sd(X, na.rm = TRUE) == 0) next
      
      fit <- lm(Y ~ X)
      b <- coef(summary(fit))["X", c("Estimate","Std. Error")]
      
      rows[[length(rows)+1]] <- tibble(
        Predictor = sprintf("%s (%s vs %s)",
                            nice_var(pred),
                            nice_lvl(pred, levels(f)[2]),
                            nice_lvl(pred, levels(f)[1])),
        Estimate  = b[1],
        Q2_5      = b[1] - 1.96*b[2],
        Q97_5     = b[1] + 1.96*b[2],
        Response  = ifelse(resp=="score","Score","Duration"),
        Period    = per
      )
    }
  }
}

df_plot <- bind_rows(rows) %>%
  mutate(
    Predictor = factor(Predictor, levels = rev(unique(Predictor))),
    Response  = factor(Response, levels = c("Score","Duration")),
    Period    = factor(Period, levels = periods)
  )

p <- ggplot(df_plot) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(x = Predictor, ymin = Q2_5, ymax = Q97_5, color = Response),
    position = position_dodge(width = 0.5), width = 0.3, linewidth = 0.9
  ) +
  geom_point(
    aes(x = Predictor, y = Estimate, color = Response),
    position = position_dodge(width = 0.5), size = 2
  ) +
  coord_flip() +
  facet_grid(. ~ Period) +
  labs(
    title = "Univariaattiset mallit (95 % LV): Unenlaatu ja kesto",
    x = "",
    color = "Selitettävä"
  ) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"),
                     labels = c("Score", "Duration")) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 12)
  )

print(p)

# Luo kansio ja tallenna kuva
dir.create("Figures", showWarnings = FALSE, recursive = TRUE)
ggsave("Figures/univariate_sd_plot.png", p, width = 16, height = 10, units = "in", dpi = 600)
