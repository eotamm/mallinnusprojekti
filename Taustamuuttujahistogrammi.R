library(dplyr)
library(forcats)
library(ggplot2)
library(rlang)
library(patchwork)


dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 


# Haetaan puhdisteut aineistot
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()

# Tasojen uudelleennimeäminen
recode_background_factors <- function(df) {
  chr <- function(x) if (is.factor(x)) as.character(x) else as.character(x)
  
  if ("age_category" %in% names(df)) {
    x <- chr(df$age_category)
    df$age_category <- factor(
      dplyr::case_when(
        x %in% c("0","Under 30","under 30","<30") ~ "Under 30",
        x %in% c("1","30 or more","30+","over 30") ~ "30 or more",
        TRUE ~ x
      ),
      levels = c("Under 30","30 or more")
    )
  }
  
  if ("bmi_bl2" %in% names(df)) {
    x <- chr(df$bmi_bl2)
    df$bmi_bl2 <- factor(
      dplyr::case_when(
        x %in% c("1","Overweight","overweight") ~ "Overweight",
        x %in% c("2","Obesity","obesity") ~ "Obesity",
        TRUE ~ x
      ),
      levels = c("Overweight","Obesity")
    )
  }
  
  if ("delivery_method" %in% names(df)) {
    x <- chr(df$delivery_method)
    df$delivery_method <- factor(
      dplyr::case_when(
        x %in% c("1","Vaginal","vaginal") ~ "Vaginal",
        x %in% c("2","Vacuum-assisted","vacuum-assisted","vacuum") ~ "Vacuum-assisted",
        x %in% c("3","Caesarian section","Caesarean section","c-section","cesarean","sectio") ~ "Caesarian section",
        TRUE ~ x
      ),
      levels = c("Vaginal","Vacuum-assisted","Caesarian section")
    )
  }
  
  if ("epds_category" %in% names(df)) {
    x <- chr(df$epds_category)
    df$epds_category <- factor(
      dplyr::case_when(
        x %in% c("0","No depression","no depression") ~ "No depression",
        x %in% c("1","Possible depr","possible depr","Possible depression","possible depression") ~ "Possible depr",
        TRUE ~ x
      ),
      levels = c("No depression","Possible depr")
    )
  }
  
  if ("education" %in% names(df)) {
    x <- chr(df$education)
    df$education <- factor(
      dplyr::case_when(
        x %in% c("1","secondary education or below","secondary","below secondary") ~ "secondary education or below",
        x %in% c("2","college or university","college","university","higher") ~ "college or university",
        TRUE ~ x
      ),
      levels = c("secondary education or below","college or university")
    )
  }
  
  # HUOM: käytetään vain gt_weight_gain (teksti), ei within_or_less -muuttujaa
  if ("gt_weight_gain" %in% names(df)) {
    x <- chr(df$gt_weight_gain)
    df$gt_weight_gain <- factor(
      dplyr::case_when(
        x %in% c("within or less","within","within recommendations") ~ "within or less",
        x %in% c("more than recommendation","more than recommendat","more") ~ "more than recommendation",
        TRUE ~ x
      ),
      levels = c("within or less","more than recommendation")
    )
  }
  
  if ("pp_weight_lost" %in% names(df)) {
    x <- chr(df$pp_weight_lost)
    df$pp_weight_lost <- factor(
      dplyr::case_when(
        x %in% c("1","Yes","yes","TRUE","T") ~ "Yes",
        x %in% c("0","No","no","FALSE","F") ~ "No",
        TRUE ~ x
      ),
      levels = c("Yes","No")
    )
  }
  
  if ("previous_children" %in% names(df)) {
    x <- chr(df$previous_children)
    df$previous_children <- factor(
      dplyr::case_when(
        x %in% c("0","nullipara","no previous children","no children") ~ "primipara",
        x %in% c("1","primi- or multipara","primipara","multipara") ~ "multipara",
        TRUE ~ x
      ),
      levels = c("primipara","multipara")
    )
  }
  
  df
}

# Paneelihistogrammi: kesto + score + tehokkuus
plot_sleep_hist_panel <- function(df,
                                  by,
                                  phase_label        = "Pregnancy",
                                  binwidth_duration  = 0.25,
                                  binwidth_score     = 5,
                                  binwidth_efficiency= 2,
                                  free_y             = TRUE,
                                  non_wear_max       = Inf) {
  stopifnot(all(c("duration","score","efficiency") %in% names(df)))
  if (!by %in% names(df)) stop(sprintf("Taustamuuttuja '%s' puuttuu datasta.", by))
  
  df2 <- df %>%
    dplyr::mutate(
      duration_h = duration / 3600,
      !!by := as.factor(.data[[by]])
    ) %>%
    dplyr::filter(!is.na(.data[[by]])) %>%
    { if ("non_wear" %in% names(.)) dplyr::filter(., is.na(non_wear) | non_wear <= non_wear_max) else . }
  
  p_dur <- df2 %>%
    dplyr::filter(!is.na(duration_h)) %>%
    ggplot2::ggplot(ggplot2::aes(x = duration_h)) +
    ggplot2::geom_histogram(binwidth = binwidth_duration, boundary = 0, closed = "left") +
    ggplot2::facet_wrap(vars(!!rlang::sym(by)), scales = if (free_y) "free_y" else "fixed", drop = TRUE) +
    ggplot2::labs(title = paste(phase_label, "- Unen kesto (histogrammi)"),
                  subtitle = paste("Ryhmittäin:", by),
                  x = "Unen kesto (h)", y = "Lukumäärä") +
    ggplot2::theme_minimal(base_size = 12)
  
  p_sco <- df2 %>%
    dplyr::filter(!is.na(score)) %>%
    ggplot2::ggplot(ggplot2::aes(x = score)) +
    ggplot2::geom_histogram(binwidth = binwidth_score, boundary = 0, closed = "left") +
    ggplot2::facet_wrap(vars(!!rlang::sym(by)), scales = if (free_y) "free_y" else "fixed", drop = TRUE) +
    ggplot2::labs(title = paste(phase_label, "- Oura score (histogrammi)"),
                  subtitle = paste("Ryhmittäin:", by),
                  x = "Oura score (0–100)", y = "Lukumäärä") +
    ggplot2::theme_minimal(base_size = 12)
  
  p_eff <- df2 %>%
    dplyr::filter(!is.na(efficiency)) %>%
    ggplot2::ggplot(ggplot2::aes(x = efficiency)) +
    ggplot2::geom_histogram(binwidth = binwidth_efficiency, boundary = 0, closed = "left") +
    ggplot2::facet_wrap(vars(!!rlang::sym(by)), scales = if (free_y) "free_y" else "fixed", drop = TRUE) +
    ggplot2::labs(title = paste(phase_label, "- Unen tehokkuus (histogrammi)"),
                  subtitle = paste("Ryhmittäin:", by),
                  x = "Tehokkuus (%-yksikköä, 0–100)", y = "Lukumäärä") +
    ggplot2::theme_minimal(base_size = 12)
  
  # Kolme paneelia päällekkäin
  p_dur / p_sco / p_eff + patchwork::plot_layout(heights = c(1, 1, 1))
}


# Datat uusilla nimillä
pregnancy  <- recode_background_factors(pregnancy)
postpartum <- recode_background_factors(postpartum)



# Luo alakansio: figures/taustamuuttujahistogrammit
out_dir <- file.path("figures", "taustamuuttujahistogrammit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# PREGNANCY
p_preg_age  <- plot_sleep_hist_panel(pregnancy, by = "age_category",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_age_category.png"),
       p_preg_age, width = 9, height = 8, dpi = 300)

p_preg_edu  <- plot_sleep_hist_panel(pregnancy, by = "education",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_education.png"),
       p_preg_edu, width = 9, height = 8, dpi = 300)

p_preg_prev <- plot_sleep_hist_panel(pregnancy, by = "previous_children",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_previous_children.png"),
       p_preg_prev, width = 9, height = 8, dpi = 300)

p_preg_epds <- plot_sleep_hist_panel(pregnancy, by = "epds_category",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_epds_category.png"),
       p_preg_epds, width = 9, height = 8, dpi = 300)

p_preg_bmi  <- plot_sleep_hist_panel(pregnancy, by = "bmi_bl2",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_bmi_bl2.png"),
       p_preg_bmi, width = 9, height = 8, dpi = 300)

p_preg_gwg  <- plot_sleep_hist_panel(pregnancy, by = "gt_weight_gain",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_gt_weight_gain.png"),
       p_preg_gwg, width = 9, height = 8, dpi = 300)


# POSTPARTUM
p_post_age  <- plot_sleep_hist_panel(postpartum, by = "age_category",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_age_category.png"),
       p_post_age, width = 9, height = 8, dpi = 300)

p_post_edu  <- plot_sleep_hist_panel(postpartum, by = "education",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_education.png"),
       p_post_edu, width = 9, height = 8, dpi = 300)

p_post_prev <- plot_sleep_hist_panel(postpartum, by = "previous_children",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_previous_children.png"),
       p_post_prev, width = 9, height = 8, dpi = 300)

p_post_epds <- plot_sleep_hist_panel(postpartum, by = "epds_category",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_epds_category.png"),
       p_post_epds, width = 9, height = 8, dpi = 300)

p_post_bmi  <- plot_sleep_hist_panel(postpartum, by = "bmi_bl2",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_bmi_bl2.png"),
       p_post_bmi, width = 9, height = 8, dpi = 300)

p_post_gwg  <- plot_sleep_hist_panel(postpartum, by = "gt_weight_gain",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_gt_weight_gain.png"),
       p_post_gwg, width = 9, height = 8, dpi = 300)

p_post_del  <- plot_sleep_hist_panel(postpartum, by = "delivery_method",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_delivery_method.png"),
       p_post_del, width = 9, height = 8, dpi = 300)

p_post_ppwl <- plot_sleep_hist_panel(postpartum, by = "pp_weight_lost",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_pp_weight_lost.png"),
       p_post_ppwl, width = 9, height = 8, dpi = 300)
