##################################################################
############ Taustamuuttujien suhteen tehdyt kuvaajat ############

library(dplyr)
library(forcats)
library(ggplot2)
library(rlang)
library(patchwork)


dir.path <- file.path("./data") # symlink 
#dir.path <- file.path("../../Seafile/Projektikurssi") # Arille 

source("Taustamuuttujafunktiot.R")

# Haetaan puhdisteut aineistot
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame()


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
       p_preg_age, width = 10, height = 8, dpi = 300)

p_preg_edu  <- plot_sleep_hist_panel(pregnancy, by = "education",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_education.png"),
       p_preg_edu, width = 10, height = 8, dpi = 300)

p_preg_prev <- plot_sleep_hist_panel(pregnancy, by = "previous_children",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_previous_children.png"),
       p_preg_prev, width = 10, height = 8, dpi = 300)

p_preg_epds <- plot_sleep_hist_panel(pregnancy, by = "epds_category",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_epds_category.png"),
       p_preg_epds, width = 10, height = 8, dpi = 300)

p_preg_bmi  <- plot_sleep_hist_panel(pregnancy, by = "bmi_bl2",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_bmi_bl2.png"),
       p_preg_bmi, width = 10, height = 8, dpi = 300)

p_preg_gwg  <- plot_sleep_hist_panel(pregnancy, by = "gt_weight_gain",
                                     phase_label = "Pregnancy",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "pregnancy_hist_panel_gt_weight_gain.png"),
       p_preg_gwg, width = 10, height = 8, dpi = 300)


# POSTPARTUM
p_post_age  <- plot_sleep_hist_panel(postpartum, by = "age_category",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_age_category.png"),
       p_post_age, width = 10, height = 8, dpi = 300)

p_post_edu  <- plot_sleep_hist_panel(postpartum, by = "education",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_education.png"),
       p_post_edu, width = 10, height = 8, dpi = 300)

p_post_prev <- plot_sleep_hist_panel(postpartum, by = "previous_children",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_previous_children.png"),
       p_post_prev, width = 10, height = 8, dpi = 300)

p_post_epds <- plot_sleep_hist_panel(postpartum, by = "epds_category",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_epds_category.png"),
       p_post_epds, width = 10, height = 8, dpi = 300)

p_post_bmi  <- plot_sleep_hist_panel(postpartum, by = "bmi_bl2",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_bmi_bl2.png"),
       p_post_bmi, width = 10, height = 8, dpi = 300)

p_post_gwg  <- plot_sleep_hist_panel(postpartum, by = "gt_weight_gain",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_gt_weight_gain.png"),
       p_post_gwg, width = 10, height = 8, dpi = 300)

p_post_del  <- plot_sleep_hist_panel(postpartum, by = "delivery_method",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_delivery_method.png"),
       p_post_del, width = 10, height = 8, dpi = 300)

p_post_ppwl <- plot_sleep_hist_panel(postpartum, by = "pp_weight_lost",
                                     phase_label = "Postpartum",
                                     binwidth_duration = 0.5, binwidth_score = 5)
ggsave(file.path(out_dir, "postpartum_hist_panel_pp_weight_lost.png"),
       p_post_ppwl, width = 10, height = 8, dpi = 300)




# Boxplot
out_dir_box <- file.path("figures", "taustamuuttujaboxplotit")
dir.create(out_dir_box, recursive = TRUE, showWarnings = FALSE)

## PREGNANCY
p_preg_dur_box <- plot_sleep_box_panel_all_bg(
  pregnancy,
  outcome     = "duration",
  phase_label = "Pregnancy"
)
ggsave(file.path(out_dir_box, "pregnancy_boxpanel_duration.png"),
       p_preg_dur_box, width = 10, height = 14, dpi = 300)

p_preg_score_box <- plot_sleep_box_panel_all_bg(
  pregnancy,
  outcome     = "score",
  phase_label = "Pregnancy"
)
ggsave(file.path(out_dir_box, "pregnancy_boxpanel_score.png"),
       p_preg_score_box, width = 10, height = 14, dpi = 300)

p_preg_eff_box <- plot_sleep_box_panel_all_bg(
  pregnancy,
  outcome     = "efficiency",
  phase_label = "Pregnancy"
)
ggsave(file.path(out_dir_box, "pregnancy_boxpanel_efficiency.png"),
       p_preg_eff_box, width = 10, height = 14, dpi = 300)


## POSTPARTUM
p_post_dur_box <- plot_sleep_box_panel_all_bg(
  postpartum,
  outcome     = "duration",
  phase_label = "Postpartum"
)
ggsave(file.path(out_dir_box, "postpartum_boxpanel_duration.png"),
       p_post_dur_box, width = 10, height = 14, dpi = 300)

p_post_score_box <- plot_sleep_box_panel_all_bg(
  postpartum,
  outcome     = "score",
  phase_label = "Postpartum"
)
ggsave(file.path(out_dir_box, "postpartum_boxpanel_score.png"),
       p_post_score_box, width = 10, height = 14, dpi = 300)

p_post_eff_box <- plot_sleep_box_panel_all_bg(
  postpartum,
  outcome     = "efficiency",
  phase_label = "Postpartum"
)
ggsave(file.path(out_dir_box, "postpartum_boxpanel_efficiency.png"),
       p_post_eff_box, width = 10, height = 14, dpi = 300)

