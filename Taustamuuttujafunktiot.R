# Vaikutusten plottaus
plot_coef_forest_pretty <- function(mod, title = "Fixed effects (95% CI)",
                                    alpha_ns = 0.35,
                                    col_sig = "red",
                                    col_ns  = "black") {
  
  dd <- broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    filter(!grepl("ns\\(|bs\\(|poly\\(|(^|:)s\\(", term)) %>%
    mutate(
      sig  = ifelse(conf.low > 0 | conf.high < 0, "Merkitsevä", "Ei merkitsevä"),
      type = ifelse(grepl(":", term), "Interaktio", "Päävaikutus"),
      term = fct_reorder(term, estimate)
    )
  
  ggplot(dd, aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.6, color = "#9E9E9E") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, alpha = sig, color = sig),
                  width = 0.15, linewidth = 1) +
    geom_point(aes(shape = type, alpha = sig, color = sig),
               size = 2.8, stroke = 0.7) +
    scale_color_manual(values = c("Merkitsevä" = col_sig,
                                  "Ei merkitsevä" = col_ns)) +
    scale_alpha_manual(values = c("Merkitsevä" = 1, "Ei merkitsevä" = alpha_ns), guide = "none") +
    scale_shape_manual(values = c("Päävaikutus" = 16, "Interaktio" = 17)) +
    labs(x = "Kerroin (95% CI)", y = NULL, title = title, color = NULL, shape = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
}


# Värit + teema
golden_coast_colors <- c("#1E90FF", "#FF6F20", "#FFD700", "#FFB6C1", "#20B2AA")
theme_golden_coast <- theme_minimal(base_size = 13) +
  theme(
    text = element_text(color = "#2F3E46"),
    plot.title = element_text(face = "bold", color = "black"),
    axis.title = element_text(color = "#2F3E46"),
    axis.text  = element_text(color = "#2F3E46"),
    panel.grid.major = element_line(color = scales::alpha("#2F3E46", 0.08)),
    panel.grid.minor = element_line(color = scales::alpha("#2F3E46", 0.04)),
    legend.title = element_blank()
  )

# Kiinteiden tekijöiden perusmuuttujat
.fixed_preds_lme <- function(mod) {
  tl <- tryCatch(attr(terms(formula(mod)), "term.labels"), error = function(e) character(0))
  if (!length(tl)) return(character(0))
  tl <- gsub("\\b(ns|bs|poly|s|scale|log|I)\\s*\\(([^\\)]*)\\)", "\\2", tl)
  trimws(unique(unlist(strsplit(tl, ":", fixed = TRUE))))
}

# Facetoitu interaktiokuva
plot_interaction_facets <- function(
    by,
    focal  = c("average_met", "average_met_lag1", "average_met_mean3", "average_met_mean3_z"),
    xlab   = NULL,
    title  = NULL,
    outdir = out_dir,
    fname  = NULL
) {
  models <- list(
    "Pregnancy: duration"    = m_preg_dur_int,
    "Pregnancy: score"       = m_preg_score_int,
    "Pregnancy: efficiency"  = m_preg_eff_int,
    "Postpartum: duration"   = m_post_dur_int,
    "Postpartum: score"      = m_post_score_int,
    "Postpartum: efficiency" = m_post_eff_int
  )
  
  dfl <- list(); used_focals <- character(0)
  for (nm in names(models)) {
    mod <- models[[nm]]
    preds <- .fixed_preds_lme(mod)
    focal_here <- intersect(focal, preds)[1]
    if (is.na(focal_here) || !(by %in% preds)) next
    eff <- ggeffects::ggpredict(mod, terms = c(paste0(focal_here, " [all]"), by))
    eff$model <- nm
    eff$Stage <- ifelse(grepl("^Pregnancy", nm), "Pregnancy", "Postpartum")
    eff$Outcome <- ifelse(grepl("duration", nm, TRUE), "Duration",
                          ifelse(grepl("score", nm, TRUE), "Score", "Efficiency"))
    eff$focal <- focal_here
    eff$by <- by
    dfl[[length(dfl) + 1]] <- eff
    used_focals <- c(used_focals, focal_here)
  }
  if (!length(dfl)) stop("Yhdessäkään mallissa ei ollut sekä 'focal' (", paste(focal, collapse=", "),
                         ") että '", by, "' kiinteissä tekijöissä.")
  
  dd <- do.call(rbind, dfl)
  dd$Stage   <- factor(dd$Stage,   levels = c("Pregnancy", "Postpartum"))
  dd$Outcome <- factor(dd$Outcome, levels = c("Duration", "Score", "Efficiency"))
  
  if (is.null(title)) title <- paste0("Interaktiot: ", paste(unique(used_focals), collapse = " / "), " × ", by)
  if (is.null(xlab))  xlab  <- if (length(unique(dd$focal)) == 1) unique(dd$focal) else "Altiste"
  
  ngrp <- length(unique(dd$group))
  cols <- rep_len(golden_coast_colors, ngrp)
  
  xr_preg <- range(dd$x[dd$Stage == "Pregnancy"],  na.rm = TRUE)
  xr_post <- range(dd$x[dd$Stage == "Postpartum"], na.rm = TRUE)
  yr_dur   <- range(dd$predicted[dd$Outcome == "Duration"],
                    dd$conf.low[dd$Outcome == "Duration"],
                    dd$conf.high[dd$Outcome == "Duration"], na.rm = TRUE)
  yr_score <- range(dd$predicted[dd$Outcome == "Score"],
                    dd$conf.low[dd$Outcome == "Score"],
                    dd$conf.high[dd$Outcome == "Score"], na.rm = TRUE)
  yr_eff   <- range(dd$predicted[dd$Outcome == "Efficiency"],
                    dd$conf.low[dd$Outcome == "Efficiency"],
                    dd$conf.high[dd$Outcome == "Efficiency"], na.rm = TRUE)
  
  p <- ggplot(dd, aes(x = x, y = predicted, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.18, color = NA) +
    geom_line(linewidth = 1) +
    ggh4x::facet_grid2(rows = vars(Outcome), cols = vars(Stage), scales = "free") +
    ggh4x::facetted_pos_scales(
      x = list(
        Stage == "Pregnancy"  ~ scale_x_continuous(limits = xr_preg),
        Stage == "Postpartum" ~ scale_x_continuous(limits = xr_post)
      ),
      y = list(
        Outcome == "Duration"   ~ scale_y_continuous(limits = yr_dur),
        Outcome == "Score"      ~ scale_y_continuous(limits = yr_score),
        Outcome == "Efficiency" ~ scale_y_continuous(limits = yr_eff)
      )
    ) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values  = scales::alpha(cols, 0.25)) +
    labs(x = xlab, y = "Mallin ennuste", color = by, fill = by, title = title) +
    theme_golden_coast
  
  print(p)
  if (is.null(fname)) fname <- paste0("facet_interaction__",
                                      gsub("[^A-Za-z0-9]+","_", paste(unique(used_focals), collapse = "_OR_")),
                                      "_x_", by, ".png")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(outdir, fname), p, width = 12, height = 10, dpi = 300)
  invisible(p)
}

