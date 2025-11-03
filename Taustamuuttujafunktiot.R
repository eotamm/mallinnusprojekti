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



#### Merkitsevät interaktiot
# Merkitsevien interaktioiden muuttujat mallista
get_significant_interactions <- function(mod,
                                         focal = "average_met_mean3_z",
                                         alpha = 0.05) {
  mf <- tryCatch(getME(mod, "frame"), error = function(e) model.frame(mod))
  by_candidates <- setdiff(names(mf), c(focal, as.character(formula(mod))[2]))
  
  # Poimi interaktiotermit
  tt <- broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE)
  int_terms <- tt %>%
    filter(str_detect(term, ":"),
           str_detect(term, paste0("(^", focal, ":)|(:", focal, "$)|(:", focal, ":)"))) %>%
    mutate(is_sig = ifelse(!is.na(p.value),
                           p.value < alpha,
                           (conf.low > 0 | conf.high < 0))) %>%
    filter(is_sig) %>%
    pull(term)
  
  if (length(int_terms) == 0) return(character(0))
  
  # Etsi kullekin termille se sarakenimi
  by_vars <- lapply(int_terms, function(trm) {
    others <- setdiff(by_candidates, focal)
    hit <- others[str_detect(trm, paste0("(^|:)", others, "([^:])*($|:)"))]
    if (length(hit) >= 1) hit[[1]] else NA_character_
  }) |> unlist() |> unique()
  
  by_vars[!is.na(by_vars)]
}

# Piirrä yhden interaktion käyrät
plot_interaction <- function(mod,
                             focal = "average_met_mean3_z",
                             by,
                             title = NULL) {
  resp <- as.character(formula(mod))[2]
  eff <- ggpredict(mod, terms = c(paste0(focal, " [all]"), by))
  if (is.null(title)) title <- paste("Interaktio:", focal, "×", by)
  ggplot(eff, aes(x = x, y = predicted, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
                alpha = .15, color = NA) +
    geom_line(linewidth = 1) +
    labs(x = focal,
         y = paste("Mallin ennuste:", resp),
         color = by, fill = by,
         title = title) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

# Aja kaikki merkitsevät ja tallenna
plot_all_sig_interactions <- function(mod,
                                      model_tag,
                                      focal = "average_met_mean3_z",
                                      outdir = "figures/taustamuuttujamallit",
                                      alpha = 0.05) {
  dir_create(outdir)
  by_vars <- get_significant_interactions(mod, focal = focal, alpha = alpha)
  if (length(by_vars) == 0) {
    message("[", model_tag, "] Ei merkitseviä interaktioita (alpha = ", alpha, ").")
    return(invisible(NULL))
  }
  for (bv in by_vars) {
    p <- plot_interaction(mod, focal = focal, by = bv,
                          title = paste0(model_tag, ": ", focal, " × ", bv))
    print(p)
    fn <- file.path(outdir, paste0(model_tag, "__", focal, "_x_", bv, ".png"))
    ggsave(fn, p, width = 7, height = 5, dpi = 300)
    message("Tallennettu: ", fn)
  }
  invisible(by_vars)
}


# Halutut interaktiot
plot_interaction_facets <- function(
    by,
    focal  = "average_met_mean3_z",
    xlab   = NULL,
    title  = NULL,
    outdir = out_dir,
    fname  = NULL
) {
  models <- list(
    "Pregnancy: duration" = m_preg_dur_int,
    "Pregnancy: score"    = m_preg_score_int,
    "Postpartum: duration"= m_post_dur_int,
    "Postpartum: score"   = m_post_score_int
  )
  
  dfl <- list()
  for (nm in names(models)) {
    mod <- models[[nm]]
    mf <- tryCatch(getME(mod, "frame"), error = function(e) model.frame(mod))
    if (!(focal %in% names(mf) && by %in% names(mf))) next
    eff <- ggpredict(mod, terms = c(paste0(focal, " [all]"), by))
    eff$model <- nm
    eff$resp  <- as.character(formula(mod))[2]
    dfl[[nm]] <- eff
  }
  if (length(dfl) == 0) stop("Yhdessäkään mallissa ei löytynyt sekä '", focal, "' että '", by, "'.")
  
  dd <- do.call(rbind, dfl)
  dd$Stage   <- ifelse(grepl("^Pregnancy", dd$model), "Pregnancy", "Postpartum")
  dd$Outcome <- ifelse(grepl("duration",  dd$model, ignore.case = TRUE), "Duration", "Score")
  dd$Stage   <- factor(dd$Stage,   levels = c("Pregnancy", "Postpartum"))
  dd$Outcome <- factor(dd$Outcome, levels = c("Duration", "Score"))
  
  if (is.null(title)) title <- paste0("Interaktiot: ", focal, " × ", by)
  if (is.null(xlab))  xlab  <- focal
  
  p <- ggplot(dd, aes(x = x, y = predicted, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    facet_grid(Outcome ~ Stage, scales = "free_y") +
    labs(x = xlab, y = "Mallin ennuste", color = by, fill = by, title = title) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  print(p)
  if (is.null(fname)) fname <- paste0("facet_interaction__", focal, "_x_", by, ".png")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(outdir, fname), p, width = 9, height = 7, dpi = 300)
  invisible(p)
}
