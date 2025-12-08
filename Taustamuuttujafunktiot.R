## Nimet
pretty_terms <- c(
  "average_met"         = "Aktiivisuus (z)",
  "average_met_lag1"    = "Aktiivisuus (ed. pvä, z)",
  "age_category30 or more"              = "Ikä: 30+ vs. <30",
  "education2"                          = "Koulutus: korkea-aste",
  "previous_children1"                  = "Aiemmat lapset: kyllä",
  "epds_categoryPossible depr"          = "EPDS: mahdollinen masennus",
  "bmi_bl22"                            = "Lähtö-BMI: lihavuus",
  "gt_weight_gainmore than recommendat" = "Painonnousu: yli suosituksen",
  "pp_weight_lost0"                     = "PP painonlasku: ei",
  "pp_weight_lost1"                     = "PP painonlasku: kyllä",
  "delivery_methodVacuum-assisted"      = "Synnytystapa: imukuppi",
  "delivery_methodCaesarian section"    = "Synnytystapa: sektio"
)

## Poistettavat termit (intercept + splinet)
drop_pattern <- "(^\\(Intercept\\)$)|ns\\(|bs\\(|poly\\(|(^|:)s\\(|scale\\(|log\\(|I\\("

make_pretty_label <- function(term) {
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  parts_pretty <- map_chr(parts, ~ ifelse(.x %in% names(pretty_terms), pretty_terms[.x], .x))
  if (length(parts_pretty) == 1) parts_pretty else paste(parts_pretty, collapse = " $\\times$ ")
}
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "$<0.001$" else sprintf("$%.3f$", p)
}

# Latex taulukko
latex_main_inter_table <- function(mod,
                                   alpha = 0.05,
                                   digits = 3,
                                   sort_within = c("none","p","abs_est"),
                                   caption = "Kiinteät vaikutukset: päävaikutukset ja interaktiot",
                                   label   = "tab:fixed_main_inter") {
  sort_within <- match.arg(sort_within)
  
  td <- broom.mixed::tidy(mod, effects = "fixed") %>%
    filter(!str_detect(term, drop_pattern)) %>%   # poista intercept & splinet
    mutate(type = if_else(str_detect(term, ":"), "Interaktio", "Päävaikutus"))
  
  if (!("df" %in% names(td))) stop("Tidy ei sisältänyt 'df' -saraketta; päivitä broom.mixed.")
  
  tcrit <- qt(1 - alpha/2, df = td$df)
  td <- td %>%
    mutate(
      conf.low  = estimate - tcrit * std.error,
      conf.high = estimate + tcrit * std.error,
      label     = vapply(term, make_pretty_label, character(1)),
      est_txt   = sprintf(paste0("%.", digits, "f"), estimate),
      lo_txt    = sprintf(paste0("%.", digits, "f"), conf.low),
      hi_txt    = sprintf(paste0("%.", digits, "f"), conf.high),
      p_txt     = map_chr(p.value, format_p),
      ci_txt    = paste0("$[", lo_txt, ",\\,", hi_txt, "]$")
    )
  
  # Järjestys ryhmien sisällä
  td <- td %>%
    group_by(type) %>%
    { 
      if (sort_within == "p") arrange(., p.value, .by_group = TRUE)
      else if (sort_within == "abs_est") arrange(., desc(abs(estimate)), .by_group = TRUE)
      else .
    } %>%
    ungroup()
  
  # Rakenna LaTeX-runko kahdella osuudella
  make_rows <- function(d) sprintf("%s & $%s$ & %s & %s \\\\",
                                   d$label, d$est_txt, d$ci_txt, d$p_txt)
  
  rows_main <- td %>% filter(type == "Päävaikutus") %>% make_rows() %>% paste(collapse = "\n")
  rows_int  <- td %>% filter(type == "Interaktio")  %>% make_rows() %>% paste(collapse = "\n")
  
  body <- paste0(
    "\\multicolumn{4}{l}{\\textit{Päävaikutukset}}\\\\\n",
    rows_main,
    "\n\\addlinespace\n\\multicolumn{4}{l}{\\textit{Interaktiot}}\\\\\n",
    rows_int
  )
  
  out <- sprintf("\\begin{table}[H]
\\centering
\\begin{tabular}{lccc}
\\toprule
Muuttuja & Estimaatti & 95\\%% CI & p-arvo \\\\
\\midrule
%s
\\bottomrule
\\end{tabular}
\\caption{%s}
\\label{%s}
\\end{table}", body, caption, label)
  
  structure(out, data = td)
}


# Vaikutusten plottaus
plot_coef_forest_pretty <- function(mod,
                                    title    = "Kiinteät vaikutukset (95% CI)",
                                    alpha_ns = 0.35,
                                    col_sig  = "#FF6F20",
                                    col_ns   = "black",
                                    pretty_terms_map = pretty_terms,
                                    drop_re   = drop_pattern) {
  
  # paikallinen labelointiapuri (käyttää annettua map:ia)
  make_pretty_label_local <- function(term) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    parts_pretty <- map_chr(
      parts,
      ~ ifelse(.x %in% names(pretty_terms_map), pretty_terms_map[.x], .x)
    )
    if (length(parts_pretty) == 1) parts_pretty else paste(parts_pretty, collapse = " X ")
  }
  
  dd <- broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE) %>%
    filter(!str_detect(term, drop_re)) %>%             # pois intercept & splinet
    mutate(
      sig   = ifelse(conf.low > 0 | conf.high < 0, "Merkitsevä", "Ei merkitsevä"),
      type  = ifelse(str_detect(term, ":"), "Interaktio", "Päävaikutus"),
      label = vapply(term, make_pretty_label_local, character(1))
    ) %>%
    # järkevä järjestys: lajittele ensin tyypeittäin, sitten estimaatin mukaan
    group_by(type) %>%
    mutate(label = fct_reorder(label, estimate)) %>%
    ungroup()
  
  ggplot(dd, aes(x = estimate, y = label)) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.6, color = "#9E9E9E") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, alpha = sig, color = sig),
                  width = 0.15, linewidth = 1) +
    geom_point(aes(shape = type, alpha = sig, color = sig),
               size = 2.8, stroke = 0.7) +
    scale_color_manual(values = c("Merkitsevä" = col_sig,
                                  "Ei merkitsevä" = col_ns)) +
    scale_alpha_manual(values = c("Merkitsevä" = 1, "Ei merkitsevä" = alpha_ns),
                       guide = "none") +
    scale_shape_manual(values = c("Päävaikutus" = 16, "Interaktio" = 17)) +
    labs(x = "Kerroin (95% CI)", y = NULL, title = title, color = NULL, shape = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold")
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


# Kiinteiden tekijöiden poiminta
.fixed_preds_lme <- function(mod) {
  tl <- tryCatch(attr(terms(formula(mod)), "term.labels"), error = function(e) character(0))
  if (!length(tl)) return(character(0))
  tl <- gsub("\\b(ns|bs|poly|s|scale|log|I)\\s*\\(([^\\)]*)\\)", "\\2", tl)
  trimws(unique(unlist(strsplit(tl, ":", fixed = TRUE))))
}

# Legendin otsikot (suomeksi)
LEGEND_TITLES <- c(
  age_category      = "Ikä",
  bmi_bl2           = "Lähtö-BMI",
  delivery_method   = "Synnytystapa",
  epds_category     = "EPDS",
  education         = "Koulutus",
  gt_weight_gain    = "Raskaudenaikainen painonnousu",
  pp_weight_lost    = "Synnytyksen jälkeinen painonlasku",
  previous_children = "Aiemmat lapset"
)

.legend_title <- function(by) ifelse(by %in% names(LEGEND_TITLES), LEGEND_TITLES[[by]], by)

.recode_group_labels <- function(by, group) {
  raw <- as.character(group)
  lab <- switch(by,
                age_category      = dplyr::recode(raw, "Under 30"="Alle 30", "30 or more"="30+",
                                                  "0"="Alle 30", "1"="30+"),
                bmi_bl2           = dplyr::recode(raw, "1"="Ylipaino", "2"="Lihavuus",
                                                  "Overweight"="Ylipaino", "Obesity"="Lihavuus"),
                delivery_method   = dplyr::recode(raw, "1"="Alatiesynnytys", "2"="Imukuppi", "3"="Sectio",
                                                  "Vaginal"="Alatiesynnytys", "Vacuum-assisted"="Imukuppi",
                                                  "Caesarian section"="Sectio"),
                epds_category     = dplyr::recode(raw, "No depression"="Ei masennusta",
                                                  "Possible depr"="Mahd. masennus", "0"="Ei masennusta", "1"="Mahd. masennus"),
                education         = dplyr::recode(raw, "1"="Toinen aste tai alle", "2"="Korkea-aste",
                                                  "secondary education or below"="Toinen aste tai alle",
                                                  "college or university"="Korkea-aste"),
                gt_weight_gain    = dplyr::recode(raw, "within or less"="Suosituksen sisällä/alle",
                                                  "more than recommendat"="Yli suosituksen",
                                                  "0"="Suosituksen sisällä/alle", "1"="Yli suosituksen"),
                pp_weight_lost    = dplyr::recode(raw, "0"="Ei", "1"="Kyllä", "No"="Ei", "Yes"="Kyllä"),
                previous_children = dplyr::recode(raw, "0"="Nullipara", "1"="Primi-/Multipara",
                                                  "nullipara"="Nullipara", "multipara"="Primi-/Multipara"),
                raw
  )
  lev <- switch(by,
                age_category        = c("Alle 30","30+"),
                bmi_bl2             = c("Ylipaino","Lihavuus"),
                delivery_method     = c("Alatiesynnytys","Imukuppi","Sectio"),
                epds_category       = c("Ei masennusta","Mahd. masennus"),
                education           = c("Toinen aste tai alle","Korkea-aste"),
                gt_weight_gain      = c("Suosituksen sisällä/alle","Yli suosituksen"),
                pp_weight_lost      = c("Ei","Kyllä"),
                previous_children   = c("Nullipara","Primi-/Multipara"),
                sort(unique(lab))
  )
  factor(lab, levels = lev)
}

make_pretty_label <- function(term, pretty_terms_map = pretty_terms) {
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  parts_pretty <- purrr::map_chr(parts, ~ ifelse(.x %in% names(pretty_terms_map), pretty_terms_map[.x], .x))
  if (length(parts_pretty) == 1) parts_pretty else paste(parts_pretty, collapse = " × ")
}

# Facetoitu interaktiokuvaaja
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
    eff$group <- .recode_group_labels(by, eff$group)
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
  
  # x-akselin otsikko kauniilla nimellä (focal)
  if (is.null(xlab)) {
    if (length(unique(dd$focal)) == 1) {
      xlab <- make_pretty_label(unique(dd$focal))
    } else {
      xlab <- "Altiste"
    }
  }
  if (is.null(title)) title <- paste0("Interaktiot: ", paste(unique(make_pretty_label(unique(used_focals))), collapse = " / "), " × ", .legend_title(by))
  
  ngrp <- length(unique(dd$group))
  cols <- rep_len(golden_coast_colors, ngrp)
  
  xr_preg <- range(dd$x[dd$Stage == "Pregnancy"],  na.rm = TRUE)
  xr_post <- range(dd$x[dd$Stage == "Postpartum"], na.rm = TRUE)
  yr_dur   <- range(dd$predicted[dd$Outcome == "Duration"],   dd$conf.low[dd$Outcome == "Duration"],   dd$conf.high[dd$Outcome == "Duration"],   na.rm = TRUE)
  yr_score <- range(dd$predicted[dd$Outcome == "Score"],      dd$conf.low[dd$Outcome == "Score"],      dd$conf.high[dd$Outcome == "Score"],      na.rm = TRUE)
  yr_eff   <- range(dd$predicted[dd$Outcome == "Efficiency"], dd$conf.low[dd$Outcome == "Efficiency"], dd$conf.high[dd$Outcome == "Efficiency"], na.rm = TRUE)
  
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
    labs(x = xlab, y = "Mallin ennuste",
         color = .legend_title(by), fill = .legend_title(by),
         title = title) +
    theme_golden_coast
  
  print(p)
  if (is.null(fname)) fname <- paste0("facet_interaction__",
                                      gsub("[^A-Za-z0-9]+","_", paste(unique(used_focals), collapse = "_OR_")),
                                      "_x_", by, ".png")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(outdir, fname), p, width = 12, height = 10, dpi = 300)
  invisible(p)
}

# Interaktiokuvaaja
plot_interaction_single <- function(
    model,
    by,
    focal      = c("average_met", "average_met_lag1", "average_met_mean3", "average_met_mean3_z"),
    model_name = NULL,
    xlab       = NULL,
    title      = NULL,
    outdir     = out_dir,
    fname      = NULL,
    width      = 8, height = 6, dpi = 300
) {
  stopifnot(requireNamespace("ggeffects"), requireNamespace("ggplot2"))
  
  preds <- .fixed_preds_lme(model)
  focal_here <- intersect(focal, preds)[1]
  if (is.na(focal_here)) stop("Yksikään 'focal' ei löytynyt mallista.")
  if (!(by %in% preds)) stop("Muuttuja '", by, "' ei ole mallissa.")
  
  eff <- ggeffects::ggpredict(model, terms = c(paste0(focal_here, " [all]"), by))
  eff$group <- .recode_group_labels(by, eff$group)
  eff$focal <- focal_here
  eff$by    <- by
  
  xr <- range(eff$x, na.rm = TRUE)
  yr <- range(eff$predicted, eff$conf.low, eff$conf.high, na.rm = TRUE)
  
  if (is.null(xlab))  xlab  <- make_pretty_label(focal_here)
  if (is.null(title)) {
    base <- ifelse(is.null(model_name), "", paste0(model_name, " — "))
    title <- paste0(base, "Interaktio: ", make_pretty_label(focal_here), " × ", .legend_title(by))
  }
  
  ngrp <- length(unique(eff$group))
  cols <- rep_len(golden_coast_colors, ngrp)
  
  p <- ggplot2::ggplot(eff, ggplot2::aes(x = x, y = predicted, color = group)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = group),
                         alpha = 0.18, color = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_continuous(limits = xr) +
    ggplot2::scale_y_continuous(limits = yr) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_fill_manual(values  = scales::alpha(cols, 0.25)) +
    ggplot2::labs(x = xlab, y = "Mallin ennuste",
                  color = .legend_title(by), fill = .legend_title(by),
                  title = title) +
    theme_golden_coast
  
  print(p)
  
  if (is.null(fname)) {
    base <- paste0("interaction__",
                   gsub("[^A-Za-z0-9]+","_", ifelse(is.null(model_name), "model", model_name)),
                   "__", gsub("[^A-Za-z0-9]+","_", focal_here),
                   "_x_", gsub("[^A-Za-z0-9]+","_", by), ".png")
    fname <- base
  }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(outdir, fname), p, width = width, height = height, dpi = dpi)
  
  invisible(p)
}


# Tasojen uudelleennimeäminen
.recode_group_labels <- function(by, group) {
  raw <- as.character(group)
  
  lab <- switch(
    by,
    
    age_category = dplyr::recode(
      raw,
      "Under 30"   = "Alle 30",
      "30 or more" = "30+",
      "0"          = "Alle 30",
      "1"          = "30+"
    ),
    
    bmi_bl2 = dplyr::recode(
      raw,
      "1"          = "Ylipaino",
      "2"          = "Lihavuus",
      "Overweight" = "Ylipaino",
      "Obesity"    = "Lihavuus"
    ),
    
    delivery_method = dplyr::recode(
      raw,
      "1"                 = "Alatiesynnytys",
      "2"                 = "Imukuppi",
      "3"                 = "Sectio",
      "Vaginal"           = "Alatiesynnytys",
      "Vacuum-assisted"   = "Imukuppi",
      "Caesarian section" = "Sectio"
    ),
    
    epds_category = dplyr::recode(
      raw,
      "No depression" = "Ei masennusta",
      "Possible depr" = "Mahd. masennus",
      "0"             = "Ei masennusta",
      "1"             = "Mahd. masennus"
    ),
    
    education = dplyr::recode(
      raw,
      "1"                            = "Toinen aste tai alle",
      "2"                            = "Korkea-aste",
      "secondary education or below" = "Toinen aste tai alle",
      "college or university"        = "Korkea-aste"
    ),
    
    gt_weight_gain = dplyr::recode(
      raw,
      "within or less"            = "Suosituksen sisällä/alle",
      "more than recommendation"  = "Yli suosituksen",
      "within"                   = "Suosituksen sisällä/alle",
      "within recommendations"   = "Suosituksen sisällä/alle",
      "more than recommendat"    = "Yli suosituksen",
      "more"                     = "Yli suosituksen",
      "0"                        = "Suosituksen sisällä/alle",
      "1"                        = "Yli suosituksen"
    ),
    
    pp_weight_lost = dplyr::recode(
      raw,
      "0"   = "Ei painonlaskua",
      "No"  = "Ei painonlaskua",
      "1"   = "Painonlasku",
      "Yes" = "Painonlasku"
    ),

    previous_children = dplyr::recode(
      raw,
      "primipara"  = "Ei",
      "multipara"  = "Kyllä",
      "0"                  = "Ei",
      "1"                  = "Kyllä",
      "nullipara"          = "Ei",
      "no previous children" = "Ei",
      "no children"        = "Ei",
      "primi- or multipara" = "Kyllä"
    ),
    raw
  )
  
  lev <- switch(
    by,
    age_category      = c("Alle 30","30+"),
    bmi_bl2           = c("Ylipaino","Lihavuus"),
    delivery_method   = c("Alatiesynnytys","Imukuppi","Sectio"),
    epds_category     = c("Ei masennusta","Mahd. masennus"),
    education         = c("Toinen aste tai alle","Korkea-aste"),
    gt_weight_gain    = c("Suosituksen sisällä/alle","Yli suosituksen"),
    pp_weight_lost    = c("Ei painonlaskua","Painonlasku"),
    previous_children = c("Ei","Kyllä"),
    sort(unique(lab))
  )
  
  factor(lab, levels = lev)
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
  
  # Perusmuokkaukset + suomenkielinen ryhmänimi
  df2 <- df %>%
    dplyr::mutate(
      duration_h  = duration / 3600,
      group_label = .recode_group_labels(by, .data[[by]])
    ) %>%
    dplyr::filter(!is.na(group_label)) %>%
    { if ("non_wear" %in% names(.)) dplyr::filter(., is.na(non_wear) | non_wear <= non_wear_max) else . }
  
  # Facet-otsikko suomeksi
  facet_labeller <- ggplot2::as_labeller(
    c(setNames(LEGEND_TITLES, names(LEGEND_TITLES))),  # varmistetaan, että löytyy
    default = ggplot2::label_value
  )
  subtitle_txt <- paste("Ryhmittäin:", .legend_title(by))
  
  p_dur <- df2 %>%
    dplyr::filter(!is.na(duration_h)) %>%
    ggplot2::ggplot(ggplot2::aes(x = duration_h)) +
    ggplot2::geom_histogram(
      binwidth = binwidth_duration,
      boundary = 0,
      closed   = "left",
      fill     = "grey70",
      colour   = "grey30"
    ) +
    ggplot2::facet_wrap(
      ~ group_label,
      scales = if (free_y) "free_y" else "fixed",
      drop   = TRUE
    ) +
    ggplot2::labs(
      title    = paste(phase_label, "- Unen kesto (histogrammi)"),
      subtitle = subtitle_txt,
      x        = "Unen kesto (h)",
      y        = "Lukumäärä"
    ) +
    theme_golden_coast +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  
  p_sco <- df2 %>%
    dplyr::filter(!is.na(score)) %>%
    ggplot2::ggplot(ggplot2::aes(x = score)) +
    ggplot2::geom_histogram(
      binwidth = binwidth_score,
      boundary = 0,
      closed   = "left",
      fill     = "grey70",
      colour   = "grey30"
    ) +
    ggplot2::facet_wrap(
      ~ group_label,
      scales = if (free_y) "free_y" else "fixed",
      drop   = TRUE
    ) +
    ggplot2::labs(
      title    = paste(phase_label, "- Oura score (histogrammi)"),
      subtitle = subtitle_txt,
      x        = "Oura score (0–100)",
      y        = "Lukumäärä"
    ) +
    theme_golden_coast +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  
  p_eff <- df2 %>%
    dplyr::filter(!is.na(efficiency)) %>%
    ggplot2::ggplot(ggplot2::aes(x = efficiency)) +
    ggplot2::geom_histogram(
      binwidth = binwidth_efficiency,
      boundary = 0,
      closed   = "left",
      fill     = "grey70",
      colour   = "grey30"
    ) +
    ggplot2::facet_wrap(
      ~ group_label,
      scales = if (free_y) "free_y" else "fixed",
      drop   = TRUE
    ) +
    ggplot2::labs(
      title    = paste(phase_label, "- Unen tehokkuus (histogrammi)"),
      subtitle = subtitle_txt,
      x        = "Tehokkuus (%-yksikköä, 0–100)",
      y        = "Lukumäärä"
    ) +
    theme_golden_coast +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  
  # Kolme paneelia päällekkäin
  p_dur / p_sco / p_eff + patchwork::plot_layout(heights = c(1, 1, 1))
}


# BOX-PLOT: duration, score, efficiency
plot_sleep_box_panel_all_bg <- function(df,
                                        outcome        = c("duration","score","efficiency"),
                                        phase_label    = "Pregnancy",
                                        non_wear_max   = Inf) {
  outcome <- match.arg(outcome)
  
  # Taustamuuttujat, joille meillä on suomenkieliset nimet
  background_vars_all <- c(
    "age_category",
    "education",
    "previous_children",
    "epds_category",
    "bmi_bl2",
    "gt_weight_gain",
    "delivery_method",
    "pp_weight_lost"
  )
  background_vars <- intersect(background_vars_all, names(df))
  if (length(background_vars) == 0) {
    stop("Datassa ei ole yhtään tunnistettua taustamuuttujaa.")
  }
  
  df2 <- df %>%
    dplyr::mutate(
      duration_h = duration / 3600
    ) %>%
    { if ("non_wear" %in% names(.)) dplyr::filter(., is.na(non_wear) | non_wear <= non_wear_max) else . }
  
  # Vaste + label
  if (outcome == "duration") {
    y_var   <- "duration_h"
    y_label <- "Unen kesto (h)"
  } else if (outcome == "score") {
    y_var   <- "score"
    y_label <- "Oura score (0–100)"
  } else if (outcome == "efficiency") {
    y_var   <- "efficiency"
    y_label <- "Tehokkuus (0–100)"
  }
  
  if (!y_var %in% names(df2)) {
    stop(sprintf("Vaste '%s' puuttuu datasta.", y_var))
  }
  
  # Pitkä formaatti + suomenkieliset ryhmänimet
  df_long <- df2 %>%
    dplyr::select(dplyr::all_of(c(y_var, background_vars))) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(background_vars),
      names_to  = "background_factor",
      values_to = "raw_group"
    ) %>%
    dplyr::filter(!is.na(.data[[y_var]]), !is.na(raw_group)) %>%
    dplyr::group_by(background_factor) %>%
    dplyr::mutate(
      group_label = .recode_group_labels(
        by    = dplyr::first(background_factor),
        group = raw_group
      )
    ) %>%
    dplyr::ungroup()
  
  # Facettien otsikot samasta sanastosta (LEGEND_TITLES)
  facet_labeller <- ggplot2::as_labeller(
    LEGEND_TITLES,
    default = ggplot2::label_value
  )
  
  ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x = group_label,
      y = .data[[y_var]]
    )
  ) +
    ggplot2::geom_boxplot(fill = "grey70", colour = "grey30") +
    ggplot2::facet_wrap(
      ~ background_factor,
      scales   = "free_x",
      drop     = TRUE,
      labeller = facet_labeller
    ) +
    ggplot2::labs(
      title = sprintf("%s – %s taustamuuttujien mukaan", phase_label, y_label),
      x     = NULL,
      y     = y_label
    ) +
    theme_golden_coast +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text      = ggplot2::element_text(face = "bold"),
      legend.position = "none"
    )
}
