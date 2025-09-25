# Visualisaatiot
#
# Tässä dokumentissa toteutetaan aineiston ensisijaiset visualisoinnit 
# ennen varsinaisia analyysejä. Tarkoitus on saada yleiskuva aineistosta ja sen 
# laadusta, sekä tunnistaa mahdollisia piirteitä, jotka voivat vaikuttaa myöhempiin malleihin.
#
#Funktioitten käyttö: Laita var muuttujan paikalle joku aikasarja muuttuja ja meta paikkalle joku kategorinen tausta muuttuja. 

# Kirjastot
library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(rlang)
library(ggplot2)
library(patchwork)
#Aineiston tuonti
dir.path <- file.path("./data") # symlink. 

# Haetaan puhdisteut aineistot
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-09-24.rds"))  %>% as.data.frame()
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-09-24.rds")) %>% as.data.frame()

# Visualisointi funktioita: 

# Katsoo kaikkien arvojen jakauman. Sallii vertailun taustamuuttujilla. 
plot_histo_kaikki <- function(df, var, meta = NULL, bins = 30,
                              title = NULL, mode = c("facet", "split")) {
  mode <- match.arg(mode)
  
  base_plot <- function(d, extra = "") {
    n_na <- sum(is.na(d[[var]]))
    ggplot(d, aes(x = .data[[var]])) +
      geom_histogram(bins = bins, na.rm = TRUE) +
      labs(
        title = if (is.null(title)) paste0("Jakauma: ", var, extra) else paste0(title, extra),
        subtitle = if (n_na > 0) paste0("Pudotettu ", n_na, " NA-arvoa") else NULL,
        x = var, y = "N"
      )
  }
  
  if (is.null(meta)) return(base_plot(df))
  
  if (mode == "facet") {
    base_plot(df) + facet_wrap(as.formula(paste("~", meta)), scales = "free_y")
  } else { # split
    sp <- split(df, df[[meta]], drop = TRUE)
    out <- lapply(names(sp), function(level) {
      base_plot(sp[[level]], paste0(" — ", meta, " = ", level))
    })
    names(out) <- names(sp)
    out
  }
}

# Agglomeroi muuttujat halutulle tasolle: 
agglomeroi <- function(
    df,                 # Aineisto
    var,                # Aggrekoitava muuttuja tai muuttujat. Jos annat NULL agregaatio tapahtuu kaikille muuttujille. 
    meta = NULL,        # Jos aggregointi ryhmitellään tietylle taustamuuttujalle.
    aika = c("week", "trimester"),  # Aggregoi viikon tai kolmanneksen mukaan. 
    id = FALSE,         # Yksilö vai yhteisö taso. 
    format = c("long", "wide"),     # tuloksena wide tai long aineito muoto
    min_obs = 1,        # Minimi määrä havaintoja per ryhmä
    compute_ci = FALSE, # Haluatko 95% CI välin (bootstrap)
    conf_level = 0.95,  # Confidence level for CI
    time = c("pre", "post") # Jos haluat kolmannekset (3vk) post partumille annan tähän "post" valinnan trimester kanssa. 
){
  # --- Tarkistus ---------------------------------------------------------------

  # Muuttujat ja niiden tyypit
  if (!"week" %in% colnames(df)) {
    stop("Aineiston on sisällytettävä 'week' sarake.")
  }
  if (id && !"id" %in% colnames(df)) {
    stop("Aineiston on sisällytettävä 'id' sarake kun id = TRUE.")
  }
 
  if (!identical(var, "full")) {
    if (!all(var %in% names(df))) {
      stop("Jotkut `var` eivät ole aineistossa.")
    }
    non_numeric <- var[!sapply(df[var], is.numeric)]
    if (length(non_numeric) > 0) {
      stop("Kaikkien aggregoitavien muuttujien pitää olla numeerisia. Tarkista: ", paste(non_numeric, collapse = ", "))
    }
  }
  if (!is.null(meta)) {
    if (!all(meta %in% names(df))) {
      stop("Some variables in `meta` are not in the data frame.")
    }
  }
  if (id && !is.null(meta)) {
    stop("Valitse id = TRUE tai meta != NULL, ei molempia")
  }
  
  # Valintojen tarkistus
  aika <- match.arg(aika)
  time <- match.arg(time)
  format <- match.arg(format)
  
  # meta 
  if (!is.null(meta)) {
    if (!is.character(meta)) stop("`meta` pitää olla merkkivektori tai NULL.")
    miss_meta <- setdiff(meta, names(df))
    if (length(miss_meta)) stop("Meta-muuttujia puuttuu: ", paste(miss_meta, collapse = ", "))
  }
  
  # handle var = NULL or "full"
  kaikki <- is.null(var) || identical(var, "full")
  if (!kaikki) {
    if (!is.character(var)) stop("`var` pitää olla merkkivektori, NULL tai 'full'.")
    miss_var <- setdiff(var, names(df))
    if (length(miss_var)) stop("Muuttujia puuttuu `df`: ", paste(miss_var, collapse = ", "))
    non_num <- var[!vapply(df[var], is.numeric, logical(1))]
    if (length(non_num)) {
      stop("Kaikkien aggregoitavien muuttujien pitää olla numeerisia. Tarkista: ",
           paste(non_num, collapse = ", "))
    }
  }
  
  # tallenna meta informaatio
  meta_vars<- c("age_category","education","previous_children","epds_category","bmi_bl2", "gt_weight_gain","gt_weight_gain_within_or_less", "pp_weight_lost",               
  "delivery_method")
  
  if ("id" %in% names(df)) {
    meta_dat<- df %>% dplyr::select(id,dplyr::any_of(meta_vars)) %>% dplyr::distinct()
    }
 
  # --- Aika valinta --------------------------------------------------------
  # Laskee kolmannekset, jos "trimester". Onko "T" Ongelma jos tehdään aika sarjaa....
  df <- df[!is.na(df$week), ]  # Optional: warn about how many dropped
  
  if (aika == "trimester") {
    df$time_group <- if (time == "pre") {
      dplyr::case_when(
        df$week <= 12         ~ "T1",
        df$week >= 13 & df$week <= 27 ~ "T2",
        df$week >= 28         ~ "T3"
      )
    } else {
      # Postpartum → group into 3-week intervals (1–3, 4–6, ..., 10–12)
      df$time_group <- cut(
        df$week,
        breaks = c(0, 4, 8, 12),
        include_lowest = TRUE,
        right = TRUE,
        labels = c("P1", "P2", "P3")
      )
     
    }
    group_vars <-"time_group"
    name_col <- "time_group"
    
  } else {
    group_vars <-"week"
    name_col <- "week"

  }
  
  
  # Valitaan agregointi tasot: 

  if (id) {
    group_vars <- c("id",group_vars) # id pitää olla ensimmäinen
    df <- df %>% dplyr::select(-summary_date) #ei päiväkkohtaista informaatiota. Jos päiväkohtainen tarvitaan niin pitää lisätä tähän funktioon. 

  }
  if (!is.null(meta)) {
    group_vars <- c(group_vars, meta)

    df <- df %>% dplyr::select(-id, -summary_date)
  }
  
  # ---  Valitaan agregoitavat muuttujat.  -------------------------------------
  # Idea: jos var = NULL/"full" => kaikki numeeriset muuttujat,
  #      mutta ei ikinä ryhmittelymuuttujia.
  
  if (kaikki) {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    exclude_from_calc <- unique(c("week", "time_group", "id"))  # minimirajaus
  
    exclude_from_calc <- unique(c(exclude_from_calc, meta, meta_vars)) # ottaa pois meta muuttujat. 
    var <- setdiff(num_cols, exclude_from_calc)
    if (!length(var)) stop("Ei löytynyt numeerisia mittamuuttujia laskettavaksi (pois lukien week/time_group/id).")
  }
  
  # ---  Bootstrap-apufunktio ----------------------------------------- Tämän voisi ehkä siirtää ulkopuolelle niin ei run joka kerta. 
  bootstrap_ci <- function(x, conf = 0.95, R = 1000L) {
    x <- x[!is.na(x)]
    if (!length(x)) return(c(lower = NA_real_, upper = NA_real_))
    means <- replicate(R, mean(sample(x, replace = TRUE)))
    q <- stats::quantile(means, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2), names = FALSE)
    stats::setNames(q, c("lower", "upper"))
  }
   # Aggregointi: 
  agg_df <- df %>%  
  dplyr::group_by(dplyr::across(all_of(group_vars))) %>% 
  dplyr::filter(dplyr::n() >= min_obs) %>% 
  dplyr::summarise(
    dplyr::across(
      all_of(var),
      list(mean = ~ mean(.x, na.rm = TRUE)),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )
  if (is.null(meta) && isTRUE(id) && exists("meta_dat")) {
    agg_df <- dplyr::left_join(agg_df, meta_dat, by = "id")
  }
  
  #Lisää luottamusväli
  if (compute_ci) {
    bootstrap_summary <- function(x, conf = 0.95, R = 1000) {
      means <- replicate(R, mean(sample(x, replace = TRUE), na.rm = TRUE))
      stats::quantile(means, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE)
    }
    
    agg_df <- df |>
      dplyr::group_by(dplyr::across(all_of(group_vars))) |>
      dplyr::filter(dplyr::n() >= min_obs) |>
      dplyr::summarise(
        dplyr::across(
          all_of(var),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            lower = ~ bootstrap_summary(.x, conf_level)[1],
            upper = ~ bootstrap_summary(.x, conf_level)[2]
          ),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
  }
  
  # if (is.null(meta) && isTRUE(id) && exists("meta_dat")) {
  #   agg_df <- dplyr::left_join(agg_df, meta_dat, by = "id")
  # }
  
  if (format == "wide") {
    
    agg_df <- tidyr::pivot_wider(
      agg_df,
      names_from  = dplyr::all_of(name_col),
      values_from = dplyr::matches(paste(var, collapse = "|"))
    )
  
  }

  
  agg_df
}
# ____________Apufunktioita____________________________________________________

resolve_time_col <- function(dat) {
  if ("time_group" %in% names(dat)) {
    list(time_col = "time_group", x_lab = "Trimester / Postpartum")
  } else if ("week" %in% names(dat)) {
    list(time_col = "week", x_lab = "Viikko")
  } else {
    stop("Datan pitää sisältää `week` or `time_group` sarake.")
  }
}
pick_var_cols <- function(dat, var) {
  mean_col  <- paste0(var, "_mean")
  lower_col <- paste0(var, "_lower")
  upper_col <- paste0(var, "_upper")
  if (!mean_col %in% names(dat)) {
    stop("Sarake `", mean_col, "` ei löytynyt.")
  }
  has_ci <- all(c(lower_col, upper_col) %in% names(dat))
  list(mean_col = mean_col,
       lower_col = if (has_ci) lower_col else NULL,
       upper_col = if (has_ci) upper_col else NULL,
       has_ci = has_ci)
}


#______Line plots_________________--____________________________________________
plot_var_ts <- function(
    dat,                  # agglomeroi(..., id = FALSE, compute_ci = TRUE)  
    var,                  # Ei ota vektoria 
    facet = NULL         
) {
  # guard: this plot is for pooled data only
  if ("id" %in% names(dat)) {
    stop("`plot_var_ts()` expects pooled data (id = FALSE). Use the id-level plot for per-id lines.")
  }
  
  tc   <- resolve_time_col(dat)
  cols <- pick_var_cols(dat, var)
  
  df <- dat |>
    dplyr::mutate(
      .mean  = .data[[cols$mean_col]],
      .lower = if (!is.null(cols$lower_col)) .data[[cols$lower_col]] else NA_real_,
      .upper = if (!is.null(cols$upper_col)) .data[[cols$upper_col]] else NA_real_
    )
  has_ci <- !is.null(cols$lower_col)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[tc$time_col]], y = .mean, group = 1))
  
  if (has_ci) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .lower, ymax = .upper),
      alpha = 0.15, fill = "#6BAED6", colour = NA
    )
  }
  
  p <- p +
    ggplot2::geom_line(linewidth = 0.9, colour = "#2C3E50") +
    ggplot2::geom_point(size = 1.6, colour = "#2C3E50") +
    ggplot2::labs(x = tc$x_lab, y = var) +
    ggplot2::theme_minimal(base_size = 12)
  
  if (!is.null(facet) && facet %in% names(df)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)))
  }
  
  p
}

plot_var_id <- function(
    dat,
    var,
    facet = NULL,          # can be quoted or unquoted column name
    ids = NULL,
    overlay_mean = TRUE,
    line_alpha = 0.30,
    line_width = 0.6,
    point_size = 0.9
) {
  if (!"id" %in% names(dat)) {
    stop("Tämä funktio vaatii per-id -datan (agglomeroi(..., id = TRUE)).")
  }
  
  tc   <- resolve_time_col(dat)
  cols <- pick_var_cols(dat, var)
  
  # allow quoted or unquoted facet
  facet_sym <- if (is.null(facet)) NULL else rlang::ensym(facet)
  has_facet <- !is.null(facet_sym)
  time_sym  <- rlang::sym(tc$time_col)
  
  df <- dat |>
    dplyr::mutate(.mean = .data[[cols$mean_col]])
  
  if (!is.null(ids)) {
    df <- dplyr::filter(df, id %in% ids)
  }
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[tc$time_col]], y = .mean, group = id)
  ) +
    ggplot2::geom_line(alpha = line_alpha, linewidth = line_width, colour = "#2C3E50") +
    ggplot2::geom_point(alpha = line_alpha, size = point_size, colour = "#2C3E50") +
    ggplot2::labs(x = tc$x_lab, y = var) +
    ggplot2::theme_minimal(base_size = 12)
  
  if (isTRUE(overlay_mean)) {
    # 1) yksi arvo per id per aika (+ facet)
    per_id <- if (has_facet) {
      df |>
        dplyr::group_by(id, !!time_sym, !!facet_sym) |>
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop")
    } else {
      df |>
        dplyr::group_by(id, !!time_sym) |>
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop")
    }
    
    # 2) keskiarvo yli id:ien per aika (+ facet)
    overlay <- if (has_facet) {
      per_id |>
        dplyr::group_by(!!time_sym, !!facet_sym) |>
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(!!time_sym, !!facet_sym)
    } else {
      per_id |>
        dplyr::group_by(!!time_sym) |>
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(!!time_sym)
    }
    
    p <- p + ggplot2::geom_line(
      data = overlay,
      ggplot2::aes(x = !!time_sym, y = .mean, group = 1),
      inherit.aes = FALSE,
      colour = "#E74C3C",
      linewidth = 1.1
    )
  }
  
  if (has_facet) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet_sym))
  }
  
  p
}

plot_var_box <- function(
    dat,
    var,
    facet = NULL,           # quoted or unquoted column name
    ids = NULL,
    point_alpha = 0.5,      # läpinäkyvyys pisteille
    point_size = 1.3,
    box_width = 0.65
) {
  if (!"id" %in% names(dat)) {
    stop("Tämä funktio vaatii per-id -datan (agglomeroi(..., id = TRUE)).")
  }
  
  # Hakee ajan sarakkeen nimen ja muut tarvittavat tiedot
  tc        <- resolve_time_col(dat)
  cols      <- pick_var_cols(dat, var)
  facet_sym <- if (is.null(facet)) NULL else rlang::ensym(facet)
  has_facet <- !is.null(facet_sym)
  time_sym  <- rlang::sym(tc$time_col)
  
  # Luodaan uusi sarake analyysiin
  df <- dat |>
    dplyr::mutate(.mean = .data[[cols$mean_col]])
  
  if (!is.null(ids)) {
    df <- dplyr::filter(df, id %in% ids)
  }
  
  # Boxplotin data: yksi arvo per id, per time (+ facet)
  per_id <- if (has_facet) {
    df |>
      dplyr::group_by(id, !!time_sym, !!facet_sym) |>
      dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop")
  } else {
    df |>
      dplyr::group_by(id, !!time_sym) |>
      dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop")
  }
  
  # Piirretään boxplot + yksittäiset pisteet
  p <- ggplot2::ggplot(
    per_id,
    ggplot2::aes(x = !!time_sym, y = .mean)
  ) +
    ggplot2::geom_boxplot(
      width = box_width,
      outlier.shape = NA,      # estetään outlierien tuplapiirto
      colour = "#2C3E50",
      fill = NA
    ) +
    ggplot2::geom_jitter(
      width = 0.15,            # vaakasuuntainen hajonta, ettei pisteet mene päällekkäin
      alpha = point_alpha,
      size = point_size,
      colour = "#2C3E50"
    ) +
    ggplot2::labs(x = tc$x_lab, y = var) +
    ggplot2::theme_minimal(base_size = 12)
  
  # Facetit pystysuunnassa
  if (has_facet) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet_sym), ncol = 1)
  }
  
  return(p)
}


# Esimerkki kuvien luonti. 
# Unen pituus alle ja yli 30 vuotiailla. 
p1_preg<- plot_histo_kaikki(pregnancy,  "duration", "age_category", bins= 30, title="Yleinen unen pituus alle ja yli 30 vuotiailla", mode="facet")
p2_preg<- plot_histo_kaikki(pregnancy,  "steps", "age_category", bins= 30, title="Yleinen askelten määrä pituus alle ja yli 30 vuotiailla", mode="facet")

p1_post<- plot_histo_kaikki(postpartum_df,  "duration","age_category", bins= 30, title="Yleinen unen pituus alle ja yli 30 vuotiailla", mode="facet")
p2_post<- plot_histo_kaikki(postpartum_df,  "steps", bins= 30, title="Yleinen askelten määrä pituus alle ja yli 30 vuotiailla", mode="facet")

wrap_plots(p1_preg,p1_post)
wrap_plots(p2_preg,p2_post)


#Testataan agglomeraatio funktiota: 
testi1<- agglomeroi(pregnancy, c("steps", "duration"), NULL, "week", id=FALSE, format="long", compute_ci = FALSE) 
testi2<- agglomeroi(pregnancy, c("steps", "duration"), "education", "week", id=FALSE, format="long", compute_ci = FALSE)
testi2.2<- agglomeroi(pregnancy, c("steps", "duration"), "education", "week", id=FALSE, format="long", compute_ci = TRUE)
testi3<- agglomeroi(pregnancy, c("steps", "duration"), NULL, "week", id=TRUE, format="long", compute_ci = FALSE)
testi3.3<- agglomeroi(pregnancy, c("steps", "duration"), NULL, "week", id=TRUE, format="long", compute_ci = FALSE)

testi_tri1<- agglomeroi(pregnancy, c("steps", "duration"), NULL, "trimester", id=TRUE, format="long", compute_ci = FALSE)
testi_tri2 <- agglomeroi(pregnancy, c("steps", "duration"), NULL, "trimester", id=NULL, format="long", compute_ci = TRUE)
agglomeroi(pregnancy, c("steps", "duration"), NULL, "trimester", id=TRUE, format="wide", compute_ci = TRUE)


plot_var_ts(testi2, "steps", "education")
plot_var_ts(testi2.2, "duration", "education")
plot_var_id(testi3, "steps",facet="delivery_method")


plot_var_id(testi_tri1, "steps", "delivery_method")
plot_var_box(testi_tri1, "steps", "delivery_method")
