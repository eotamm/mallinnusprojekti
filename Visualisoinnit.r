# Visualisaatiot
# Tässä dokumentissa toteutetaan aineiston ensisijaiset visualisoinnit 
# ennen varsinaisia analyysejä. Tarkoitus on saada yleiskuva aineistosta ja sen 
# laadusta, sekä tunnistaa mahdollisia piirteitä, jotka voivat vaikuttaa myöhempiin malleihin.

# Funktioitten käyttö: Laita var muuttujan paikalle joku aikasarja muuttuja ja meta paikkalle joku kategorinen tausta muuttuja. 

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
pregnancy  <- readRDS(file.path(dir.path, "pregnancy_2025-10-09.rds"))  %>% as.data.frame() %>% mutate(duration=duration/3600)
postpartum <- readRDS(file.path(dir.path, "postpartum_2025-10-09.rds")) %>% as.data.frame() %>% mutate(duration=duration/3600)

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
    time = c("pre", "post"), # Jos haluat kolmannekset (3vk) post partumille annan tähän "post" valinnan trimester kanssa. 
    R_boot = 2000L, 
    block_len = NULL 
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
  # bootstrap_ci <- function(x, conf = 0.95, R = 1000L) {
  #   x <- x[!is.na(x)]
  #   if (!length(x)) return(c(lower = NA_real_, upper = NA_real_))
  #   means <- replicate(R, mean(sample(x, replace = TRUE)))
  #   q <- stats::quantile(means, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2), names = FALSE)
  #   stats::setNames(q, c("lower", "upper"))
  # }
  # Block bootstrap CI aikasarjan keskiarvolle (percentile CI)
  bootstrap_ci <- function(x, conf = 0.95, R = 2000L, block_len = NULL, na_rm = TRUE) {
    x <- if (na_rm) x[is.finite(x)] else x
    n <- length(x) 
    print(n)
    if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
    if (is.null(block_len)) block_len <- ceiling(n^(1/3))  # peukalosääntö
    # montako blokkia tarvitaan kattamaan pituus n
    B <- ceiling(n / block_len)
    
    # circular indeksointi (wrap-around)
    circ_index <- function(start, len) {
      idx <- start + seq_len(len) - 1L
      ((idx - 1L) %% n) + 1L
    }
    
    means <- numeric(R)
    for (r in seq_len(R)) {
      # arvo B aloitusindeksiä 1..n, liimaa blokit peräkkäin
      starts <- sample.int(n, B, replace = TRUE)
      idx    <- unlist(lapply(starts, circ_index, len = block_len), use.names = FALSE)
      idx    <- idx[seq_len(n)]              # leikkaa täsmälleen pituuteen n
      means[r] <- mean(x[idx])
    }
    q <- stats::quantile(means, probs = c((1 - conf)/2, 1 - (1 - conf)/2), names = FALSE, type = 7)
    stats::setNames(q, c("lower","upper"))
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
  

  
  if (compute_ci) {
    agg_df <- df %>%
      dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
      dplyr::filter(dplyr::n() >= min_obs) %>%
      dplyr::summarise(
        dplyr::across(
          all_of(var),
          list(
            mean  = ~ mean(.x, na.rm = TRUE),
            lower = ~ bootstrap_ci(.x, conf = conf_level, R = R_boot,
                                   block_len = block_len, na_rm = TRUE)["lower"],
            upper = ~ bootstrap_ci(.x, conf = conf_level, R = R_boot,
                                   block_len = block_len, na_rm = TRUE)["upper"]
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
{
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

}
#______Line plots_____________________________________________________________
plot_var_ts <- function(
    dat,                  # agglomeroi(..., id = FALSE, compute_ci = TRUE)  
    var,                  # Ei ota vektoria 
    facet = NULL         
) {
  # check
  if ("id" %in% names(dat)) {
    stop("`plot_var_ts()` expects pooled data (id = FALSE). Use the id-level plot for per-id lines.")
  }
  
  tc   <- resolve_time_col(dat)
  cols <- pick_var_cols(dat, var)
  
  df <- dat %>%
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
    facet = NULL,          
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
  
  facet_sym <- if (is.null(facet)) NULL else rlang::ensym(facet)
  has_facet <- !is.null(facet_sym)
  time_sym  <- rlang::sym(tc$time_col)
  
  df <- dat %>%
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
    # yksi arvo per id per aika (+ facet)
    per_id <- if (has_facet) {
      df %>%
        dplyr::group_by(id, !!time_sym, !!facet_sym) %>%
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop")
    } else {
      df %>%
        dplyr::group_by(id, !!time_sym) %>%
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop")
    }
    
    # keskiarvo yli id:ien per aika (+ facet)
    overlay <- if (has_facet) {
      per_id %>%
        dplyr::group_by(!!time_sym, !!facet_sym) %>%
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(!!time_sym, !!facet_sym)
    } else {
      per_id %>%
        dplyr::group_by(!!time_sym) %>%
        dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop") %>%
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
  df <- dat %>% 
    dplyr::mutate(.mean = .data[[cols$mean_col]])
  
  if (!is.null(ids)) {
    df <- dplyr::filter(df, id %in% ids)
  }
  
  # Boxplotin data: yksi arvo per id, per time (+ facet)
  per_id <- if (has_facet) {
    df %>% 
      dplyr::group_by(id, !!time_sym, !!facet_sym) %>% 
      dplyr::summarise(.mean = mean(.mean, na.rm = TRUE), .groups = "drop")
  } else {
    df %>% 
      dplyr::group_by(id, !!time_sym) %>% 
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
      width = 0.15,            
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

# lisää time_group (sama logiikka kuin agglomeroi())
make_time_group <- function(df, aika = c("trimester","week"), time = c("pre","post")) {
  aika <- match.arg(aika); time <- match.arg(time)
  stopifnot("week" %in% names(df))
  df <- dplyr::filter(df, !is.na(week))
  if (aika == "trimester") {
    if (time == "pre") {
      df$time_group <- dplyr::case_when(
        df$week <= 12 ~ "T1",
        df$week >= 13 & df$week <= 27 ~ "T2",
        df$week >= 28 ~ "T3"
      )
    } else { # postpartum 3 viikon lohkot
      df$time_group <- cut(
        df$week,
        breaks = c(0, 4, 8, 12),
        include_lowest = TRUE,
        right = TRUE,
        labels = c("P1","P2","P3")
      )
    }
  }
  df
}

# histogrammi "kaikista" numeerisista mittamuuttujista
# Facetoi histogrammit trimesterin SISÄLLÄ kategorisilla muuttujilla
plot_hist_by_trimester <- function(
    df,
    var,                        # numeerinen mitta (esim. "steps")
    cat = NULL,                 # kategorinen muuttuja (esim. "education")
    aika = c("trimester","week"),
    time = c("pre","post"),
    bins = 30,
    overlay = FALSE,            # TRUE = päällekkäin saman paneelin sisään fillillä
    alpha = 0.35,               # läpinäkyvyys overlay-tilassa
    free_y = TRUE,              # skaalataanko paneelien y-akseli erikseen
    title = NULL
){
  aika <- match.arg(aika); time <- match.arg(time)
  stopifnot("week" %in% names(df))
  
  # --- lisää time_group samalla logiikalla kuin agglomeroi() ---
  df <- df[!is.na(df$week), ]
  if (aika == "trimester") {
    if (time == "pre") {
      df$time_group <- dplyr::case_when(
        df$week <= 12 ~ "T1",
        df$week >= 13 & df$week <= 27 ~ "T2",
        df$week >= 28 ~ "T3"
      )
    } else {
      df$time_group <- cut(
        df$week, breaks = c(0,4,8,12), include_lowest = TRUE, right = TRUE,
        labels = c("P1","P2","P3")
      )
    }
  } else {
    df$time_group <- df$week
  }
  
  # --- perusasetukset ---
  ttl <- if (is.null(title)) {
    paste0("Jakauma: ", var,
           if (!is.null(cat)) paste0(" — ", cat), 
           " (", if (time=="pre") "Trimesterit" else "Postpartum-lohkot", ")")
  } else title
  
  # --- facet-grid tai overlay ---
  if (isFALSE(overlay)) {
    # Facet-grid: rivit = trimesterit, sarakkeet = cat
    # Jos cat = NULL → vain rivit = trimesterit
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
      ggplot2::geom_histogram(bins = bins, na.rm = TRUE) +
      ggplot2::labs(title = ttl, x = var, y = "N") +
      ggplot2::theme_minimal(base_size = 12)
    
    if (!is.null(cat)) {
      p <- p + ggplot2::facet_grid(rows = ggplot2::vars(time_group),
                                   cols = ggplot2::vars(.data[[cat]]),
                                   scales = if (free_y) "free_y" else "fixed")
    } else {
      p <- p + ggplot2::facet_wrap(~ time_group, scales = if (free_y) "free_y" else "fixed")
    }
    
  } else {
    # Overlay: facet pelkästään trimesterillä; cat tulee väriksi (fill)
    if (is.null(cat)) stop("overlay=TRUE vaatii `cat`-muuttujan.")
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]], fill = .data[[cat]])) +
      ggplot2::geom_histogram(bins = bins, position = "identity", alpha = alpha, na.rm = TRUE) +
      ggplot2::labs(title = ttl, x = var, y = "N", fill = cat) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::facet_wrap(~ time_group, scales = if (free_y) "free_y" else "fixed")
  }
  
  # NA-laskuri alatitreeniksi (valinnainen)
  n_na <- sum(is.na(df[[var]]))
  if (n_na > 0) {
    p <- p + ggplot2::labs(subtitle = paste0("Pudotettu ", n_na, " NA-arvoa"))
  }
  
  p
}

plot_hist_by_trimester(
  pregnancy,
  var = "duration",
  cat = "age_category",
  aika = "trimester",
  time = "pre",
  bins = 30,
  overlay = FALSE
)


#score on vasemmalle vino normaalisti 
p1<- plot_histo_kaikki(pregnancy,  "score", bins= 100, title="Ei muunnettu")

p2<- pregnancy %>% dplyr::mutate(score=log(100-score)) %>%
  plot_histo_kaikki(.,  "score", bins= 100, title="log(100-score)")

p3<- pregnancy %>% dplyr::mutate(score=log(100-score)) %>%
  plot_histo_kaikki(.,  "score","age_category", bins= 100, title="age catgory")
p4<- pregnancy %>% dplyr::mutate(score=log(100-score)) %>%
  plot_histo_kaikki(.,  "score","education", bins= 100, title="education")
((p1 | p2) / (p3)/(p4) )+
  plot_annotation(title = "Score pregnancy") &
  theme(plot.title = element_text(hjust = 0.5))



pregnancy %>% dplyr::mutate(duration=duration) %>%
  plot_histo_kaikki(.,  "duration", bins= 100, title="Duration jakauma")

pregnancy %>% dplyr::mutate(steps=log10(steps)) %>%
  plot_histo_kaikki(.,  "steps", bins= 100, title="Steps jakauma")


{

pregnancy %>% nrow()# Pregnancy
p1<- pregnancy %>% 
  dplyr::filter(!is.na(duration)) %>% 
  plot_histo_kaikki(var = "duration",
                    title = "Pregnancy: Unen kesto (h)")
p2<- pregnancy %>% 
  dplyr::filter(!is.na(score)) %>% 
  plot_histo_kaikki(var = "score",
                    title = "Pregnancy: Unen laatu")

p3<- pregnancy %>% 
  dplyr::filter(!is.na(score)) %>% 
  plot_histo_kaikki(var = "efficiency",
                    title = "Pregnancy: Unen tehokkuus")

plots <- p1 / p2 / p3
png("kuvak1/Pregnancy_histograms.png", 
    width = 1200, height = 1600, res = 150)

print(plots)   

dev.off()
  }
{
# Postpartum 
p1<- postpartum %>% 
  dplyr::filter(!is.na(duration)) %>% 
  plot_histo_kaikki(var = "duration",
                    title = "Postpartum: Unen kesto (h)")
p2<- postpartum %>% 
  dplyr::filter(!is.na(score)) %>% 
  plot_histo_kaikki(var = "score",
                    title = "Postpartum: Unen laatu")

p3<- postpartum %>% 
  dplyr::filter(!is.na(score)) %>% 
  plot_histo_kaikki(var = "efficiency",
                    title = "Postpartum: Unen tehokkuus")
plots <- p1 / p2 / p3
png("kuvak1/Postpartum_histograms.png", 
    width = 1200, height = 1600, res = 150)

print(plots)  
dev.off()
}


{
  preg_dur_week <- agglomeroi(
    df         = pregnancy,
    var        = "duration",   
    meta       = NULL,         
    aika       = "week",       
    id         = FALSE,        
    format     = "long",       
    min_obs    = 1,            
    compute_ci = TRUE,         
    conf_level = 0.95,
    time       = "pre"         
  )
  preg_eff_week <- agglomeroi(
    df         = pregnancy,
    var        = "efficiency",   
    meta       = NULL,         
    aika       = "week",       
    id         = FALSE,        
    format     = "long",      
    min_obs    = 1,            
    compute_ci = TRUE,       
    conf_level = 0.95,
    time       = "pre"         
  )
  preg_sc_week <- agglomeroi(
    df         = pregnancy,
    var        = "score",   
    meta       = NULL,         
    aika       = "week",       
    id         = FALSE,        
    format     = "long",      
    min_obs    = 1,            
    compute_ci = TRUE,       
    conf_level = 0.95,
    time       = "pre"         
  )
  
  p_preg_dur <- plot_var_ts(
    dat  = preg_dur_week,
    var  = "duration"          
  ) +
    ggplot2::labs(
      title = NULL,
      y     = "Unen kesto (h)"
    )
  p_preg_eff <- plot_var_ts(
    dat  = preg_eff_week,
    var  = "efficiency"         
  ) +
    ggplot2::labs(
      title = NULL,
      y     = "Unen tehokkuus"
    )
  p_preg_sc <- plot_var_ts(
    dat  = preg_sc_week,
    var  = "score"          
  ) +
    ggplot2::labs(
      title = NULL,
      y     = "Unen laatu"
    )
  
  plots<- p_preg_dur/p_preg_eff/p_preg_sc
}
png("kuvak1/pregnancy_week.png", 
    width = 1200, height = 1600, res = 150)
plots
dev.off()
{
  preg_dur_week <- agglomeroi(
    df         = postpartum,
    var        = "duration",   
    meta       = NULL,         
    aika       = "week",       
    id         = FALSE,        
    format     = "long",       
    min_obs    = 1,            
    compute_ci = TRUE,         
    conf_level = 0.95,
    time       = "post"         
  )
  preg_eff_week <- agglomeroi(
    df         = postpartum,
    var        = "efficiency",   
    meta       = NULL,         
    aika       = "week",       
    id         = FALSE,        
    format     = "long",      
    min_obs    = 1,            
    compute_ci = TRUE,       
    conf_level = 0.95,
    time       = "post"         
  )
  preg_sc_week <- agglomeroi(
    df         = postpartum,
    var        = "score",   
    meta       = NULL,         
    aika       = "week",       
    id         = FALSE,        
    format     = "long",      
    min_obs    = 1,            
    compute_ci = TRUE,       
    conf_level = 0.95,
    time       = "post"         
  )
  
  p_preg_dur <- plot_var_ts(
    dat  = preg_dur_week,
    var  = "duration"          
  ) +
    ggplot2::labs(
      title = NULL,
      y     = "Unen kesto (h)"
    )
  p_preg_eff <- plot_var_ts(
    dat  = preg_eff_week,
    var  = "efficiency"         
  ) +
    ggplot2::labs(
      title = NULL,
      y     = "Unen tehokkuus"
    )
  p_preg_sc <- plot_var_ts(
    dat  = preg_sc_week,
    var  = "score"          
  ) +
    ggplot2::labs(
      title = NULL,
      y     = "Unen laatu"
    )
  
  plots<- p_preg_dur/p_preg_eff/p_preg_sc
}
png("kuvak1/postpartum_week.png", 
    width = 1200, height = 1600, res = 150)
plots
dev.off()
