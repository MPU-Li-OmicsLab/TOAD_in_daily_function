#!/usr/bin/env Rscript

# Country-specific ADL/IADL turning point estimation and cross-country meta-analysis.
#
# Usage:
#   Rscript 02_tp_meta_analysis.R clean_outputs tp_meta_outputs 5000 50000 20260601 1 0.2 0.1
#
# Arguments:
#   1 input_dir    Directory containing *.country.data.clean.csv files.
#   2 output_dir   Directory where all TP/meta-analysis outputs are saved.
#   3 n_boot       Bootstrap repetitions per country. Default: 5000.
#   4 sample_cap   Maximum records sampled per bootstrap draw. Default: 50000.
#   5 seed         Random seed. Default: 20260601.
#   6 n_cores      Parallel workers within each country. Default: 1.
#   7 acc_threshold_prop  Onset threshold as a proportion of max positive acceleration. Default: 0.2.
#   8 age_grid_by         Age grid interval for second derivative calculation. Default: 0.1.

suppressPackageStartupMessages({
  required_pkgs <- c("data.table", "dplyr", "ggplot2", "metafor", "minpack.lm", "patchwork")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
         ". Please install them before running this script.")
  }
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(metafor)
  library(minpack.lm)
  library(patchwork)
})

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "clean_outputs"
output_dir <- if (length(args) >= 2) args[[2]] else "tp_meta_outputs"
n_boot <- if (length(args) >= 3) as.integer(args[[3]]) else 5000L
sample_cap <- if (length(args) >= 4) as.integer(args[[4]]) else 50000L
seed <- if (length(args) >= 5) as.integer(args[[5]]) else 20260601L
n_cores <- if (length(args) >= 6) as.integer(args[[6]]) else 1L
ACC_THRESHOLD_PROP <- if (length(args) >= 7) as.numeric(args[[7]]) else 0.2
AGE_GRID_BY <- if (length(args) >= 8) as.numeric(args[[8]]) else 0.1

if (is.na(n_boot) || n_boot < 1) stop("n_boot must be a positive integer.")
if (is.na(sample_cap) || sample_cap < 1) stop("sample_cap must be a positive integer.")
if (is.na(n_cores) || n_cores < 1) n_cores <- 1L
if (is.na(ACC_THRESHOLD_PROP) || ACC_THRESHOLD_PROP <= 0 || ACC_THRESHOLD_PROP > 1) {
  stop("acc_threshold_prop must be in (0, 1].")
}
if (is.na(AGE_GRID_BY) || AGE_GRID_BY <= 0) stop("age_grid_by must be positive.")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(seed)

country_files <- list.files(
  input_dir,
  pattern = "\\.country\\.data\\.clean\\.csv$",
  full.names = TRUE
)
country_files <- country_files[basename(country_files) != "ALL.country.data.clean.csv"]
if (length(country_files) == 0) {
  stop("No country clean files found in: ", normalizePath(input_dir, mustWork = FALSE))
}

country_from_file <- function(path) sub("\\.country\\.data\\.clean\\.csv$", "", basename(path))

needed_cols <- c(
  "ID", "age", "ADL_totalScore", "IADL_totalScore",
  "gender", "gender_std", "education", "education_cat",
  "personal_income", "household_income", "total_income"
)

read_country <- function(path) {
  x <- fread(path, showProgress = FALSE)
  missing_cols <- setdiff(c("ID", "age", "ADL_totalScore", "IADL_totalScore"), names(x))
  if (length(missing_cols) > 0) {
    stop("File ", basename(path), " is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  keep <- intersect(needed_cols, names(x))
  x <- x[, ..keep]
  x[, ID := as.character(ID)]
  x[, age := suppressWarnings(as.numeric(age))]
  x[, age_year := as.integer(round(age))]
  x[, ADL_totalScore := suppressWarnings(as.numeric(ADL_totalScore))]
  x[, IADL_totalScore := suppressWarnings(as.numeric(IADL_totalScore))]
  x <- x[!is.na(age_year) & age_year >= 60 & age_year <= 100]
  x
}

# ----------------------------
# 4PL Logistic function, kept consistent with the prior Hong Kong analysis.
# ----------------------------
logistic4 <- function(x, ymin, R, k, l) {
  ymin + R / (1 + exp(-k * (x - l)))
}

# ----------------------------
# Aggregate by age: one mean point per age, with n_age as fitting weight.
# ----------------------------
make_age_mean_data <- function(data, score_var) {
  data %>%
    filter(
      !is.na(age),
      !is.na(.data[[score_var]])
    ) %>%
    group_by(age) %>%
    summarise(
      mean_score = mean(.data[[score_var]], na.rm = TRUE),
      n_age = n(),
      .groups = "drop"
    ) %>%
    filter(is.finite(mean_score), n_age > 0) %>%
    arrange(age)
}

# ----------------------------
# Second derivative from the fitted age grid.
# ----------------------------
calc_second_derivative <- function(age_seq, pred) {
  if (length(age_seq) < 5 || length(pred) < 5) {
    return(NULL)
  }

  d1 <- diff(pred) / diff(age_seq)
  d2 <- diff(d1) / diff(age_seq[-1])
  tp_seq <- age_seq[2:(length(age_seq) - 1)]

  valid <- is.finite(d2) & is.finite(tp_seq)

  data.frame(
    age = tp_seq[valid],
    d2 = d2[valid]
  )
}

# ----------------------------
# Logistic onset TP: first age where acceleration reaches
# max_positive_acceleration * threshold_prop.
# ----------------------------
tp_logistic_onset <- function(age_seq, pred,
                              threshold_prop = ACC_THRESHOLD_PROP) {
  d2_df <- calc_second_derivative(age_seq, pred)

  if (is.null(d2_df) || nrow(d2_df) == 0) {
    return(NA_real_)
  }

  positive_d2 <- d2_df$d2[d2_df$d2 > 0]

  if (length(positive_d2) == 0) {
    return(NA_real_)
  }

  threshold <- max(positive_d2, na.rm = TRUE) * threshold_prop
  onset_idx <- which(d2_df$d2 >= threshold)[1]

  if (is.na(onset_idx)) {
    return(NA_real_)
  }

  as.numeric(d2_df$age[onset_idx])
}

# ----------------------------
# One 4PL fit and onset TP calculation.
# Returns TP, age means, and fitted curve.
# ----------------------------
fit_logistic_onset_once <- function(data, score_var) {
  df_age <- make_age_mean_data(data, score_var)

  if (nrow(df_age) < 4) {
    return(NULL)
  }

  age_min <- min(df_age$age, na.rm = TRUE)
  age_max <- max(df_age$age, na.rm = TRUE)

  if (!is.finite(age_min) || !is.finite(age_max) || age_min >= age_max) {
    return(NULL)
  }

  ymin_start <- min(df_age$mean_score, na.rm = TRUE)
  ymax_start <- max(df_age$mean_score, na.rm = TRUE)
  R_start <- max(ymax_start - ymin_start, 1e-4)
  l_start <- weighted.mean(df_age$age, w = df_age$n_age)

  fit <- tryCatch({
    nlsLM(
      mean_score ~ logistic4(age, ymin, R, k, l),
      data = df_age,
      weights = n_age,
      start = list(
        ymin = ymin_start,
        R = R_start,
        k = 0.2,
        l = l_start
      ),
      lower = c(
        ymin = -Inf,
        R = 1e-6,
        k = 1e-6,
        l = age_min
      ),
      upper = c(
        ymin = Inf,
        R = Inf,
        k = Inf,
        l = age_max
      ),
      control = nls.lm.control(maxiter = 500)
    )
  }, error = function(e) NULL)

  if (is.null(fit)) {
    return(NULL)
  }

  cf <- coef(fit)
  age_seq <- seq(age_min, age_max, by = AGE_GRID_BY)
  pred <- logistic4(age_seq, cf["ymin"], cf["R"], cf["k"], cf["l"])
  tp <- tp_logistic_onset(age_seq, pred)

  list(
    tp = tp,
    df_age = df_age,
    pred_df = data.frame(age = age_seq, pred = as.numeric(pred)),
    coef = cf
  )
}

fit_logistic_onset_validated <- function(data, score_var) {
  fit <- fit_logistic_onset_once(data, score_var)
  if (is.null(fit)) {
    return(list(valid = FALSE, tp = NA_real_, fit = NULL, reason = "fit_failed"))
  }
  if (!is.finite(fit$tp) || fit$tp < 60 || fit$tp > 100) {
    return(list(valid = FALSE, tp = NA_real_, fit = fit, reason = "tp_not_found"))
  }
  list(valid = TRUE, tp = fit$tp, fit = fit, reason = "ok")
}

fit_country_full_curves <- function(x, country) {
  specs <- data.table(
    outcome = c("ADL", "IADL"),
    outcome_col = c("ADL_totalScore", "IADL_totalScore")
  )

  rbindlist(lapply(seq_len(nrow(specs)), function(i) {
    fit <- fit_logistic_onset_validated(x, specs$outcome_col[i])
    if (is.null(fit$fit) || is.null(fit$fit$df_age) || nrow(fit$fit$df_age) == 0) return(NULL)

    curve <- as.data.table(fit$fit$df_age)
    curve[, `:=`(
      country = country,
      outcome = specs$outcome[i],
      point_type = "observed_age_mean",
      fitted_score = NA_real_
    )]
    setnames(curve, "mean_score", "score")
    setnames(curve, "n_age", "n")
    setnames(curve, "age", "age_year")

    age_grid <- as.data.table(fit$fit$pred_df)
    if (nrow(age_grid) > 0) {
      age_grid[, `:=`(
        country = country,
        outcome = specs$outcome[i],
        point_type = "fitted_4PL",
        score = pred,
        n = NA_integer_
      )]
      setnames(age_grid, "age", "age_year")
      age_grid[, fitted_score := pred]
      age_grid <- age_grid[, .(country, outcome, age_year, score, n, point_type, fitted_score)]
    }

    observed <- curve[, .(country, outcome, age_year, score, n, point_type, fitted_score)]
    rbindlist(list(observed, age_grid), use.names = TRUE, fill = TRUE)
  }), use.names = TRUE, fill = TRUE)
}

sample_bootstrap_rows <- function(x, sample_cap) {
  n <- nrow(x)
  m <- min(sample_cap, n)
  x[sample.int(n = n, size = m, replace = TRUE)]
}

one_bootstrap <- function(x, sample_cap, iter) {
  samp <- sample_bootstrap_rows(x, sample_cap)

  adl_fit <- fit_logistic_onset_validated(samp, "ADL_totalScore")
  iadl_fit <- fit_logistic_onset_validated(samp, "IADL_totalScore")

  data.table(
    bootstrap_id = iter,
    bootstrap_sample_size = nrow(samp),
    ADL_TP = adl_fit$tp,
    IADL_TP = iadl_fit$tp,
    ADL_valid = isTRUE(adl_fit$valid),
    IADL_valid = isTRUE(iadl_fit$valid),
    ADL_reason = adl_fit$reason,
    IADL_reason = iadl_fit$reason
  )
}

summarise_bootstrap <- function(boot, country, n_records, n_persons, bootstrap_sample_size) {
  adl <- boot[ADL_valid == TRUE & is.finite(ADL_TP), ADL_TP]
  iadl <- boot[IADL_valid == TRUE & is.finite(IADL_TP), IADL_TP]
  diff <- boot[ADL_valid == TRUE & IADL_valid == TRUE &
                 is.finite(ADL_TP) & is.finite(IADL_TP), ADL_TP - IADL_TP]

  make_row <- function(values, outcome, valid_n) {
    if (length(values) == 0) {
      return(data.table(
        country = country, outcome = outcome, n_records = n_records, n_persons = n_persons,
        bootstrap_sample_size = bootstrap_sample_size,
        n_boot = n_boot, valid_n = valid_n, valid_rate = valid_n / n_boot,
        mean = NA_real_, median = NA_real_, sd = NA_real_,
        ci_low = NA_real_, ci_high = NA_real_
      ))
    }
    data.table(
      country = country,
      outcome = outcome,
      n_records = n_records,
      n_persons = n_persons,
      bootstrap_sample_size = bootstrap_sample_size,
      n_boot = n_boot,
      valid_n = valid_n,
      valid_rate = valid_n / n_boot,
      mean = mean(values, na.rm = TRUE),
      median = median(values, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE),
      ci_low = as.numeric(quantile(values, 0.025, na.rm = TRUE, names = FALSE)),
      ci_high = as.numeric(quantile(values, 0.975, na.rm = TRUE, names = FALSE))
    )
  }

  rbindlist(list(
    make_row(iadl, "IADL_TP", sum(boot$IADL_valid, na.rm = TRUE)),
    make_row(adl, "ADL_TP", sum(boot$ADL_valid, na.rm = TRUE)),
    make_row(diff, "ADL_minus_IADL_TP", sum(boot$ADL_valid & boot$IADL_valid, na.rm = TRUE))
  ))
}

run_country <- function(path) {
  country <- country_from_file(path)
  message("[", country, "] reading data")
  x <- read_country(path)
  n_records <- nrow(x)
  n_persons <- uniqueN(x$ID)
  if (n_records == 0 || n_persons == 0) stop("No usable rows in ", basename(path))

  bootstrap_sample_size <- min(sample_cap, n_records)
  message("[", country, "] fitting full-data curves and running bootstrap: n_boot=", n_boot,
          ", bootstrap_sample_size=min(", sample_cap, ", ", n_records, ")=", bootstrap_sample_size)
  full_curves <- fit_country_full_curves(x, country)

  setkey(x, ID)
  seeds <- seed + seq_len(n_boot) + match(country, vapply(country_files, country_from_file, character(1))) * 100000L

  boot_fun <- function(i) {
    set.seed(seeds[i])
    one_bootstrap(x, sample_cap = sample_cap, iter = i)
  }

  if (n_cores > 1 && .Platform$OS.type != "windows") {
    boot <- rbindlist(parallel::mclapply(seq_len(n_boot), boot_fun, mc.cores = n_cores), use.names = TRUE)
  } else {
    boot <- rbindlist(lapply(seq_len(n_boot), boot_fun), use.names = TRUE)
  }
  boot[, country := country]
  boot[, ADL_minus_IADL_TP := fifelse(ADL_valid & IADL_valid, ADL_TP - IADL_TP, NA_real_)]

  summary <- summarise_bootstrap(boot, country, n_records, n_persons, bootstrap_sample_size)
  list(country = country, boot = boot, summary = summary, curves = full_curves)
}

country_results <- lapply(country_files, run_country)

boot_all <- rbindlist(lapply(country_results, `[[`, "boot"), use.names = TRUE, fill = TRUE)
summary_all <- rbindlist(lapply(country_results, `[[`, "summary"), use.names = TRUE, fill = TRUE)
curves_all <- rbindlist(lapply(country_results, `[[`, "curves"), use.names = TRUE, fill = TRUE)

setorder(summary_all, outcome, country)
fwrite(boot_all, file.path(output_dir, "country_bootstrap_tp_raw.csv"))
fwrite(summary_all, file.path(output_dir, "country_tp_bootstrap_summary.csv"))

summary_wide <- dcast(
  summary_all,
  country + n_records + n_persons + bootstrap_sample_size + n_boot ~ outcome,
  value.var = c("mean", "median", "sd", "ci_low", "ci_high", "valid_rate")
)
fwrite(summary_wide, file.path(output_dir, "country_tp_bootstrap_summary_wide.csv"))
fwrite(curves_all, file.path(output_dir, "country_age_mean_and_4pl_curves.csv"))

meta_input <- copy(summary_all)
meta_input[, yi := mean]
meta_input[, raw_se := fifelse(
  is.finite(sd) & sd > 0,
  sd,
  fifelse(
    is.finite(ci_low) & is.finite(ci_high) & (ci_high - ci_low) > 0,
    (ci_high - ci_low) / (2 * 1.96),
    NA_real_
  )
)]
meta_input[, se_floor := {
  positive_se <- raw_se[is.finite(raw_se) & raw_se > 0]
  if (length(positive_se) > 0) {
    median(positive_se, na.rm = TRUE)
  } else {
    max(AGE_GRID_BY, 0.1)
  }
}, by = outcome]
meta_input[, sei := fifelse(is.finite(raw_se) & raw_se > 0, raw_se, se_floor)]
meta_input[, sei_source := fifelse(is.finite(raw_se) & raw_se > 0, "bootstrap_sd", "outcome_se_floor")]
meta_input <- meta_input[is.finite(yi) & is.finite(sei) & sei > 0 & valid_n >= 10]
fwrite(meta_input, file.path(output_dir, "random_effects_meta_analysis_input.csv"))

run_meta <- function(x, outcome_name) {
  d <- x[outcome == outcome_name]
  if (nrow(d) < 2) {
    return(list(
      result = data.table(
        outcome = outcome_name, k = nrow(d), pooled = NA_real_,
        ci_low = NA_real_, ci_high = NA_real_, tau2 = NA_real_, I2 = NA_real_,
        Q = NA_real_, Q_p = NA_real_, method = "REML"
      ),
      model = NULL
    ))
  }
  model <- rma.uni(yi = d$yi, sei = d$sei, method = "REML", slab = d$country)
  result <- data.table(
    outcome = outcome_name,
    k = model$k,
    pooled = as.numeric(model$b[1]),
    ci_low = model$ci.lb,
    ci_high = model$ci.ub,
    tau2 = model$tau2,
    I2 = model$I2,
    Q = model$QE,
    Q_p = model$QEp,
    method = "REML"
  )
  list(result = result, model = model)
}

outcomes <- c("IADL_TP", "ADL_TP", "ADL_minus_IADL_TP")
meta_results <- lapply(outcomes, function(o) run_meta(meta_input, o))
meta_summary <- rbindlist(lapply(meta_results, `[[`, "result"), use.names = TRUE, fill = TRUE)
fwrite(meta_summary, file.path(output_dir, "random_effects_meta_analysis_summary.csv"))

plot_curves <- function(curves, summary_table, output_path) {
  tp_rect <- summary_table[outcome %in% c("ADL_TP", "IADL_TP")]
  tp_rect[, curve_outcome := fifelse(outcome == "ADL_TP", "ADL", "IADL")]
  tp_line <- tp_rect[, .(country, curve_outcome, median, ci_low, ci_high, valid_rate)]

  observed <- curves[point_type == "observed_age_mean"]
  fitted <- curves[point_type == "fitted_4PL"]
  tp_annot <- dcast(
    tp_line,
    country ~ curve_outcome,
    value.var = "median"
  )
  for (col in c("ADL", "IADL")) {
    if (!col %in% names(tp_annot)) tp_annot[, (col) := NA_real_]
  }
  label_pos <- observed[, .(
    x = min(age_year, na.rm = TRUE) + 1,
    y = max(score, na.rm = TRUE)
  ), by = country]
  label_pos[!is.finite(y), y := 0]
  label_pos[, y := fifelse(y <= 0, 0.05, y * 0.92)]
  tp_annot <- merge(tp_annot, label_pos, by = "country", all.x = TRUE)
  tp_annot[, label := sprintf("ADL TP = %.1f\nIADL TP = %.1f", ADL, IADL)]

  p <- ggplot() +
    geom_rect(
      data = tp_line,
      aes(xmin = ci_low, xmax = ci_high, ymin = -Inf, ymax = Inf, fill = curve_outcome),
      alpha = 0.08,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = observed,
      aes(x = age_year, y = score, color = outcome, size = 3.5),
      alpha = 0.85
    ) +
    geom_line(
      data = fitted,
      aes(x = age_year, y = score, color = outcome),
      linewidth = 1.15
    ) +
    geom_vline(
      data = tp_line,
      aes(xintercept = median, color = curve_outcome),
      linetype = "dashed",
      linewidth = 0.9
    ) +
    geom_label(
      data = tp_annot,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 3,
      linewidth = 0.2,
      fill = "white",
      alpha = 0.86
    ) +
    facet_wrap(~ country, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c(ADL = "#f9ada4", IADL = "#6bb084")) +
    scale_fill_manual(values = c(ADL = "#f9ada4", IADL = "#6bb084")) +
    scale_size_continuous(range = c(0.6, 2.5), guide = "none") +
    labs(
      title = "Country-specific ADL/IADL age curves and 4PL turning points",
      x = "Age",
      y = "Mean total score",
      color = "Outcome",
      fill = "TP 95% CI"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      panel.grid.minor = element_blank()
    )

  ggsave(output_path, p, width = 11, height = 20, device = "pdf")
}

plot_forest <- function(meta_dat, meta_summary_row, outcome_name, output_path) {
  country_colors <- c(
    CHARLS = "#ff6f00",
    CRELES = "#c71000",
    ELSA = "#008ea0",
    HRS = "#8a4198",
    KLoSA = "#5a9599",
    LASI = "#ff6348",
    MHAS = "#84d7e1",
    SHARE = "#ff95a8",
    "Pooled random-effects" = "#3d3b25"
  )

  d <- meta_dat[outcome == outcome_name]
  d[, plot_label := country]
  d[, plot_mean := yi]
  d[, plot_low := ci_low]
  d[, plot_high := ci_high]
  tau2 <- ifelse(is.finite(meta_summary_row$tau2), meta_summary_row$tau2, 0)
  d[, weight := 100 / (sei^2 + tau2) / sum(1 / (sei^2 + tau2), na.rm = TRUE)]

  pooled <- data.table(
    plot_label = "Pooled random-effects",
    plot_mean = meta_summary_row$pooled,
    plot_low = meta_summary_row$ci_low,
    plot_high = meta_summary_row$ci_high,
    weight = 100,
    valid_rate = NA_real_,
    row_type = "pooled"
  )
  d[, row_type := "country"]
  plot_dat <- rbindlist(list(
    d[, .(plot_label, plot_mean, plot_low, plot_high, weight, valid_rate, row_type)],
    pooled
  ), fill = TRUE)

  plot_dat <- plot_dat[is.finite(plot_mean) & is.finite(plot_low) & is.finite(plot_high)]
  country_order <- c(sort(d$plot_label), "Pooled random-effects")
  plot_dat[, plot_label := factor(plot_label, levels = rev(country_order))]
  plot_dat[, color_key := as.character(plot_label)]
  plot_dat[, estimate_text := sprintf("%.2f (%.2f, %.2f)", plot_mean, plot_low, plot_high)]
  plot_dat[, weight_text := fifelse(row_type == "pooled", "100.0%", sprintf("%.1f%%", weight))]
  plot_dat[, value_label := sprintf("%.2f", plot_mean)]

  x_label <- if (outcome_name == "ADL_minus_IADL_TP") {
    "ADL TP - IADL TP (years)"
  } else {
    "Turning point age (years)"
  }

  header_fill <- "#21666a"
  i2_label <- if (is.finite(meta_summary_row$I2)) {
    sprintf("I2 = %.1f%%", meta_summary_row$I2)
  } else {
    "I2 = NA"
  }
  x_range <- range(c(plot_dat$plot_low, plot_dat$plot_high), na.rm = TRUE)
  x_span <- diff(x_range)
  if (!is.finite(x_span) || x_span <= 0) x_span <- 1
  x_limits <- c(x_range[1] - 0.08 * x_span, x_range[2] + 0.12 * x_span)
  header_y_min <- length(levels(plot_dat$plot_label)) + 0.55
  header_y_max <- length(levels(plot_dat$plot_label)) + 1.35
  header_y_mid <- (header_y_min + header_y_max) / 2
  header_label <- switch(
    outcome_name,
    IADL_TP = "IADL TP",
    ADL_TP = "ADL TP",
    ADL_minus_IADL_TP = "ADL - IADL TP",
    outcome_name
  )

  left_plot <- ggplot(plot_dat, aes(y = plot_label)) +
    geom_tile(
      aes(x = 1, fill = color_key),
      width = 0.72,
      height = 0.92,
      color = "white",
      linewidth = 0.35
    ) +
    geom_text(
      aes(x = 0.55, label = as.character(plot_label)),
      hjust = 1,
      size = 3.7,
      fontface = ifelse(plot_dat$row_type == "pooled", "bold", "plain")
    ) +
    scale_fill_manual(values = country_colors, guide = "none") +
    scale_x_continuous(limits = c(-1.1, 1.45), expand = c(0, 0)) +
    scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.45, 1.45))) +
    labs(title = "Country", x = NULL, y = NULL) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.15, size = 11),
      plot.margin = margin(18, 0, 8, 4)
    )

  main_plot <- ggplot(plot_dat, aes(y = plot_label, x = plot_mean)) +
    annotate(
      "rect",
      xmin = -Inf, xmax = Inf, ymin = header_y_min,
      ymax = header_y_max,
      fill = header_fill,
      color = "black",
      linewidth = 0.35
    ) +
    annotate(
      "text",
      x = mean(x_range),
      y = header_y_mid,
      label = header_label,
      color = "white",
      fontface = "bold",
      size = 8
    ) +
    annotate(
      "text",
      x = x_limits[2] - 0.02 * x_span,
      y = header_y_mid,
      label = i2_label,
      color = "white",
      hjust = 1,
      fontface = "bold",
      size = 8
    ) +
    geom_vline(xintercept = meta_summary_row$pooled, color = "grey45", linetype = "dashed") +
    geom_errorbar(
      aes(xmin = plot_low, xmax = plot_high, color = color_key),
      orientation = "y",
      width = 0.17,
      linewidth = 0.8
    ) +
    geom_point(
      data = plot_dat[row_type == "country"],
      aes(color = color_key),
      shape = 18,
      size = 3.6
    ) +
    geom_point(
      data = plot_dat[row_type == "pooled"],
      aes(color = color_key),
      shape = 18,
      size = 7
    ) +
    geom_text(
      data = plot_dat[row_type == "country"],
      aes(label = value_label, color = color_key),
      nudge_y = 0.28,
      size = 8
    ) +
    geom_text(
      data = plot_dat[row_type == "pooled"],
      aes(label = value_label, color = color_key),
      nudge_y = 0.32,
      size = 8
    ) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(limits = x_limits, expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.45, 1.45))) +
    labs(
      x = x_label,
      y = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(18, 6, 8, 0)
    )

  right_plot <- ggplot(plot_dat, aes(y = plot_label)) +
    geom_text(
      aes(x = 0.05, label = weight_text, color = color_key),
      hjust = 0,
      size = 8,
      fontface = ifelse(plot_dat$row_type == "pooled", "bold", "plain")
    ) +
    geom_text(
      aes(x = 0.78, label = estimate_text, color = color_key),
      hjust = 0,
      size = 8,
      fontface = ifelse(plot_dat$row_type == "pooled", "bold", "plain")
    ) +
    annotate("text", x = 0.05, y = length(levels(plot_dat$plot_label)) + 0.7,
             label = "Weight (%)", hjust = 0, fontface = "bold", size = 3.7) +
    annotate("text", x = 0.78, y = length(levels(plot_dat$plot_label)) + 0.7,
             label = "TP (95% CI)", hjust = 0, fontface = "bold", size = 3.7) +
    scale_color_manual(values = country_colors, guide = "none") +
    scale_x_continuous(limits = c(0, 2.05), expand = c(0, 0)) +
    scale_y_discrete(drop = FALSE, expand = expansion(add = c(0.45, 1.45))) +
    theme_void(base_size = 11) +
    theme(plot.margin = margin(18, 4, 8, 4))

  combined <- left_plot + main_plot + right_plot +
    plot_layout(widths = c(1.65, 3.0, 2.75))

  ggsave(output_path, combined, width = 24, height = 13, device = "pdf")
}

plot_curves(
  curves_all,
  summary_all,
  file.path(output_dir, "country_adl_iadl_age_curves_tp_95ci.pdf")
)

for (outcome_name in outcomes) {
  meta_row <- meta_summary[outcome == outcome_name]
  if (nrow(meta_row) == 1 && is.finite(meta_row$pooled)) {
    safe_name <- gsub("[^A-Za-z0-9]+", "_", outcome_name)
    plot_forest(
      meta_input,
      meta_row,
      outcome_name,
      file.path(output_dir, paste0("forest_plot_", safe_name, ".pdf"))
    )
  }
}

message("TP and meta-analysis complete.")
message("Input directory: ", normalizePath(input_dir, mustWork = FALSE))
message("Output directory: ", normalizePath(output_dir, mustWork = FALSE))
message("Main outputs:")
message("  - country_tp_bootstrap_summary.csv")
message("  - random_effects_meta_analysis_summary.csv")
message("  - country_adl_iadl_age_curves_tp_95ci.pdf")
message("  - forest_plot_IADL_TP.pdf")
message("  - forest_plot_ADL_TP.pdf")
message("  - forest_plot_ADL_minus_IADL_TP.pdf")
