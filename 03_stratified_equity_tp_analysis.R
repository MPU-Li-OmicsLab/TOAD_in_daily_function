#!/usr/bin/env Rscript

# Stratified within-country TP analysis for health equity questions.
#
# Usage:
#   Rscript 03_stratified_equity_tp_analysis.R clean_outputs stratified_equity_outputs 1000 30000 200 20260603 6 0.2 0.1 200 10000
#
# Arguments:
#   1 input_dir     Directory containing *.country.data.clean.csv files.
#   2 output_dir    Directory where outputs are saved.
#   3 n_boot        Bootstrap repetitions per country-subgroup. Default: 1000.
#   4 sample_cap    Maximum records sampled per bootstrap draw. Default: 50000.
#   5 n_perm        Permutations per country-stratum-outcome. Default: 500.
#   6 seed          Random seed. Default: 20260603.
#   7 n_cores       Parallel workers. Default: 1.
#   8 acc_threshold_prop  Onset threshold as a proportion of max positive acceleration. Default: 0.2.
#   9 age_grid_by         Age grid interval for second derivative calculation. Default: 0.1.
#  10 min_group_n         Minimum non-missing records per subgroup. Default: 200.
#  11 perm_sample_cap     Maximum rows used per permutation test dataset. Default: 10000.

suppressPackageStartupMessages({
  required_pkgs <- c("data.table", "dplyr", "ggplot2", "minpack.lm")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
         ". Please install them before running this script.")
  }
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(minpack.lm)
})

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "clean_outputs"
output_dir <- if (length(args) >= 2) args[[2]] else "stratified_equity_outputs"
n_boot <- if (length(args) >= 3) as.integer(args[[3]]) else 1000L
sample_cap <- if (length(args) >= 4) as.integer(args[[4]]) else 50000L
n_perm <- if (length(args) >= 5) as.integer(args[[5]]) else 500L
seed <- if (length(args) >= 6) as.integer(args[[6]]) else 20260603L
n_cores <- if (length(args) >= 7) as.integer(args[[7]]) else 1L
ACC_THRESHOLD_PROP <- if (length(args) >= 8) as.numeric(args[[8]]) else 0.2
AGE_GRID_BY <- if (length(args) >= 9) as.numeric(args[[9]]) else 0.1
min_group_n <- if (length(args) >= 10) as.integer(args[[10]]) else 200L
perm_sample_cap <- if (length(args) >= 11) as.integer(args[[11]]) else 10000L

if (is.na(n_boot) || n_boot < 1) stop("n_boot must be a positive integer.")
if (is.na(sample_cap) || sample_cap < 1) stop("sample_cap must be a positive integer.")
if (is.na(n_perm) || n_perm < 0) stop("n_perm must be a non-negative integer.")
if (is.na(n_cores) || n_cores < 1) n_cores <- 1L
if (is.na(ACC_THRESHOLD_PROP) || ACC_THRESHOLD_PROP <= 0 || ACC_THRESHOLD_PROP > 1) {
  stop("acc_threshold_prop must be in (0, 1].")
}
if (is.na(AGE_GRID_BY) || AGE_GRID_BY <= 0) stop("age_grid_by must be positive.")
if (is.na(min_group_n) || min_group_n < 20) stop("min_group_n must be at least 20.")
if (is.na(perm_sample_cap) || perm_sample_cap < 2 * min_group_n) {
  stop("perm_sample_cap must be at least 2 * min_group_n.")
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
fig_dir <- file.path(output_dir, "forest_plots_by_country")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
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

country_palette <- c(
  CHARLS = "#ff6f00",
  CRELES = "#c71000",
  ELSA = "#008ea0",
  HRS = "#8a4198",
  KLoSA = "#5a9599",
  LASI = "#ff6348",
  MHAS = "#84d7e1",
  SHARE = "#ff95a8",
  Pooled = "#3d3b25"
)

stratum_palette <- c(
  Sex = "#a30543",
  Education = "#f36f43",
  Income = "#80cba4",
  Rurality = "#4965b0"
)

outcome_specs <- data.table(
  outcome = c("ADL", "IADL"),
  score_var = c("ADL_totalScore", "IADL_totalScore")
)

required_cols <- c("ID", "age", "ADL_totalScore", "IADL_totalScore")
optional_cols <- c(
  "gender_std", "education", "education_cat", "rural_std",
  "personal_income", "household_income", "total_income"
)

clean_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | tolower(x) %in% c("na", "nan", "null", "missing")] <- NA_character_
  x
}

normalise_gender <- function(x) {
  x <- tolower(clean_chr(x))
  out <- fifelse(x %in% c("male", "m", "1", "man"), "male",
                 fifelse(x %in% c("female", "f", "2", "woman"), "female", NA_character_))
  factor(out, levels = c("male", "female"))
}

normalise_education <- function(x) {
  x_raw <- clean_chr(x)
  x_low <- tolower(x_raw)
  out <- rep(NA_character_, length(x_low))
  out[x_low %in% c("primary_or_below", "primary or below", "low", "lower", "primary", "none",
                   "no schooling", "elementary", "less_than_primary", "illiterate")] <- "low"
  out[x_low %in% c("secondary", "middle", "mid", "medium", "high school", "junior high",
                   "senior high", "middle_school", "secondary_school")] <- "middle"
  out[x_low %in% c("college_or_above", "college or above", "high", "college", "university",
                   "tertiary", "post-secondary", "postsecondary", "degree")] <- "high"
  out[is.na(out) & grepl("primary|below|none|illiterate|elementary", x_low)] <- "low"
  out[is.na(out) & grepl("secondary|middle|school|junior|senior", x_low)] <- "middle"
  out[is.na(out) & grepl("college|university|tertiary|degree", x_low)] <- "high"
  factor(out, levels = c("low", "middle", "high"))
}

recode_education_by_database <- function(education, database) {
  education_years <- suppressWarnings(as.numeric(education))
  db <- tolower(as.character(database))
  out <- fcase(
    is.na(education_years), NA_character_,
    db == "hrs" & education_years <= 2, "low",
    db == "hrs" & education_years <= 4, "middle",
    db == "hrs" & education_years == 5, "high",
    db != "hrs" & education_years == 1, "low",
    db != "hrs" & education_years == 2, "middle",
    db != "hrs" & education_years == 3, "high",
    default = NA_character_
  )
  factor(out, levels = c("low", "middle", "high"))
}

normalise_rural <- function(x) {
  x <- tolower(clean_chr(x))
  out <- rep(NA_character_, length(x))
  out[x %in% c("rural", "1", "yes")] <- "rural"
  out[x %in% c("urban", "0", "no")] <- "urban"
  out[is.na(out) & grepl("rural", x)] <- "rural"
  out[is.na(out) & grepl("urban|city|town", x)] <- "urban"
  factor(out, levels = c("urban", "rural"))
}

as_num <- function(x) suppressWarnings(as.numeric(x))

read_country <- function(path, database = NULL) {
  x <- fread(path, showProgress = FALSE)
  missing_required <- setdiff(required_cols, names(x))
  if (length(missing_required) > 0) {
    stop("File ", basename(path), " is missing required column(s): ",
         paste(missing_required, collapse = ", "))
  }
  for (nm in optional_cols) {
    if (!nm %in% names(x)) x[, (nm) := NA]
  }
  keep_cols <- unique(c(required_cols, optional_cols))
  x <- x[, ..keep_cols]
  x[, ID := as.character(ID)]
  x[, age := as_num(age)]
  x[, ADL_totalScore := as_num(ADL_totalScore)]
  x[, IADL_totalScore := as_num(IADL_totalScore)]
  x[, gender_std := normalise_gender(gender_std)]
  if (!is.null(database) && "education" %in% names(x)) {
    x[, education_cat := recode_education_by_database(education, database)]
  } else {
    x[, education_cat := normalise_education(education_cat)]
  }
  x[, rural_std := normalise_rural(rural_std)]
  x[, personal_income := as_num(personal_income)]
  x[, household_income := as_num(household_income)]
  x[, total_income := as_num(total_income)]
  x <- x[!is.na(age) & age >= 60 & age <= 100]
  x
}

# ----------------------------
# 4PL Logistic function, kept consistent with prior TP analyses.
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

bootstrap_tps <- function(data, score_var, n_boot, sample_cap, n_cores = 1L) {
  n <- nrow(data)
  if (n < min_group_n) {
    return(rep(NA_real_, n_boot))
  }
  m <- min(sample_cap, n)
  one_draw <- function(i) {
    idx <- sample.int(n, size = m, replace = TRUE)
    fit <- fit_logistic_onset_validated(data[idx, ], score_var)
    fit$tp
  }
  if (n_cores > 1L && .Platform$OS.type != "windows") {
    unlist(parallel::mclapply(seq_len(n_boot), one_draw, mc.cores = n_cores), use.names = FALSE)
  } else {
    vapply(seq_len(n_boot), one_draw, numeric(1))
  }
}

summarise_tp <- function(tp_vec) {
  valid <- tp_vec[is.finite(tp_vec)]
  if (length(valid) == 0) {
    return(data.table(
      tp_mean = NA_real_,
      tp_median = NA_real_,
      tp_sd = NA_real_,
      tp_ci_low = NA_real_,
      tp_ci_high = NA_real_,
      valid_n = 0L,
      boot_n = length(tp_vec),
      valid_rate = 0
    ))
  }
  data.table(
    tp_mean = mean(valid),
    tp_median = median(valid),
    tp_sd = if (length(valid) > 1) sd(valid) else NA_real_,
    tp_ci_low = as.numeric(quantile(valid, 0.025, names = FALSE, na.rm = TRUE)),
    tp_ci_high = as.numeric(quantile(valid, 0.975, names = FALSE, na.rm = TRUE)),
    valid_n = length(valid),
    boot_n = length(tp_vec),
    valid_rate = length(valid) / length(tp_vec)
  )
}

choose_income_source <- function(x, country) {
  candidates <- c("personal_income", "household_income", "total_income")
  rows <- rbindlist(lapply(candidates, function(v) {
    z <- x[[v]]
    z <- z[is.finite(z)]
    data.table(
      country = country,
      income_source = v,
      non_missing_n = length(z),
      unique_n = length(unique(z)),
      zero_n = sum(z == 0, na.rm = TRUE),
      min_value = if (length(z) > 0) min(z, na.rm = TRUE) else NA_real_,
      max_value = if (length(z) > 0) max(z, na.rm = TRUE) else NA_real_
    )
  }))
  eligible <- rows[non_missing_n >= 2 * min_group_n & unique_n >= 5]
  if (nrow(eligible) == 0) {
    rows[, selected := FALSE]
    rows[, selection_reason := "No income column has enough non-missing records and at least 5 unique values."]
    return(list(source = NA_character_, audit = rows))
  }
  setorder(eligible, -non_missing_n, -unique_n)
  source <- eligible$income_source[1]
  rows[, selected := income_source == source]
  rows[, selection_reason := fifelse(selected, "Selected: fewest missing values among eligible income columns.",
                                     "Not selected.")]
  list(source = source, audit = rows)
}

add_income_quintiles <- function(x, source) {
  x[, income_value := NA_real_]
  x[, income_quintile := NA_integer_]
  x[, income_group_extreme := NA_character_]
  if (is.na(source) || !source %in% names(x)) {
    return(x)
  }
  z <- as_num(x[[source]])
  valid <- is.finite(z)
  if (sum(valid) < 2 * min_group_n || length(unique(z[valid])) < 5) {
    return(x)
  }
  z_valid <- z[valid]
  ord <- order(z_valid, seq_along(z_valid), na.last = NA)
  q <- rep(NA_integer_, length(z_valid))
  q_by_order <- ceiling(seq_along(ord) / length(ord) * 5)
  q[ord] <- pmin(pmax(q_by_order, 1L), 5L)
  x[valid, income_value := z[valid]]
  x[valid, income_quintile := as.integer(q)]
  x[income_quintile == 1L, income_group_extreme := "Q1_lowest"]
  x[income_quintile == 5L, income_group_extreme := "Q5_highest"]
  x
}

format_p <- function(p) {
  out <- rep("p=NA", length(p))
  finite <- is.finite(p)
  out[finite & p < 0.001] <- "p<0.001"
  out[finite & p >= 0.001] <- paste0("p=", sprintf("%.3f", p[finite & p >= 0.001]))
  out
}

format_star <- function(p) {
  out <- rep("", length(p))
  out[is.finite(p) & p < 0.05] <- "*"
  out[is.finite(p) & p < 0.01] <- "**"
  out[is.finite(p) & p < 0.001] <- "***"
  out
}

stratum_specs <- list(
  list(
    stratum = "Sex",
    var = "gender_std",
    levels = c("male", "female"),
    labels = c(male = "Male", female = "Female"),
    reference = "male",
    contrast = "female - male"
  ),
  list(
    stratum = "Education",
    var = "education_cat",
    levels = c("low", "middle", "high"),
    labels = c(low = "Low education", middle = "Middle education", high = "High education"),
    reference = "high",
    contrast = "low - high"
  ),
  list(
    stratum = "Income",
    var = "income_group_extreme",
    levels = c("Q1_lowest", "Q5_highest"),
    labels = c(Q1_lowest = "Lowest income quintile", Q5_highest = "Highest income quintile"),
    reference = "Q5_highest",
    contrast = "Q1 - Q5"
  ),
  list(
    stratum = "Rurality",
    var = "rural_std",
    levels = c("urban", "rural"),
    labels = c(urban = "Urban", rural = "Rural"),
    reference = "urban",
    contrast = "rural - urban"
  )
)

make_group_data <- function(x, spec) {
  if (!spec$var %in% names(x)) return(NULL)
  group_value <- clean_chr(x[[spec$var]])
  if (all(is.na(group_value))) return(NULL)
  out <- copy(x)
  out[, group_value := group_value]
  out <- out[group_value %in% spec$levels]
  if (nrow(out) == 0) return(NULL)
  out[, group_value := factor(group_value, levels = spec$levels)]
  out[, group_label := unname(spec$labels[as.character(group_value)])]
  out
}

estimate_stratum_tps <- function(x, country, spec, income_source) {
  gx <- make_group_data(x, spec)
  if (is.null(gx)) {
    return(rbindlist(lapply(seq_len(nrow(outcome_specs)), function(outcome_i) {
      outcome <- outcome_specs$outcome[outcome_i]
      score_var <- outcome_specs$score_var[outcome_i]
      rbindlist(lapply(spec$levels, function(group) {
        data.table(
          country = country,
          stratum = spec$stratum,
          stratum_var = spec$var,
          group_value = group,
          group_label = unname(spec$labels[group]),
          outcome = outcome,
          score_var = score_var,
          income_source = ifelse(spec$stratum == "Income", income_source, NA_character_),
          n_group = 0L,
          n_age = 0L,
          full_tp = NA_real_,
          tp_mean = NA_real_,
          tp_median = NA_real_,
          tp_sd = NA_real_,
          tp_ci_low = NA_real_,
          tp_ci_high = NA_real_,
          valid_n = 0L,
          boot_n = n_boot,
          valid_rate = 0,
          status = "stratum_not_available"
        )
      }))
    }), fill = TRUE))
  }

  out <- list()
  k <- 1L
  for (outcome_i in seq_len(nrow(outcome_specs))) {
    outcome <- outcome_specs$outcome[outcome_i]
    score_var <- outcome_specs$score_var[outcome_i]
    for (group in spec$levels) {
      subgroup <- gx[as.character(group_value) == group & !is.na(get(score_var))]
      n_group <- nrow(subgroup)
      age_n <- uniqueN(subgroup$age[!is.na(subgroup$age)])
      message("  TP bootstrap: ", country, " | ", spec$stratum, " | ", outcome,
              " | ", unname(spec$labels[group]), " | n=", n_group)
      if (n_group < min_group_n || age_n < 4) {
        out[[k]] <- data.table(
          country = country,
          stratum = spec$stratum,
          stratum_var = spec$var,
          group_value = group,
          group_label = unname(spec$labels[group]),
          outcome = outcome,
          score_var = score_var,
          income_source = ifelse(spec$stratum == "Income", income_source, NA_character_),
          n_group = n_group,
          n_age = age_n,
          full_tp = NA_real_,
          tp_mean = NA_real_,
          tp_median = NA_real_,
          tp_sd = NA_real_,
          tp_ci_low = NA_real_,
          tp_ci_high = NA_real_,
          valid_n = 0L,
          boot_n = n_boot,
          valid_rate = 0,
          status = "skipped_small_group"
        )
        k <- k + 1L
        next
      }
      full_fit <- fit_logistic_onset_validated(subgroup, score_var)
      boot_tps <- bootstrap_tps(subgroup, score_var, n_boot, sample_cap, n_cores)
      sm <- summarise_tp(boot_tps)
      out[[k]] <- cbind(
        data.table(
          country = country,
          stratum = spec$stratum,
          stratum_var = spec$var,
          group_value = group,
          group_label = unname(spec$labels[group]),
          outcome = outcome,
          score_var = score_var,
          income_source = ifelse(spec$stratum == "Income", income_source, NA_character_),
          n_group = n_group,
          n_age = age_n,
          full_tp = full_fit$tp,
          status = full_fit$reason
        ),
        sm
      )
      k <- k + 1L
    }
  }
  rbindlist(out, fill = TRUE)
}

calc_contrast_from_tps <- function(tp_by_group, spec) {
  valid <- tp_by_group[is.finite(tp)]
  if (nrow(valid) < 2) {
    return(list(stat = NA_real_, contrast_value = NA_real_, contrast_label = spec$contrast))
  }
  if (spec$stratum == "Education") {
    if (all(c("low", "high") %in% valid$group_value)) {
      contrast_value <- valid[group_value == "low", tp][1] - valid[group_value == "high", tp][1]
      return(list(stat = abs(contrast_value), contrast_value = contrast_value,
                  contrast_label = spec$contrast))
    }
    stat <- max(valid$tp) - min(valid$tp)
    return(list(stat = stat, contrast_value = stat, contrast_label = "max - min"))
  }
  ref <- spec$reference
  other <- setdiff(spec$levels, ref)
  if (length(other) == 1 && all(c(ref, other) %in% valid$group_value)) {
    contrast_value <- valid[group_value == other, tp][1] - valid[group_value == ref, tp][1]
    return(list(stat = abs(contrast_value), contrast_value = contrast_value,
                contrast_label = spec$contrast))
  }
  stat <- max(valid$tp) - min(valid$tp)
  list(stat = stat, contrast_value = stat, contrast_label = "max - min")
}

fit_group_tps_no_boot <- function(gx, spec, score_var) {
  rbindlist(lapply(spec$levels, function(group) {
    subgroup <- gx[as.character(group_value) == group & !is.na(get(score_var))]
    if (nrow(subgroup) < min_group_n || uniqueN(subgroup$age[!is.na(subgroup$age)]) < 4) {
      return(data.table(group_value = group, tp = NA_real_))
    }
    fit <- fit_logistic_onset_validated(subgroup, score_var)
    data.table(group_value = group, tp = fit$tp)
  }), fill = TRUE)
}

permutation_test_stratum <- function(x, country, spec) {
  gx0 <- make_group_data(x, spec)
  if (is.null(gx0) || n_perm == 0) {
    return(rbindlist(lapply(outcome_specs$outcome, function(o) {
      data.table(
        country = country,
        stratum = spec$stratum,
        outcome = o,
        contrast = spec$contrast,
        observed_difference = NA_real_,
        observed_stat = NA_real_,
        permutation_p = NA_real_,
        n_perm = n_perm,
        valid_perm_n = 0L,
        status = "not_available"
      )
    })))
  }

  rbindlist(lapply(seq_len(nrow(outcome_specs)), function(i) {
    outcome <- outcome_specs$outcome[i]
    score_var <- outcome_specs$score_var[i]
    gx <- gx0[!is.na(get(score_var))]
    if (nrow(gx) > perm_sample_cap) {
      set.seed(seed + nchar(country) + i + match(spec$stratum, vapply(stratum_specs, `[[`, character(1), "stratum")))
      gx <- gx[sample.int(.N, perm_sample_cap, replace = FALSE)]
    }
    available_groups <- gx[, .N, by = group_value][N >= min_group_n, as.character(group_value)]
    gx <- gx[as.character(group_value) %in% available_groups]
    if (length(available_groups) < 2 || nrow(gx) < 2 * min_group_n) {
      return(data.table(
        country = country,
        stratum = spec$stratum,
        outcome = outcome,
        contrast = spec$contrast,
        observed_difference = NA_real_,
        observed_stat = NA_real_,
        permutation_p = NA_real_,
        n_perm = n_perm,
        valid_perm_n = 0L,
        status = "skipped_fewer_than_two_groups"
      ))
    }

    obs_tps <- fit_group_tps_no_boot(gx, spec, score_var)
    obs <- calc_contrast_from_tps(obs_tps, spec)
    if (!is.finite(obs$stat)) {
      return(data.table(
        country = country,
        stratum = spec$stratum,
        outcome = outcome,
        contrast = obs$contrast_label,
        observed_difference = obs$contrast_value,
        observed_stat = obs$stat,
        permutation_p = NA_real_,
        n_perm = n_perm,
        valid_perm_n = 0L,
        status = "observed_tp_invalid"
      ))
    }

    original_groups <- as.character(gx$group_value)
    message("  Permutation: ", country, " | ", spec$stratum, " | ", outcome,
            " | rows=", nrow(gx), " | n_perm=", n_perm)
    one_perm <- function(b) {
      gx_perm <- copy(gx)
      gx_perm[, group_value := factor(sample(original_groups, replace = FALSE), levels = spec$levels)]
      perm_tps <- fit_group_tps_no_boot(gx_perm, spec, score_var)
      calc_contrast_from_tps(perm_tps, spec)$stat
    }
    if (n_cores > 1L && .Platform$OS.type != "windows") {
      perm_stats <- unlist(parallel::mclapply(seq_len(n_perm), one_perm, mc.cores = n_cores), use.names = FALSE)
    } else {
      perm_stats <- vapply(seq_len(n_perm), one_perm, numeric(1))
    }
    valid_stats <- perm_stats[is.finite(perm_stats)]
    p_value <- if (length(valid_stats) == 0) {
      NA_real_
    } else {
      (sum(valid_stats >= obs$stat) + 1) / (length(valid_stats) + 1)
    }
    data.table(
      country = country,
      stratum = spec$stratum,
      outcome = outcome,
      contrast = obs$contrast_label,
      observed_difference = obs$contrast_value,
      observed_stat = obs$stat,
      permutation_p = p_value,
      n_perm = n_perm,
      valid_perm_n = length(valid_stats),
      status = ifelse(is.finite(p_value), "ok", "permutation_tp_invalid")
    )
  }), fill = TRUE)
}

estimate_income_quintile_trend <- function(x, country, income_source) {
  if (is.na(income_source) || !all(c("income_quintile", "income_value") %in% names(x))) {
    return(NULL)
  }
  trend_x <- copy(x[!is.na(income_quintile)])
  if (nrow(trend_x) == 0) return(NULL)
  trend_x[, income_quintile_group := paste0("Q", income_quintile)]
  trend_x[, income_quintile_group := factor(income_quintile_group, levels = paste0("Q", 1:5))]

  rbindlist(lapply(seq_len(nrow(outcome_specs)), function(i) {
    outcome <- outcome_specs$outcome[i]
    score_var <- outcome_specs$score_var[i]
    rbindlist(lapply(paste0("Q", 1:5), function(group) {
      subgroup <- trend_x[as.character(income_quintile_group) == group & !is.na(get(score_var))]
      n_group <- nrow(subgroup)
      age_n <- uniqueN(subgroup$age[!is.na(subgroup$age)])
      if (n_group < min_group_n || age_n < 4) {
        return(data.table(
          country = country,
          income_source = income_source,
          income_quintile = group,
          outcome = outcome,
          n_group = n_group,
          n_age = age_n,
          full_tp = NA_real_,
          tp_mean = NA_real_,
          tp_median = NA_real_,
          tp_ci_low = NA_real_,
          tp_ci_high = NA_real_,
          valid_rate = 0,
          status = "skipped_small_group"
        ))
      }
      full_fit <- fit_logistic_onset_validated(subgroup, score_var)
      boot_tps <- bootstrap_tps(subgroup, score_var, n_boot, sample_cap, n_cores)
      sm <- summarise_tp(boot_tps)
      cbind(
        data.table(
          country = country,
          income_source = income_source,
          income_quintile = group,
          outcome = outcome,
          n_group = n_group,
          n_age = age_n,
          full_tp = full_fit$tp,
          status = full_fit$reason
        ),
        sm
      )
    }), fill = TRUE)
  }), fill = TRUE)
}

availability_table <- function(x, country, income_source) {
  data.table(
    country = country,
    n_total = nrow(x),
    gender_non_missing = sum(!is.na(x$gender_std)),
    education_non_missing = sum(!is.na(x$education_cat)),
    rural_non_missing = sum(!is.na(x$rural_std)),
    personal_income_non_missing = sum(is.finite(x$personal_income)),
    household_income_non_missing = sum(is.finite(x$household_income)),
    total_income_non_missing = sum(is.finite(x$total_income)),
    selected_income_source = income_source
  )
}

plot_country_forest <- function(summary_dt, p_dt, country) {
  country_name <- country
  plot_dt <- copy(summary_dt[country == country_name])
  if (nrow(plot_dt) == 0) return(invisible(NULL))

  plot_dt[, stratum := factor(stratum, levels = c("Sex", "Education", "Income", "Rurality"))]
  plot_dt[, group_order := fifelse(stratum == "Sex" & group_value == "male", 1,
                            fifelse(stratum == "Sex" & group_value == "female", 2,
                            fifelse(stratum == "Education" & group_value == "low", 3,
                            fifelse(stratum == "Education" & group_value == "middle", 4,
                            fifelse(stratum == "Education" & group_value == "high", 5,
                            fifelse(stratum == "Income" & group_value == "Q1_lowest", 6,
                            fifelse(stratum == "Income" & group_value == "Q5_highest", 7,
                            fifelse(stratum == "Rurality" & group_value == "urban", 8, 9))))))))]
  plot_dt[, tp_ci_text := fifelse(
    is.finite(tp_median) & is.finite(tp_ci_low) & is.finite(tp_ci_high),
    sprintf("%.1f (%.1f-%.1f)", tp_median, tp_ci_low, tp_ci_high),
    paste0("NA; n=", n_group, "; ", status)
  )]

  p_lab <- copy(p_dt[country == country_name])
  p_lab[, p_label := paste0(contrast, ", ", format_p(permutation_p))]

  finite_ci <- plot_dt[is.finite(tp_ci_low) & is.finite(tp_ci_high)]
  x_min <- if (nrow(finite_ci) > 0) max(55, floor(min(finite_ci$tp_ci_low) - 2)) else 60
  x_max <- if (nrow(finite_ci) > 0) min(105, ceiling(max(finite_ci$tp_ci_high) + 2)) else 100
  if (!is.finite(x_min) || !is.finite(x_max) || x_min >= x_max) {
    x_min <- 60
    x_max <- 100
  }
  label_x <- x_min - 20
  tp_text_x <- x_max + 7
  p_text_x <- x_max + 21
  right_x <- p_text_x + 7

  country_color <- if (country %in% names(country_palette)) country_palette[[country]] else "#008ea0"

  row_list <- list()
  row_i <- 0L
  for (outcome_name in outcome_specs$outcome) {
    row_i <- row_i + 1L
    row_list[[row_i]] <- data.table(
      row_type = "outcome_header",
      outcome = outcome_name,
      stratum = NA_character_,
      subgroup_text = outcome_name,
      group_value = NA_character_,
      group_label = NA_character_,
      tp_median = NA_real_,
      tp_ci_low = NA_real_,
      tp_ci_high = NA_real_,
      tp_ci_text = "",
      p_text = ""
    )

    for (spec in stratum_specs) {
      sub <- copy(plot_dt[outcome == outcome_name & as.character(stratum) == spec$stratum])
      include_stratum <- nrow(sub) > 0 && (
        sum(sub$n_group, na.rm = TRUE) > 0 ||
          spec$stratum %in% c("Sex", "Education")
      )
      if (!include_stratum) next
      setorder(sub, group_order)
      p_row <- p_lab[outcome == outcome_name & stratum == spec$stratum]
      p_text <- if (nrow(p_row) > 0) p_row$p_label[1] else paste0(spec$contrast, ", p=NA")

      row_i <- row_i + 1L
      row_list[[row_i]] <- data.table(
        row_type = "stratum_header",
        outcome = outcome_name,
        stratum = spec$stratum,
        subgroup_text = spec$stratum,
        group_value = NA_character_,
        group_label = NA_character_,
        tp_median = NA_real_,
        tp_ci_low = NA_real_,
        tp_ci_high = NA_real_,
        tp_ci_text = "",
        p_text = p_text
      )

      for (j in seq_len(nrow(sub))) {
        row_i <- row_i + 1L
        row_list[[row_i]] <- data.table(
          row_type = "group",
          outcome = outcome_name,
          stratum = as.character(sub$stratum[j]),
          subgroup_text = paste0("   ", sub$group_label[j]),
          group_value = as.character(sub$group_value[j]),
          group_label = as.character(sub$group_label[j]),
          tp_median = sub$tp_median[j],
          tp_ci_low = sub$tp_ci_low[j],
          tp_ci_high = sub$tp_ci_high[j],
          tp_ci_text = sub$tp_ci_text[j],
          p_text = ""
        )
      }
    }
  }

  rows <- rbindlist(row_list, fill = TRUE)
  rows[, y := rev(seq_len(.N))]
  rows[, stratum := factor(stratum, levels = c("Sex", "Education", "Income", "Rurality"))]
  header_y <- max(rows$y) + 1.1
  ci_rows <- rows[row_type == "group" & is.finite(tp_median) & is.finite(tp_ci_low) & is.finite(tp_ci_high)]
  group_rows <- rows[row_type == "group"]
  outcome_header_rows <- rows[row_type == "outcome_header"]
  stratum_header_rows <- rows[row_type == "stratum_header"]

  p <- ggplot(rows, aes(y = y)) +
    geom_rect(
      data = outcome_header_rows,
      aes(ymin = y - 0.45, ymax = y + 0.45),
      xmin = label_x,
      xmax = right_x,
      inherit.aes = FALSE,
      fill = country_color,
      alpha = 0.95
    ) +
    geom_rect(
      data = stratum_header_rows,
      aes(ymin = y - 0.45, ymax = y + 0.45),
      xmin = label_x,
      xmax = right_x,
      inherit.aes = FALSE,
      fill = "#f1f1f1",
      alpha = 1
    ) +
    geom_vline(xintercept = seq(60, 100, by = 10), color = "grey90", linewidth = 0.35) +
    geom_segment(
      data = ci_rows,
      aes(x = tp_ci_low, xend = tp_ci_high, yend = y, color = stratum),
      linewidth = 0.85,
      lineend = "round"
    ) +
    geom_segment(
      data = ci_rows,
      aes(x = tp_ci_low, xend = tp_ci_low, y = y - 0.13, yend = y + 0.13, color = stratum),
      linewidth = 0.85
    ) +
    geom_segment(
      data = ci_rows,
      aes(x = tp_ci_high, xend = tp_ci_high, y = y - 0.13, yend = y + 0.13, color = stratum),
      linewidth = 0.85
    ) +
    geom_point(
      data = ci_rows,
      aes(x = tp_median, fill = stratum),
      shape = 22,
      size = 3.1,
      color = "white",
      stroke = 0.35
    ) +
    geom_text(
      data = group_rows,
      aes(x = label_x, label = subgroup_text),
      hjust = 0,
      size = 3.35,
      color = "grey18"
    ) +
    geom_text(
      data = stratum_header_rows,
      aes(x = label_x, label = subgroup_text),
      hjust = 0,
      size = 3.45,
      fontface = "bold",
      color = "grey18"
    ) +
    geom_text(
      data = outcome_header_rows,
      aes(x = label_x, label = subgroup_text),
      hjust = 0,
      size = 3.55,
      fontface = "bold",
      color = "white"
    ) +
    geom_text(
      data = group_rows,
      aes(x = tp_text_x, label = tp_ci_text),
      hjust = 0.5,
      size = 3.15,
      color = "grey18"
    ) +
    geom_text(
      data = stratum_header_rows,
      aes(x = p_text_x, label = p_text),
      hjust = 0.5,
      size = 3.15,
      color = "grey18"
    ) +
    annotate("text", x = label_x, y = header_y, label = "Subgroup",
             hjust = 0, size = 3.75, fontface = "bold", color = "grey18") +
    annotate("text", x = mean(c(x_min, x_max)), y = header_y, label = "Forest Plot",
             hjust = 0.5, size = 3.75, fontface = "bold", color = "grey18") +
    annotate("text", x = tp_text_x, y = header_y, label = "TP (95% CI)",
             hjust = 0.5, size = 3.75, fontface = "bold", color = "grey18") +
    annotate("text", x = p_text_x, y = header_y, label = "Permutation P",
             hjust = 0.5, size = 3.75, fontface = "bold", color = "grey18") +
    scale_color_manual(values = stratum_palette, drop = FALSE) +
    scale_fill_manual(values = stratum_palette, drop = FALSE) +
    scale_x_continuous(
      breaks = seq(60, 100, by = 10),
      limits = c(label_x, right_x),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(limits = c(0.5, header_y + 0.6), expand = expansion(mult = c(0, 0))) +
    labs(
      title = paste0(country, " stratified turning points"),
      subtitle = "Squares are bootstrap medians; horizontal bars are bootstrap 95% CIs. P values test within-stratum TP differences.",
      x = "Turning point age",
      y = NULL
    ) +
    theme_minimal(base_family = "serif", base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = country_color),
      plot.subtitle = element_text(size = 10, color = "grey35"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(10, 15, 10, 15)
    )

  out_file <- file.path(fig_dir, paste0(country, ".stratified_tp_forest.pdf"))
  plot_height <- max(8, nrow(rows) * 0.34 + 1.5)
  ggsave(out_file, p, width = 15, height = plot_height, device = "pdf")
  invisible(out_file)
}

build_master_rows <- function() {
  row_list <- list()
  row_i <- 0L
  for (outcome_name in outcome_specs$outcome) {
    row_i <- row_i + 1L
    row_list[[row_i]] <- data.table(
      row_type = "outcome_header",
      outcome = outcome_name,
      stratum = NA_character_,
      group_value = NA_character_,
      subgroup_text = outcome_name
    )
    for (spec in stratum_specs) {
      row_i <- row_i + 1L
      row_list[[row_i]] <- data.table(
        row_type = "stratum_header",
        outcome = outcome_name,
        stratum = spec$stratum,
        group_value = NA_character_,
        subgroup_text = spec$stratum
      )
      for (group in spec$levels) {
        row_i <- row_i + 1L
        row_list[[row_i]] <- data.table(
          row_type = "group",
          outcome = outcome_name,
          stratum = spec$stratum,
          group_value = group,
          subgroup_text = paste0("   ", unname(spec$labels[group]))
        )
      }
    }
  }
  rows <- rbindlist(row_list, fill = TRUE)
  rows[, row_id := seq_len(.N)]
  rows[, y := rev(row_id)]
  rows
}

make_reviewer_table <- function(summary_dt, p_dt) {
  table_dt <- copy(summary_dt)
  table_dt <- merge(
    table_dt,
    p_dt[, .(
      country,
      outcome,
      stratum,
      contrast,
      observed_difference,
      permutation_p,
      permutation_status = status
    )],
    by = c("country", "outcome", "stratum"),
    all.x = TRUE
  )
  table_dt[, tp_median_95ci := fifelse(
    is.finite(tp_median) & is.finite(tp_ci_low) & is.finite(tp_ci_high),
    sprintf("%.1f (%.1f-%.1f)", tp_median, tp_ci_low, tp_ci_high),
    NA_character_
  )]
  table_dt[, permutation_p_text := format_p(permutation_p)]
  table_dt[, significance := format_star(permutation_p)]
  table_dt[, .(
    country,
    outcome,
    stratum,
    subgroup = group_label,
    group_value,
    n_group,
    n_age,
    tp_median,
    tp_ci_low,
    tp_ci_high,
    tp_median_95ci,
    valid_rate,
    contrast,
    observed_difference,
    permutation_p,
    permutation_p_text,
    significance,
    tp_status = status,
    permutation_status,
    income_source
  )]
}

plot_all_countries_forest <- function(summary_dt, p_dt, output_dir) {
  country_order <- c("CHARLS", "HRS", "ELSA", "SHARE", "KLoSA", "LASI", "MHAS", "CRELES")
  country_order <- country_order[country_order %in% unique(summary_dt$country)]
  country_order <- c(country_order, setdiff(sort(unique(summary_dt$country)), country_order))
  if (length(country_order) == 0) return(invisible(NULL))

  master_rows <- build_master_rows()
  countries_dt <- data.table(country = country_order, join_key = 1L)
  master_rows[, join_key := 1L]
  plot_grid <- merge(countries_dt, master_rows, by = "join_key", allow.cartesian = TRUE)
  plot_grid[, join_key := NULL]
  master_rows[, join_key := NULL]

  plot_data <- copy(summary_dt)
  plot_data[, stratum := as.character(stratum)]
  plot_data[, group_value := as.character(group_value)]
  plot_data <- merge(
    plot_grid,
    plot_data[, .(
      country,
      outcome,
      stratum,
      group_value,
      n_group,
      tp_median,
      tp_ci_low,
      tp_ci_high,
      valid_rate,
      status
    )],
    by = c("country", "outcome", "stratum", "group_value"),
    all.x = TRUE
  )

  p_stars <- copy(p_dt)
  p_stars[, sig_star := format_star(permutation_p)]
  p_stars <- p_stars[nzchar(sig_star), .(country, outcome, stratum, sig_star)]
  plot_data <- merge(
    plot_data,
    p_stars,
    by = c("country", "outcome", "stratum"),
    all.x = TRUE
  )
  plot_data[, star_text := fifelse(row_type == "stratum_header" & !is.na(sig_star), sig_star, "")]

  plot_data[, country := factor(country, levels = country_order)]
  plot_data[, stratum := factor(stratum, levels = c("Sex", "Education", "Income", "Rurality"))]

  finite_ci <- plot_data[is.finite(tp_ci_low) & is.finite(tp_ci_high)]
  x_min <- if (nrow(finite_ci) > 0) max(55, floor(min(finite_ci$tp_ci_low) - 2)) else 60
  x_max <- if (nrow(finite_ci) > 0) min(105, ceiling(max(finite_ci$tp_ci_high) + 2)) else 100
  if (!is.finite(x_min) || !is.finite(x_max) || x_min >= x_max) {
    x_min <- 60
    x_max <- 100
  }
  label_x <- x_min - 24
  panel_x_min <- x_min - 1.5
  star_x <- x_max + 1.8
  right_x <- x_max + 3.5
  header_y <- max(master_rows$y) + 1.2

  ci_rows <- plot_data[row_type == "group" & is.finite(tp_median) & is.finite(tp_ci_low) & is.finite(tp_ci_high)]
  left_label_rows <- unique(plot_data[country == country_order[1], .(row_type, y, subgroup_text)])
  left_label_rows[, country := factor(country_order[1], levels = country_order)]
  outcome_header_rows <- unique(plot_data[row_type == "outcome_header", .(country, y)])
  stratum_header_rows <- unique(plot_data[row_type == "stratum_header", .(country, y)])
  star_rows <- plot_data[row_type == "stratum_header" & nzchar(star_text)]
  subgroup_header <- data.table(
    country = factor(country_order[1], levels = country_order),
    x = label_x,
    y = header_y,
    label = "Subgroup"
  )
  forest_header <- data.table(
    country = factor(country_order, levels = country_order),
    x = mean(c(x_min, x_max)),
    y = header_y,
    label = "Forest Plot"
  )

  p <- ggplot(plot_data, aes(y = y)) +
    geom_rect(
      data = outcome_header_rows,
      aes(ymin = y - 0.45, ymax = y + 0.45, fill = country),
      xmin = panel_x_min,
      xmax = right_x,
      inherit.aes = FALSE,
      alpha = 0.96
    ) +
    geom_rect(
      data = stratum_header_rows,
      aes(ymin = y - 0.45, ymax = y + 0.45),
      xmin = panel_x_min,
      xmax = right_x,
      inherit.aes = FALSE,
      fill = "#f1f1f1",
      alpha = 1
    ) +
    geom_vline(xintercept = seq(60, 100, by = 10), color = "grey88", linewidth = 0.3) +
    geom_segment(
      data = ci_rows,
      aes(x = tp_ci_low, xend = tp_ci_high, yend = y, color = stratum),
      linewidth = 1.1,
      lineend = "round"
    ) +
    geom_segment(
      data = ci_rows,
      aes(x = tp_ci_low, xend = tp_ci_low, y = y - 0.10, yend = y + 0.10, color = stratum),
      linewidth = 1.1
    ) +
    geom_segment(
      data = ci_rows,
      aes(x = tp_ci_high, xend = tp_ci_high, y = y - 0.10, yend = y + 0.10, color = stratum),
      linewidth = 1.1
    ) +
    geom_point(
      data = ci_rows,
      aes(x = tp_median, fill = stratum),
      shape = 22,
      size = 3.1,
      color = "white",
      stroke = 0.4
    ) +
    geom_text(
      data = left_label_rows[row_type == "group"],
      aes(x = label_x, y = y, label = subgroup_text),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4.0,
      color = "grey18"
    ) +
    geom_text(
      data = left_label_rows[row_type == "stratum_header"],
      aes(x = label_x, y = y, label = subgroup_text),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4.2,
      fontface = "bold",
      color = "grey18"
    ) +
    geom_text(
      data = left_label_rows[row_type == "outcome_header"],
      aes(x = label_x, y = y, label = subgroup_text),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4.3,
      fontface = "bold",
      color = "white"
    ) +
    geom_text(
      data = star_rows,
      aes(x = star_x, label = star_text),
      hjust = 0.5,
      size = 6.2,
      fontface = "bold",
      color = "#1f4f9a"
    ) +
    geom_text(
      data = subgroup_header,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      size = 4.4,
      fontface = "bold",
      color = "grey18"
    ) +
    geom_text(
      data = forest_header,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0.5,
      size = 4.4,
      fontface = "bold",
      color = "grey18"
    ) +
    facet_wrap(~country, nrow = 1, scales = "free_x") +
    scale_color_manual(values = stratum_palette, drop = FALSE) +
    scale_fill_manual(values = c(country_palette, stratum_palette), drop = FALSE) +
    scale_x_continuous(
      breaks = seq(60, 100, by = 10),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(limits = c(0.5, header_y + 0.6), expand = expansion(mult = c(0, 0))) +
    labs(
      title = "Stratified ADL/IADL turning points across countries",
      subtitle = "Squares are bootstrap medians; horizontal bars are bootstrap 95% CIs. Stars mark significant within-country permutation tests: * p<0.05, ** p<0.01, *** p<0.001.",
      x = "Turning point age",
      y = NULL
    ) +
    theme_minimal(base_family = "serif", base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(size = 9, color = "grey35"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 14),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 13),
      strip.background = element_blank(),
      legend.position = "none",
      panel.spacing.x = unit(0.18, "lines"),
      plot.margin = margin(10, 10, 10, 10)
    )

  out_file <- file.path(output_dir, "all_countries_stratified_tp_forest.pdf")
  plot_width <- max(18, length(country_order) * 2.25 + 2.8)
  plot_height <- max(9, nrow(master_rows) * 0.22 + 1.7)
  ggsave(out_file, p, width = plot_width, height = plot_height, device = "pdf", limitsize = FALSE)
  invisible(out_file)
}

all_summary <- list()
all_perm <- list()
all_availability <- list()
all_income_audit <- list()
all_income_trend <- list()

message("Input directory: ", normalizePath(input_dir, mustWork = FALSE))
message("Output directory: ", normalizePath(output_dir, mustWork = FALSE))
message("n_boot=", n_boot, ", sample_cap=", sample_cap, ", n_perm=", n_perm,
        ", min_group_n=", min_group_n, ", perm_sample_cap=", perm_sample_cap)

for (path in sort(country_files)) {
  country <- country_from_file(path)
  message("\nProcessing ", country, " ...")
  x <- read_country(path, tolower(country))
  income_choice <- choose_income_source(x, country)
  x <- add_income_quintiles(x, income_choice$source)

  all_income_audit[[country]] <- income_choice$audit
  all_availability[[country]] <- availability_table(x, country, income_choice$source)

  country_summary <- rbindlist(lapply(stratum_specs, function(spec) {
    estimate_stratum_tps(x, country, spec, income_choice$source)
  }), fill = TRUE)

  country_perm <- rbindlist(lapply(stratum_specs, function(spec) {
    permutation_test_stratum(x, country, spec)
  }), fill = TRUE)

  country_income_trend <- estimate_income_quintile_trend(x, country, income_choice$source)

  all_summary[[country]] <- country_summary
  all_perm[[country]] <- country_perm
  all_income_trend[[country]] <- country_income_trend

  if (!is.null(country_summary) && nrow(country_summary) > 0) {
    plot_country_forest(country_summary, country_perm, country)
  }

  message("Finished ", country, ".")
}

summary_dt <- rbindlist(all_summary, fill = TRUE)
perm_dt <- rbindlist(all_perm, fill = TRUE)
availability_dt <- rbindlist(all_availability, fill = TRUE)
income_audit_dt <- rbindlist(all_income_audit, fill = TRUE)
income_trend_dt <- rbindlist(all_income_trend, fill = TRUE)

setorder(summary_dt, country, outcome, stratum, group_value)
setorder(perm_dt, country, outcome, stratum)
setorder(availability_dt, country)
setorder(income_audit_dt, country, -selected, income_source)
if (nrow(income_trend_dt) > 0) setorder(income_trend_dt, country, outcome, income_quintile)

reviewer_table_dt <- make_reviewer_table(summary_dt, perm_dt)
setorder(reviewer_table_dt, country, outcome, stratum, group_value)

fwrite(summary_dt, file.path(output_dir, "stratified_tp_summary.csv"))
fwrite(perm_dt, file.path(output_dir, "stratified_permutation_pvalues.csv"))
fwrite(availability_dt, file.path(output_dir, "stratified_variable_availability.csv"))
fwrite(income_audit_dt, file.path(output_dir, "income_source_selection.csv"))
fwrite(income_trend_dt, file.path(output_dir, "income_quintile_trend_tp_summary.csv"))
fwrite(reviewer_table_dt, file.path(output_dir, "reviewer_stratified_tp_table.csv"))

plot_all_countries_forest(summary_dt, perm_dt, output_dir)

message("\nSaved outputs:")
message("  ", file.path(output_dir, "stratified_tp_summary.csv"))
message("  ", file.path(output_dir, "stratified_permutation_pvalues.csv"))
message("  ", file.path(output_dir, "stratified_variable_availability.csv"))
message("  ", file.path(output_dir, "income_source_selection.csv"))
message("  ", file.path(output_dir, "income_quintile_trend_tp_summary.csv"))
message("  ", file.path(output_dir, "reviewer_stratified_tp_table.csv"))
message("  ", file.path(output_dir, "all_countries_stratified_tp_forest.pdf"))
message("  ", fig_dir)
