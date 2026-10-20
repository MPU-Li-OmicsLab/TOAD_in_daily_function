#!/usr/bin/env Rscript

# Country-level explanatory attribution analyses for ADL/IADL turning points.
#
# This is intentionally hypothesis-generating: SHARE has been excluded by the
# prior data-preparation plan, so the primary country-level models use 7 points.
#
# Usage:
#   Rscript 05_country_context_attribution_analysis.R country_context_attribution_outputs country_context_attribution_outputs

suppressPackageStartupMessages({
  required_pkgs <- c("data.table", "ggplot2", "metafor")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
         ". Please install them before running this script.")
  }
  library(data.table)
  library(ggplot2)
  library(metafor)
})

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "country_context_attribution_outputs"
output_dir <- if (length(args) >= 2) args[[2]] else "country_context_attribution_outputs"
fig_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

merged_long_path <- file.path(input_dir, "tp_context_merged_long.csv")
context_path <- file.path(input_dir, "country_context_clean.csv")
if (!file.exists(merged_long_path)) stop("Cannot find: ", merged_long_path)
if (!file.exists(context_path)) stop("Cannot find: ", context_path)

dt <- fread(merged_long_path)
context_dt <- fread(context_path)

country_order <- c("CHARLS", "CRELES", "ELSA", "HRS", "KLoSA", "LASI", "MHAS", "SHARE")
country_palette <- c(
  CHARLS = "#6d256b",
  CRELES = "#d8258d",
  ELSA = "#ff95a8",
  HRS = "#f58625",
  KLoSA = "#0092be",
  LASI = "#037f41",
  MHAS = "#0058a3",
  SHARE = "#999999"
)
outcome_labels <- c(
  ADL_TP = "ADL TP",
  IADL_TP = "IADL TP",
  ADL_minus_IADL_TP = "ADL - IADL TP"
)

zscore <- function(x) {
  if (sum(is.finite(x)) < 2) return(rep(NA_real_, length(x)))
  as.numeric(scale(x))
}

scalar_num <- function(x) {
  if (length(x) == 0 || is.null(x)) return(NA_real_)
  as.numeric(x[[1]])
}

format_p_short <- function(p) {
  ifelse(
    !is.finite(p), "p=NA",
    ifelse(p < 0.001, "p<0.001", paste0("p=", sprintf("%.3f", p)))
  )
}

dt[, z_gdp_ppp_pc_raw := zscore(gdp_ppp_pc)]
dt[, z_health_exp_pc_ppp_raw := zscore(health_exp_pc_ppp)]
context_dt[, z_gdp_ppp_pc_raw := zscore(gdp_ppp_pc)]
context_dt[, z_health_exp_pc_ppp_raw := zscore(health_exp_pc_ppp)]

se_floor <- dt[
  is.finite(tp_se_from_ci) & tp_se_from_ci > 0,
  .(tp_se_floor = median(tp_se_from_ci, na.rm = TRUE)),
  by = outcome
]
dt <- merge(dt, se_floor, by = "outcome", all.x = TRUE)
dt[, tp_se_analysis := fifelse(
  is.finite(tp_se_from_ci) & tp_se_from_ci > 0,
  tp_se_from_ci,
  tp_se_floor
)]
dt[, tp_se_analysis_source := fifelse(
  is.finite(tp_se_from_ci) & tp_se_from_ci > 0,
  "ci_width",
  "outcome_median_positive_ci_se_floor"
)]
dt[, floor_tp_flag := outcome %in% c("ADL_TP", "IADL_TP") & is.finite(tp_median) & tp_median <= 60.2]
dt[, outcome_label := outcome_labels[outcome]]
dt[, database := factor(database, levels = country_order)]

analysis_qc <- dt[, .(
  n_countries = uniqueN(database),
  n_floor_tp = sum(floor_tp_flag, na.rm = TRUE),
  n_zero_or_missing_se = sum(tp_se_analysis_source != "ci_width", na.rm = TRUE),
  se_floor = unique(tp_se_floor)[1],
  included_countries = paste(as.character(database), collapse = "; ")
), by = outcome]
fwrite(analysis_qc, file.path(output_dir, "country_context_analysis_qc.csv"))

primary_predictors <- data.table(
  predictor_id = c(
    "log_gdp_ppp_pc",
    "log_health_exp_pc_ppp",
    "health_exp_gdp_pct",
    "hale_60",
    "hofstede_individualism"
  ),
  predictor_z = c(
    "z_log_gdp_ppp_pc",
    "z_log_health_exp_pc_ppp",
    "z_health_exp_gdp_pct",
    "z_hale_60",
    "z_hofstede_individualism"
  ),
  predictor_raw = c(
    "gdp_ppp_pc",
    "health_exp_pc_ppp",
    "health_exp_gdp_pct",
    "hale_60",
    "hofstede_individualism"
  ),
  predictor_label = c(
    "GDP per capita PPP (log)",
    "Health expenditure per capita PPP (log)",
    "Health expenditure (% GDP)",
    "HALE at age 60",
    "Hofstede individualism"
  ),
  model_scale = "primary_z"
)

raw_scale_predictors <- data.table(
  predictor_id = c("gdp_ppp_pc_raw", "health_exp_pc_ppp_raw"),
  predictor_z = c("z_gdp_ppp_pc_raw", "z_health_exp_pc_ppp_raw"),
  predictor_raw = c("gdp_ppp_pc", "health_exp_pc_ppp"),
  predictor_label = c(
    "GDP per capita PPP (raw)",
    "Health expenditure per capita PPP (raw)"
  ),
  model_scale = "sensitivity_raw_z"
)

predictor_sets <- rbind(primary_predictors, raw_scale_predictors, fill = TRUE)

get_model_df <- function(data, outcome_name, predictor_z_col, exclude_floor = FALSE) {
  d <- copy(data[outcome == outcome_name])
  if (exclude_floor) d <- d[floor_tp_flag == FALSE]
  d <- d[
    is.finite(tp_median) &
      is.finite(tp_ci_low) &
      is.finite(tp_ci_high) &
      is.finite(tp_se_analysis) &
      tp_se_analysis > 0 &
      is.finite(get(predictor_z_col))
  ]
  d[, x := get(predictor_z_col)]
  d[]
}

fit_meta_regression <- function(data, outcome_name, predictor_z_col, predictor_id,
                                predictor_label, model_scale, analysis_set = "primary",
                                excluded_country = NA_character_) {
  d <- get_model_df(data, outcome_name, predictor_z_col, exclude_floor = analysis_set == "exclude_floor_tp")
  if (!is.na(excluded_country)) d <- d[as.character(database) != excluded_country]
  n <- nrow(d)
  if (n < 4 || uniqueN(d$x) < 2) {
    return(data.table(
      analysis_set = analysis_set,
      model_type = "ivw_random_effects_meta_regression",
      outcome = outcome_name,
      outcome_label = outcome_labels[outcome_name],
      predictor_id = predictor_id,
      predictor_label = predictor_label,
      model_scale = model_scale,
      excluded_country = excluded_country,
      n_countries = n,
      beta = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      tau2 = NA_real_,
      I2 = NA_real_,
      QM = NA_real_,
      QMp = NA_real_,
      status = "insufficient_complete_countries"
    ))
  }

  fit <- tryCatch({
    rma.uni(
      yi = tp_median,
      sei = tp_se_analysis,
      mods = ~ x,
      data = d,
      method = "REML",
      test = "knha"
    )
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    return(data.table(
      analysis_set = analysis_set,
      model_type = "ivw_random_effects_meta_regression",
      outcome = outcome_name,
      outcome_label = outcome_labels[outcome_name],
      predictor_id = predictor_id,
      predictor_label = predictor_label,
      model_scale = model_scale,
      excluded_country = excluded_country,
      n_countries = n,
      beta = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      tau2 = NA_real_,
      I2 = NA_real_,
      QM = NA_real_,
      QMp = NA_real_,
      status = paste0("model_error: ", conditionMessage(fit))
    ))
  }

  sm <- coef(summary(fit))
  slope <- sm["x", ]
  data.table(
    analysis_set = analysis_set,
    model_type = "ivw_random_effects_meta_regression",
    outcome = outcome_name,
    outcome_label = outcome_labels[outcome_name],
    predictor_id = predictor_id,
    predictor_label = predictor_label,
    model_scale = model_scale,
    excluded_country = excluded_country,
    n_countries = n,
    beta = scalar_num(slope[["estimate"]]),
    se = scalar_num(slope[["se"]]),
    ci_low = scalar_num(slope[["ci.lb"]]),
    ci_high = scalar_num(slope[["ci.ub"]]),
    p_value = scalar_num(slope[["pval"]]),
    tau2 = scalar_num(fit$tau2),
    I2 = scalar_num(fit$I2),
    QM = scalar_num(fit$QM),
    QMp = scalar_num(fit$QMp),
    status = "ok"
  )
}

fit_unweighted_ols <- function(data, outcome_name, predictor_z_col, predictor_id,
                               predictor_label, model_scale, analysis_set = "primary") {
  d <- get_model_df(data, outcome_name, predictor_z_col, exclude_floor = analysis_set == "exclude_floor_tp")
  n <- nrow(d)
  if (n < 4 || uniqueN(d$x) < 2) {
    return(data.table(
      analysis_set = analysis_set,
      model_type = "unweighted_ols",
      outcome = outcome_name,
      outcome_label = outcome_labels[outcome_name],
      predictor_id = predictor_id,
      predictor_label = predictor_label,
      model_scale = model_scale,
      n_countries = n,
      beta = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      status = "insufficient_complete_countries"
    ))
  }
  fit <- tryCatch(lm(tp_median ~ x, data = d), error = function(e) e)
  if (inherits(fit, "error")) {
    return(data.table(
      analysis_set = analysis_set,
      model_type = "unweighted_ols",
      outcome = outcome_name,
      outcome_label = outcome_labels[outcome_name],
      predictor_id = predictor_id,
      predictor_label = predictor_label,
      model_scale = model_scale,
      n_countries = n,
      beta = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      status = paste0("model_error: ", conditionMessage(fit))
    ))
  }
  sm <- summary(fit)
  cf <- coef(sm)
  ci <- suppressMessages(confint(fit))
  data.table(
    analysis_set = analysis_set,
    model_type = "unweighted_ols",
    outcome = outcome_name,
    outcome_label = outcome_labels[outcome_name],
    predictor_id = predictor_id,
    predictor_label = predictor_label,
    model_scale = model_scale,
    n_countries = n,
    beta = unname(cf["x", "Estimate"]),
    se = unname(cf["x", "Std. Error"]),
    ci_low = unname(ci["x", 1]),
    ci_high = unname(ci["x", 2]),
    p_value = unname(cf["x", "Pr(>|t|)"]),
    r_squared = sm$r.squared,
    status = "ok"
  )
}

outcomes <- c("IADL_TP", "ADL_TP", "ADL_minus_IADL_TP")

spearman_results <- rbindlist(lapply(outcomes, function(o) {
  rbindlist(lapply(seq_len(nrow(primary_predictors)), function(i) {
    p <- primary_predictors[i]
    d <- get_model_df(dt, o, p$predictor_z, exclude_floor = FALSE)
    if (nrow(d) < 4 || uniqueN(d$x) < 2) {
      return(data.table(
        outcome = o,
        outcome_label = outcome_labels[o],
        predictor_id = p$predictor_id,
        predictor_label = p$predictor_label,
        n_countries = nrow(d),
        spearman_rho = NA_real_,
        p_value = NA_real_,
        status = "insufficient_complete_countries"
      ))
    }
    ct <- suppressWarnings(cor.test(d$x, d$tp_median, method = "spearman", exact = FALSE))
    data.table(
      outcome = o,
      outcome_label = outcome_labels[o],
      predictor_id = p$predictor_id,
      predictor_label = p$predictor_label,
      n_countries = nrow(d),
      spearman_rho = unname(ct$estimate),
      p_value = ct$p.value,
      status = "ok"
    )
  }))
}), fill = TRUE)

primary_meta_results <- rbindlist(lapply(outcomes, function(o) {
  rbindlist(lapply(seq_len(nrow(primary_predictors)), function(i) {
    p <- primary_predictors[i]
    fit_meta_regression(dt, o, p$predictor_z, p$predictor_id, p$predictor_label, p$model_scale, "primary")
  }))
}), fill = TRUE)

ols_results <- rbindlist(lapply(outcomes, function(o) {
  rbindlist(lapply(seq_len(nrow(primary_predictors)), function(i) {
    p <- primary_predictors[i]
    fit_unweighted_ols(dt, o, p$predictor_z, p$predictor_id, p$predictor_label, p$model_scale, "primary")
  }))
}), fill = TRUE)

raw_scale_meta_results <- rbindlist(lapply(outcomes, function(o) {
  rbindlist(lapply(seq_len(nrow(raw_scale_predictors)), function(i) {
    p <- raw_scale_predictors[i]
    fit_meta_regression(dt, o, p$predictor_z, p$predictor_id, p$predictor_label, p$model_scale, "sensitivity_raw_z")
  }))
}), fill = TRUE)

exclude_floor_meta_results <- rbindlist(lapply(outcomes, function(o) {
  rbindlist(lapply(seq_len(nrow(primary_predictors)), function(i) {
    p <- primary_predictors[i]
    fit_meta_regression(dt, o, p$predictor_z, p$predictor_id, p$predictor_label, p$model_scale, "exclude_floor_tp")
  }))
}), fill = TRUE)

leave_one_out_results <- rbindlist(lapply(outcomes, function(o) {
  rbindlist(lapply(seq_len(nrow(primary_predictors)), function(i) {
    p <- primary_predictors[i]
    d <- get_model_df(dt, o, p$predictor_z)
    countries <- as.character(d$database)
    rbindlist(c(
      list(fit_meta_regression(dt, o, p$predictor_z, p$predictor_id, p$predictor_label, p$model_scale, "primary", NA_character_)),
      lapply(countries, function(cc) {
        fit_meta_regression(dt, o, p$predictor_z, p$predictor_id, p$predictor_label, p$model_scale, "leave_one_country_out", cc)
      })
    ), fill = TRUE)
  }))
}), fill = TRUE)

fwrite(spearman_results, file.path(output_dir, "spearman_correlation_results.csv"))
fwrite(primary_meta_results, file.path(output_dir, "meta_regression_results.csv"))
fwrite(ols_results, file.path(output_dir, "unweighted_ols_sensitivity_results.csv"))
fwrite(raw_scale_meta_results, file.path(output_dir, "sensitivity_raw_scale_meta_regression_results.csv"))
fwrite(exclude_floor_meta_results, file.path(output_dir, "sensitivity_exclude_floor_tp_meta_regression_results.csv"))
fwrite(leave_one_out_results, file.path(output_dir, "leave_one_country_out_results.csv"))

# ----------------------------
# Figure 1: Country-level contextual indicators.
# ----------------------------
indicator_plot_dt <- rbindlist(list(
  context_dt[, .(database, country, indicator = "GDP per capita PPP", value = gdp_ppp_pc)],
  context_dt[, .(database, country, indicator = "Health expenditure per capita PPP", value = health_exp_pc_ppp)],
  context_dt[, .(database, country, indicator = "Health expenditure (% GDP)", value = health_exp_gdp_pct)],
  context_dt[, .(database, country, indicator = "HALE at age 60", value = hale_60)],
  context_dt[, .(database, country, indicator = "Hofstede individualism", value = hofstede_individualism)]
), fill = TRUE)
indicator_plot_dt[, database := factor(database, levels = rev(country_order))]
indicator_plot_dt[, indicator := factor(
  indicator,
  levels = c(
    "GDP per capita PPP",
    "Health expenditure per capita PPP",
    "Health expenditure (% GDP)",
    "HALE at age 60",
    "Hofstede individualism"
  )
)]

p_indicator <- ggplot(indicator_plot_dt, aes(x = value, y = database, fill = database)) +
  geom_col(width = 0.72, alpha = 0.9) +
  facet_wrap(~indicator, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = country_palette, drop = FALSE) +
  labs(
    title = "Country-level contextual indicators",
    subtitle = "Values are matched to each cohort's survey year; Hofstede individualism is a static cultural index.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold")
  )
ggsave(file.path(fig_dir, "fig_country_context_indicator_bars.pdf"),
       p_indicator, width = 11, height = 8, device = "pdf")

# ----------------------------
# Figure 2: Scatter plots for ADL/IADL TP vs contextual predictors.
# ----------------------------
scatter_dt <- rbindlist(lapply(seq_len(nrow(primary_predictors)), function(i) {
  p <- primary_predictors[i]
  tmp <- copy(dt[outcome %in% c("IADL_TP", "ADL_TP")])
  tmp[, predictor_value := get(p$predictor_z)]
  tmp[, predictor_label := p$predictor_label]
  tmp[, predictor_id := p$predictor_id]
  tmp
}), fill = TRUE)
scatter_dt <- scatter_dt[is.finite(predictor_value) & is.finite(tp_median)]
scatter_dt[, predictor_label := factor(scatter_dt$predictor_label, levels = primary_predictors$predictor_label)]
scatter_dt[, outcome_label := factor(outcome_label, levels = c("IADL TP", "ADL TP"))]

spearman_annot <- copy(spearman_results[outcome %in% c("IADL_TP", "ADL_TP") & status == "ok"])
spearman_annot[, outcome_label := factor(outcome_label, levels = c("IADL TP", "ADL TP"))]
spearman_annot[, predictor_label := factor(predictor_label, levels = primary_predictors$predictor_label)]
spearman_annot[, label := paste0(
  "Spearman rho=", sprintf("%.2f", spearman_rho),
  "\n", format_p_short(p_value)
)]

p_scatter <- ggplot(scatter_dt, aes(x = predictor_value, y = tp_median, color = database)) +
  geom_errorbar(aes(ymin = tp_ci_low, ymax = tp_ci_high), width = 0, alpha = 0.75, linewidth = 0.55) +
  geom_point(size = 2.8, alpha = 0.95) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "grey35", linewidth = 0.45, linetype = "dashed") +
  geom_text(aes(label = database), vjust = -0.85, size = 2.6, show.legend = FALSE) +
  geom_label(
    data = spearman_annot,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.04,
    vjust = 1.08,
    size = 2.5,
    lineheight = 0.92,
    linewidth = 0,
    fill = "white",
    alpha = 0.82
  ) +
  facet_grid(outcome_label ~ predictor_label, scales = "free_x") +
  scale_color_manual(values = country_palette, drop = TRUE) +
  labs(
    title = "Country-level contextual indicators and ADL/IADL turning points",
    subtitle = "Predictors are z-standardized; vertical bars show bootstrap 95% CIs for TP. Panel labels report Spearman correlations.",
    x = "Country-level predictor (z-score)",
    y = "Turning point age (years)",
    color = "Cohort"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )
ggsave(file.path(fig_dir, "fig_context_scatter_adl_iadl.pdf"),
       p_scatter, width = 16, height = 7.8, device = "pdf")

# ----------------------------
# Figure 3: Meta-regression coefficient plot.
# ----------------------------
coef_dt <- primary_meta_results[
  outcome %in% c("IADL_TP", "ADL_TP") &
    status == "ok" &
    is.finite(beta) &
    is.finite(ci_low) &
    is.finite(ci_high)
]
coef_dt[, predictor_label := factor(predictor_label, levels = rev(primary_predictors$predictor_label))]
coef_dt[, outcome_label := factor(outcome_label, levels = c("IADL TP", "ADL TP"))]

p_coef <- ggplot(coef_dt, aes(x = beta, y = predictor_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    orientation = "y",
    width = 0.2,
    linewidth = 0.8,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 2.8, position = position_dodge(width = 0.55)) +
  scale_color_manual(values = c("IADL TP" = "#d95f02", "ADL TP" = "#2d6a9f"), drop = FALSE) +
  labs(
    title = "Single-predictor meta-regression coefficients",
    subtitle = "Primary analysis only; estimates are years of TP shift per 1-SD higher predictor.",
    x = "Change in turning point age (years per 1 SD)",
    y = NULL,
    color = "Outcome"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
ggsave(file.path(fig_dir, "fig_meta_regression_coefficients.pdf"),
       p_coef, width = 11.5, height = 5.8, device = "pdf")

# ----------------------------
# Figure 3b: IADL floor-effect sensitivity coefficient plot.
# ----------------------------
iadl_floor_dt <- rbind(
  primary_meta_results,
  exclude_floor_meta_results,
  fill = TRUE
)[
  outcome == "IADL_TP" &
    status == "ok" &
    is.finite(beta) &
    is.finite(ci_low) &
    is.finite(ci_high)
]
iadl_floor_dt[, analysis_label := fifelse(
  analysis_set == "primary",
  "Primary",
  "Exclude floor TP"
)]
iadl_floor_dt[, analysis_label := factor(analysis_label, levels = c("Primary", "Exclude floor TP"))]
iadl_floor_dt[, predictor_label := factor(predictor_label, levels = rev(primary_predictors$predictor_label))]

p_iadl_floor <- ggplot(iadl_floor_dt, aes(x = beta, y = predictor_label, color = analysis_label, shape = analysis_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    orientation = "y",
    width = 0.2,
    linewidth = 0.8,
    position = position_dodge(width = 0.58)
  ) +
  geom_point(size = 2.8, position = position_dodge(width = 0.58)) +
  scale_color_manual(values = c("Primary" = "#2d6a9f", "Exclude floor TP" = "#d95f02"), drop = FALSE) +
  scale_shape_manual(values = c("Primary" = 16, "Exclude floor TP" = 17), drop = FALSE) +
  labs(
    title = "IADL TP floor-effect sensitivity",
    subtitle = "Primary vs exclude-floor estimates; effects are years of IADL TP shift per 1-SD higher predictor.",
    x = "Change in IADL turning point age (years per 1 SD)",
    y = NULL,
    color = "Analysis",
    shape = "Analysis"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
ggsave(file.path(fig_dir, "fig_iadl_floor_effect_sensitivity_coefficients.pdf"),
       p_iadl_floor, width = 10, height = 5.8, device = "pdf")

# ----------------------------
# Figure 4: Leave-one-country-out influence plot.
# ----------------------------
loo_plot_dt <- leave_one_out_results[
  outcome %in% c("IADL_TP", "ADL_TP") &
    status == "ok" &
    is.finite(beta) &
    is.finite(ci_low) &
    is.finite(ci_high)
]
loo_plot_dt[, excluded_label := fifelse(is.na(excluded_country), "Full model", paste0("Exclude ", excluded_country))]
loo_plot_dt[, excluded_label := factor(excluded_label, levels = rev(c("Full model", paste0("Exclude ", country_order))))]
loo_plot_dt[, predictor_label := factor(predictor_label, levels = primary_predictors$predictor_label)]
loo_plot_dt[, outcome_label := factor(outcome_label, levels = c("IADL TP", "ADL TP"))]

p_loo <- ggplot(loo_plot_dt, aes(x = beta, y = excluded_label, color = outcome_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.16, linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_grid(outcome_label ~ predictor_label, scales = "free_x") +
  labs(
    title = "Leave-one-country-out sensitivity of meta-regression slopes",
    subtitle = "Each point is the single-predictor slope after excluding one country; full model shown for reference.",
    x = "Change in turning point age (years per 1 SD)",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank()
  )
ggsave(file.path(fig_dir, "fig_leave_one_out.pdf"),
       p_loo, width = 16, height = 7.8, device = "pdf")

message("\nSaved outputs:")
message("  ", file.path(output_dir, "spearman_correlation_results.csv"))
message("  ", file.path(output_dir, "meta_regression_results.csv"))
message("  ", file.path(output_dir, "unweighted_ols_sensitivity_results.csv"))
message("  ", file.path(output_dir, "sensitivity_raw_scale_meta_regression_results.csv"))
message("  ", file.path(output_dir, "sensitivity_exclude_floor_tp_meta_regression_results.csv"))
message("  ", file.path(output_dir, "leave_one_country_out_results.csv"))
message("  ", fig_dir)
