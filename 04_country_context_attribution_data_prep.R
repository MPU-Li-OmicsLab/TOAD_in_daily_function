#!/usr/bin/env Rscript

# Download and prepare country-level contextual indicators for explanatory
# attribution analyses of country-specific ADL/IADL turning points.
#
# This script covers data collection and QC only:
#   1) download raw external data from official sources;
#   2) match countries and observation years;
#   3) generate clean country-context files;
#   4) merge with country-level TP outcomes from the previous analysis.
#
# Usage:
#   Rscript 04_country_context_attribution_data_prep.R tp_meta_outputs country_context_inputs country_context_attribution_outputs

suppressPackageStartupMessages({
  required_pkgs <- c("data.table", "jsonlite")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
         ". Please install them before running this script.")
  }
  library(data.table)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
tp_dir <- if (length(args) >= 1) args[[1]] else "tp_meta_outputs"
input_dir <- if (length(args) >= 2) args[[2]] else "country_context_inputs"
output_dir <- if (length(args) >= 3) args[[3]] else "country_context_attribution_outputs"

raw_dir <- file.path(input_dir, "raw")
clean_dir <- file.path(input_dir, "clean")
country_dir <- file.path(output_dir, "by_country")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)

download_with_record <- function(url, destfile, source_name) {
  message("Downloading ", source_name, ": ", url)
  ok <- tryCatch({
    utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    warning("Download failed for ", source_name, ": ", conditionMessage(e))
    FALSE
  })
  if (!ok || !file.exists(destfile) || file.info(destfile)$size == 0) {
    stop("Could not download required source: ", source_name,
         ". URL: ", url)
  }
  destfile
}

country_meta <- data.table(
  database = c("CHARLS", "HRS", "SHARE", "ELSA", "KLoSA", "LASI", "MHAS", "CRELES"),
  cohort_country = c("China", "United States", "Europe multi-country", "United Kingdom",
                     "South Korea", "India", "Mexico", "Costa Rica"),
  iso3 = c("CHN", "USA", NA_character_, "GBR", "KOR", "IND", "MEX", "CRI"),
  wave = c(4L, 14L, 8L, 9L, 7L, 2L, 4L, 5L),
  survey_year = c(2018L, 2018L, 2017L, 2018L, 2018L, 2020L, 2015L, 2015L),
  include_context_analysis = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
  exclusion_reason = c(NA, NA, "SHARE is a multi-country European cohort; excluded by analysis plan.",
                       NA, NA, NA, NA, NA)
)
country_meta <- country_meta[order(match(database, c("CHARLS", "HRS", "ELSA", "KLoSA", "LASI", "MHAS", "CRELES", "SHARE")))]

fwrite(country_meta, file.path(clean_dir, "country_wave_year_mapping.csv"))

analysis_countries <- country_meta[include_context_analysis == TRUE]
iso_list <- paste(analysis_countries$iso3, collapse = ";")

source_catalog <- data.table(
  source_name = c(
    "World Bank WDI GDP per capita PPP",
    "World Bank WDI current health expenditure per capita PPP",
    "World Bank WDI current health expenditure % GDP",
    "WHO GHO HALE at age 60",
    "The Culture Factor / Hofstede country comparison tool"
  ),
  indicator = c(
    "NY.GDP.PCAP.PP.CD",
    "SH.XPD.CHEX.PP.CD",
    "SH.XPD.CHEX.GD.ZS",
    "WHOSIS_000007",
    "individualism"
  ),
  url = c(
    paste0("https://api.worldbank.org/v2/country/", iso_list,
           "/indicator/NY.GDP.PCAP.PP.CD?format=json&per_page=20000"),
    paste0("https://api.worldbank.org/v2/country/", iso_list,
           "/indicator/SH.XPD.CHEX.PP.CD?format=json&per_page=20000"),
    paste0("https://api.worldbank.org/v2/country/", iso_list,
           "/indicator/SH.XPD.CHEX.GD.ZS?format=json&per_page=20000"),
    "https://ghoapi.azureedge.net/api/WHOSIS_000007",
    "https://www.theculturefactor.com/country-comparison-tool"
  ),
  source_note = c(
    "World Bank API, WDI indicator: GDP per capita, PPP (current international $).",
    "World Bank API, WDI indicator: Current health expenditure per capita, PPP (current international $).",
    "World Bank API, WDI indicator: Current health expenditure (% of GDP).",
    "WHO Global Health Observatory OData API, indicator WHOSIS_000007: Healthy life expectancy (HALE) at age 60, both sexes.",
    "The Culture Factor official country comparison tool; Hofstede 6-D model values parsed from downloaded HTML."
  )
)
fwrite(source_catalog, file.path(clean_dir, "country_context_source_catalog.csv"))

world_bank_files <- data.table(
  context_var = c("gdp_ppp_pc", "health_exp_pc_ppp", "health_exp_gdp_pct"),
  indicator = c("NY.GDP.PCAP.PP.CD", "SH.XPD.CHEX.PP.CD", "SH.XPD.CHEX.GD.ZS"),
  raw_file = file.path(raw_dir, c(
    "world_bank_NY.GDP.PCAP.PP.CD.json",
    "world_bank_SH.XPD.CHEX.PP.CD.json",
    "world_bank_SH.XPD.CHEX.GD.ZS.json"
  ))
)

for (i in seq_len(nrow(world_bank_files))) {
  url <- source_catalog[indicator == world_bank_files$indicator[i], url]
  download_with_record(url, world_bank_files$raw_file[i], world_bank_files$indicator[i])
}

who_raw_file <- file.path(raw_dir, "who_gho_WHOSIS_000007.json")
download_with_record(
  source_catalog[indicator == "WHOSIS_000007", url],
  who_raw_file,
  "WHO GHO WHOSIS_000007"
)

hofstede_raw_file <- file.path(raw_dir, "theculturefactor_country_comparison_tool.html")
download_with_record(
  source_catalog[indicator == "individualism", url],
  hofstede_raw_file,
  "The Culture Factor country comparison tool"
)

parse_world_bank_json <- function(path, context_var, indicator) {
  x <- fromJSON(path, simplifyDataFrame = TRUE)
  if (length(x) < 2 || is.null(x[[2]]) || nrow(x[[2]]) == 0) {
    stop("Unexpected World Bank response in: ", path)
  }
  dat <- as.data.table(x[[2]])
  out <- data.table(
    iso3 = dat$countryiso3code,
    year = suppressWarnings(as.integer(dat$date)),
    value = suppressWarnings(as.numeric(dat$value)),
    indicator = indicator,
    context_var = context_var,
    source = "World Bank WDI API"
  )
  out[!is.na(iso3) & !is.na(year)]
}

wb_long <- rbindlist(lapply(seq_len(nrow(world_bank_files)), function(i) {
  parse_world_bank_json(
    world_bank_files$raw_file[i],
    world_bank_files$context_var[i],
    world_bank_files$indicator[i]
  )
}), fill = TRUE)
fwrite(wb_long, file.path(clean_dir, "world_bank_context_long.csv"))

parse_who_hale <- function(path) {
  x <- fromJSON(path, simplifyDataFrame = TRUE)
  if (is.null(x$value) || nrow(x$value) == 0) {
    stop("Unexpected WHO GHO response in: ", path)
  }
  dat <- as.data.table(x$value)
  out <- data.table(
    iso3 = dat$SpatialDim,
    year = suppressWarnings(as.integer(dat$TimeDim)),
    value = suppressWarnings(as.numeric(dat$NumericValue)),
    value_low = suppressWarnings(as.numeric(dat$Low)),
    value_high = suppressWarnings(as.numeric(dat$High)),
    sex = dat$Dim1,
    indicator = dat$IndicatorCode,
    context_var = "hale_60",
    source = "WHO GHO API"
  )
  out[sex == "SEX_BTSX" & iso3 %in% analysis_countries$iso3 & !is.na(year)]
}

who_hale <- parse_who_hale(who_raw_file)
fwrite(who_hale, file.path(clean_dir, "who_hale_60_long.csv"))

parse_hofstede_html <- function(path) {
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  starts <- gregexpr('<div class="c-overview fadeInUp" data-country="[^"]+">', html, perl = TRUE)[[1]]
  if (length(starts) == 1 && starts[1] == -1) {
    warning("Could not find Hofstede country overview blocks in downloaded HTML.")
    return(data.table())
  }
  ends <- c(starts[-1] - 1L, nchar(html))
  blocks <- substring(html, starts, ends)

  rbindlist(lapply(blocks, function(block) {
    key <- sub('.*data-country="([^"]+)".*', "\\1", regmatches(block, regexpr('data-country="[^"]+"', block)))
    h4_match <- regmatches(block, regexpr("<h4[^>]*>[^<]+</h4>", block, perl = TRUE))
    country_name <- if (length(h4_match) > 0) gsub("<[^>]+>", "", h4_match) else key
    get_span <- function(cls) {
      pattern <- paste0('<span class="', cls, '[^"]*">[[:space:]]*[-0-9.]+[[:space:]]*</span>')
      m <- regmatches(block, regexpr(pattern, block, perl = TRUE))
      if (length(m) == 0 || identical(m, character(0))) return(NA_real_)
      suppressWarnings(as.numeric(gsub("<[^>]+>", "", m)))
    }
    data.table(
      hofstede_key = key,
      country_name = country_name,
      power_distance = get_span("power-distance"),
      individualism = get_span("individualism"),
      motivation = get_span("motivation"),
      uncertainty_avoidance = get_span("uncertainty-avoidance"),
      long_term_orientation = get_span("long-term-orientation"),
      indulgence = get_span("indulgence"),
      source = "The Culture Factor country comparison tool"
    )
  }), fill = TRUE)
}

hofstede_all <- parse_hofstede_html(hofstede_raw_file)
fwrite(hofstede_all, file.path(clean_dir, "hofstede_country_comparison_all_parsed.csv"))

hofstede_map <- data.table(
  database = c("CHARLS", "HRS", "ELSA", "KLoSA", "LASI", "MHAS", "CRELES"),
  iso3 = c("CHN", "USA", "GBR", "KOR", "IND", "MEX", "CRI"),
  expected_country_name = c("China", "United States", "United Kingdom",
                            "South Korea", "India", "Mexico", "Costa Rica")
)

if (nrow(hofstede_all) > 0) {
  hofstede_all[, match_name := tolower(trimws(country_name))]
  hofstede_map[, match_name := tolower(trimws(expected_country_name))]
  hofstede_selected <- merge(
    hofstede_map,
    hofstede_all,
    by = "match_name",
    all.x = TRUE
  )
  hofstede_selected[, match_name := NULL]
} else {
  hofstede_selected <- copy(hofstede_map)
  hofstede_selected[, `:=`(
    hofstede_key = NA_character_,
    power_distance = NA_real_,
    individualism = NA_real_,
    motivation = NA_real_,
    uncertainty_avoidance = NA_real_,
    long_term_orientation = NA_real_,
    indulgence = NA_real_,
    source = "The Culture Factor country comparison tool"
  )]
}
setcolorder(hofstede_selected, c("database", "iso3", "expected_country_name",
                                 setdiff(names(hofstede_selected), c("database", "iso3", "expected_country_name"))))
fwrite(hofstede_selected, file.path(clean_dir, "hofstede_individualism_selected.csv"))

nearest_indicator <- function(dat, iso, target_year, context_var_name, max_preferred_gap = 3L) {
  cand <- dat[iso3 == iso & context_var == context_var_name & is.finite(value) & !is.na(year)]
  if (nrow(cand) == 0) {
    return(data.table(
      value = NA_real_,
      indicator_year_used = NA_integer_,
      year_gap = NA_integer_,
      year_match_quality = "missing_all_years"
    ))
  }
  cand[, abs_gap := abs(year - target_year)]
  setorder(cand, abs_gap, -year)
  best <- cand[1]
  quality <- if (best$abs_gap == 0) {
    "exact_year"
  } else if (best$abs_gap <= max_preferred_gap) {
    "nearest_within_3y"
  } else if (best$abs_gap <= 5) {
    "nearest_within_5y_flag"
  } else {
    "nearest_over_5y_flag"
  }
  data.table(
    value = best$value,
    indicator_year_used = best$year,
    year_gap = as.integer(best$abs_gap),
    year_match_quality = quality
  )
}

context_rows <- rbindlist(lapply(seq_len(nrow(analysis_countries)), function(i) {
  row <- analysis_countries[i]
  iso <- row$iso3
  target_year <- row$survey_year

  gdp <- nearest_indicator(wb_long, iso, target_year, "gdp_ppp_pc")
  hexp_pc <- nearest_indicator(wb_long, iso, target_year, "health_exp_pc_ppp")
  hexp_gdp <- nearest_indicator(wb_long, iso, target_year, "health_exp_gdp_pct")
  hale <- nearest_indicator(who_hale, iso, target_year, "hale_60")
  hof <- hofstede_selected[database == row$database]

  data.table(
    database = row$database,
    country = row$cohort_country,
    iso3 = iso,
    wave = row$wave,
    indicator_year_target = target_year,
    gdp_ppp_pc = gdp$value,
    gdp_ppp_pc_year = gdp$indicator_year_used,
    gdp_ppp_pc_year_gap = gdp$year_gap,
    gdp_ppp_pc_qc = gdp$year_match_quality,
    health_exp_pc_ppp = hexp_pc$value,
    health_exp_pc_ppp_year = hexp_pc$indicator_year_used,
    health_exp_pc_ppp_year_gap = hexp_pc$year_gap,
    health_exp_pc_ppp_qc = hexp_pc$year_match_quality,
    health_exp_gdp_pct = hexp_gdp$value,
    health_exp_gdp_pct_year = hexp_gdp$indicator_year_used,
    health_exp_gdp_pct_year_gap = hexp_gdp$year_gap,
    health_exp_gdp_pct_qc = hexp_gdp$year_match_quality,
    hale_60 = hale$value,
    hale_60_year = hale$indicator_year_used,
    hale_60_year_gap = hale$year_gap,
    hale_60_qc = hale$year_match_quality,
    hofstede_individualism = if (nrow(hof) > 0) hof$individualism[1] else NA_real_,
    hofstede_country_name = if (nrow(hof) > 0) hof$expected_country_name[1] else NA_character_,
    hofstede_parse_key = if (nrow(hof) > 0) hof$hofstede_key[1] else NA_character_,
    source = paste(
      "World Bank WDI API",
      "WHO GHO API",
      "The Culture Factor country comparison tool",
      sep = "; "
    ),
    notes = paste(
      "SHARE excluded by analysis plan.",
      "World Bank/WHO values matched to cohort survey year using nearest non-missing year.",
      "Hofstede individualism is a static cultural dimension and should be interpreted as reporting/cultural context, not a policy intervention.",
      sep = " "
    )
  )
}), fill = TRUE)

context_rows[, `:=`(
  log_gdp_ppp_pc = ifelse(is.finite(gdp_ppp_pc) & gdp_ppp_pc > 0, log(gdp_ppp_pc), NA_real_),
  log_health_exp_pc_ppp = ifelse(is.finite(health_exp_pc_ppp) & health_exp_pc_ppp > 0, log(health_exp_pc_ppp), NA_real_)
)]

zscore <- function(x) {
  if (sum(is.finite(x)) < 2) return(rep(NA_real_, length(x)))
  as.numeric(scale(x))
}
context_rows[, `:=`(
  z_log_gdp_ppp_pc = zscore(log_gdp_ppp_pc),
  z_log_health_exp_pc_ppp = zscore(log_health_exp_pc_ppp),
  z_health_exp_gdp_pct = zscore(health_exp_gdp_pct),
  z_hale_60 = zscore(hale_60),
  z_hofstede_individualism = zscore(hofstede_individualism)
)]

fwrite(context_rows, file.path(clean_dir, "country_context_clean.csv"))
fwrite(context_rows, file.path(output_dir, "country_context_clean.csv"))

qc_rows <- rbindlist(lapply(c("gdp_ppp_pc", "health_exp_pc_ppp", "health_exp_gdp_pct", "hale_60"), function(v) {
  data.table(
    database = context_rows$database,
    iso3 = context_rows$iso3,
    context_var = v,
    target_year = context_rows$indicator_year_target,
    year_used = context_rows[[paste0(v, "_year")]],
    year_gap = context_rows[[paste0(v, "_year_gap")]],
    qc_status = context_rows[[paste0(v, "_qc")]],
    value = context_rows[[v]]
  )
}), fill = TRUE)
hof_qc <- data.table(
  database = context_rows$database,
  iso3 = context_rows$iso3,
  context_var = "hofstede_individualism",
  target_year = NA_integer_,
  year_used = NA_integer_,
  year_gap = NA_integer_,
  qc_status = ifelse(is.finite(context_rows$hofstede_individualism), "static_value_parsed", "missing_or_not_parsed"),
  value = context_rows$hofstede_individualism
)
qc_rows <- rbind(qc_rows, hof_qc, fill = TRUE)
fwrite(qc_rows, file.path(output_dir, "country_context_qc_report.csv"))

tp_path <- file.path(tp_dir, "country_tp_bootstrap_summary.csv")
if (!file.exists(tp_path)) {
  stop("Cannot find TP summary file: ", tp_path)
}
tp_long <- fread(tp_path)
tp_long <- tp_long[country != "SHARE"]
tp_long[, tp_se_from_ci := (ci_high - ci_low) / (2 * 1.96)]
tp_long[, tp_se_qc := fifelse(
  is.finite(tp_se_from_ci) & tp_se_from_ci > 0,
  "se_from_ci",
  "zero_or_missing_width_ci"
)]
tp_keep <- tp_long[, .(
  database = country,
  outcome,
  tp_mean = mean,
  tp_median = median,
  tp_sd = sd,
  tp_ci_low = ci_low,
  tp_ci_high = ci_high,
  tp_se_from_ci,
  tp_se_qc,
  n_records,
  n_persons,
  n_boot,
  valid_rate
)]

tp_wide <- dcast(
  tp_keep,
  database + n_records + n_persons + n_boot ~ outcome,
  value.var = c("tp_mean", "tp_median", "tp_sd", "tp_ci_low", "tp_ci_high", "tp_se_from_ci", "tp_se_qc", "valid_rate")
)

merged_long <- merge(context_rows, tp_keep, by = "database", all.x = TRUE)
merged_wide <- merge(context_rows, tp_wide, by = "database", all.x = TRUE)

fwrite(tp_keep, file.path(output_dir, "tp_outcomes_long_clean.csv"))
fwrite(tp_wide, file.path(output_dir, "tp_outcomes_wide_clean.csv"))
fwrite(merged_long, file.path(output_dir, "tp_context_merged_long.csv"))
fwrite(merged_wide, file.path(output_dir, "tp_context_merged.csv"))

for (db in context_rows$database) {
  one_context <- context_rows[database == db]
  one_long <- merged_long[database == db]
  one_wide <- merged_wide[database == db]
  fwrite(one_context, file.path(country_dir, paste0(db, ".country_context_clean.csv")))
  fwrite(one_long, file.path(country_dir, paste0(db, ".tp_context_merged_long.csv")))
  fwrite(one_wide, file.path(country_dir, paste0(db, ".tp_context_merged.csv")))
}

excluded <- country_meta[include_context_analysis == FALSE]
fwrite(excluded, file.path(output_dir, "excluded_country_context_cohorts.csv"))

message("\nSaved outputs:")
message("  ", file.path(clean_dir, "country_wave_year_mapping.csv"))
message("  ", file.path(clean_dir, "country_context_source_catalog.csv"))
message("  ", file.path(clean_dir, "country_context_clean.csv"))
message("  ", file.path(output_dir, "country_context_clean.csv"))
message("  ", file.path(output_dir, "country_context_qc_report.csv"))
message("  ", file.path(output_dir, "tp_outcomes_long_clean.csv"))
message("  ", file.path(output_dir, "tp_outcomes_wide_clean.csv"))
message("  ", file.path(output_dir, "tp_context_merged_long.csv"))
message("  ", file.path(output_dir, "tp_context_merged.csv"))
message("  ", country_dir)
