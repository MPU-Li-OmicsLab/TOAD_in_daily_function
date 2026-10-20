#!/usr/bin/env Rscript

# Step 1-3 cleaning script for external validation cohorts.
# Usage:
#   Rscript 01_clean_external_validation_data.R demo.csv clean_outputs
#   Rscript 01_clean_external_validation_data.R /path/to/full_merged_data.csv clean_outputs

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required. Please install it with install.packages('data.table').")
  }
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1) args[[1]] else "demo.csv"
output_dir <- if (length(args) >= 2) args[[2]] else "clean_outputs"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

dt <- fread(
  input_file,
  na.strings = c("", "NA", "NaN", ".", "NULL"),
  showProgress = TRUE
)

required_cols <- c("ID", "wave", "database", "age")
missing_required <- setdiff(required_cols, names(dt))
if (length(missing_required) > 0) {
  stop("Missing required column(s): ", paste(missing_required, collapse = ", "))
}

setDT(dt)
dt[, database := tolower(as.character(database))]

db_labels <- c(
  charls = "CHARLS",
  hrs = "HRS",
  share = "SHARE",
  elsa = "ELSA",
  klosa = "KLoSA",
  lasi = "LASI",
  mhas = "MHAS",
  creles = "CRELES"
)

country_labels <- c(
  charls = "China",
  hrs = "United States",
  share = "Europe",
  elsa = "England",
  klosa = "South Korea",
  lasi = "India",
  mhas = "Mexico",
  creles = "Costa Rica"
)

ensure_col <- function(x, col, type = c("numeric", "character")) {
  type <- match.arg(type)
  if (!col %in% names(x)) {
    if (type == "numeric") x[, (col) := as.numeric(NA)] else x[, (col) := NA_character_]
  }
  invisible(NULL)
}

add_missing_core_columns <- function(x) {
  numeric_cols <- c(
    "gender", "education", "year",
    "personal_income", "household_income", "total_income",
    "rural", "rural_hukou", "rural_live",
    "cancer_ever", "cancer_meds", "cancer_chemo", "cancer_surgery",
    "cancer_radiation", "cancer_active", "cancer_history",
    "ad_alzhe", "ad_alzhee", "ad_alzdeme",
    "pd_parkinson", "pd_parkine", "dementia",
    "stroke", "fracture_hip",
    "hypertension", "diabetes", "cancer", "lung_disease",
    "heart_disease", "arthritis", "psychiatric"
  )
  for (col in numeric_cols) ensure_col(x, col, "numeric")
  invisible(NULL)
}

add_missing_core_columns(dt)

missing_codes <- c(
  -9:-1, 7:9, 97:99, 997:999, 9997:9999,
  99997:99999, 999997:999999
)

to_binary01 <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  y[y %in% missing_codes] <- NA_real_
  fifelse(is.na(y), NA_integer_, fifelse(y == 0, 0L, 1L))
}

to_urban_rural <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  y[y %in% missing_codes] <- NA_real_
  fifelse(is.na(y), NA_character_, fifelse(y == 1, "rural", "urban"))
}

coalesce_cols <- function(x, cols) {
  present <- intersect(cols, names(x))
  if (length(present) == 0) return(rep(NA_real_, nrow(x)))
  out <- suppressWarnings(as.numeric(x[[present[[1]]]]))
  if (length(present) > 1) {
    for (col in present[-1]) {
      candidate <- suppressWarnings(as.numeric(x[[col]]))
      out[is.na(out)] <- candidate[is.na(out)]
    }
  }
  out
}

make_item_by_database <- function(x, mapping) {
  fallback_cols <- unique(unlist(mapping, use.names = FALSE))
  fallback_value <- coalesce_cols(x, fallback_cols)
  out <- rep(NA_real_, nrow(x))
  for (db in names(mapping)) {
    idx <- x$database == db
    if (!any(idx, na.rm = TRUE)) next
    source_value <- coalesce_cols(x[idx], mapping[[db]])
    source_value[is.na(source_value)] <- fallback_value[idx][is.na(source_value)]
    out[idx] <- source_value
  }
  to_binary01(out)
}

adl_item_map <- list(
  adl_dress = list(
    charls = c("dressa"), hrs = c("dress"), share = c("dressa"),
    elsa = c("dressa"), klosa = c("dressb"), lasi = c("dressa"),
    mhas = c("dressa"), creles = c("dressa")
  ),
  adl_walk = list(
    charls = c("walksa"), hrs = c("walkr"), share = c("walksa"),
    elsa = c("walkra"), klosa = c("walksb"),
    lasi = c("walksa"), mhas = c("walksa"), creles = c("walksa")
  ),
  adl_bath = list(
    charls = c("batha"), hrs = c("bath"), share = c("batha"),
    elsa = c("batha"), klosa = c("bathb"), lasi = c("batha"),
    mhas = c("batha"), creles = c("batha")
  ),
  adl_eat = list(
    charls = c("eata"), hrs = c("eat"), share = c("eata"),
    elsa = c("eata"), klosa = c("eatb"), lasi = c("eata"),
    mhas = c("eata"), creles = c("eata")
  ),
  adl_toilet = list(
    charls = c("toileta"), hrs = c("toilt"), share = c("toileta"),
    elsa = c("toileta"), klosa = c("toiletb"), lasi = c("toileta"),
    mhas = c("toileta"), creles = c("toileta")
  )
)

iadl_items <- c("iadl_meal", "iadl_shop", "iadl_meds", "iadl_money")
adl_items <- names(adl_item_map)

for (item in adl_items) {
  dt[, (item) := make_item_by_database(.SD, adl_item_map[[item]])]
}

for (item in iadl_items) {
  ensure_col(dt, item, "numeric")
  dt[, (item) := to_binary01(get(item))]
}

score_total_from_items <- function(x, items) {
  mat <- as.matrix(x[, ..items])
  non_missing <- rowSums(!is.na(mat))
  score <- rowSums(mat, na.rm = TRUE)
  score[non_missing == 0] <- NA_integer_
  as.integer(score)
}

score_mean_from_items <- function(x, items) {
  mat <- as.matrix(x[, ..items])
  non_missing <- rowSums(!is.na(mat))
  score <- rowSums(mat, na.rm = TRUE)
  mean_score <- score / non_missing
  mean_score[non_missing == 0] <- NA_real_
  mean_score
}

dt[, ADL_totalScore := score_total_from_items(.SD, adl_items)]
dt[, IADL_totalScore := score_total_from_items(.SD, iadl_items)]
dt[, ADL_meanScore := score_mean_from_items(.SD, adl_items)]
dt[, IADL_meanScore := score_mean_from_items(.SD, iadl_items)]
dt[, ADL_nonmissing_items := rowSums(!is.na(as.matrix(.SD))), .SDcols = adl_items]
dt[, IADL_nonmissing_items := rowSums(!is.na(as.matrix(.SD))), .SDcols = iadl_items]
dt[, functional_items_available := (ADL_nonmissing_items + IADL_nonmissing_items) > 0]

dt[, database_label := unname(db_labels[database])]
dt[is.na(database_label), database_label := toupper(database)]
dt[, country_region := unname(country_labels[database])]
dt[is.na(country_region), country_region := database_label]

dt[, age := suppressWarnings(as.numeric(age))]
dt[, age_60_100 := !is.na(age) & age >= 60 & age <= 100]

dt[, gender_std := {
  g_num <- suppressWarnings(as.numeric(gender))
  g_chr <- tolower(trimws(as.character(gender)))
  fifelse(
    g_num == 1 | g_chr %in% c("male", "m", "man", "男", "男性"),
    "male",
    fifelse(g_num == 2 | g_chr %in% c("female", "f", "woman", "女", "女性"), "female", NA_character_)
  )
}]

dt[, education_years := suppressWarnings(as.numeric(education))]
dt[education_years %in% missing_codes, education_years := NA_real_]

dt[, education_cat_method := {
  edu <- education_years[!is.na(education_years)]
  looks_like_1_to_5_level <- length(edu) > 0 &&
    max(edu, na.rm = TRUE) <= 5 &&
    min(edu, na.rm = TRUE) >= 0 &&
    all(abs(edu - round(edu)) < 1e-8)
  if (looks_like_1_to_5_level) "education_level_1_5" else "education_years"
}, by = database]

dt[, education_cat := fcase(
  is.na(education_years), NA_character_,
  education_cat_method == "education_level_1_5" & education_years <= 2, "primary_or_below",
  education_cat_method == "education_level_1_5" & education_years <= 4, "secondary",
  education_cat_method == "education_level_1_5" & education_years >= 5, "college_or_above",
  education_cat_method == "education_years" & education_years <= 6, "primary_or_below",
  education_cat_method == "education_years" & education_years <= 12, "secondary",
  education_cat_method == "education_years" & education_years >= 13, "college_or_above"
)]

dt[, rural_value := fifelse(
  database == "charls",
  coalesce_cols(.SD, c("rural_live", "rural", "rural_hukou")),
  coalesce_cols(.SD, c("rural", "rural_live", "rural_hukou"))
)]
dt[, rural_std := to_urban_rural(rural_value)]

dt[, personal_income := suppressWarnings(as.numeric(personal_income))]
dt[, household_income := suppressWarnings(as.numeric(household_income))]
dt[, total_income := suppressWarnings(as.numeric(total_income))]
for (income_col in c("personal_income", "household_income", "total_income")) {
  dt[get(income_col) %in% missing_codes, (income_col) := NA_real_]
}

dt[, income_for_quintile := fcoalesce(total_income, household_income, personal_income)]

make_quintile <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  if (sum(ok) < 5 || length(unique(x[ok])) < 2) return(out)
  probs <- quantile(x[ok], probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  probs <- unique(probs)
  if (length(probs) < 3) return(out)
  out[ok] <- as.integer(cut(x[ok], breaks = probs, include.lowest = TRUE, labels = FALSE))
  out
}

dt[, income_quintile_year := make_quintile(income_for_quintile), by = .(database, year)]
dt[, income_quintile_all := make_quintile(income_for_quintile), by = database]
dt[, income_quintile := fcoalesce(income_quintile_year, income_quintile_all)]

disease_cols <- c(
  "ad_alzhe", "ad_alzhee", "ad_alzdeme",
  "pd_parkinson", "pd_parkine",
  "dementia", "stroke", "fracture_hip",
  "cancer_active", "cancer_meds", "cancer_chemo",
  "cancer_surgery", "cancer_radiation"
)

for (col in disease_cols) {
  ensure_col(dt, col, "numeric")
  dt[, (col) := to_binary01(get(col))]
}

dt[, exclusion_ad := Reduce(`|`, lapply(.SD, function(x) x == 1L)), .SDcols = c("ad_alzhe", "ad_alzhee", "ad_alzdeme")]
dt[, exclusion_pd := Reduce(`|`, lapply(.SD, function(x) x == 1L)), .SDcols = c("pd_parkinson", "pd_parkine")]
dt[, exclusion_dementia := dementia == 1L]
dt[, exclusion_stroke := stroke == 1L]
dt[, exclusion_hip_fracture := fracture_hip == 1L]
dt[, exclusion_active_cancer := cancer_active == 1L]
dt[, exclusion_cancer_treatment := Reduce(`|`, lapply(.SD, function(x) x == 1L)),
   .SDcols = c("cancer_meds", "cancer_chemo", "cancer_surgery", "cancer_radiation")]

exclusion_flags <- c(
  "exclusion_ad", "exclusion_pd", "exclusion_dementia",
  "exclusion_stroke", "exclusion_hip_fracture",
  "exclusion_active_cancer", "exclusion_cancer_treatment"
)

for (col in exclusion_flags) {
  dt[is.na(get(col)), (col) := FALSE]
}

dt[, major_disabling_condition_observed := Reduce(`|`, lapply(.SD, identity)), .SDcols = exclusion_flags]
dt[, relatively_healthy := age_60_100 & !major_disabling_condition_observed]
dt[, final_clean_sample := relatively_healthy & !is.na(age) & functional_items_available]

summary_table <- dt[, .(
  raw_records = .N,
  raw_persons = uniqueN(ID),
  age_60_100_records = sum(age_60_100, na.rm = TRUE),
  age_60_100_persons = uniqueN(ID[age_60_100]),
  excluded_records_after_age = sum(age_60_100 & major_disabling_condition_observed, na.rm = TRUE),
  excluded_persons_after_age = uniqueN(ID[age_60_100 & major_disabling_condition_observed]),
  dropped_all_adl_iadl_missing_records = sum(relatively_healthy & !functional_items_available, na.rm = TRUE),
  dropped_all_adl_iadl_missing_persons = uniqueN(ID[relatively_healthy & !functional_items_available]),
  final_records = sum(final_clean_sample, na.rm = TRUE),
  final_persons = uniqueN(ID[final_clean_sample])
), by = .(database_label, database, country_region)]

setorder(summary_table, database_label)
fwrite(summary_table, file.path(output_dir, "sample_selection_summary.csv"))

source_map_string <- function(mapping) {
  paste(
    sprintf("%s(%s)", unname(db_labels[names(mapping)]), vapply(mapping, paste, character(1), collapse = "/")),
    collapse = "; "
  )
}

present_map_string <- function(mapping, x) {
  paste(
    sprintf(
      "%s(%s)",
      unname(db_labels[names(mapping)]),
      vapply(mapping, function(cols) {
        present <- intersect(cols, names(x))
        if (length(present) == 0) "missing" else paste(present, collapse = "/")
      }, character(1))
    ),
    collapse = "; "
  )
}

harmonization_table <- rbindlist(list(
  data.table(
    domain = "ADL",
    item = c("dressing", "walking", "bathing", "eating", "toileting"),
    harmonized_variable = adl_items,
    source_original_by_database = vapply(adl_item_map, source_map_string, character(1)),
    source_columns_present_in_input = vapply(adl_item_map, present_map_string, character(1), x = dt),
    coding = "0=no difficulty; 1=any difficulty; 7/8/9 and extended missing codes set to NA",
    included_in_main_analysis = TRUE
  ),
  data.table(
    domain = "IADL",
    item = c("preparing_hot_meals", "shopping", "taking_medications", "managing_money"),
    harmonized_variable = iadl_items,
    source_original_by_database = c(
      "CHARLS(mealsa); HRS(meals); SHARE(mealsa); ELSA(mealsa); KLoSA(mealsb); LASI(mealsa); MHAS(mealsa); CRELES(mealsa)",
      "CHARLS(shopa); HRS(shop); SHARE(shopa); ELSA(shopa); KLoSA(shopb); LASI(shopa); MHAS(shopa); CRELES(shopa)",
      "CHARLS(medsa); HRS(meds); SHARE(medsa); ELSA(medsa); KLoSA(medsb); LASI(medsa); MHAS(medsa); CRELES(medsa)",
      "CHARLS(moneya); HRS(money); SHARE(moneya); ELSA(moneya); KLoSA(moneyb); LASI(moneya); MHAS(moneya); CRELES(moneya)"
    ),
    source_columns_present_in_input = ifelse(iadl_items %in% names(dt), iadl_items, "missing"),
    coding = "0=no difficulty; 1=any difficulty; 7/8/9 and extended missing codes set to NA",
    included_in_main_analysis = TRUE
  )
), use.names = TRUE)

fwrite(harmonization_table, file.path(output_dir, "adl_iadl_harmonization_table.csv"))

core_cols <- c(
  "ID",
  "age", "gender", "gender_std", "education", "education_cat",
  "rural", "rural_live", "rural_hukou", "rural_value", "rural_std",
  "personal_income", "household_income", "total_income",
  adl_items, iadl_items,
  "ADL_totalScore", "IADL_totalScore",
  "ADL_meanScore", "IADL_meanScore"
)

core_cols <- intersect(core_cols, names(dt))
clean_dt <- dt[final_clean_sample == TRUE, c("database", core_cols), with = FALSE]

for (db in sort(unique(clean_dt$database))) {
  db_label <- unname(db_labels[db])
  if (is.na(db_label)) db_label <- toupper(db)
  out_file <- file.path(output_dir, paste0(db_label, ".country.data.clean.csv"))
  fwrite(clean_dt[database == db, ..core_cols], out_file)
}

fwrite(clean_dt[, ..core_cols], file.path(output_dir, "ALL.country.data.clean.csv"))

message("Cleaning complete.")
message("Input file: ", normalizePath(input_file, mustWork = FALSE))
message("Output directory: ", normalizePath(output_dir, mustWork = FALSE))
message("Wrote: sample_selection_summary.csv")
message("Wrote: adl_iadl_harmonization_table.csv")
message("Wrote one clean CSV per database plus ALL.country.data.clean.csv")
