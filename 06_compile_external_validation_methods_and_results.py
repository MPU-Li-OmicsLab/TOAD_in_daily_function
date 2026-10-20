from __future__ import annotations

from pathlib import Path
from textwrap import dedent

import pandas as pd
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.shared import Pt
from docx.oxml.ns import qn
from openpyxl import Workbook, load_workbook
from openpyxl.styles import Alignment, Font, PatternFill, Border, Side
from openpyxl.utils import get_column_letter


BASE_DIR = Path(__file__).resolve().parent
OUTPUT_DIR = BASE_DIR / "06.table_outputs"
XLSX_PATH = OUTPUT_DIR / "multinational_cohort_key_results_for_submission.xlsx"
DOCX_PATH = OUTPUT_DIR / "external_validation_methods_for_manuscript.docx"
MD_PATH = OUTPUT_DIR / "external_validation_methods_for_manuscript.md"


METHODS_TEXT = dedent(
    """
    Multinational external validation cohorts

    We used eight harmonised ageing cohorts as external validation datasets for the functional decline turning-point analyses: CHARLS (China), HRS (United States), ELSA (England), SHARE (multi-country Europe), KLoSA (South Korea), LASI (India), MHAS (Mexico), and CRELES (Costa Rica). These cohorts differ from the Hong Kong interRAI assessment population because they are primarily community-based ageing surveys rather than care-assessment system datasets. We therefore treated them as external validation cohorts to test whether the temporal structure of functional decline observed in Hong Kong could be reproduced across populations, care systems, and national contexts.

    Data cleaning and construction of the relatively healthy validation sample

    Data cleaning was performed with 01_clean_external_validation_data.R, using the data.table package for high-volume data import, recoding, and file output. The script read the merged source file with data.table::fread(), using "", "NA", "NaN", ".", and "NULL" as missing-value codes. Required fields were ID, wave, database, and age. Database names were standardised to lower case and then mapped to cohort labels and countries or regions. The analytic validation sample was restricted to participants aged 60-100 years. Participants were excluded if they had an observed major disabling condition among the available disease variables in that cohort. The exclusion conditions were Alzheimer’s disease or related Alzheimer variables, Parkinson’s disease, dementia, stroke, hip fracture, active cancer, or recorded cancer treatment. When a disease variable was not available in a given cohort it was not used to exclude participants, consistent with a principle of excluding only observable major disabling conditions. Cancer history alone was not used as an exclusion criterion unless active cancer or cancer treatment was observed. Participants with all selected ADL and IADL items missing were removed, as were participants with missing age.

    ADL and IADL harmonisation was also implemented in 01_clean_external_validation_data.R. Five ADL items were retained for the primary validation analysis: dressing, walking, bathing, eating, and toileting. Four IADL items were retained: preparing hot meals, shopping, taking medications, and managing money. For ADL items, cohort-specific source variables were prioritised according to the prespecified mapping: dressing was extracted from dressa in CHARLS, ELSA, SHARE, LASI, MHAS, and CRELES, dress in HRS, and dressb in KLoSA; walking from walksa in CHARLS, SHARE, LASI, MHAS, and CRELES, walkr in HRS, walkra in ELSA, and walksb in KLoSA; bathing from batha in CHARLS, ELSA, SHARE, LASI, MHAS, and CRELES, bath in HRS, and bathb in KLoSA; eating from eata in CHARLS, ELSA, SHARE, LASI, MHAS, and CRELES, eat in HRS, and eatb in KLoSA; and toileting from toileta in CHARLS, ELSA, SHARE, LASI, MHAS, and CRELES, toilt in HRS, and toiletb in KLoSA. If the cohort-specific variable was missing in the merged file, the script attempted to use available mapped fallback columns with non-missing values. All selected ADL and IADL items were harmonised to binary indicators, where 0 indicated no difficulty and 1 indicated any difficulty; extended missing codes including negative codes, 7-9, 97-99, 997-999, 9997-9999, 99997-99999, and 999997-999999 were set to missing. ADL_totalScore and IADL_totalScore were calculated as the number of difficulty items among non-missing selected items; ADL_meanScore and IADL_meanScore were calculated as total score divided by the number of non-missing items. If all items in a domain were missing, the corresponding domain score was set to missing.

    Covariates were harmonised for later stratified analyses. Sex was standardised to male or female. Education was preferentially treated as years of education; when values resembled an ordinal 1-5 educational-level code, cohort-specific education categories were derived accordingly. Education was then grouped as primary or below, secondary, or college or above. Rurality was standardised as urban or rural; for CHARLS, actual residence variables were prioritised over hukou where available. Personal, household, and total income were converted to numeric after removal of extended missing codes.

    The cleaning script produced one clean file per cohort in 01.clean_outputs, together with ALL.country.data.clean.csv, sample_selection_summary.csv, and adl_iadl_harmonization_table.csv. Cleaned files retained ID, age, sex, education, rurality, income variables, the selected ADL and IADL items, ADL/IADL total scores, and ADL/IADL mean scores.

    Country-specific turning-point estimation and cross-country meta-analysis

    Country-specific ADL and IADL turning points were estimated with 02_tp_meta_analysis.R. The script used data.table, dplyr, ggplot2, metafor, minpack.lm, and patchwork. Clean cohort files were read with data.table::fread(). Analyses were restricted to participants aged 60-100 years. The primary outcomes were ADL_totalScore and IADL_totalScore.

    For each cohort and outcome, age-specific mean scores were calculated using make_age_mean_data(), which grouped observations by age and computed mean_score and n_age. These age-level data were fitted with a four-parameter logistic model using minpack.lm::nlsLM(). The logistic function was defined as ymin + R / (1 + exp(-k * (age - l))), where ymin is the lower asymptote, R is the response range, k is the growth-rate parameter, and l is the location parameter. The fitting procedure weighted each age-specific mean by n_age. Starting values were ymin_start equal to the minimum age-specific mean score, R_start equal to the observed score range with a minimum of 1e-4, k_start = 0.2, and l_start equal to the n_age-weighted mean age. Parameter bounds were ymin from -Inf to Inf, R >= 1e-6, k >= 1e-6, and l constrained between the minimum and maximum observed ages. The nls.lm.control(maxiter = 500) setting was used.

    Turning points were defined as the onset of accelerated functional decline. After fitting the 4-parameter logistic curve, fitted values were predicted on an age grid with interval AGE_GRID_BY = 0.1 years. The first and second numerical derivatives were calculated with finite differences. The turning point was defined as the first age at which the second derivative reached at least ACC_THRESHOLD_PROP times the maximum positive second derivative, with ACC_THRESHOLD_PROP = 0.2. Turning points outside 60-100 years or non-finite turning points were treated as invalid.

    Uncertainty was estimated using nonparametric bootstrap resampling. For each country, n_boot = 5000 bootstrap replicates were run. In each replicate, rows were sampled with replacement with sample size m = min(sample_cap, N), where sample_cap = 50000 and N was the number of cleaned records in the cohort. This cap was used to preserve reliability in smaller cohorts while limiting computation time in very large cohorts. ADL and IADL turning points were estimated in every bootstrap replicate; ADL minus IADL turning-point differences were calculated when both estimates were valid. The script reported bootstrap mean, median, standard deviation, 2.5th and 97.5th percentiles, valid bootstrap counts, and valid rates.

    Cross-country meta-analyses were performed with metafor::rma.uni() using random-effects restricted maximum likelihood (REML). For each cohort, the bootstrap mean turning point was used as yi. The standard error was primarily the bootstrap SD. If the bootstrap SD was zero or missing, the SE was approximated from the bootstrap CI width as (upper - lower)/(2 * 1.96); if this remained unavailable, an outcome-specific positive SE floor was used. Separate random-effects meta-analyses were fitted for IADL_TP, ADL_TP, and ADL_minus_IADL_TP. The outputs included pooled estimates, 95% CIs, tau-squared, I-squared, Cochran Q, and Q-test p values. Visual outputs included country-specific fitted age curves, ADL/IADL forest plots, and ADL-IADL difference forest plots.

    Developed versus less-developed cohort subgroup meta-analysis

    A prespecified exploratory subgroup meta-analysis was implemented in 02b_development_group_meta_analysis.R using data.table, dplyr, ggplot2, metafor, and patchwork. Developed cohorts were defined as ELSA, HRS, KLoSA, and SHARE; less-developed cohorts were defined as CHARLS, CRELES, LASI, and MHAS. This operational grouping was used for descriptive and exploratory comparison rather than causal interpretation. The script read random_effects_meta_analysis_input.csv from 02.tp_meta_outputs and did not rerun bootstrap estimation.

    Within each outcome, developed and less-developed groups were pooled separately using metafor::rma.uni() with REML. Between-group differences were tested using random-effects meta-regression with a binary moderator for developed cohorts. The model was fitted with metafor::rma.uni(yi, sei, mods = developed_binary, method = "REML", test = "z"). The regression coefficient represents the mean difference in turning-point age between developed and less-developed cohorts, in years. The script reported beta, SE, 95% CI, z statistic, p value, tau-squared, I-squared, and omnibus moderator test statistics. Forest plots displayed country-level estimates ordered by IADL turning-point age, subgroup pooled estimates, overall random-effects estimates, and the moderator test statistics.

    Within-country stratified and health-equity analyses

    Stratified analyses were performed with 03_stratified_equity_tp_analysis.R. The script used data.table, dplyr, ggplot2, and minpack.lm. It estimated ADL and IADL turning points within each country by sex, education, income, and rurality when the relevant variables were available and sample sizes were sufficient. The default settings were n_boot = 1000 bootstrap replicates per country-subgroup, sample_cap = 50000 rows per bootstrap draw, n_perm = 500 permutations per country-stratum-outcome, seed = 20260603, n_cores = 1, ACC_THRESHOLD_PROP = 0.2, AGE_GRID_BY = 0.1, min_group_n = 200 records, and perm_sample_cap = 10000 rows.

    Sex was analysed as male versus female. Education was analysed as low, middle, and high education, with high education as the reference for the low-high contrast. Income groups were created within country using the income variable with the fewest missing values among personal_income, household_income, and total_income, provided the variable had sufficient non-missing values and variation. Rank-based within-country quintiles were constructed to reduce instability from tied or zero income values; the main contrast compared the lowest income quintile (Q1) with the highest income quintile (Q5). Rurality was analysed as urban versus rural where available. Subgroups with fewer than min_group_n records or fewer than four age values were skipped.

    For each subgroup, the same 4-parameter logistic model and second-derivative onset definition used in the main TP analysis were applied. Bootstrap summaries included subgroup sample size, number of age values, full-data TP, bootstrap mean, median, SD, 95% CI, valid counts, and valid rate. Permutation tests were used to assess within-country subgroup differences in turning-point age. For each country, stratum, and outcome, group labels were permuted n_perm times after optionally downsampling to perm_sample_cap rows. Turning points were refitted without bootstrap in each permuted dataset, and the permutation statistic was the absolute contrast in TP age between the relevant groups. P values were calculated as (number of permuted statistics >= observed statistic + 1)/(number of valid permutations + 1). Outputs included stratified_tp_summary.csv, stratified_permutation_pvalues.csv, stratified_variable_availability.csv, income_source_selection.csv, income_quintile_trend_tp_summary.csv, reviewer_stratified_tp_table.csv, per-country forest plots, and an all-country stratified forest plot.

    Country-level contextual data collection and quality control

    Country-level attribution data were downloaded and prepared with 04_country_context_attribution_data_prep.R using data.table and jsonlite. Cohort survey years were mapped as follows: CHARLS wave 4, 2018; HRS wave 14, 2018; SHARE wave 8, 2017; ELSA wave 9, 2018; KLoSA wave 7, 2018; LASI wave 2, 2020; MHAS wave 4, 2015; and CRELES wave 5, 2015. SHARE was excluded from country-level attribution analyses because it is a multi-country European cohort.

    External contextual indicators were downloaded from official sources. World Bank WDI API indicators were GDP per capita, PPP (current international dollars; NY.GDP.PCAP.PP.CD), current health expenditure per capita, PPP (current international dollars; SH.XPD.CHEX.PP.CD), and current health expenditure as a percentage of GDP (SH.XPD.CHEX.GD.ZS). WHO Global Health Observatory indicator WHOSIS_000007 was used for healthy life expectancy at age 60, both sexes. Hofstede individualism scores were parsed from the official Culture Factor country comparison tool. Raw downloads were stored in 04.country_context_inputs/raw and cleaned long-format files were stored in 04.country_context_inputs/clean.

    For World Bank and WHO indicators, each cohort was matched to its survey year. If the survey-year value was unavailable, the nearest non-missing year was used and the year gap was recorded. Match quality was flagged as exact_year, nearest_within_3y, nearest_within_5y_flag, nearest_over_5y_flag, or missing_all_years. GDP per capita PPP and health expenditure per capita PPP were log-transformed because of right-skewed distributions. Continuous contextual indicators were z-standardised before modelling. The cleaned country-context file included database, country, ISO3, target survey year, indicator values, year used, year gap, quality-control status, source, notes, log-transformed values, and z-scores. These files were merged with country-level TP outcomes from 02.tp_meta_outputs, producing tp_context_merged.csv and tp_context_merged_long.csv.

    Country-level attribution analyses

    Attribution analyses were performed with 05_country_context_attribution_analysis.R using data.table, ggplot2, and metafor. These analyses were intentionally hypothesis-generating because only seven country-level observations were available after excluding SHARE. Outcomes were IADL_TP, ADL_TP, and ADL_minus_IADL_TP. For each country and outcome, the bootstrap median TP was used as the outcome estimate, and the standard error was approximated from the bootstrap 95% CI as (upper - lower)/(2 * 1.96). If the CI width was zero or missing, an outcome-specific median positive SE floor was used and recorded in country_context_analysis_qc.csv. TP estimates at or below 60.2 years were flagged as floor-effect TPs for sensitivity analyses.

    Descriptive associations were assessed using Spearman rank correlations with stats::cor.test(method = "spearman", exact = FALSE). Primary single-predictor meta-regressions were fitted with metafor::rma.uni(), using inverse-variance weights, random effects, REML estimation, and Knapp-Hartung adjustment (test = "knha"). Each model included one z-standardised country-level predictor: log GDP per capita PPP, log health expenditure per capita PPP, health expenditure as a percentage of GDP, HALE at age 60, or Hofstede individualism. The coefficient represents the change in turning-point age in years per 1-SD higher predictor value. Unweighted ordinary least squares models were fitted as sensitivity analyses using stats::lm(). Additional sensitivity analyses repeated meta-regressions on raw-scale z-standardised GDP and health expenditure per capita, excluded floor-effect TP countries, and performed leave-one-country-out refitting. Outputs included Spearman correlation results, primary meta-regression results, unweighted OLS sensitivity results, raw-scale sensitivity results, floor-effect sensitivity results, leave-one-country-out results, QC summaries, and publication figures.
    """
).strip()


def read_csv(path: str | Path) -> pd.DataFrame:
    path = Path(path)
    if not path.exists():
        return pd.DataFrame({"missing_source_file": [str(path)]})
    return pd.read_csv(path)


def add_sheet_from_df(wb: Workbook, name: str, df: pd.DataFrame, source: str | None = None):
    ws = wb.create_sheet(title=name[:31])
    if source:
        ws["A1"] = "Source file"
        ws["B1"] = source
        start_row = 3
    else:
        start_row = 1

    if df.empty:
        ws.cell(row=start_row, column=1, value="No rows")
        return ws

    for c_idx, col in enumerate(df.columns, start=1):
        ws.cell(row=start_row, column=c_idx, value=str(col))
    for r_idx, row in enumerate(df.itertuples(index=False), start=start_row + 1):
        for c_idx, value in enumerate(row, start=1):
            if pd.isna(value):
                value = None
            ws.cell(row=r_idx, column=c_idx, value=value)

    header_fill = PatternFill("solid", fgColor="1F4E78")
    header_font = Font(color="FFFFFF", bold=True)
    thin = Side(style="thin", color="D9E2F3")
    for cell in ws[start_row]:
        cell.fill = header_fill
        cell.font = header_font
        cell.alignment = Alignment(horizontal="center", vertical="center", wrap_text=True)
    for row in ws.iter_rows(min_row=start_row, max_row=ws.max_row, min_col=1, max_col=ws.max_column):
        for cell in row:
            cell.border = Border(bottom=thin)
            cell.alignment = Alignment(vertical="top", wrap_text=True)

    ws.freeze_panes = ws.cell(row=start_row + 1, column=1).coordinate
    ws.auto_filter.ref = f"A{start_row}:{get_column_letter(ws.max_column)}{ws.max_row}"
    for col_idx, col_cells in enumerate(ws.columns, start=1):
        max_len = 0
        for cell in col_cells:
            if cell.value is not None:
                max_len = max(max_len, len(str(cell.value)))
        ws.column_dimensions[get_column_letter(col_idx)].width = min(max(max_len + 2, 10), 42)
    return ws


def build_analysis_parameters() -> pd.DataFrame:
    rows = [
        ["Data cleaning", "01_clean_external_validation_data.R", "data.table", "fread, fwrite, fcase, fifelse, fcoalesce", "Age restricted to 60-100; exclusion for observed AD/dementia/PD/stroke/hip fracture/active cancer/cancer treatment; ADL/IADL binary coding; total and mean scores calculated."],
        ["TP estimation", "02_tp_meta_analysis.R", "data.table; dplyr; ggplot2; metafor; minpack.lm; patchwork", "nlsLM; rma.uni; ggsave", "n_boot=5000; sample_cap=50000; ACC_THRESHOLD_PROP=0.2; AGE_GRID_BY=0.1; nls.lm.control(maxiter=500); random-effects REML meta-analysis."],
        ["Developed subgroup meta", "02b_development_group_meta_analysis.R", "data.table; dplyr; ggplot2; metafor; patchwork", "rma.uni; ggsave", "Developed cohorts: ELSA, HRS, KLoSA, SHARE; less-developed cohorts: CHARLS, CRELES, LASI, MHAS; random-effects subgroup pooling and binary moderator test."],
        ["Stratified equity analysis", "03_stratified_equity_tp_analysis.R", "data.table; dplyr; ggplot2; minpack.lm", "nlsLM; bootstrap_tps; permutation_test_stratum; ggsave", "n_boot=1000; sample_cap=50000; n_perm=500; seed=20260603; ACC_THRESHOLD_PROP=0.2; AGE_GRID_BY=0.1; min_group_n=200; perm_sample_cap=10000."],
        ["Context data preparation", "04_country_context_attribution_data_prep.R", "data.table; jsonlite", "download.file; fromJSON; fwrite", "World Bank WDI, WHO GHO, Culture Factor/Hofstede; nearest-year matching; SHARE excluded; log transform and z-score for contextual predictors."],
        ["Context attribution analysis", "05_country_context_attribution_analysis.R", "data.table; ggplot2; metafor", "cor.test; rma.uni; lm; ggsave", "Seven countries; Spearman correlations; single-predictor IVW random-effects meta-regression using REML and Knapp-Hartung; unweighted OLS, raw-scale, floor-effect, and leave-one-out sensitivity analyses."],
    ]
    return pd.DataFrame(rows, columns=["analysis_module", "script", "R_packages", "key_functions", "key_parameters"])


def build_readme() -> pd.DataFrame:
    return pd.DataFrame(
        [
            ["Workbook purpose", "Key result tables for the multinational external validation and country-context attribution analyses."],
            ["Generated from", "Existing output CSV files in 01.clean_outputs, 02.tp_meta_outputs, 03.stratified_equity_outputs, 04.country_context_inputs, and 05.country_context_attribution_outputs."],
            ["Important note", "Bootstrap raw replicate-level outputs were not included to keep this submission workbook compact; summary tables include valid rates and uncertainty intervals."],
            ["Methods file", str(DOCX_PATH)],
        ],
        columns=["item", "description"],
    )


def make_workbook():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    wb = Workbook()
    wb.remove(wb.active)

    sheets = [
        ("README", build_readme(), None),
        ("analysis_parameters", build_analysis_parameters(), None),
        ("sample_selection", read_csv(BASE_DIR / "01.clean_outputs/sample_selection_summary.csv"), "01.clean_outputs/sample_selection_summary.csv"),
        ("harmonization", read_csv(BASE_DIR / "01.clean_outputs/adl_iadl_harmonization_table.csv"), "01.clean_outputs/adl_iadl_harmonization_table.csv"),
        ("country_tp_summary", read_csv(BASE_DIR / "02.tp_meta_outputs/country_tp_bootstrap_summary_wide.csv"), "02.tp_meta_outputs/country_tp_bootstrap_summary_wide.csv"),
        ("overall_meta", read_csv(BASE_DIR / "02.tp_meta_outputs/random_effects_meta_analysis_summary.csv"), "02.tp_meta_outputs/random_effects_meta_analysis_summary.csv"),
        ("development_subgroup", read_csv(BASE_DIR / "02.tp_meta_outputs/development_group_subgroup_meta_summary.csv"), "02.tp_meta_outputs/development_group_subgroup_meta_summary.csv"),
        ("development_test", read_csv(BASE_DIR / "02.tp_meta_outputs/development_group_meta_regression_test.csv"), "02.tp_meta_outputs/development_group_meta_regression_test.csv"),
        ("stratified_tp", read_csv(BASE_DIR / "03.stratified_equity_outputs/stratified_tp_summary.csv"), "03.stratified_equity_outputs/stratified_tp_summary.csv"),
        ("permutation_pvalues", read_csv(BASE_DIR / "03.stratified_equity_outputs/stratified_permutation_pvalues.csv"), "03.stratified_equity_outputs/stratified_permutation_pvalues.csv"),
        ("stratified_reviewer", read_csv(BASE_DIR / "03.stratified_equity_outputs/reviewer_stratified_tp_table.csv"), "03.stratified_equity_outputs/reviewer_stratified_tp_table.csv"),
        ("income_source", read_csv(BASE_DIR / "03.stratified_equity_outputs/income_source_selection.csv"), "03.stratified_equity_outputs/income_source_selection.csv"),
        ("context_indicators", read_csv(BASE_DIR / "05.country_context_attribution_outputs/country_context_clean.csv"), "05.country_context_attribution_outputs/country_context_clean.csv"),
        ("context_qc", read_csv(BASE_DIR / "05.country_context_attribution_outputs/country_context_qc_report.csv"), "05.country_context_attribution_outputs/country_context_qc_report.csv"),
        ("spearman", read_csv(BASE_DIR / "05.country_context_attribution_outputs/spearman_correlation_results.csv"), "05.country_context_attribution_outputs/spearman_correlation_results.csv"),
        ("context_meta_reg", read_csv(BASE_DIR / "05.country_context_attribution_outputs/meta_regression_results.csv"), "05.country_context_attribution_outputs/meta_regression_results.csv"),
        ("context_ols", read_csv(BASE_DIR / "05.country_context_attribution_outputs/unweighted_ols_sensitivity_results.csv"), "05.country_context_attribution_outputs/unweighted_ols_sensitivity_results.csv"),
        ("raw_scale_sens", read_csv(BASE_DIR / "05.country_context_attribution_outputs/sensitivity_raw_scale_meta_regression_results.csv"), "05.country_context_attribution_outputs/sensitivity_raw_scale_meta_regression_results.csv"),
        ("floor_effect_sens", read_csv(BASE_DIR / "05.country_context_attribution_outputs/sensitivity_exclude_floor_tp_meta_regression_results.csv"), "05.country_context_attribution_outputs/sensitivity_exclude_floor_tp_meta_regression_results.csv"),
        ("leave_one_out", read_csv(BASE_DIR / "05.country_context_attribution_outputs/leave_one_country_out_results.csv"), "05.country_context_attribution_outputs/leave_one_country_out_results.csv"),
        ("context_analysis_qc", read_csv(BASE_DIR / "05.country_context_attribution_outputs/country_context_analysis_qc.csv"), "05.country_context_attribution_outputs/country_context_analysis_qc.csv"),
    ]

    for name, df, source in sheets:
        add_sheet_from_df(wb, name, df, source)

    wb.save(XLSX_PATH)


def save_methods_files():
    MD_PATH.write_text(METHODS_TEXT + "\n", encoding="utf-8")
    doc = Document()
    styles = doc.styles
    styles["Normal"].font.name = "Times New Roman"
    styles["Normal"]._element.rPr.rFonts.set(qn("w:eastAsia"), "Times New Roman")
    styles["Normal"].font.size = Pt(11)

    title = doc.add_paragraph()
    title.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = title.add_run("Methods: Multinational External Validation and Country-Level Attribution Analyses")
    run.bold = True
    run.font.name = "Times New Roman"
    run._element.rPr.rFonts.set(qn("w:eastAsia"), "Times New Roman")
    run.font.size = Pt(14)

    for block in METHODS_TEXT.split("\n\n"):
        if len(block.split()) < 12 and not block.endswith("."):
            p = doc.add_paragraph()
            r = p.add_run(block)
            r.bold = True
            r.font.name = "Times New Roman"
            r._element.rPr.rFonts.set(qn("w:eastAsia"), "Times New Roman")
            r.font.size = Pt(12)
        else:
            p = doc.add_paragraph(block)
            p.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
            p.paragraph_format.space_after = Pt(6)
            for r in p.runs:
                r.font.name = "Times New Roman"
                r._element.rPr.rFonts.set(qn("w:eastAsia"), "Times New Roman")
                r.font.size = Pt(11)
    doc.save(DOCX_PATH)


def verify_workbook():
    wb = load_workbook(XLSX_PATH, read_only=True, data_only=True)
    summary = []
    for ws in wb.worksheets:
        summary.append((ws.title, ws.max_row, ws.max_column))
    return summary


def main():
    save_methods_files()
    make_workbook()
    summary = verify_workbook()
    print(f"Saved methods DOCX: {DOCX_PATH}")
    print(f"Saved methods Markdown: {MD_PATH}")
    print(f"Saved workbook: {XLSX_PATH}")
    print("Workbook sheets:")
    for title, rows, cols in summary:
        print(f"  {title}: {rows} rows x {cols} cols")


if __name__ == "__main__":
    main()
