# --- Setup ---
rm(list = ls())
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE
  # results = "hide"
)

# Load Libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(systemfit)
library(car)
library(broom)
library(qualtRics)
library(knitr)
library(rmarkdown)
library(ggplot2)
library(grid)
library(readxl)
library(tidyverse)
library(stringr) # For str_wrap

# Set options
options(scipen = 999)

# --- Data Loading ---
cat("--- Loading Data ---\n")
tryCatch({
  korea <- fetch_survey(surveyID = 'SV_dmB7eDS3HuQEuHk', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter((!is.na(Q39) | !is.na(PROLIFIC_PID)) & !is.na(cond)) |> mutate(country_source = "South Korea") |> arrange(StartDate) |> group_by(Q39) |> filter(is.na(Q39) | row_number() == 1) |> ungroup() |> slice_head(n=375)
  china <- fetch_survey(surveyID = 'SV_2i88cD6n2mANcdE', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter(!is.na(PROLIFIC_PID) & !is.na(cond)) |> mutate(country_source = "China") |> slice_head(n=375)
  italy <- fetch_survey(surveyID = 'SV_egNzgDfVDqBeCgu', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter(!is.na(PROLIFIC_PID) & !is.na(cond)) |> mutate(country_source = "Italy") |> slice_head(n=375)
  germany <- fetch_survey(surveyID = 'SV_8tUjiDlGKbcM7RA', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter(!is.na(PROLIFIC_PID) & !is.na(cond)) |> mutate(country_source = "Germany") |> slice_head(n=375)
}, error = function(e) { stop("Error fetching survey data: ", e$message) })
cat("Qualtrics data fetched.\n")

metadata_file <- "Academic -- dataset.xlsx"
tryCatch({ scholar_metadata_raw <- read_excel(metadata_file); cat("Metadata loaded.\n") }, error = function(e) { stop(paste("Error loading metadata:", metadata_file)) })

# --- Data Cleaning and Preprocessing ---
cat("--- Cleaning and Preprocessing Data ---\n")
scholar_lookup <- scholar_metadata_raw %>%
  mutate(
    Region = case_when(Citizenship %in% c("China", "South_Korea", "Thailand") ~ "Eastern", Citizenship %in% c("Germany", "Italy", "United_Kingdom") ~ "Western", TRUE ~ NA_character_),
    Nationality = Citizenship, Gender = as.factor(Gender), First_name = as.character(First_name)
  ) %>% select(First_name, Gender, Nationality, Region) %>% distinct(First_name, .keep_all = TRUE)

all_data_raw <- bind_rows(korea, china, italy, germany)
n_raw <- nrow(all_data_raw); cat("Initial N:", n_raw, "\n")

# --- REMOVED DEDUPLICATION STEP ---
cat("--- NOTE: Deduplication step based on PROLIFIC_PID has been removed. Using all rows meeting initial filters. ---\n")
data_after_initial_filters <- all_data_raw
n_after_initial <- nrow(data_after_initial_filters); cat("N after initial filters:", n_after_initial, "\n")

data_passed_attn <- data_after_initial_filters # Placeholder for attention checks
data_pre_analysis <- data_passed_attn %>% mutate(choice7 = as.character(choice7))
dropped_after_condition <- data_pre_analysis %>% filter(is.na(choice7) | choice7 == "")
n_dropped_late <- nrow(dropped_after_condition); cat("N dropped late:", n_dropped_late, "\n")
analysis_data_unmerged <- data_pre_analysis %>% filter(!is.na(choice7) & choice7 != "")
n_unmerged <- nrow(analysis_data_unmerged); cat("N before merge:", n_unmerged, "\n")
analysis_data <- analysis_data_unmerged %>% left_join(scholar_lookup, by = c("choice7" = "First_name"))
n_analysis <- nrow(analysis_data); cat("N for primary analysis:", n_analysis, "\n")

# --- Variable Creation ---
cat("--- Creating Analysis Variables ---\n")
analysis_data <- analysis_data %>%
  mutate(
    western_female = ifelse(!is.na(Gender) & Gender == "Female" & Region == "Western", 1, 0),
    eastern_female = ifelse(!is.na(Gender) & Gender == "Female" & Region == "Eastern", 1, 0),
    gender_feedback = ifelse(cond == "treat", 1, 0),
    western_participant = ifelse(country %in% c("Italy", "Germany"), 1, 0),
    eastern_participant = 1 - western_participant,
    Chinese_female = ifelse(!is.na(Gender) & Gender == "Female" & Nationality == "China", 1, 0),
    SouthKorean_female = ifelse(!is.na(Gender) & Gender == "Female" & Nationality == "South_Korea", 1, 0),
    Italian_female = ifelse(!is.na(Gender) & Gender == "Female" & Nationality == "Italy", 1, 0),
    German_female = ifelse(!is.na(Gender) & Gender == "Female" & Nationality == "Germany", 1, 0),
    is_chinese_participant = ifelse(country == "China", 1, 0),
    is_korean_participant = ifelse(country == "South Korea", 1, 0),
    is_italian_participant = ifelse(country == "Italy", 1, 0),
    is_german_participant = ifelse(country == "Germany", 1, 0),
    gender_male = ifelse(gender == "Man", 1, 0),
    race_white = ifelse(race == "White / Caucasian", 1, 0),
    age = as.numeric(age)
  )

# --- Descriptive Statistics ---
cat("\n--- Descriptive Statistics (N =", n_analysis, ") ---\n")
cat("\nCondition Assignment Check:\n"); print(table(analysis_data$gender_feedback, useNA = "ifany"))

# --- Helper Functions ---
get_stars <- function(p_value) { if (is.na(p_value)) return(""); if (p_value < 0.001) return("***"); if (p_value < 0.01) return("**"); if (p_value < 0.05) return("*"); return("n.s.") }
format_p_caption <- function(p_value){ if (is.na(p_value)) return("NA"); if (p_value < 0.001) return("p < .001"); return(paste0("p = ", format(round(p_value, 3), nsmall = 3))) }

# --- PRIMARY ANALYSIS ---
cat("\n--- Running Primary Analysis (H1 & H2) ---\n")
fixed_effects_terms <- ""
model_h1 <- NULL; wald_h1 <- NULL; model_h2 <- NULL; wald_h2 <- NULL
interaction_term_west <- "gender_feedback:western_participant"
interaction_term_east <- "gender_feedback:eastern_participant"

# H1 Model & Test
h1_formula_str <- paste("list(western = western_female ~ gender_feedback + western_participant", fixed_effects_terms, ",", "eastern = eastern_female ~ gender_feedback + eastern_participant", fixed_effects_terms, ")")
h1_formula <- eval(parse(text = h1_formula_str))
tryCatch({
  model_h1 <- systemfit(h1_formula, method = "SUR", data = analysis_data)
  wald_h1 <- linearHypothesis(model_h1,"western_gender_feedback - eastern_gender_feedback = 0", test = "Chisq")
  cat("\n--- H1 Model & Wald Test Ran ---\n")
}, error = function(e) { cat("\nError H1 SUR:\n"); print(e) })

# H2 Model & Test
h2_formula_str <- paste("list(western = western_female ~ gender_feedback * western_participant", fixed_effects_terms, ",", "eastern = eastern_female ~ gender_feedback * eastern_participant", fixed_effects_terms, ")")
h2_formula <- eval(parse(text = h2_formula_str))
tryCatch({
  model_h2 <- systemfit(h2_formula, method = "SUR", data = analysis_data)
  wald_h2 <- linearHypothesis(model_h2, paste0("western_", interaction_term_west, " - eastern_", interaction_term_east, " = 0"), test = "Chisq")
  cat("\n--- H2 Model & Wald Test Ran ---\n")
}, error = function(e) { cat("\nError H2 SUR:\n"); print(e) })

# --- Extract Stats for Plotting ---
cat("--- Extracting Stats for Plotting ---\n")
p_wald_h1 <- NA; p_wald_h2_comparison <- NA
p_feedback_simple_eastP_westS <- NA; p_feedback_simple_eastP_eastS <- NA
p_feedback_simple_westP_westS <- NA; p_feedback_simple_westP_eastS <- NA
if (!is.null(wald_h1)) { p_wald_h1 <- wald_h1$"Pr(>Chisq)"[2] } else {warning("H1 Wald results missing")}
if (!is.null(model_h2)) {
  summary_h2 <- summary(model_h2)
  if (!is.null(wald_h2)) { p_wald_h2_comparison <- wald_h2$"Pr(>Chisq)"[2] } else {warning("H2 Wald results missing")}
  if("gender_feedback" %in% rownames(coef(summary_h2$eq[[1]]))) { p_feedback_simple_eastP_westS <- coef(summary_h2$eq[[1]])["gender_feedback", "Pr(>|t|)"] } else { warning("Coef 'gender_feedback' not found in model_h2 eq1") }
  if("gender_feedback" %in% rownames(coef(summary_h2$eq[[2]]))) { p_feedback_simple_eastP_eastS <- coef(summary_h2$eq[[2]])["gender_feedback", "Pr(>|t|)"] } else { warning("Coef 'gender_feedback' not found in model_h2 eq2") }
  tryCatch({ test_westP_westS <- linearHypothesis(model_h2, "western_gender_feedback + western_gender_feedback:western_participant = 0", test = "Chisq"); p_feedback_simple_westP_westS <- test_westP_westS$"Pr(>Chisq)"[2] }, error=function(e) {warning("Could not calculate simple effect WpWs")})
  tryCatch({ test_westP_eastS <- linearHypothesis(model_h2, "eastern_gender_feedback + eastern_gender_feedback:eastern_participant = 0", test = "Chisq"); p_feedback_simple_westP_eastS <- test_westP_eastS$"Pr(>Chisq)"[2] }, error=function(e) {warning("Could not calculate simple effect WpEs")})
} else { warning("H2 model object is NULL.") }


# --- OTHER ANALYSIS (Country-Specific) ---
cat("\n--- Country-Specific Analysis (OLS with Robust SEs) ---\n")
# *** MODIFIED FUNCTION: Removed the minimum group size check ***
run_country_model <- function(dv_name, participant_dummy_name, data) {
  formula_str <- paste0(dv_name, " ~ gender_feedback * ", participant_dummy_name)
  model_results <- NULL
  tryCatch({
    model <- lm(as.formula(formula_str), data = data)
    # Check if model coefficients could be estimated (might fail with perfect separation/collinearity)
    if (any(is.na(coef(model)))) {
      cat("\n--- Skipping Model for DV:", dv_name, "due to NA coefficients (likely perfect separation or collinearity) ---\n")
      return(NULL)
    }
    cat("\n--- Model for DV:", dv_name, "---\n")
    robust_summary <- coeftest(model, vcov. = vcovHC(model, type = "HC3"))
    print(robust_summary)
    model_results <- list(model = model, robust_summary = robust_summary)
  }, error = function(e) {
    cat("\nError running country-specific LM model for:", dv_name, "\n")
    print(e)
  })
  return(model_results)
}
# Run models
model_chinese_results <- run_country_model("Chinese_female", "is_chinese_participant", analysis_data)
model_korean_results <- run_country_model("SouthKorean_female", "is_korean_participant", analysis_data)
model_italian_results <- run_country_model("Italian_female", "is_italian_participant", analysis_data)
model_german_results <- run_country_model("German_female", "is_german_participant", analysis_data)
# Extract coefficients
get_coef_estimate <- function(model_results, term_name) { est <- NA; if (!is.null(model_results) && !is.null(model_results$robust_summary) && term_name %in% rownames(model_results$robust_summary)) { est <- model_results$robust_summary[term_name, "Estimate"] }; return(est) }
coef_main_feedback <- c( Chinese = get_coef_estimate(model_chinese_results, "gender_feedback"), Korean = get_coef_estimate(model_korean_results, "gender_feedback"), Italian = get_coef_estimate(model_italian_results, "gender_feedback"), German = get_coef_estimate(model_german_results, "gender_feedback") )
coef_interaction <- c( Chinese = get_coef_estimate(model_chinese_results, "gender_feedback:is_chinese_participant"), Korean = get_coef_estimate(model_korean_results, "gender_feedback:is_korean_participant"), Italian = get_coef_estimate(model_italian_results, "gender_feedback:is_italian_participant"), German = get_coef_estimate(model_german_results, "gender_feedback:is_german_participant") )
cat("\nMain effect of gender_feedback on selecting own-country female (Robust Estimates):\n"); print(coef_main_feedback)
cat("\nInteraction effect (gender_feedback * is_own_country_participant) (Robust Estimates):\n"); print(coef_interaction)


# --- ROBUSTNESS CHECKS ---
cat("\n--- Robustness Checks ---\n")

# 1. Primary Models with SIMPLIFIED Demographic Controls
cat("--- Robustness Check: H1 & H2 with Simplified Demographics ---\n")
demographic_controls_simple <- "+ gender_male + race_white + age"

# H1 with simplified demographics
h1_demog_formula_str_simple <- paste( "list(western = western_female ~ gender_feedback + western_participant", fixed_effects_terms, demographic_controls_simple, ",", "eastern = eastern_female ~ gender_feedback + eastern_participant", fixed_effects_terms, demographic_controls_simple, ")")
h1_demog_formula_simple <- eval(parse(text = h1_demog_formula_str_simple))
tryCatch({
  analysis_data_demog <- analysis_data %>% filter(!is.na(gender_male) & !is.na(race_white) & !is.na(age))
  n_demog <- nrow(analysis_data_demog)
  cat("N for demographic robustness check:", n_demog, "(", n_analysis - n_demog, "rows removed due to missing demographics)\n")
  if(n_demog > 10){
    model_h1_demog_simple <- systemfit(h1_demog_formula_simple, method = "SUR", data = analysis_data_demog)
    cat("\n--- H1 Model with Simplified Demographics ---\n"); print(summary(model_h1_demog_simple))
    wald_h1_demog_simple <- linearHypothesis(model_h1_demog_simple, "western_gender_feedback - eastern_gender_feedback = 0", test = "Chisq")
    cat("Wald Test (H1 w/ Simplified Demographics): p-value =", wald_h1_demog_simple$"Pr(>Chisq)"[2], "\n")
  } else { cat("Skipping H1 demographic robustness check due to insufficient data after NA removal.\n") }
}, error = function(e) { cat("\nError H1 SUR model w/ Simplified Demographics:\n"); print(e) })

# H2 with simplified demographics
h2_demog_formula_str_simple <- paste( "list(western = western_female ~ gender_feedback * western_participant", fixed_effects_terms, demographic_controls_simple, ",", "eastern = eastern_female ~ gender_feedback * eastern_participant", fixed_effects_terms, demographic_controls_simple, ")")
h2_demog_formula_simple <- eval(parse(text = h2_demog_formula_str_simple))
tryCatch({
  analysis_data_demog <- analysis_data %>% filter(!is.na(gender_male) & !is.na(race_white) & !is.na(age))
  n_demog <- nrow(analysis_data_demog)
  if(n_demog > 10){
    model_h2_demog_simple <- systemfit(h2_demog_formula_simple, method = "SUR", data = analysis_data_demog)
    cat("\n--- H2 Model with Simplified Demographics ---\n"); print(summary(model_h2_demog_simple))
    wald_h2_demog_simple <- linearHypothesis(model_h2_demog_simple, paste0("western_", interaction_term_west, " - eastern_", interaction_term_east, " = 0"), test = "Chisq")
    cat("Wald Test (H2 w/ Simplified Demographics): p-value =", wald_h2_demog_simple$"Pr(>Chisq)"[2], "\n")
  } else { cat("Skipping H2 demographic robustness check due to insufficient data after NA removal.\n") }
}, error = function(e) { cat("\nError H2 SUR model w/ Simplified Demographics:\n"); print(e) })
cat("--- NOTE: Demographic models exclude participants with missing gender, race, or age. ---\n")

# 2. Handling Dropouts
cat("\n--- Robustness Check: Dropout Analysis (H1 Base Model) ---\n")
h1_formula_base <- list( western = western_female ~ gender_feedback + western_participant, eastern = eastern_female ~ gender_feedback + eastern_participant )
run_dropout_analysis <- function(assumption_label, df_dropped_modified) {
  cat("\nDropout Robustness: Assuming", assumption_label, "\n")
  cols_to_keep <- c("PROLIFIC_PID", "ResponseId", "western_female", "eastern_female", "gender_feedback", "western_participant", "eastern_participant", "country", "cond")
  # Use analysis_data defined *after* removing duplicates if that was intended, or data_after_initial_filters if no deduplication
  analysis_data_slim <- analysis_data %>% select(any_of(cols_to_keep)) # Using analysis_data which includes duplicates now
  df_dropped_slim <- df_dropped_modified %>% select(any_of(cols_to_keep))
  analysis_data_slim <- analysis_data_slim %>% mutate(across(where(is.factor), as.character))
  df_dropped_slim <- df_dropped_slim %>% mutate(across(where(is.factor), as.character))
  combined_data <- bind_rows(analysis_data_slim, df_dropped_slim)
  p_value <- NA
  tryCatch({
    model_dropout <- systemfit(h1_formula_base, method = "SUR", data = combined_data)
    wald_dropout <- linearHypothesis(model_dropout, "western_gender_feedback - eastern_gender_feedback = 0", test = "Chisq")
    p_value <- wald_dropout$"Pr(>Chisq)"[2]
    cat("Wald Test (H1 dropout -", assumption_label, "): p =", format.pval(p_value, digits=3, eps=0.001), "\n")
  }, error = function(e) { cat("Error running Wald test for dropout assumption:", assumption_label, "\n"); print(e) })
  return(p_value)
}
dropped_after_condition_prep <- dropped_after_condition %>% mutate( western_participant = ifelse(country %in% c("Italy", "Germany"), 1, 0), eastern_participant = 1 - western_participant, gender_feedback = ifelse(cond == "treat", 1, 0) )
dropped_male <- dropped_after_condition_prep %>% mutate(western_female = 0, eastern_female = 0); p_drop_male <- run_dropout_analysis("Male", dropped_male)
dropped_eastern <- dropped_after_condition_prep %>% mutate(western_female = 0, eastern_female = 1); p_drop_eastern <- run_dropout_analysis("Eastern Female", dropped_eastern)
dropped_western <- dropped_after_condition_prep %>% mutate(western_female = 1, eastern_female = 0); p_drop_western <- run_dropout_analysis("Western Female", dropped_western)

# --- GENDER RECOGNITION ANALYSIS ---
cat("\n--- Gender Recognition Analysis ---\n")
gr_cols <- paste0("Q33_", 1:24)
missing_gr_cols <- setdiff(gr_cols, names(analysis_data))
if (length(missing_gr_cols) > 0) {
  warning("Missing required Gender Recognition columns (Q33_1:24): ", paste(missing_gr_cols, collapse=", "))
  cat("Skipping Gender Recognition Analysis due to missing Q33 columns.\n")
} else {
  # Reshape data
  analysis_data_long_gr <- analysis_data %>%
    select(PROLIFIC_PID, ResponseId, western_participant, gender_feedback, all_of(gr_cols)) %>%
    pivot_longer( cols = all_of(gr_cols), names_to = "Scholar_Rated_Column", values_to = "Gender_Rating") %>%
    mutate(
      unrecognized_rating = ifelse(tolower(Gender_Rating) %in% c("gender-neutral", "unknown"), 1, 0),
      unrecognized_rating = ifelse(is.na(Gender_Rating) | Gender_Rating == "" | !tolower(Gender_Rating) %in% c("female", "male", "gender-neutral", "unknown"), NA, unrecognized_rating)
    )
  # Calculate P_gender_unrecognized
  participant_gr_summary <- analysis_data_long_gr %>%
    filter(!is.na(unrecognized_rating)) %>% group_by(PROLIFIC_PID) %>%
    summarise( n_ratings = n(), n_unrecognized = sum(unrecognized_rating), P_gender_unrecognized = ifelse(n_ratings > 0, n_unrecognized / n_ratings, 0) ) %>% ungroup()
  # Merge back
  if (nrow(participant_gr_summary) == 0) {
    warning("P_gender_unrecognized calculation failed.")
    analysis_data$P_gender_unrecognized <- NA
  } else {
    analysis_data <- analysis_data %>%
      left_join(participant_gr_summary %>% select(PROLIFIC_PID, P_gender_unrecognized), by = "PROLIFIC_PID") %>%
      mutate(P_gender_unrecognized = ifelse(is.na(P_gender_unrecognized), 0, P_gender_unrecognized))
  }
  
  # Name-level analysis placeholder
  cat("\n--- Name-Level Gender Recognition Analysis (Requires Mapping Q33_x to Scholar Region) ---\n")
  cat("Code for name-level t-tests and regressions requires a mapping file/dataframe.\n")
  
  # Feedback Treatment Interaction with P_gender_unrecognized
  cat("\n--- Feedback Treatment Interaction with Participant Gender Recognition Rate ---\n")
  analysis_data <- analysis_data %>% mutate(female_pick = ifelse(!is.na(Gender) & Gender == "Female", 1, 0))
  if ("P_gender_unrecognized" %in% names(analysis_data) && !all(is.na(analysis_data$P_gender_unrecognized))) {
    if (var(analysis_data$P_gender_unrecognized, na.rm = TRUE) < 1e-10) {
      cat("Skipping feedback interaction: P_gender_unrecognized has zero or near-zero variance.\n")
    } else {
      model_gr_feedback_interaction <- lm(female_pick ~ gender_feedback * P_gender_unrecognized, data = analysis_data)
      cat("\nModel: female_pick ~ gender_feedback * P_gender_unrecognized (All Participants)\n"); print(coeftest(model_gr_feedback_interaction, vcov. = vcovHC(model_gr_feedback_interaction, type = "HC3")))
      data_west_p <- filter(analysis_data, western_participant == 1)
      if(nrow(data_west_p) > 10 && n_distinct(data_west_p$gender_feedback) > 1 && var(data_west_p$P_gender_unrecognized, na.rm = TRUE) > 1e-10) {
        model_gr_feedback_interaction_west <- lm(female_pick ~ gender_feedback * P_gender_unrecognized, data = data_west_p)
        cat("\nModel (Western Participants Only)\n"); print(coeftest(model_gr_feedback_interaction_west, vcov. = vcovHC(model_gr_feedback_interaction_west, type = "HC3")))
      } else { cat("Insufficient data/variation for Western participants GR robustness check.\n")}
      data_east_p <- filter(analysis_data, western_participant == 0)
      if(nrow(data_east_p) > 10 && n_distinct(data_east_p$gender_feedback) > 1 && var(data_east_p$P_gender_unrecognized, na.rm = TRUE) > 1e-10) {
        model_gr_feedback_interaction_east <- lm(female_pick ~ gender_feedback * P_gender_unrecognized, data = data_east_p)
        cat("\nModel (Eastern Participants Only)\n"); print(coeftest(model_gr_feedback_interaction_east, vcov. = vcovHC(model_gr_feedback_interaction_east, type = "HC3")))
      } else { cat("Insufficient data/variation for Eastern participants GR robustness check.\n")}
    }
  } else { cat("Skipping feedback interaction: P_gender_unrecognized missing/invalid.\n") }
}


# --- Generating Plot with Annotations (Manual Approach v9 - No Deduplication) ---
cat("\n--- Generating Final Plot ---\n")

tryCatch({
  # Calculate summary stats
  plot_data_summary <- analysis_data %>%
    group_by(gender_feedback, western_participant) %>%
    summarise(
      mean_west_female = mean(western_female, na.rm = TRUE),
      se_west_female = sd(western_female, na.rm = TRUE) / sqrt(n()),
      mean_east_female = mean(eastern_female, na.rm = TRUE),
      se_east_female = sd(eastern_female, na.rm = TRUE) / sqrt(n()),
      n_group = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      Feedback = factor(if_else(gender_feedback == 1, "Feedback provided", "No feedback"), levels = c("No feedback", "Feedback provided")),
      Participant_Region = factor(if_else(western_participant == 1, "Western Participants", "Eastern Participants"), levels = c("Eastern Participants", "Western Participants"))
    ) %>%
    pivot_longer(
      cols = starts_with("mean_") | starts_with("se_"),
      names_to = c(".value", "Scholar_Region"),
      names_pattern = "(mean|se)_(west|east)_female"
    ) |>
    mutate(
      Scholar_Region = factor(case_when(Scholar_Region == "west" ~ "Western Scholar", Scholar_Region == "east" ~ "Eastern Scholar", TRUE ~ Scholar_Region), levels = c("Eastern Scholar", "Western Scholar")),
      text_y = mean * 100 * 0.5,
      error_bar_top = (mean + se) * 100,
      x_group_center = ifelse(Scholar_Region == "Eastern Scholar", 1, 2)
    ) %>%
    group_by(Participant_Region, Scholar_Region) %>%
    mutate(bracket_y_base = max(error_bar_top, na.rm = TRUE) + 1.5) %>%
    mutate(bracket_y_final = bracket_y_base + 2.0) %>%
    ungroup()
  
  # Data frame for annotations
  annotation_data <- data.frame(
    Participant_Region = factor(rep(c("Eastern Participants", "Western Participants"), each=2), levels=levels(plot_data_summary$Participant_Region)),
    Scholar_Region = factor(rep(c("Eastern Scholar", "Western Scholar"), 2), levels=levels(plot_data_summary$Scholar_Region)),
    p_simple_effect = c(p_feedback_simple_eastP_eastS, p_feedback_simple_eastP_westS,
                        p_feedback_simple_westP_eastS, p_feedback_simple_westP_westS)
  ) %>%
    mutate(stars = sapply(p_simple_effect, get_stars)) %>%
    left_join(plot_data_summary %>%
                distinct(Participant_Region, Scholar_Region, bracket_y_final, bracket_y_base, x_group_center),
              by = c("Participant_Region", "Scholar_Region")) %>%
    mutate(
      dodge_width = 0.9,
      x_pos_1 = x_group_center - dodge_width/4,
      x_pos_2 = x_group_center + dodge_width/4
    )
  
  # Determine overall max y for plot limits
  plot_max_y <- max(c(plot_data_summary$error_bar_top, annotation_data$bracket_y_final), na.rm = TRUE) * 1.30
  
  # Data frame for H1/H2 summary text annotations
  h_summary_text_data <- data.frame(
    Participant_Region = factor(c("Eastern Participants"), levels = levels(annotation_data$Participant_Region)),
    x_pos = 1.5,
    y_pos = plot_max_y * 0.97,
    label = paste(
      sprintf("H1 Wald (Effect Diff W vs E): %s %s", format_p_caption(p_wald_h1), get_stars(p_wald_h1)),
      sprintf("H2 Wald (Interaction Diff.): %s %s", format_p_caption(p_wald_h2_comparison), get_stars(p_wald_h2_comparison)),
      sep="\n")
  )
  
  # Create the final plot
  plot_final <- ggplot(plot_data_summary, aes(x = Scholar_Region, y = mean * 100, fill = Feedback)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8, color="grey20", linewidth=0.3) +
    geom_errorbar(aes(ymin = pmax(0, (mean - se) * 100), ymax = error_bar_top),
                  position = position_dodge(width = 0.9), width = 0.25, linewidth=0.7, color="gray30") +
    geom_text(aes(label = sprintf("%.1f%%", mean * 100)),
              position = position_dodge(width = 0.9),
              vjust = 1.5, size = 3.5, color = "white", fontface="bold") +
    facet_wrap(~ Participant_Region) +
    scale_fill_manual(values = c("No feedback" = "navyblue", "Feedback provided" = "firebrick"), name = "Gender Feedback Status:") +
    scale_y_continuous(limits = c(0, plot_max_y), expand = expansion(mult = c(0, 0.05))) +
    
    # Draw Brackets Manually
    geom_segment(data = annotation_data, aes(x = x_pos_1, xend = x_pos_1, y = bracket_y_base, yend = bracket_y_final), inherit.aes = FALSE, linewidth=0.7, color="black") +
    geom_segment(data = annotation_data, aes(x = x_pos_2, xend = x_pos_2, y = bracket_y_base, yend = bracket_y_final), inherit.aes = FALSE, linewidth=0.7, color="black") +
    geom_segment(data = annotation_data, aes(x = x_pos_1, xend = x_pos_2, y = bracket_y_final, yend = bracket_y_final), inherit.aes = FALSE, linewidth=1, color="black") +
    # Add Stars above brackets for Simple Effects
    geom_text(data = annotation_data, aes(x = (x_pos_1 + x_pos_2)/2, y = bracket_y_final, label = stars),
              inherit.aes = FALSE, size = 7, vjust = -0.5, fontface="bold", color="black") +
    
    # Add H1/H2 Summary Text Annotations
    geom_text(data = h_summary_text_data, aes(x = x_pos, y = y_pos, label = label),
              inherit.aes = FALSE, size = 3.8, hjust = 0.5, vjust = 1, fontface = "italic", lineheight=1.1, color="black") +
    
    labs(
      title = "Effect of Gender Feedback on Selecting Female Scholars",
      subtitle = "Comparing Feedback vs. No Feedback by Participant and Scholar Origin",
      y = "Percentage Selecting Female Scholar (%)",
      x = "Origin of Female Scholar Selected",
      caption = str_wrap("Plot shows mean percentage ± SE. Brackets compare simple effect of Feedback vs. No Feedback within each group (significance stars based on H2 SUR model, robust SEs: *** p<.001, * p<.05, n.s. not significant).", width = 100)
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color="black", linewidth = 0.5),
      plot.title = element_text(hjust = 0.5, face="bold", size=16, margin=margin(b=5)),
      plot.subtitle = element_text(hjust = 0.5, size=12, margin=margin(b=20)),
      strip.background = element_rect(fill="grey90", color="black", linewidth = 0.5),
      strip.text = element_text(face="bold", size=12),
      legend.position = "top",
      legend.title = element_text(size=11, face="bold"),
      legend.text = element_text(size=10),
      axis.title.x = element_text(size=12, face="bold", margin = margin(t = 15)),
      axis.title.y = element_text(size=12, face="bold", margin = margin(r = 10)),
      axis.text = element_text(color="black", size=11),
      axis.ticks = element_line(color="black", linewidth=0.5),
      plot.caption = element_text(hjust = 0, size=9.5, face="italic", lineheight = 1.2, margin=margin(t=10)),
      plot.margin = margin(t = 15, r = 20, b = 50, l = 15) # Adjusted bottom margin
    )
  
  print(plot_final)
  ggsave("study2_final_plot_no_dedup.png", plot_final, width = 10.5, height = 7.5, dpi=300)
  cat("\nPlot saved as study2_final_plot_no_dedup.png\n")
  
}, error = function(e) {
  cat("\nError generating plot. Check summary data, p-values, and ggplot code.\n")
  print(e)
})

print("--- Analysis Script Completed ---")


# --- PRELIMINARY STEP: Generate the Raw CSV ---
# Load necessary libraries ONLY for this step
library(dplyr)
library(qualtRics)
library(readxl) # Needed if you re-run this after clearing env

# Fetch data (same as before)
korea <- fetch_survey(surveyID = 'SV_dmB7eDS3HuQEuHk', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter((!is.na(Q39) | !is.na(PROLIFIC_PID)) & !is.na(cond)) |> mutate(country_source = "South Korea") |> arrange(StartDate) |> group_by(Q39) |> filter(is.na(Q39) | row_number() == 1) |> ungroup() |> slice_head(n=375)
china <- fetch_survey(surveyID = 'SV_2i88cD6n2mANcdE', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter(!is.na(PROLIFIC_PID) & !is.na(cond)) |> mutate(country_source = "China") |> slice_head(n=375)
italy <- fetch_survey(surveyID = 'SV_egNzgDfVDqBeCgu', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter(!is.na(PROLIFIC_PID) & !is.na(cond)) |> mutate(country_source = "Italy") |> slice_head(n=375)
germany <- fetch_survey(surveyID = 'SV_8tUjiDlGKbcM7RA', label = TRUE, convert = FALSE, start_date = "2025-03-07", force_request = TRUE) |> filter(!is.na(PROLIFIC_PID) & !is.na(cond)) |> mutate(country_source = "Germany") |> slice_head(n=375)

# Combine raw data
all_data_raw <- bind_rows(korea, china, italy, germany)

# Write the combined raw data to CSV
# Ensure file path is correct for your working directory
write.csv(all_data_raw, "combined_study_data_N1500.csv", row.names = FALSE, na = "") # Write NA as empty string
cat("File 'combined_study_data_N1500.csv' created with", nrow(all_data_raw), "rows.\n")

# Clear environment before running the Rmd (optional, but good practice)
# rm(list = ls())
