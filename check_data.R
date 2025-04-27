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
# (Data loading code remains the same)
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
data_deduplicated <- all_data_raw %>% arrange(RecordedDate) %>% distinct(PROLIFIC_PID, .keep_all = TRUE)
n_after_dedup <- nrow(data_deduplicated); cat("N after deduplication:", n_after_dedup, "\n")
data_passed_attn <- data_deduplicated # Placeholder
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
    # Demographics & Missing Indicators (using imputed age later)
    gender_code = case_when(gender == "Man" ~ 1, gender == "Woman" ~ 0, TRUE ~ NA_real_),
    race_code_white = case_when(race == "White / Caucasian" ~ 1, !is.na(race) & race != "" ~ 0, TRUE ~ NA_real_),
    age = as.numeric(age),
    gender_missing = ifelse(is.na(gender_code) | gender %in% c(""," ", NA, "Non-binary"), 1, 0),
    race_missing = ifelse(is.na(race_code_white) | race %in% c(""," ", NA), 1, 0),
    age_missing = ifelse(is.na(age), 1, 0),
    gender_code = ifelse(gender_missing == 1, 0, gender_code),
    race_code_white = ifelse(race_missing == 1, 0, race_code_white),
    age_imputed = ifelse(age_missing == 1, mean(age, na.rm = TRUE), age)
  )

# --- Descriptive Statistics ---
# ...(Omitted for brevity)...
cat("\n--- Condition Assignment Check ---\n")
print(table(analysis_data$gender_feedback, useNA = "ifany"))

# --- Helper Functions ---
# Function to get significance stars
get_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("n.s.")
}

# Function to format p-values for plot annotations
format_p_plot <- function(p_value) {
  stars <- get_stars(p_value)
  if (is.na(p_value)) return("")
  if (stars == "n.s.") {
    return("n.s.")
  } else {
    if (p_value < 0.001) return("p < .001")
    return(paste0("p = ", format(round(p_value, 3), nsmall = 3)))
  }
}

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
}, error = function(e) { cat("\nError H1 SUR:\n"); print(e) })

# H2 Model & Test
h2_formula_str <- paste("list(western = western_female ~ gender_feedback * western_participant", fixed_effects_terms, ",", "eastern = eastern_female ~ gender_feedback * eastern_participant", fixed_effects_terms, ")")
h2_formula <- eval(parse(text = h2_formula_str))
tryCatch({
  model_h2 <- systemfit(h2_formula, method = "SUR", data = analysis_data)
  wald_h2 <- linearHypothesis(model_h2, paste0("western_", interaction_term_west, " - eastern_", interaction_term_east, " = 0"), test = "Chisq")
}, error = function(e) { cat("\nError H2 SUR:\n"); print(e) })

# --- Extract Stats for Plotting ---
cat("--- Extracting Stats for Plotting ---\n")
p_wald_h1 <- NA; p_wald_h2_comparison <- NA
p_feedback_simple_eastP_westS <- NA; p_feedback_simple_eastP_eastS <- NA
p_feedback_simple_westP_westS <- NA; p_feedback_simple_westP_eastS <- NA

if (!is.null(wald_h1)) { p_wald_h1 <- wald_h1$"Pr(>Chisq)"[2] }
if (!is.null(model_h2)) {
  summary_h2 <- summary(model_h2)
  if (!is.null(wald_h2)) { p_wald_h2_comparison <- wald_h2$"Pr(>Chisq)"[2] }
  if("gender_feedback" %in% names(coef(summary_h2$eq[[1]]))) { p_feedback_simple_eastP_westS <- coef(summary_h2$eq[[1]])["gender_feedback", "Pr(>|t|)"] }
  if("gender_feedback" %in% names(coef(summary_h2$eq[[2]]))) { p_feedback_simple_eastP_eastS <- coef(summary_h2$eq[[2]])["gender_feedback", "Pr(>|t|)"] }
  tryCatch({ test_westP_westS <- linearHypothesis(model_h2, "western_gender_feedback + western_gender_feedback:western_participant = 0", test = "Chisq"); p_feedback_simple_westP_westS <- test_westP_westS$"Pr(>Chisq)"[2] }, error=function(e) {})
  tryCatch({ test_westP_eastS <- linearHypothesis(model_h2, "eastern_gender_feedback + eastern_gender_feedback:eastern_participant = 0", test = "Chisq"); p_feedback_simple_westP_eastS <- test_westP_eastS$"Pr(>Chisq)"[2] }, error=function(e) {})
} else { warning("H2 model object is NULL.") }

# --- OTHER ANALYSIS (Country-Specific) ---
# ...(Country-specific analysis code remains here)...

# --- ROBUSTNESS CHECKS ---
# ...(Robustness checks code remains here)...

# --- GENDER RECOGNITION ANALYSIS ---
# ...(Gender recognition analysis code remains here)...


# --- Generating Plot with Annotations ---
cat("\n--- Generating Final Plot with Annotations ---\n")

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
      bracket_y = (mean + se) * 100 + 1,
      x_pos_1 = ifelse(Scholar_Region == "Eastern Scholar", 1, 2) - 0.9/4,
      x_pos_2 = ifelse(Scholar_Region == "Eastern Scholar", 1, 2) + 0.9/4
    ) %>%
    group_by(Participant_Region, Scholar_Region) %>%
    mutate(bracket_y_final = max(bracket_y, na.rm = TRUE) + 2) %>%
    ungroup()
  
  # Data frame for annotations
  annotation_data <- data.frame(
    Participant_Region = factor(rep(c("Eastern Participants", "Western Participants"), each=2), levels=levels(plot_data_summary$Participant_Region)),
    Scholar_Region = factor(rep(c("Eastern Scholar", "Western Scholar"), 2), levels=levels(plot_data_summary$Scholar_Region)),
    p_simple_effect = c(p_feedback_simple_eastP_eastS, p_feedback_simple_eastP_westS,
                        p_feedback_simple_westP_eastS, p_feedback_simple_westP_westS)
  ) %>%
    mutate(p_text = sapply(p_simple_effect, format_p_plot)) %>%
    left_join(plot_data_summary %>% distinct(Participant_Region, Scholar_Region, bracket_y_final, x_pos_1, x_pos_2),
              by = c("Participant_Region", "Scholar_Region"))
  
  # Determine overall max y for plot limits
  plot_max_y <- max(annotation_data$bracket_y_final, na.rm = TRUE) * 1.18 # Increased multiplier for more space
  
  # Create the plot
  plot_final <- ggplot(plot_data_summary, aes(x = Scholar_Region, y = mean * 100, fill = Feedback)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8, color="grey20", linewidth=0.3) + # Slightly darker outline
    geom_errorbar(aes(ymin = pmax(0, (mean - se) * 100), ymax = (mean + se) * 100),
                  position = position_dodge(width = 0.9), width = 0.25, linewidth=0.7, color="gray30") +
    # Text inside bars
    geom_text(aes(label = sprintf("%.1f%%", mean * 100)),
              position = position_dodge(width = 0.9),
              vjust = 0.5, size = 3.5, color = "white", fontface="bold") +
    facet_wrap(~ Participant_Region) +
    scale_fill_manual(values = c("No feedback" = "navyblue", "Feedback provided" = "firebrick"), name = "Gender Feedback Status:") + # Added colon
    scale_y_continuous(limits = c(0, plot_max_y), expand = expansion(mult = c(0, 0))) + # Start at 0, use calculated max y
    # Add brackets and p-value text for simple effects comparisons
    geom_segment(data = annotation_data,
                 aes(x = x_pos_1, xend = x_pos_2, y = bracket_y_final, yend = bracket_y_final), inherit.aes = FALSE, linewidth=1, color="black") + # Thicker bracket
    geom_text(data = annotation_data,
              aes(x = (x_pos_1 + x_pos_2)/2, y = bracket_y_final, label = p_text),
              inherit.aes = FALSE, size = 4.5, vjust = -0.6, fontface="bold.italic", color="black") + # Larger, bolder p-value text
    labs(
      title = "Effect of Gender Feedback on Selecting Female Scholars",
      subtitle = "Comparing Feedback vs. No Feedback by Participant and Scholar Origin", # Clarified subtitle
      y = "Percentage Selecting Female Scholar (%)", # Clarified Y axis
      x = "Origin of Female Scholar Selected",
      caption = str_wrap(sprintf(
        "Plot shows mean percentage ± SE. Brackets compare Feedback vs. No Feedback within each group (simple effects from H2 SUR model, robust SEs).
H1: Feedback effect differs for Western vs. Eastern scholars overall (Wald test: p %s %s)
H2: Participant origin moderates the feedback effect differently for Western vs. Eastern scholars (Wald test: p = %.3f %s)",
        ifelse(is.na(p_wald_h1), "NA", format.pval(p_wald_h1, digits=3, eps=0.001)), get_stars(p_wald_h1),
        ifelse(is.na(p_wald_h2_comparison), NA, p_wald_h2_comparison), get_stars(p_wald_h2_comparison)), width=110)
    ) +
    theme_classic(base_size = 12) + # Changed theme
    theme(
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
      axis.line = element_line(color="black", linewidth = 0.5),
      plot.caption = element_text(hjust = 0, size=9.5, face="italic", lineheight = 1.2, margin=margin(t=15)),
      plot.margin = margin(t = 15, r = 20, b = 20, l = 15) # Increased margins
    )
  
  print(plot_final)
  ggsave("study2_annotated_final_v5.png", plot_final, width = 9, height = 7, dpi=300) # Adjusted height
  cat("\nPlot saved as study2_annotated_final_v5.png\n")
  
}, error = function(e) {
  cat("\nError generating plot. Check summary data and ggplot code.\n")
  print(e)
})

print("--- Analysis Script Completed ---")
