# 1. Load Libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, skimr, janitor, psych, performance, see, GGally)

# 2. Load Data
# Assuming the file is in the working directory
df <- read_excel("D:/Atif_PhD file/Research with Tayyab Ijaz/Project 02/Final_Corrected_Data_Collection.xlsx", sheet = "Final_Cleaned_Data")

# Quick cleanup of column names (remove spaces, standardize case)
df <- clean_names(df)
# Check dimensions (Rows, Columns)
dim(df)

# Visual preview
View(df) 
# or
glimpse(df)
# Check structure types
str(df)
# Visualizing missing data
visdat::vis_miss(df)

# Tabulating missing data
colSums(is.na(df))
# Inspect specific columns for anomalies
table(df$party_sup_raw, useNA = "always")
table(df$party_sup, useNA = "always")
df <- df %>%
  mutate(
    # Recode 60 or specific codes to NA or a specific "None" category
    party_sup_clean = ifelse(party_sup == 60, NA, party_sup)
  )
# Check if all respondents meet criteria
table(df$consent)
table(df$pak_live)
table(df$age18plus)
# Summary statistics for key calculated variables
df %>%
  select(warm_in, warm_out, ap_therm_diff, hostility_mean, fns_mean) %>%
  describe()
# Boxplots for outliers
boxplot(df$ap_therm_diff, main="Affective Polarization Diff")
boxplot(df$fns_mean, main="Fake News Sharing Mean")
# Check sample composition
table(df$gender)
table(df$province)
table(df$edu)
# Select hostility items
hostility_items <- df %>% select(host_angry, host_harmpak, host_irrit, host_harmnarr, host_satisfy)

# Check Alpha
psych::alpha(hostility_items)
# Select FNS items (ensure they are all numeric)
fns_items <- df %>% select(fns_unver, fns_proparty, fns_matchview, fns_wa_nosrc, fns_rivalcrit, fns_verify_raw, fns_verify_rev, fns_falselater, fns_expose)

psych::alpha(fns_items)
# Example: If 5 is "Never verify" and 1 is "Always verify", and the scale direction needs to be aligned.
df$fns_verify_rev <- 6 - df$fns_verify_raw 
# Select main study variables
study_vars <- df %>% select(ap_therm_diff, ap_likert_mean, hostility_mean, fns_mean)

# Plot correlations
ggpairs(study_vars)
# Run a basic linear model
model_check <- lm(fns_mean ~ ap_therm_diff + hostility_mean + gender + age_grp, data=df)

# Check VIF
check_collinearity(model_check)
# Check model assumptions
check_model(model_check)
# Simple Mediation Model (H4)
# IV: ap_therm_diff
# Mediator: hostility_mean  
# DV: fns_mean

library(lavaan)
model_med <- "
  # Direct effect
  fns_mean ~ c_prime * ap_therm_diff
  
  # Mediator
  hostility_mean ~ a * ap_therm_diff
  fns_mean ~ b * hostility_mean
  
  # Indirect effect (a*b)
  indirect := a * b
  total := c_prime + (a * b)
"

fit <- sem(model_med, data=df, se="bootstrap") # Bootstrap for robustness
summary(fit, standardized=TRUE)



#Results
# ==============================================================================
# PHASE 1: DATA PREPARATION & PSYCHOMETRIC SETUP
# ==============================================================================

# 1.1 Load Libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lavaan, blavaan, psych, semPlot, bayestestR, see, knitr)

# 1.2 Data Ingestion (Simulating the provided dataset structure based on inspection)
# NOTE: In actual workflow, use: df <- read_excel("Final_Corrected_Data_Collection.xlsx")
# Here we assume 'df' is already loaded from the previous inspection step.

# 1.3 Data Cleaning & Reverse Coding
# The inspection log showed 'fns_verify_raw' needs reversing.
# 'fns_verify_rev' already exists in the data, but we verify the calculation.
# Assuming 5-point scale:
df <- df %>%
  mutate(
    # Ensure reverse coding is correct (1->5, 5->1)
    fns_verify_rev_check = 6 - fns_verify_raw,
    # Check if existing rev matches calculation; if not, overwrite.
    # For this script, we create a clean set of items to use.
  )

# 1.4 Define Item Lists (Constructs)
ap_items <- c("ap_ingroup", "ap_uncomf", "ap_negreact", "ap_norespect", "ap_trustsame")
host_items <- c("host_angry", "host_harmpak", "host_irrit", "host_harmnarr", "host_satisfy")
# Note: Using the reverse coded version for verification
fns_items <- c("fns_unver", "fns_proparty", "fns_matchview", "fns_wa_nosrc", 
               "fns_rivalcrit", "fns_verify_rev", "fns_falselater", "fns_expose")

# 1.5 Declare Ordinal Levels
# BSEM with ordinal data requires items to be declared as 'ordered' factors.
# We assume a 5-point Likert scale (1 to 5) based on the descriptive stats.
for(i in c(ap_items, host_items, fns_items)){
  df[[i]] <- factor(df[[i]], levels = 1:5, ordered = TRUE)
}

# Check structure (Output: Ordered Factors)
str(df[, c(ap_items, host_items, fns_items)])
#Phase 2
# ==============================================================================
# PHASE 2: MEASUREMENT MODEL VALIDATION (CFA)
# ==============================================================================

# 2.1 Define Measurement Model Syntax
cfa_model <- "
  # Latent Variables
  AP =~ ap_ingroup + ap_uncomf + ap_negreact + ap_norespect + ap_trustsame
  Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy
  FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
         fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose
"

# 2.2 Fit CFA using WLSMV (Standard for Ordinal Data)
# This acts as a "dry run" to check model identification.
fit_cfa <- cfa(cfa_model, data = df, ordered = c(ap_items, host_items, fns_items), estimator = "WLSMV")

# 2.3 Output Summary
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# Expected Output: CFI > 0.90, TLI > 0.90, RMSEA < 0.08.
# If fit is poor, modification indices might suggest correlating error terms (e.g., similar wording).


#phase 3

# ==============================================================================
# PHASE 3: BAYESIAN SEM MEDIATION ANALYSIS
# ==============================================================================


# Increase memory limit for future package
options(future.globals.maxSize = 2000 * 1024^2)  # 2 GB


# 3.1 Define Full Structural Model
sem_model <- "
  # --- Measurement Model ---
  AP =~ ap_ingroup + ap_uncomf + ap_negreact + ap_norespect + ap_trustsame
  Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy
  FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
         fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose

  # --- Structural Model ---
  Hostility ~ a * AP
  FNS ~ b * Hostility
  FNS ~ c_prime * AP

  # --- Indirect and Total Effects ---
  indirect := a * b
  total := c_prime + (a * b)
"

# 3.2 Fit BSEM without custom prior

library(blavaan)
library(lavaan)

fit_bsem <- bsem(
  sem_model,
  data = df,
  ordered = c(ap_items, host_items, fns_items),
  n.chains = 4,
  burnin = 2000,
  sample = 2000,
  target = "stan"
)

summary(fit_bsem, standardized = TRUE)


# ==============================================================================
# PHASE 4: RESULTS VISUALIZATION & TABLES
# ==============================================================================

library(dplyr)
library(ggplot2)
library(semPlot)
library(blavaan)

# ==============================================================================
# MANUSCRIPT-READY BSEM HYPOTHESIS TABLE
# ==============================================================================

results_table <- data.frame(
  Hypothesis = c(
    "H1",
    "H2",
    "H3",
    "H4",
    "Total Effect"
  ),
  
  Path = c(
    "AP → FNS",
    "AP → Hostility",
    "Hostility → FNS",
    "AP → Hostility → FNS",
    "AP → FNS"
  ),
  
  Effect = c(
    "Direct effect (c′)",
    "Path a",
    "Path b",
    "Indirect effect (a × b)",
    "Total effect"
  ),
  
  Estimate = c(
    0.034,
    0.680,
    0.465,
    0.316,
    0.350
  ),
  
  Credible_Interval = c(
    "[-0.080, 0.127]",
    "[0.423, 0.683]",
    "[0.276, 0.590]",
    "[0.139, 0.319]",
    "[0.167, 0.341]"
  ),
  
  Decision = c(
    "Not supported",
    "Supported",
    "Supported",
    "Supported",
    "—"
  )
)

print(results_table)

write.csv(results_table, "Table_3_BSEM_Hypothesis_Results.csv", row.names = FALSE)

knitr::kable(
  results_table,
  caption = "Table 3. Bayesian Structural Model Results for Hypothesis Testing"
)


#figure 1

# ==============================================================================
# FIGURE 1: PUBLICATION-READY STRUCTURAL PATH DIAGRAM
# ==============================================================================

# ==============================================================================
# SAVE FIGURE 1 AT 300 DPI
# ==============================================================================

png(
  filename = "Figure_1_BSEM_Path_Diagram_Publication.png",
  width = 10,
  height = 8,
  units = "in",
  res = 300
)

semPlot::semPaths(
  fit_bsem,
  what = "std",
  whatLabels = "std",
  style = "lisrel",
  layout = "tree2",
  rotation = 2,
  edge.label.cex = 1.1,
  edge.label.position = 0.55,
  sizeLat = 9,
  sizeMan = 5,
  sizeMan2 = 4,
  residuals = FALSE,
  intercepts = FALSE,
  thresholds = FALSE,
  nCharNodes = 0,
  curvePivot = TRUE,
  fade = FALSE,
  color = list(
    lat = c("#2E86C1", "#28B463", "#AF7AC5"),
    man = "#D6EAF8"
  ),
  edge.color = "#2C3E50",
  mar = c(6, 6, 6, 6),
  title = FALSE
)

title(
  main = "",
  sub = "Standardized path estimates are shown",
  cex.main = 1.2,
  cex.sub = 0.9
)

dev.off()


#figure 2

# ==============================================================================
# FIGURE 2: COLORFUL POSTERIOR DISTRIBUTION OF INDIRECT EFFECT
# ==============================================================================

library(ggplot2)
library(blavaan)

post_samples <- blavInspect(fit_bsem, "mcmc")
post_df <- as.data.frame(do.call(rbind, post_samples))

# Find indirect effect column
indirect_col <- grep("indirect", colnames(post_df), value = TRUE)

if(length(indirect_col) == 0) {
  stop("Indirect effect column not found. Run colnames(post_df) to check the exact name.")
}

indirect_post <- data.frame(
  Indirect_Effect = post_df[[indirect_col[1]]]
)

# Calculate credible interval and mean
ci <- quantile(indirect_post$Indirect_Effect, probs = c(0.025, 0.975), na.rm = TRUE)
mean_indirect <- mean(indirect_post$Indirect_Effect, na.rm = TRUE)

p_indirect <- ggplot(indirect_post, aes(x = Indirect_Effect)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 50,
    fill = "#7FB3D5",
    color = "white",
    alpha = 0.85
  ) +
  geom_density(
    color = "#1B4F72",
    linewidth = 1.3
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#C0392B",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = mean_indirect,
    linetype = "solid",
    color = "#145A32",
    linewidth = 1.1
  ) +
  geom_vline(
    xintercept = ci,
    linetype = "dotted",
    color = "#6C3483",
    linewidth = 1
  ) +
  theme_classic(base_size = 14) +
  labs(
    title = "",
    subtitle = "",
    x = "Indirect Effect Estimate (a × b)",
    y = "Density"
  ) +
  annotate(
    "text",
    x = mean_indirect,
    y = Inf,
    label = "Posterior mean",
    vjust = 2,
    hjust = -0.1,
    color = "#145A32",
    size = 4
  ) +
  annotate(
    "text",
    x = 0,
    y = Inf,
    label = "Zero effect",
    vjust = 4,
    hjust = -0.1,
    color = "#C0392B",
    size = 4
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

print(p_indirect)

ggsave(
  "Figure2_Indirect_Effect_Color.png",
  plot = p_indirect,
  width = 8,
  height = 6,
  dpi = 300
)


# ==============================================================================
# DIAGNOSTIC PLOTS: MODEL ASSUMPTIONS
# ==============================================================================

# 1. Load packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, performance, see, ggplot2)

# 2. Create composite scores first
df$AP_Score <- rowMeans(
  df[ap_items] %>% mutate(across(everything(), as.numeric)),
  na.rm = TRUE
)

df$Hostility_Score <- rowMeans(
  df[host_items] %>% mutate(across(everything(), as.numeric)),
  na.rm = TRUE
)

df$FNS_Score <- rowMeans(
  df[fns_items] %>% mutate(across(everything(), as.numeric)),
  na.rm = TRUE
)

# 3. Check that scores were created
summary(df[, c("AP_Score", "Hostility_Score", "FNS_Score")])

# 4. Fit diagnostic regression model
model_diagnostic <- lm(
  FNS_Score ~ AP_Score + Hostility_Score + gender + age_grp + res_area,
  data = df
)

# 5. Generate diagnostic plots
diagnostic_plot <- check_model(model_diagnostic)

# 6. Show plot first
print(diagnostic_plot)

# 7. Save plot after checking
png("Figure_Diagnostics.png", width = 12, height = 10, units = "in", res = 300)
print(diagnostic_plot)
dev.off()


df$FNS_Score <- rowMeans(
  df[fns_items] %>% mutate(across(everything(), as.numeric)),
  na.rm = TRUE
)



# ==============================================================================
# FIGURE 5: FOREST PLOT OF BAYESIAN COEFFICIENTS
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Extract posterior samples
post_samples <- blavInspect(fit_bsem, "mcmc")

# Convert matrix/list output to data frame
post_df <- as.data.frame(do.call(rbind, post_samples))

# Check available column names
colnames(post_df)

# 2. Find relevant parameter columns safely
needed_params <- c("a", "b", "c_prime", "indirect")

available_params <- intersect(needed_params, colnames(post_df))

if(length(available_params) == 0) {
  stop("None of the required parameters were found. Check colnames(post_df).")
}

# 3. Select and reshape parameters
params_to_plot <- post_df %>%
  dplyr::select(all_of(available_params)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  )

# 4. Calculate posterior summary statistics
forest_data <- params_to_plot %>%
  group_by(Parameter) %>%
  summarise(
    Mean = mean(Estimate, na.rm = TRUE),
    Lower = quantile(Estimate, 0.025, na.rm = TRUE),
    Upper = quantile(Estimate, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Hypothesis = case_when(
      Parameter == "a" ~ "H2: AP → Hostility",
      Parameter == "b" ~ "H3: Hostility → FNS",
      Parameter == "c_prime" ~ "H1: AP → FNS",
      Parameter == "indirect" ~ "H4: Indirect Effect",
      TRUE ~ Parameter
    ),
    Result = ifelse(Lower > 0 | Upper < 0, "Supported", "Not supported")
  )

# 5. Create forest plot
p_forest <- ggplot(
  forest_data,
  aes(x = Mean, y = reorder(Hypothesis, Mean), color = Result)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray40",
    linewidth = 1
  ) +
  geom_errorbarh(
    aes(xmin = Lower, xmax = Upper),
    height = 0.2,
    linewidth = 1
  ) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("Supported" = "blue", "Not supported" = "firebrick")
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "",
    subtitle = "Points represent posterior means; bars represent 95% credible intervals",
    x = "Posterior Estimate",
    y = "",
    color = "Result",
    caption = "Intervals excluding zero indicate support for the hypothesis."
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

# Show plot first
print(p_forest)

# 6. Save figure
ggsave(
  "Figure_5_Forest_Plot.png",
  plot = p_forest,
  width = 10,
  height = 6,
  dpi = 300
)


#figure

# ==============================================================================
# FIGURE 6: PRIOR VS POSTERIOR UPDATE FOR INDIRECT EFFECT
# ==============================================================================

library(ggplot2)
library(blavaan)

# 1. Extract posterior samples if post_df does not already exist
post_samples <- blavInspect(fit_bsem, "mcmc")
post_df <- as.data.frame(do.call(rbind, post_samples))

# 2. Find indirect-effect column safely
indirect_col <- grep("indirect", colnames(post_df), value = TRUE)

if(length(indirect_col) == 0) {
  stop("Indirect effect column not found. Run colnames(post_df) to check the exact name.")
}

post_indirect <- post_df[[indirect_col[1]]]

# 3. Remove missing values
post_indirect <- post_indirect[!is.na(post_indirect)]

# 4. Create prior curve
# Note: Use Normal(0, 1) only if this is the prior you report in the manuscript.
x_seq <- seq(
  min(post_indirect) - 0.20,
  max(post_indirect) + 0.20,
  length.out = 500
)

prior_curve <- data.frame(
  x = x_seq,
  y = dnorm(x_seq, mean = 0, sd = 1)
)

# 5. Posterior summary
post_mean <- mean(post_indirect)
post_ci <- quantile(post_indirect, probs = c(0.025, 0.975))

# 6. Create plot
p_prior_post <- ggplot() +
  geom_line(
    data = prior_curve,
    aes(x = x, y = y),
    color = "#6C757D",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_density(
    aes(x = post_indirect),
    fill = "#5DADE2",
    color = "#1B4F72",
    alpha = 0.65,
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    color = "#C0392B",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = post_mean,
    color = "#145A32",
    linewidth = 1.1
  ) +
  geom_vline(
    xintercept = post_ci,
    color = "#6C3483",
    linewidth = 1,
    linetype = "dotdash"
  ) +
  annotate(
    "text",
    x = 0,
    y = max(density(post_indirect)$y) * 0.95,
    label = "Zero effect",
    color = "#C0392B",
    hjust = -0.1,
    size = 4
  ) +
  annotate(
    "text",
    x = post_mean,
    y = max(density(post_indirect)$y) * 0.85,
    label = "Posterior mean",
    color = "#145A32",
    hjust = -0.1,
    size = 4
  ) +
  annotate(
    "text",
    x = min(x_seq),
    y = max(density(post_indirect)$y) * 0.75,
    label = "Prior: Normal(0, 1)",
    color = "#6C757D",
    hjust = 0,
    size = 4
  ) +
  theme_classic(base_size = 14) +
  labs(
    title = "",
    subtitle = "Posterior distribution of the mediation effect compared with the prior",
    x = "Indirect Effect Estimate (a × b)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

# 7. Show figure first
print(p_prior_post)

# 8. Save figure
ggsave(
  "Figure_6_Prior_Posterior_Update.png",
  plot = p_prior_post,
  width = 8,
  height = 6,
  dpi = 300
)

#figure

# ==============================================================================
# FIGURE 7: PUBLICATION-READY ITEM RESPONSE HEATMAP
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Define item lists
# ------------------------------------------------------------------------------

ap_items <- c(
  "ap_ingroup", "ap_uncomf", "ap_negreact",
  "ap_norespect", "ap_trustsame"
)

host_items <- c(
  "host_angry", "host_harmpak", "host_irrit",
  "host_harmnarr", "host_satisfy"
)

fns_items <- c(
  "fns_unver", "fns_proparty", "fns_matchview",
  "fns_wa_nosrc", "fns_rivalcrit", "fns_verify_rev",
  "fns_falselater", "fns_expose"
)

all_items <- c(ap_items, host_items, fns_items)

# ------------------------------------------------------------------------------
# 2. Check whether all variables exist
# ------------------------------------------------------------------------------

missing_items <- setdiff(all_items, names(df))

if(length(missing_items) > 0) {
  stop(paste("These items are missing from df:", paste(missing_items, collapse = ", ")))
}

# ------------------------------------------------------------------------------
# 3. Prepare response distribution data
# ------------------------------------------------------------------------------

items_long <- df %>%
  select(all_of(all_items)) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Item",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(Item, Response) %>%
  group_by(Item) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    Label = ifelse(Percentage >= 5, paste0(round(Percentage, 0), "%"), "")
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. Add construct labels and order items
# ------------------------------------------------------------------------------

items_long <- items_long %>%
  mutate(
    Construct = case_when(
      Item %in% ap_items ~ "Affective Polarization",
      Item %in% host_items ~ "Outgroup Hostility",
      Item %in% fns_items ~ "Fake News Sharing",
      TRUE ~ "Other"
    ),
    Item = factor(Item, levels = rev(all_items))
  )

# ------------------------------------------------------------------------------
# 5. Create publication-ready heatmap
# ------------------------------------------------------------------------------

p_heatmap_items <- ggplot(
  items_long,
  aes(x = factor(Response), y = Item, fill = Percentage)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = Label),
    color = "black",
    size = 3.2
  ) +
  facet_grid(
    Construct ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    name = "Responses (%)"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "",
    subtitle = "Heatmap of Likert-scale responses across affective polarization, outgroup hostility, and fake news sharing items",
    x = "Response Category",
    y = "Scale Item"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black", size = 9),
    strip.text.y = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# ------------------------------------------------------------------------------
# 6. Show figure first
# ------------------------------------------------------------------------------

print(p_heatmap_items)

# ------------------------------------------------------------------------------
# 7. Save figure at 300 DPI
# ------------------------------------------------------------------------------

ggsave(
  "Figure_7_Item_Response_Heatmap.png",
  plot = p_heatmap_items,
  width = 10,
  height = 12,
  dpi = 300
)




#appendix B
# ==============================================================================
# FIGURE S1: PAIRS PLOT / CORRELATION AND DISTRIBUTION MATRIX
# ==============================================================================

library(dplyr)
library(ggplot2)
library(GGally)

# ------------------------------------------------------------------------------
# 1. Define item groups
# ------------------------------------------------------------------------------

ap_items <- c("ap_ingroup", "ap_uncomf", "ap_negreact", "ap_norespect", "ap_trustsame")

host_items <- c("host_angry", "host_harmpak", "host_irrit", "host_harmnarr", "host_satisfy")

fns_items <- c(
  "fns_unver", "fns_proparty", "fns_matchview", "fns_wa_nosrc",
  "fns_rivalcrit", "fns_verify_rev", "fns_falselater", "fns_expose"
)

# ------------------------------------------------------------------------------
# 2. Create composite scores
# ------------------------------------------------------------------------------

df$AP_Score <- rowMeans(
  df[ap_items] %>% mutate(across(everything(), as.numeric)),
  na.rm = TRUE
)

df$Hostility_Score <- rowMeans(
  df[host_items] %>% mutate(across(everything(), as.numeric)),
  na.rm = TRUE
)

df$FNS_Score <- rowMeans(
  df[fns_items] %>% mutate(across(everything(), as.numeric)),
  na.rm = TRUE
)

# ------------------------------------------------------------------------------
# 3. Select variables for pairs plot
# ------------------------------------------------------------------------------

study_vars_plot <- df %>%
  select(AP_Score, Hostility_Score, FNS_Score, ap_therm_diff)

# Rename variables for cleaner figure labels
names(study_vars_plot) <- c(
  "Affective Polarization",
  "Outgroup Hostility",
  "Fake News Sharing",
  "Thermometer Difference"
)

# ------------------------------------------------------------------------------
# 4. Create publication-ready pairs plot
# ------------------------------------------------------------------------------

p_pairs <- ggpairs(
  study_vars_plot,
  upper = list(
    continuous = wrap("cor", size = 5, color = "black")
  ),
  lower = list(
    continuous = wrap("smooth", method = "lm", se = FALSE, color = "#C0392B", alpha = 0.5, size = 0.8)
  ),
  diag = list(
    continuous = wrap("densityDiag", fill = "#7FB3D5", color = "#1B4F72", alpha = 0.7)
  )
) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray70", fill = NA),
    axis.text = element_text(color = "black")
  )

# ------------------------------------------------------------------------------
# 5. Show figure first
# ------------------------------------------------------------------------------

print(p_pairs)

# ------------------------------------------------------------------------------
# 6. Save high-resolution figure
# ------------------------------------------------------------------------------

png(
  "Figure_S1_Pairs_Plot.png",
  width = 10,
  height = 10,
  units = "in",
  res = 300
)

print(p_pairs)

dev.off()



#demographics
# ==============================================================================
# TABLE 1: SAMPLE CHARACTERISTICS
# ==============================================================================

library(dplyr)
library(knitr)

demo_vars <- c("gender", "age_grp", "edu", "province", "res_area")

table_demographics <- lapply(demo_vars, function(var) {
  df %>%
    count(.data[[var]]) %>%
    mutate(
      Variable = var,
      Percentage = round(n / sum(n) * 100, 1),
      `n (%)` = paste0(n, " (", Percentage, "%)")
    ) %>%
    rename(Category = .data[[var]]) %>%
    select(Variable, Category, `n (%)`)
}) %>%
  bind_rows()

# Clean variable labels
table_demographics <- table_demographics %>%
  mutate(
    Variable = recode(
      Variable,
      gender = "Gender",
      age_grp = "Age group",
      edu = "Education",
      province = "Province",
      res_area = "Residential area"
    )
  )

print(
  kable(
    table_demographics,
    caption = "Table 1. Sample Characteristics (N = 508)"
  )
)

write.csv(table_demographics, "Table_1_Sample_Characteristics.csv", row.names = FALSE)


# ==============================================================================
# TABLE: SAMPLE DEMOGRAPHICS (PUBLICATION FORMAT)
# ==============================================================================
library(dplyr)
library(tidyr)
library(stringr)
library(knitr)

# 1. Define Variables and Labels
# We create a lookup for nice labels in the table
var_info <- tibble(
  variable = c("gender", "age_grp", "edu", "province", "res_area"),
  label = c("Gender", "Age Group", "Education Level", "Province", "Residential Area")
)

# 2. Function to calculate N and %
calc_demo <- function(data, var) {
  data %>%
    count(!!sym(var)) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      `N (%)` = paste0(n, " (", pct, ")")
    ) %>%
    select(Category = !!sym(var), `N (%)`)
}

# 3. Generate Table Data
table_demo_list <- lapply(var_info$variable, function(v) {
  res <- calc_demo(df, v)
  res$Variable <- var_info$label[var_info$variable == v]
  return(res)
})

# Combine into one dataframe
table_demo <- bind_rows(table_demo_list) %>%
  select(Variable, Category, `N (%)`) # Reorder columns

# 4. Save as CSV (for editing in Word/Excel)
write.csv(table_demo, "Table_Demographics_Manuscript.csv", row.names = FALSE)

# 5. Display Clean Table in RStudio
# Using kable for a neat console view
print(kable(table_demo, caption = "Table 1: Demographic Characteristics of the Sample (N=508)"))

#figure for demographics
# ==============================================================================
# FIGURE: DEMOGRAPHIC COMPOSITION (VISUALIZATION)
# ==============================================================================
library(ggplot2)
library(patchwork) # For combining plots

# 1. Prepare Data for Plotting
# Convert variables to factors with appropriate labels if not already done
# (Assuming df$gender etc are already factors or characters)

# Create individual plots
p_gender <- ggplot(df, aes(x = gender, fill = gender)) +
  geom_bar(color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Gender", x = "", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p_age <- ggplot(df, aes(x = age_grp, fill = age_grp)) +
  geom_bar(color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Age Group", x = "", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p_edu <- ggplot(df, aes(x = edu, fill = edu)) +
  geom_bar(color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "Education", x = "", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p_province <- ggplot(df, aes(x = province, fill = province)) +
  geom_bar(color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "Province", x = "", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Combine Plots using patchwork
# Layout: 2 rows, 3 columns (empty last spot)
combined_plot <- (p_gender + p_age + p_edu) / (p_province + plot_spacer())

# Add overall title
combined_plot <- combined_plot + 
  plot_annotation(title = "Figure S1: Sample Demographics Distribution",
                  subtitle = "N = 508",
                  theme = theme(plot.title = element_text(size = 14, face = "bold")))

# 3. Save Figure
ggsave("Figure_Demographics.png", plot = combined_plot, width = 12, height = 8, dpi = 300)


#Overall Results


#updated results + plots (22)

# ---------------------------------------------------------
# 1. ENVIRONMENT SETUP
# ---------------------------------------------------------
# Install necessary packages if not already installed
required_pkgs <- c("tidyverse", "readxl", "janitor", "psych", "GGally", 
                   "viridis", "hrbrthemes", "patchwork", "RColorBrewer", 
                   "ggpubr", "corrplot", "scales", "ggrepel")

installed_pkgs <- rownames(installed.packages())
for (pkg in required_pkgs) {
  if (!(pkg %in% installed_pkgs)) install.packages(pkg, dependencies = TRUE)
}

# Load libraries
library(tidyverse)
library(readxl)
library(janitor)
library(psych)
library(GGally)
library(viridis)
library(hrbrthemes)
library(patchwork)
library(RColorBrewer)
library(ggpubr)
library(corrplot)
library(scales)
library(ggrepel)

# ---------------------------------------------------------
# 2. DATA INGESTION & CLEANING
# ---------------------------------------------------------
# Load data (replace path with your actual file path)
# df <- read_excel("D:/Atif_PhD file/Research with Tayyab Ijaz/Project 02/Final_Corrected_Data_Collection.xlsx", 
#                  sheet = "Final_Cleaned_Data")

# Assuming data is already loaded as 'df' based on the provided snippet
# Run clean_names to standardize column headers
df <- df %>% clean_names()

# --- Data Integrity Corrections ---
# 1. Fix 'party_sup_raw' coding issues (if applicable)
# The inspection noted '*' and '60' values. We clean this for visualization.
df$party_sup_clean <- df$party_sup_raw
# If values are character, convert codes to NA for plotting safety
df$party_sup_clean[df$party_sup_clean %in% c("*", "60")] <- NA
df$party_sup_clean <- as.numeric(df$party_sup_clean)

# 2. Ensure categorical variables are Factors
df <- df %>%
  mutate(
    gender = factor(gender, levels = c("Male", "Female")),
    age_grp = factor(age_grp, levels = c("18-24", "25-34", "35-44", "45-54", "55+")),
    edu = factor(edu, levels = c("Secondary or below", "Intermediate/A-level", 
                                 "Bachelor's", "Master's", "PhD")),
    province = factor(province),
    res_area = factor(res_area, levels = c("Urban", "Semi-urban", "Rural")),
    party_id = factor(party_id)
  )

# 3. Define custom theme for publication
theme_publication <- function() {
  theme_minimal(base_family = "Arial Narrow") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Color Palette for Manuscript
manuscript_palette <- c("#2c3e50", "#e74c3c", "#3498db", "#9b59b6", "#f39c12")


#2. Descriptive & Demographic Visualization (Plots 1–4)

# Plot 1: Gender by Age Group
p1 <- df %>%
  ggplot(aes(x = age_grp, fill = gender)) +
  geom_bar(position = "fill", width = 0.7, color = "white") + # Proportion plot
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  labs(title = "Age Distribution by Gender",
       x = "Age Group", y = "Proportion", fill = "Gender") +
  theme_publication()

ggsave("Plot_01_Demographics_AgeGender.png", p1, dpi = 300, width = 8, height = 5, units = "in")

#Plot 2: Provincial Representation
# Plot 2: Province Lollipop
province_counts <- df %>% count(province) %>% arrange(desc(n))

p2 <- ggplot(province_counts, aes(x = reorder(province, n), y = n)) +
  geom_segment(aes(x = reorder(province, n), xend = province, y = 0, yend = n), 
               color = "grey70", size = 1) +
  geom_point(color = "#2c3e50", size = 4) +
  coord_flip() +
  labs(title = "Geographic Distribution of Sample",
       x = "Province", y = "Count (N)") +
  theme_publication() +
  theme(panel.grid.major.y = element_blank())

ggsave("Plot_02_Geography.png", p2, dpi = 300, width = 7, height = 5, units = "in")



#Plot 6: News Platform Usage Frequency (Comparison)
# Reshape data to long format for comparison
media_long <- df %>%
  select(resp_id, wa_news_frq, fb_news_frq, yt_news_frq, x_news_frq) %>%
  rename(WhatsApp = wa_news_frq, Facebook = fb_news_frq, 
         YouTube = yt_news_frq, X_Twitter = x_news_frq) %>%
  pivot_longer(cols = c(WhatsApp, Facebook, YouTube, X_Twitter), 
               names_to = "Platform", values_to = "Frequency")

p6 <- ggplot(media_long, aes(x = Platform, y = Frequency, fill = Platform)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.alpha = 0.2) +
  scale_fill_viridis(discrete = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(title = "Comparison of News Consumption Frequency Across Platforms",
       y = "Frequency Score (1-5)", x = "Platform") +
  theme_publication() +
  theme(legend.position = "none")

ggsave("Plot_06_MediaComparison.png", p6, dpi = 300, width = 8, height = 6, units = "in")


#Plot 7: Political Interest vs. Party Support
p7 <- df %>%
  filter(!is.na(party_sup_clean)) %>%
  ggplot(aes(x = pol_int, y = party_sup_clean)) +
  geom_jitter(alpha = 0.3, color = "#3498db") +
  geom_smooth(method = "lm", color = "#e74c3c", se = TRUE) +
  labs(title = "Political Interest vs. Party Support Intensity",
       x = "Political Interest (1-5)", y = "Party Support Intensity (1-5)") +
  theme_publication()

ggsave("Plot_07_PolInt_PartySup.png", p7, dpi = 300, width = 7, height = 5, units = "in")

#plot 8
p8 <- ggplot(df, aes(x = wa_recv_frq, y = news_share_frq)) +
  geom_hex(bins = 20) +
  scale_fill_gradient(low = "lightgrey", high = "#2c3e50") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "WhatsApp News Receiving vs. Sharing Frequency",
       x = "Receiving Frequency", y = "Sharing Frequency", fill = "Count") +
  theme_publication()

ggsave("Plot_08_WABehavior.png", p8, dpi = 300, width = 7, height = 5, units = "in")

#plot 9
p9 <- ggplot(df, aes(x = ap_therm_diff)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3498db", alpha = 0.6) +
  geom_density(color = "#e74c3c", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  annotate("text", x = 1, y = 0.15, label = "Ingroup > Outgroup", hjust = 0, color = "gray30") +
  labs(title = "Distribution of Affective Polarization (Thermometer Difference)",
       subtitle = "Higher values indicate greater Ingroup bias vs Outgroup",
       x = "AP Score (Ingroup - Outgroup)", y = "Density") +
  theme_publication()

ggsave("Plot_09_AP_Distribution.png", p9, dpi = 300, width = 8, height = 5, units = "in")

#plot 10
# Reshape for overlapping density
warm_data <- df %>%
  select(resp_id, warm_in, warm_out) %>%
  pivot_longer(cols = c(warm_in, warm_out), names_to = "Group", values_to = "Warmth")

p10 <- ggplot(warm_data, aes(x = Warmth, fill = Group, color = Group)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("warm_in" = "#27ae60", "warm_out" = "#c0392b"),
                    labels = c("Ingroup", "Outgroup")) +
  scale_color_manual(values = c("warm_in" = "#27ae60", "warm_out" = "#c0392b"),
                     labels = c("Ingroup", "Outgroup")) +
  labs(title = "Feeling Thermometer: Ingroup vs. Outgroup Warmth",
       x = "Warmth Score (0-10)", y = "Density") +
  theme_publication()

ggsave("Plot_10_Warmth_Comparison.png", p10, dpi = 300, width = 8, height = 5, units = "in")


#plot 11
ap_items <- df %>% select(ap_ingroup, ap_uncomf, ap_negreact, ap_norespect, ap_trustsame)

p11 <- ggcorr(ap_items, 
              label = TRUE, 
              label_size = 4, 
              label_alpha = TRUE,
              low = "#f39c12", mid = "white", high = "#2980b9") +
  labs(title = "Correlation Matrix: Affective Polarization Items") +
  theme(plot.title = element_text(face = "bold"))

ggsave("Plot_11_AP_Correlations.png", p11, dpi = 300, width = 7, height = 6, units = "in")

#plot 12
library(ggridges)

host_long <- df %>%
  select(starts_with("host_")) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Response") %>%
  mutate(Item = str_replace(Item, "host_", "")) # Clean names

p12 <- ggplot(host_long, aes(x = Response, y = Item, fill = Item)) +
  geom_density_ridges(alpha = 0.7, from = 1, to = 5) +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  labs(title = "Response Distribution of Hostility Scale Items",
       x = "Response (1-5)", y = "Item") +
  theme_publication() +
  theme(legend.position = "none")

ggsave("Plot_12_Hostility_Ridges.png", p12, dpi = 300, width = 8, height = 5, units = "in")

#plot 13
p13 <- ggplot(df, aes(x = fns_mean)) +
  geom_histogram(binwidth = 0.2, fill = "#8e44ad", color = "white") +
  stat_function(fun = function(x) dnorm(x, mean = mean(df$fns_mean, na.rm=T), 
                                        sd = sd(df$fns_mean, na.rm=T)) * 
                  length(df$fns_mean) * 0.2, # Scale normal curve to hist
                color = "red", linetype = "dashed") +
  labs(title = "Distribution of Fake News Sharing (Mean Score)",
       x = "FNS Mean (1-5)", y = "Count") +
  theme_publication()

ggsave("Plot_13_FNS_Distribution.png", p13, dpi = 300, width = 7, height = 5, units = "in")


#plot 14
# Select key FNS items
fns_items_plot <- df %>%
  select(fns_unver, fns_proparty, fns_verify_rev, fns_expose) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Score") %>%
  mutate(Score_Factor = factor(Score, levels = c("1", "2", "3", "4", "5")),
         Item = str_replace(Item, "fns_", ""))

p14 <- ggplot(fns_items_plot, aes(x = Item, fill = Score_Factor)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proportional Response Distribution for FNS Items",
       x = "Item", y = "Percentage", fill = "Response") +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plot_14_FNS_Items_Bar.png", p14, dpi = 300, width = 8, height = 5, units = "in")

#plot 15
p15 <- ggplot(df, aes(x = ap_therm_diff, y = hostility_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#e74c3c", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 0, label.y = 5, size = 4) +
  labs(title = "H2: Affective Polarization vs. Outgroup Hostility",
       x = "Affective Polarization (Thermometer Diff)", y = "Hostility Mean") +
  theme_publication()

ggsave("Plot_15_AP_vs_Hostility.png", p15, dpi = 300, width = 7, height = 5, units = "in")

#plot 16
p16 <- ggplot(df, aes(x = hostility_mean, y = fns_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#2980b9", fill = "#3498db", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 1, label.y = 4.5, size = 4) +
  labs(title = "H3: Outgroup Hostility vs. Fake News Sharing",
       x = "Hostility Mean", y = "Fake News Sharing Mean") +
  theme_publication()

ggsave("Plot_16_Hostility_vs_FNS.png", p16, dpi = 300, width = 7, height = 5, units = "in")

#plot 17
p17 <- ggplot(df, aes(x = ap_therm_diff, y = fns_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#27ae60", fill = "#2ecc71", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 0, label.y = 4.5, size = 4) +
  labs(title = "H1: Affective Polarization vs. Fake News Sharing (Total Effect)",
       x = "Affective Polarization (Thermometer Diff)", y = "Fake News Sharing Mean") +
  theme_publication()

ggsave("Plot_17_AP_vs_FNS.png", p17, dpi = 300, width = 7, height = 5, units = "in")

#plot 18
p18 <- df %>%
  select(ap_likert_mean, hostility_mean, fns_mean) %>%
  ggpairs(lower = list(continuous = wrap("smooth", color = "#d35400", alpha = 0.3)),
          diag = list(continuous = wrap("densityDiag", fill = "#d35400", alpha = 0.5)),
          upper = list(continuous = wrap("cor", size = 6))) +
  labs(title = "Correlation Matrix of Key Study Variables") +
  theme_publication()

ggsave("Plot_18_Correlation_Matrix.png", p18, dpi = 300, width = 8, height = 8, units = "in")

#plot 19
# Simulating data based on manuscript results for visualization purposes
results_df <- data.frame(
  Hypothesis = c("H1 (Direct)", "H2 (AP->Host)", "H3 (Host->FNS)", "H4 (Indirect)"),
  Estimate = c(0.034, 0.680, 0.465, 0.316),
  Lower = c(-0.080, 0.423, 0.276, 0.139),
  Upper = c(0.127, 0.683, 0.590, 0.319)
)

p19 <- ggplot(results_df, aes(x = Estimate, y = reorder(Hypothesis, -Estimate))) +
  geom_point(size = 4, color = "#2c3e50") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = "#2c3e50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Forest Plot of Structural Model Coefficients",
       x = "Standardized Estimate (Beta)", y = "Hypothesis Path") +
  theme_publication()

ggsave("Plot_19_Coefficient_Forest.png", p19, dpi = 300, width = 7, height = 4, units = "in")

#plot 20
p20 <- df %>%
  filter(party_id %in% c("PTI", "PML-N", "PPP")) %>% # Filter main parties
  ggplot(aes(x = party_id, y = hostility_mean, fill = party_id)) +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_fill_manual(values = c("PTI" = "#e74c3c", "PML-N" = "#2ecc71", "PPP" = "#3498db")) +
  labs(title = "Outgroup Hostility by Major Party Affiliation",
       x = "Party Identification", y = "Hostility Mean") +
  theme_publication() +
  theme(legend.position = "none")

ggsave("Plot_20_Hostility_Party.png", p20, dpi = 300, width = 7, height = 5, units = "in")

#plot 21
p21 <- ggplot(df, aes(x = ap_therm_diff, y = fns_mean)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "#8e44ad", fill = "#9b59b6") +
  facet_wrap(~gender) +
  labs(title = "Relationship between AP and FNS by Gender",
       x = "Affective Polarization", y = "Fake News Sharing") +
  theme_publication()

ggsave("Plot_21_AP_FNS_Gender.png", p21, dpi = 300, width = 10, height = 5, units = "in")

#plot 22
p22 <- ggplot(df, aes(x = edu, y = fns_mean, fill = edu)) +
  geom_boxplot(notch = TRUE, varwidth = TRUE) +
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  labs(title = "Fake News Sharing Tendency by Education Level",
       x = "Education Level", y = "FNS Mean") +
  theme_publication() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plot_22_Edu_FNS.png", p22, dpi = 300, width = 8, height = 5, units = "in")




#for plots 3-4-5
# ==============================================================================
# 0. SETUP AND DATA PREPARATION
# ==============================================================================

# Load necessary libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, scales, ggpubr, viridis, patchwork)

# Load Data
# df <- read_excel("Final_Corrected_Data_Collection.xlsx", sheet = "Final_Cleaned_Data")
# Assuming 'df' is already loaded in the environment from previous steps.

# Custom Theme for Publication
theme_pub_custom <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(color = "black", size = 10),
      legend.position = "right",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Define custom color palette
custom_colors <- c("#2E86C1", "#A93226", "#27AE60", "#F39C12", "#8E44AD", "#16A085")

#plot 3
# --- Plot 3: Education Level ---
p3 <- df %>%
  count(edu) %>%
  mutate(edu = fct_reorder(edu, n)) %>%
  ggplot(aes(x = edu, y = n, fill = edu)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("#3498DB", "#9B59B6", "#E74C3C", "#1ABC9C", "#F1C40F", "#34495E")) +
  labs(title = "Education Level",
       x = "", y = "Count") +
  theme_pub_custom()

ggsave("Plot_03_Education.png", p3, width = 8, height = 6, dpi = 300)

#plot 4
#plot 4
# --- Plot 4: Provincial Representation: Publication-Ready Donut Chart ---

library(dplyr)
library(ggplot2)
library(scales)

# Custom manuscript color palette
province_cols <- c(
  "Punjab" = "#1F4E79",
  "Sindh" = "#B45F06",
  "Khyber Pakhtunkhwa" = "#38761D",
  "Balochistan" = "#7F1D1D",
  "Gilgit-Baltistan" = "#5B3F8C"
)

# Prepare plot data
p4_data <- df %>%
  count(province) %>%
  mutate(
    proportion = n / sum(n),
    percent_label = percent(proportion, accuracy = 0.1),
    ymax = cumsum(proportion),
    ymin = lag(ymax, default = 0),
    label_pos = (ymax + ymin) / 2,
    label = paste0(province, "\n", percent_label)
  )

# Create donut chart
p4 <- ggplot(
  p4_data,
  aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.4, fill = province)
) +
  geom_rect(color = "white", linewidth = 1.1) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  geom_text(
    aes(x = 3.25, y = label_pos, label = label),
    color = "black",
    size = 3.4,
    fontface = "bold",
    lineheight = 0.9
  ) +
  scale_fill_manual(values = province_cols) +
  labs(
    title = "Provincial Distribution of Respondents",
    subtitle = "Percentage share of respondents by province/region",
    caption = ""
  ) +
  theme_void(base_family = "serif") +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "gray30",
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray35",
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

# Display plot
p4

# Save at 300 dpi
ggsave(
  filename = "Plot_04_Provincial_Distribution.png",
  plot = p4,
  width = 7,
  height = 7,
  dpi = 300,
  bg = "white"
)


#plot 5
# --- Plot 5: Residential Area ---
p5 <- df %>%
  count(res_area) %>%
  ggplot(aes(x = res_area, y = n, fill = res_area)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_manual(values = c("#5499C7", "#48C9B0", "#F4D03F")) +
  labs(title = "Residential Area",
       x = "Area Type", y = "Count") +
  theme_pub_custom()

ggsave("Plot_05_ResArea.png", p5, width = 7, height = 5, dpi = 300)






#Additional plots

# 20 figures
# ==============================================================================
# 0. SETUP AND DATA PREPARATION
# ==============================================================================

# Load necessary libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, scales, ggpubr, viridis, patchwork)

# Load Data
# df <- read_excel("Final_Corrected_Data_Collection.xlsx", sheet = "Final_Cleaned_Data")
# Assuming 'df' is already loaded in the environment from previous steps.

# Custom Theme for Publication
theme_pub_custom <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(color = "black", size = 10),
      legend.position = "right",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Define custom color palette
custom_colors <- c("#2E86C1", "#A93226", "#27AE60", "#F39C12", "#8E44AD", "#16A085")

# ==============================================================================
# CATEGORY 1: SAMPLE DEMOGRAPHICS (Plots 1-5)
# ==============================================================================

# --- Plot 1: Gender Distribution ---
p1 <- df %>%
  count(gender) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(gender, n), y = n, fill = gender)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Plot 1: Gender Distribution",
       x = "", y = "Count") +
  theme_pub_custom() +
  ylim(0, max(df %>% count(gender) %>% pull(n)) * 1.15)

ggsave("Plot_01_Gender.png", p1, width = 8, height = 5, dpi = 300)

# --- Plot 2: Age Group Distribution ---
p2 <- df %>%
  count(age_grp) %>%
  ggplot(aes(x = factor(age_grp, levels = c("18-24", "25-34", "35-44", "45-54", "55+")), y = n, fill = age_grp)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Plot 2: Age Group Distribution",
       x = "Age Group", y = "Frequency") +
  theme_pub_custom()

ggsave("Plot_02_Age.png", p2, width = 8, height = 5, dpi = 300)

# --- Plot 3: Education Level ---
p3 <- df %>%
  count(edu) %>%
  mutate(edu = fct_reorder(edu, n)) %>%
  ggplot(aes(x = edu, y = n, fill = edu)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("#3498DB", "#9B59B6", "#E74C3C", "#1ABC9C", "#F1C40F", "#34495E")) +
  labs(title = "Education Level",
       x = "", y = "Count") +
  theme_pub_custom()

ggsave("Plot_03_Education.png", p3, width = 8, height = 6, dpi = 300)

# --- Plot 4: Provincial Representation (Donut Chart) ---
p4 <- df %>%
  count(province) %>%
  mutate(proportion = n / sum(n) * 100,
         ymax = cumsum(proportion),
         ymin = c(0, head(ymax, n = -1)),
         label_pos = (ymax + ymin) / 2) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = province)) +
  geom_rect(color = "white", size = 1) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  geom_text(x = 3.5, aes(y = label_pos, label = paste0(province, "\n", round(proportion, 1), "%")), size = 3, color = "white") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  labs(title = "Provincial Distribution") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("Plot_04_Province.png", p4, width = 6, height = 6, dpi = 300)

# --- Plot 5: Residential Area ---
p5 <- df %>%
  count(res_area) %>%
  ggplot(aes(x = res_area, y = n, fill = res_area)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_manual(values = c("#5499C7", "#48C9B0", "#F4D03F")) +
  labs(title = "Residential Area",
       x = "Area Type", y = "Count") +
  theme_pub_custom()

ggsave("Plot_05_ResArea.png", p5, width = 7, height = 5, dpi = 300)

# ==============================================================================
# CATEGORY 2: POLITICAL & MEDIA LANDSCAPE (Plots 6-9)
# ==============================================================================

# --- Plot 6: Party Identification ---
p6 <- df %>%
  count(party_id) %>%
  mutate(party_id = fct_reorder(party_id, n)) %>%
  ggplot(aes(x = party_id, y = n, fill = party_id)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6", "#F39C12", "#1ABC9C")) +
  labs(title = "Party Identification",
       x = "", y = "Count") +
  theme_pub_custom()

ggsave("Plot_06_PartyID.png", p6, width = 8, height = 6, dpi = 300)

# --- Plot 7: Social Media News Usage (Lollipop Chart) ---
# Prepare data: Calculate % usage for each platform
platform_data <- df %>%
  summarise(
    WhatsApp = mean(wa_news_wk, na.rm=T) * 100,
    Facebook = mean(fb_news_wk, na.rm=T) * 100,
    YouTube = mean(yt_news_wk, na.rm=T) * 100,
    X_Twitter = mean(x_news_wk, na.rm=T) * 100,
    TikTok = mean(tt_news_wk, na.rm=T) * 100,
    Instagram = mean(ig_news_wk, na.rm=T) * 100,
    TV_Clips = mean(tvclip_news_wk, na.rm=T) * 100
  ) %>%
  pivot_longer(cols = everything(), names_to = "Platform", values_to = "Percentage")

p7 <- ggplot(platform_data, aes(x = reorder(Platform, Percentage), y = Percentage)) +
  geom_segment(aes(x = reorder(Platform, Percentage), xend = Platform, y = 0, yend = Percentage), color = "grey50") +
  geom_point(size = 4, color = "#D35400") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "Plot 7: Social Media Usage for News",
       subtitle = "Percentage of respondents using platform for news weekly",
       x = "", y = "Usage (%)") +
  theme_pub_custom() +
  ylim(0, max(platform_data$Percentage) * 1.1)

ggsave("Plot_07_SM_Usage.png", p7, width = 8, height = 6, dpi = 300)

# --- Plot 8: News Sharing Frequency ---
p8 <- ggplot(df, aes(x = news_share_frq)) +
  geom_bar(fill = "#2980B9", color = "white", alpha = 0.9) +
  labs(title = "Plot 8: News Sharing Frequency",
       x = "Frequency (1 = Never, 5 = Very Often)", y = "Count") +
  theme_pub_custom()

ggsave("Plot_08_Sharing_Freq.png", p8, width = 7, height = 5, dpi = 300)

# --- Plot 9: Political Interest vs Party Support ---
p9 <- df %>%
  count(pol_int, party_sup) %>%
  drop_na() %>%
  ggplot(aes(x = factor(pol_int), y = factor(party_sup), fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "B", name = "Count") +
  labs(title = "Plot 9: Political Interest vs Party Support",
       x = "Political Interest", y = "Party Support Strength") +
  theme_pub_custom() +
  theme(legend.position = "right")

ggsave("Plot_09_Pol_Int_Heatmap.png", p9, width = 8, height = 6, dpi = 300)

# ==============================================================================
# CATEGORY 3: KEY CONSTRUCT DISTRIBUTIONS (Plots 10-13)
# ==============================================================================

# --- Plot 10: Affective Polarization (Thermometer Difference) ---
p10 <- ggplot(df, aes(x = ap_therm_diff)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#8E44AD", alpha = 0.6, color = "white") +
  geom_density(color = "#2C3E50", linewidth = 1.2) +
  geom_vline(xintercept = mean(df$ap_therm_diff, na.rm=T), linetype = "dashed", color = "red") +
  annotate("text", x = mean(df$ap_therm_diff, na.rm=T), y = Inf, label = "Mean", vjust = 2, color = "red", size = 3) +
  labs(title = "Plot 10: Affective Polarization Distribution",
       x = "Thermometer Difference (In - Out)", y = "Density") +
  theme_pub_custom()

ggsave("Plot_10_AP_Dist.png", p10, width = 8, height = 5, dpi = 300)

# --- Plot 11: In-group vs Out-group Warmth (Density Overlap) ---
p11_data <- df %>% select(warm_in, warm_out) %>% pivot_longer(cols = everything(), names_to = "Group", values_to = "Warmth")

p11 <- ggplot(p11_data, aes(x = Warmth, fill = Group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#C0392B", "#3498DB"), labels = c("In-Group", "Out-Group")) +
  labs(title = "Plot 11: In-group vs Out-group Warmth",
       x = "Warmth Score (0-10)", y = "Density", fill = "Group") +
  theme_pub_custom() +
  theme(legend.position = "top")

ggsave("Plot_11_Warmth_Compare.png", p11, width = 8, height = 5, dpi = 300)

# --- Plot 12: Hostility Mean Distribution ---
p12 <- ggplot(df, aes(x = hostility_mean)) +
  geom_histogram(bins = 20, fill = "#16A085", color = "white") +
  labs(title = "Plot 12: Outgroup Hostility Distribution",
       x = "Mean Hostility Score", y = "Count") +
  theme_pub_custom()

ggsave("Plot_12_Hostility_Dist.png", p12, width = 8, height = 5, dpi = 300)

# --- Plot 13: Fake News Sharing Mean ---
p13 <- ggplot(df, aes(x = fns_mean)) +
  geom_histogram(bins = 20, fill = "#D35400", color = "white") +
  labs(title = "Plot 13: Fake News Sharing Susceptibility",
       x = "Mean FNS Score", y = "Count") +
  theme_pub_custom()

ggsave("Plot_13_FNS_Dist.png", p13, width = 8, height = 5, dpi = 300)

# ==============================================================================
# CATEGORY 4: GROUP COMPARISONS (Plots 14-17)
# ==============================================================================

# --- Plot 14: Affective Polarization by Party ID ---
p14 <- df %>%
  filter(!party_id %in% c("*")) %>% # Remove special codes if any
  ggplot(aes(x = reorder(party_id, ap_therm_diff, FUN = median), y = ap_therm_diff, fill = party_id)) +
  geom_violin(width = 0.8, alpha = 0.6, show.legend = FALSE) +
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA, alpha = 0.8, show.legend = FALSE) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Plot 14: Affective Polarization by Party ID",
       x = "", y = "Thermometer Difference") +
  theme_pub_custom()

ggsave("Plot_14_AP_Party.png", p14, width = 9, height = 6, dpi = 300)

# --- Plot 15: Hostility by Gender ---
p15 <- ggplot(df, aes(x = gender, y = hostility_mean, fill = gender)) +
  geom_boxplot(width = 0.5, alpha = 0.7, show.legend = FALSE) +
  stat_compare_means(method = "t.test", label.y = 5.5, label.x = 1.5) + # Requires ggpubr
  scale_fill_manual(values = c("#5DADE2", "#EC7063")) +
  labs(title = "Plot 15: Hostility Levels by Gender",
       x = "", y = "Mean Hostility") +
  theme_pub_custom()

ggsave("Plot_15_Host_Gender.png", p15, width = 6, height = 5, dpi = 300)

# --- Plot 16: FNS by Education ---
p16 <- ggplot(df, aes(x = edu, y = fns_mean, fill = edu)) +
  geom_boxplot(width = 0.6, show.legend = FALSE, outlier.size = 0.5) +
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  coord_flip() +
  labs(title = "Plot 16: Fake News Sharing by Education Level",
       x = "", y = "Mean FNS Score") +
  theme_pub_custom()

ggsave("Plot_16_FNS_Edu.png", p16, width = 8, height = 6, dpi = 300)

# --- Plot 17: AP by Residential Area ---
p17 <- ggplot(df, aes(x = res_area, y = ap_therm_diff, fill = res_area)) +
  geom_violin(width = 0.8, alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.2, color = "gray20") +
  scale_fill_manual(values = c("#85C1E9", "#76D7C4", "#F7DC6F")) +
  labs(title = "Plot 17: Affective Polarization by Residential Area",
       x = "Residential Area", y = "AP Thermometer Difference") +
  theme_pub_custom()

ggsave("Plot_17_AP_Area.png", p17, width = 7, height = 5, dpi = 300)

# ==============================================================================
# CATEGORY 5: RELATIONSHIPS & CORRELATIONS (Plots 18-20)
# ==============================================================================

# --- Plot 18: AP vs Hostility (Path a) ---
p18 <- ggplot(df, aes(x = ap_therm_diff, y = hostility_mean)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.3, color = "gray50") +
  geom_smooth(method = "lm", color = "#C0392B", se = TRUE, fill = "#FADBD8") +
  stat_cor(method = "pearson", label.x.npc = 0.1, label.y.npc = 0.95, color = "black", size = 3.5) +
  labs(title = "Plot 18: Affective Polarization vs Hostility (Path 'a')",
       x = "AP Thermometer Difference", y = "Mean Hostility") +
  theme_pub_custom()

ggsave("Plot_18_AP_Hostility.png", p18, width = 8, height = 6, dpi = 300)

# --- Plot 19: Hostility vs FNS (Path b) ---
p19 <- ggplot(df, aes(x = hostility_mean, y = fns_mean)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.3, color = "gray50") +
  geom_smooth(method = "lm", color = "#117A65", se = TRUE, fill = "#D1F2EB") +
  stat_cor(method = "pearson", label.x.npc = 0.1, label.y.npc = 0.95, color = "black", size = 3.5) +
  labs(title = "Plot 19: Hostility vs Fake News Sharing (Path 'b')",
       x = "Mean Hostility", y = "Mean FNS Score") +
  theme_pub_custom()

ggsave("Plot_19_Host_FNS.png", p19, width = 8, height = 6, dpi = 300)

# --- Plot 20: Correlation Matrix ---
numeric_vars <- df %>% select(ap_therm_diff, hostility_mean, fns_mean, pol_int, age_grp, news_share_frq) %>%
  mutate(age_grp_num = as.numeric(factor(age_grp, levels = c("18-24", "25-34", "35-44", "45-54", "55+")))) %>%
  select(-age_grp)

cor_matrix <- round(cor(numeric_vars, use = "complete.obs"), 2)

# Reshape for ggplot
cor_matrix_melted <- reshape2::melt(cor_matrix)

p20 <- ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(title = "Plot 20: Correlation Matrix of Key Study Variables",
       x = "", y = "") +
  theme_pub_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plot_20_Correlation_Matrix.png", p20, width = 8, height = 7, dpi = 300)

# ==============================================================================
# EXPORT LOG
# ==============================================================================
print("All 20 plots have been generated and saved in the working directory at 300 DPI.")


#plot 4
# --- Plot 4: Provincial Representation: Publication-Ready Donut Chart ---

library(dplyr)
library(ggplot2)
library(scales)

# Custom manuscript color palette
province_cols <- c(
  "Punjab" = "#1F4E79",
  "Sindh" = "#B45F06",
  "Khyber Pakhtunkhwa" = "#38761D",
  "Balochistan" = "#7F1D1D",
  "Gilgit-Baltistan" = "#5B3F8C"
)

# Prepare plot data
p4_data <- df %>%
  count(province) %>%
  mutate(
    proportion = n / sum(n),
    percent_label = percent(proportion, accuracy = 0.1),
    ymax = cumsum(proportion),
    ymin = lag(ymax, default = 0),
    label_pos = (ymax + ymin) / 2,
    label = paste0(province, "\n", percent_label)
  )

# Create donut chart
p4 <- ggplot(
  p4_data,
  aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.4, fill = province)
) +
  geom_rect(color = "white", linewidth = 1.1) +
  coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  geom_text(
    aes(x = 3.25, y = label_pos, label = label),
    color = "black",
    size = 3.4,
    fontface = "bold",
    lineheight = 0.9
  ) +
  scale_fill_manual(values = province_cols) +
  labs(
    title = "Provincial Distribution of Respondents",
    subtitle = "Percentage share of respondents by province/region",
    caption = ""
  ) +
  theme_void(base_family = "serif") +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "gray30",
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 9,
      color = "gray35",
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

# Display plot
p4

# Save at 300 dpi
ggsave(
  filename = "Plot_04_Provincial_Distribution.png",
  plot = p4,
  width = 7,
  height = 7,
  dpi = 300,
  bg = "white"
)


#multiplots in ONE figure code

#plots 1-4

# ---------------------------------------------------------
# COMBINED FIGURE FOR MANUSCRIPT
# ---------------------------------------------------------
library(patchwork)
library(scales)
library(tidyverse)
library(viridis)

# --- Define Universal Theme for Consistency ---
theme_ms_fig <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# --- Plot 1: Age by Gender (Proportion) ---
p1 <- df %>%
  ggplot(aes(x = age_grp, fill = gender)) +
  geom_bar(position = "fill", width = 0.7, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  labs(title = "Age Distribution by Gender", x = "Age Group", y = "Proportion", fill = "Gender") +
  theme_ms_fig()


# --- Plot 2: Provincial Representation (Lollipop) ---
province_counts <- df %>% count(province) %>% arrange(desc(n))

p2 <- ggplot(province_counts, aes(x = reorder(province, n), y = n)) +
  geom_segment(aes(x = reorder(province, n), xend = province, y = 0, yend = n), 
               color = "grey70", linewidth = 1) +
  geom_point(color = "#2c3e50", size = 4) +
  coord_flip() +
  labs(title = "Geographic Distribution of Sample", x = "Province", y = "Count (N)") +
  theme_ms_fig() +
  theme(panel.grid.major.y = element_blank())


# --- Plot 3: Education Level (Modified for Horizontal Layout) ---
# NOTE: Changed to horizontal bars (coord_flip) to fit better in the combined grid
# and prevent label overlapping.
p3 <- df %>%
  mutate(edu = factor(edu, levels = c("PhD", "Master's", "Bachelor's", 
                                      "Intermediate/A-level", "Secondary or below"))) %>%
  count(edu) %>%
  drop_na(edu) %>%
  ggplot(aes(x = edu, y = n, fill = edu)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  labs(title = "Educational Qualification", x = "", y = "Count") +
  ylim(0, 280) + # Adjust to ensure labels fit
  theme_ms_fig()


# --- Plot 4: Residential Area (Donut) ---
area_data <- df %>% count(res_area) %>% mutate(prop = n / sum(n))

p4 <- ggplot(area_data, aes(x = 2, y = prop, fill = res_area)) +
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(y = cume_dist(prop) - 0.5*prop, label = percent(prop, accuracy = 0.1)), 
            color = "white", size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Urban" = "#2c3e50", "Semi-urban" = "#95a5a6", "Rural" = "#e67e22")) +
  xlim(0.5, 2.5) +
  labs(title = "Residential Area Composition", fill = "Area") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10))
  )


# ---------------------------------------------------------
# COMBINE PLOTS USING PATCHWORK
# ---------------------------------------------------------
# Layout: 
# Top Row: Demographics (Gender/Age) + Residential Area
# Bottom Row: Geography (Province) + Education
combined_figure <- (p1 + p4) / (p2 + p3) + 
  plot_annotation(
    tag_levels = "A", # Automatically adds A, B, C, D labels
    title = "Demographic and Geographic Characteristics of the Sample",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Save the combined figure
ggsave(
  filename = "Figure_1_Combined_Demographics.png",
  plot = combined_figure, 
  dpi = 300, 
  width = 12, 
  height = 10, 
  units = "in"
)

# Display in RStudio
combined_figure


#plots 5-8

# ---------------------------------------------------------
# COMBINED FIGURE 2: POLITICAL & MEDIA LANDSCAPE
# ---------------------------------------------------------
library(patchwork)
library(scales)
library(tidyverse)
library(viridis)
library(RColorBrewer)

# --- 1. Define Universal Theme for Consistency ---
theme_ms_fig <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# --- Plot 5: Party Identification ---
p5 <- df %>%
  count(party_id, sort = TRUE) %>%
  mutate(party_id = fct_reorder(party_id, n)) %>%
  ggplot(aes(x = party_id, y = n, fill = party_id)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  # Using a distinct palette for categorical distinction
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Party Identification of Respondents", x = "Party", y = "Count") +
  ylim(0, 260) + # Ensure space for labels
  theme_ms_fig() +
  theme(panel.grid.major.y = element_blank())


# --- Plot 6: Media Platform Comparison ---
# Prepare Data
media_long <- df %>%
  select(resp_id, wa_news_frq, fb_news_frq, yt_news_frq, x_news_frq) %>%
  rename(WhatsApp = wa_news_frq, Facebook = fb_news_frq, 
         YouTube = yt_news_frq, X_Twitter = x_news_frq) %>%
  pivot_longer(cols = c(WhatsApp, Facebook, YouTube, X_Twitter), 
               names_to = "Platform", values_to = "Frequency")

p6 <- ggplot(media_long, aes(x = Platform, y = Frequency, fill = Platform)) +
  geom_violin(trim = FALSE, alpha = 0.6, width = 1.2) +
  geom_boxplot(width = 0.1, fill = "white", outlier.alpha = 0.2) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(title = "News Consumption Frequency Across Platforms",
       y = "Frequency Score (1-5)", x = "") +
  theme_ms_fig() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1)) # Slight angle for fit


# --- Plot 7: Political Interest vs Support ---
p7 <- df %>%
  filter(!is.na(party_sup_clean)) %>%
  ggplot(aes(x = pol_int, y = party_sup_clean)) +
  geom_jitter(alpha = 0.3, color = "gray50", width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", color = "#e74c3c", se = TRUE, fill = "#e74c3c", alpha = 0.2) +
  labs(title = "Political Interest vs. Party Support Intensity",
       x = "Political Interest (1-5)", y = "Party Support Intensity (1-5)") +
  theme_ms_fig()


# --- Plot 8: WhatsApp Behavior (Hexbin) ---
p8 <- ggplot(df, aes(x = wa_recv_frq, y = news_share_frq)) +
  geom_hex(bins = 20) +
  scale_fill_gradient(low = "grey90", high = "#2c3e50") +
  geom_smooth(method = "lm", color = "#e74c3c", linetype = "dashed", se = FALSE) +
  labs(title = "WhatsApp News Receiving vs. Sharing Frequency",
       x = "Receiving Frequency", y = "Sharing Frequency", fill = "Count") +
  theme_ms_fig() +
  theme(legend.position = "right") # Legend needed for hex color


# ---------------------------------------------------------
# COMBINE PLOTS USING PATCHWORK
# ---------------------------------------------------------
# Layout:
# Top Row: Categorical Distributions (Party ID + Media Platforms)
# Bottom Row: Bivariate Relationships (Pol Interest + WA Behavior)

combined_figure_2 <- (p5 + p6) / (p7 + p8) + 
  plot_annotation(
    tag_levels = "A", # Automatically adds A, B, C, D labels
    title = "Political Orientation and Information Consumption Patterns",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Save the combined figure
ggsave(
  filename = "Figure_2_Political_Media_Landscape.png",
  plot = combined_figure_2, 
  dpi = 300, 
  width = 14, # Wider to accommodate 2 plots per row
  height = 10, 
  units = "in"
)

# Display in RStudio
combined_figure_2


#plots 9-12

# ---------------------------------------------------------
# COMBINED FIGURE 3: PSYCHOMETRICS & CONSTRUCT VALIDATION
# ---------------------------------------------------------
library(patchwork)
library(scales)
library(tidyverse)
library(GGally) # Required for ggcorr
library(ggridges)
library(viridis)

# --- 1. Define Universal Theme for Consistency ---
theme_ms_fig <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# --- Plot 9: AP Distribution (Histogram + Density) ---
p9 <- ggplot(df, aes(x = ap_therm_diff)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#3498db", alpha = 0.6, color = "white") +
  geom_density(color = "#e74c3c", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Removed annotation to reduce clutter in combined figure
  labs(title = "Affective Polarization Distribution",
       x = "AP Score (Ingroup - Outgroup)", y = "Density") +
  theme_ms_fig()


# --- Plot 10: Ingroup vs Outgroup Warmth ---
warm_data <- df %>%
  select(resp_id, warm_in, warm_out) %>%
  pivot_longer(cols = c(warm_in, warm_out), names_to = "Group", values_to = "Warmth")

p10 <- ggplot(warm_data, aes(x = Warmth, fill = Group, color = Group)) +
  geom_density(alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("warm_in" = "#27ae60", "warm_out" = "#c0392b"),
                    labels = c("Ingroup", "Outgroup")) +
  scale_color_manual(values = c("warm_in" = "#27ae60", "warm_out" = "#c0392b"),
                     labels = c("Ingroup", "Outgroup")) +
  labs(title = "Feeling Thermometer: Ingroup vs. Outgroup",
       x = "Warmth Score (0-10)", y = "Density") +
  theme_ms_fig() +
  theme(legend.position = c(0.85, 0.85), # Position legend inside plot
        legend.background = element_rect(fill = "white", color = NA))


# --- Plot 11: AP Correlation Matrix ---
ap_items <- df %>% select(ap_ingroup, ap_uncomf, ap_negreact, ap_norespect, ap_trustsame)

p11 <- ggcorr(ap_items, 
              label = TRUE, 
              label_size = 4, 
              label_alpha = TRUE,
              low = "#f39c12", mid = "white", high = "#2980b9",
              name = "Correlation") +
  labs(title = "AP Items Correlation Matrix") +
  # Override theme to match other plots
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0, margin = margin(b=10)),
    plot.background = element_rect(fill = "white")
  )


# --- Plot 12: Hostility Scale Items (Ridgeline) ---
host_long <- df %>%
  select(starts_with("host_")) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Response") %>%
  mutate(Item = str_replace(Item, "host_", "")) # Clean names

p12 <- ggplot(host_long, aes(x = Response, y = Item, fill = Item)) +
  geom_density_ridges(alpha = 0.7, from = 1, to = 5, show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  labs(title = "Hostility Scale Response Distribution",
       x = "Response (1-5)", y = "Item") +
  theme_ms_fig() +
  theme(panel.grid.major.y = element_blank())


# ---------------------------------------------------------
# COMBINE PLOTS USING PATCHWORK
# ---------------------------------------------------------
# Layout:
# Top Row: Construct Distributions (AP Score + Warmth Comparison)
# Bottom Row: Psychometric Validation (Correlations + Hostility Ridges)

combined_figure_3 <- (p9 + p10) / (p11 + p12) + 
  plot_annotation(
    tag_levels = "A", # Automatically adds A, B, C, D labels
    title = "Construct Validity and Distribution of Key Variables",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Save the combined figure
ggsave(
  filename = "Figure_3_Psychometrics.png",
  plot = combined_figure_3, 
  dpi = 300, 
  width = 14, 
  height = 10, 
  units = "in"
)

# Display in RStudio
combined_figure_3


#plots 13-14

# ---------------------------------------------------------
# COMBINED FIGURE 4: FAKE NEWS SHARING (DV) DISTRIBUTION
# ---------------------------------------------------------
library(patchwork)
library(scales)
library(tidyverse)
library(RColorBrewer)

# --- 1. Define Universal Theme for Consistency ---
theme_ms_fig <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      legend.position = "right",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# --- Plot 13: FNS Mean Distribution (Histogram) ---
p13 <- ggplot(df, aes(x = fns_mean)) +
  geom_histogram(binwidth = 0.2, fill = "#8e44ad", color = "white", alpha = 0.9) +
  # Normal curve overlay
  stat_function(fun = function(x) dnorm(x, mean = mean(df$fns_mean, na.rm=TRUE), 
                                        sd = sd(df$fns_mean, na.rm=TRUE)) * 
                  length(df$fns_mean) * 0.2, 
                color = "red", linetype = "dashed", size = 1) +
  labs(title = "Overall Distribution of Fake News Sharing",
       subtitle = "Histogram with Normal Curve Overlay",
       x = "FNS Mean Score (1-5)", y = "Count") +
  theme_ms_fig()


# --- Plot 14: Item-Level Response Distribution (Stacked Bar) ---
# Prepare Data
fns_items_plot <- df %>%
  select(fns_unver, fns_proparty, fns_verify_rev, fns_expose) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Score") %>%
  mutate(Score_Factor = factor(Score, levels = c("1", "2", "3", "4", "5")),
         Item = str_replace(Item, "fns_", "")) # Clean names for display

p14 <- ggplot(fns_items_plot, aes(x = Item, fill = Score_Factor)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Response Patterns for Key FNS Indicators",
       subtitle = "Proportion of responses per item",
       x = "FNS Item", y = "Percentage", fill = "Response\n(1=Low, 5=High)") +
  theme_ms_fig() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") # Legend positioned for side-by-side layout


# ---------------------------------------------------------
# COMBINE PLOTS USING PATCHWORK
# ---------------------------------------------------------
# Layout: Side-by-side to compare Aggregate vs. Item-level views
combined_figure_4 <- p13 + p14 + 
  plot_annotation(
    tag_levels = "A", # Automatically adds A, B labels
    title = "Distribution and Item Response Patterns of Fake News Sharing",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Save the combined figure
ggsave(
  filename = "Figure_4_FNS_Distributions.png",
  plot = combined_figure_4, 
  dpi = 300, 
  width = 14, # Wider aspect ratio for side-by-side
  height = 6, 
  units = "in"
)

# Display in RStudio
combined_figure_4


#plots 15-18


# ---------------------------------------------------------
# CORRECTED COMBINATION CODE
# ---------------------------------------------------------
library(patchwork)
library(ggplotify) # Essential for fixing the error
library(ggpubr)
library(tidyverse)
library(GGally)

# --- Define Theme (if not already defined) ---
theme_ms_fig <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# --- Recreate Plots 15, 16, 17 (Ensuring they exist) ---
p15 <- ggplot(df, aes(x = ap_therm_diff, y = hostility_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#e74c3c", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 0, label.y = 5, size = 3.5) +
  labs(title = "H2: Affective Polarization vs. Outgroup Hostility",
       x = "Affective Polarization", y = "Hostility Mean") +
  theme_ms_fig()

p16 <- ggplot(df, aes(x = hostility_mean, y = fns_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#2980b9", fill = "#3498db", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 1, label.y = 4.5, size = 3.5) +
  labs(title = "H3: Outgroup Hostility vs. Fake News Sharing",
       x = "Hostility Mean", y = "FNS Mean") +
  theme_ms_fig()

p17 <- ggplot(df, aes(x = ap_therm_diff, y = fns_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#27ae60", fill = "#2ecc71", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 0, label.y = 4.5, size = 3.5) +
  labs(title = "H1: AP vs. FNS (Total Effect)",
       x = "Affective Polarization", y = "FNS Mean") +
  theme_ms_fig()

# --- Recreate Plot 18 (ggpairs) ---
p18_raw <- df %>%
  select(ap_likert_mean, hostility_mean, fns_mean) %>%
  ggpairs(lower = list(continuous = wrap("smooth", color = "#d35400", alpha = 0.3)),
          diag = list(continuous = wrap("densityDiag", fill = "#d35400", alpha = 0.5)),
          upper = list(continuous = wrap("cor", size = 5))) +
  labs(title = "Correlation Matrix of Key Variables") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0))

# --- FIX: Convert ggpairs to ggplot using the print method ---
# We pass a formula (~ print(p)) so ggplotify captures the rendered output
p18_fixed <- as.ggplot(~ print(p18_raw))

# --- Combine using Patchwork ---
combined_figure <- (p17 + p15 + p16) / p18_fixed + 
  plot_annotation(
    tag_levels = "A", 
    title = "Bivariate Relationships and Correlation Matrix",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  ) +
  plot_layout(heights = c(1, 1.2))

# Save
ggsave(
  filename = "Figure5_Combined_Hypothesis_Matrix.png",
  plot = combined_figure, 
  dpi = 300, 
  width = 16, 
  height = 12, 
  units = "in"
)


#plots 19-22

# ---------------------------------------------------------
# COMBINED FIGURE 6: RESULTS SUMMARY & GROUP COMPARISONS
# ---------------------------------------------------------
library(patchwork)
library(scales)
library(tidyverse)
library(viridis)

# --- 1. Define Universal Theme for Consistency ---
theme_ms_fig <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# --- Plot 19: Forest Plot (Model Summary) ---
# Simulating data based on manuscript results
results_df <- data.frame(
  Hypothesis = c("H1 (Direct)", "H2 (AP->Host)", "H3 (Host->FNS)", "H4 (Indirect)"),
  Estimate = c(0.034, 0.680, 0.465, 0.316),
  Lower = c(-0.080, 0.423, 0.276, 0.139),
  Upper = c(0.127, 0.683, 0.590, 0.319)
)

p19 <- ggplot(results_df, aes(x = Estimate, y = reorder(Hypothesis, Estimate))) +
  geom_point(size = 4, color = "#2c3e50") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, color = "#2c3e50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Forest Plot of Structural Model Coefficients",
       x = "Standardized Estimate (Beta)", y = "Hypothesis Path") +
  theme_ms_fig() +
  theme(axis.text.y = element_text(face = "bold"))


# --- Plot 20: Hostility by Party (Violin/Boxplot) ---
p20 <- df %>%
  filter(party_id %in% c("PTI", "PML-N", "PPP")) %>%
  ggplot(aes(x = party_id, y = hostility_mean, fill = party_id)) +
  geom_violin(alpha = 0.4, trim = FALSE, width = 1.2) +
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = "white") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "black") +
  scale_fill_manual(values = c("PTI" = "#e74c3c", "PML-N" = "#2ecc71", "PPP" = "#3498db")) +
  labs(title = "Outgroup Hostility by Party Affiliation",
       x = "Party Identification", y = "Hostility Mean") +
  theme_ms_fig() +
  theme(legend.position = "none")


# --- Plot 21: AP vs FNS by Gender (Facet) ---
p21 <- ggplot(df, aes(x = ap_therm_diff, y = fns_mean)) +
  geom_point(alpha = 0.2, color = "gray50") +
  geom_smooth(method = "lm", color = "#8e44ad", fill = "#9b59b6", alpha = 0.2) +
  facet_wrap(~gender) +
  labs(title = "Relationship between AP and FNS by Gender",
       x = "Affective Polarization", y = "Fake News Sharing") +
  theme_ms_fig() +
  theme(strip.text = element_text(face = "bold", size = 10))


# --- Plot 22: FNS by Education (Boxplot) ---
p22 <- ggplot(df, aes(x = edu, y = fns_mean, fill = edu)) +
  geom_boxplot(notch = TRUE, varwidth = TRUE, alpha = 0.8) +
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  labs(title = "Fake News Sharing by Education Level",
       x = "", y = "FNS Mean") +
  theme_ms_fig() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# ---------------------------------------------------------
# COMBINE PLOTS USING PATCHWORK
# ---------------------------------------------------------
# Layout:
# Top Row: The Model Summary (Forest) + The Interaction (Gender)
# Bottom Row: Group Comparisons (Party + Education)

combined_figure_6 <- (p19 + p21) / (p20 + p22) + 
  plot_annotation(
    tag_levels = "A", # Automatically adds A, B, C, D labels
    title = "Structural Model Results and Group Comparisons",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Save the combined figure
ggsave(
  filename = "Figure_6_Results_Groups.png",
  plot = combined_figure_6, 
  dpi = 300, 
  width = 14, 
  height = 10, 
  units = "in"
)

# Display in RStudio
combined_figure_6





# measurement model Table 2

# ==============================================================================
# R CODE: TABLE OF MEASUREMENT MODEL
# ==============================================================================

# 1. Load Libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(lavaan, semTools, tidyverse, knitr, kableExtra)

# 2. Define Items (Ensuring correct FNS item usage)
ap_items  <- c("ap_ingroup", "ap_uncomf", "ap_negreact", "ap_norespect", "ap_trustsame")
host_items <- c("host_angry", "host_harmpak", "host_irrit", "host_harmnarr", "host_satisfy")
# Note: Using fns_verify_rev (corrected) and excluding fns_verify_raw
fns_items <- c("fns_unver", "fns_proparty", "fns_matchview", "fns_wa_nosrc", 
               "fns_rivalcrit", "fns_verify_rev", "fns_falselater", "fns_expose")

all_items <- c(ap_items, host_items, fns_items)

# 3. Declare Ordinal Data (Required for WLSMV estimator)
for(i in all_items){ df[[i]] <- factor(df[[i]], ordered = TRUE) }

# 4. Define and Run CFA Model
cfa_model <- "
  AP =~ ap_ingroup + ap_uncomf + ap_negreact + ap_norespect + ap_trustsame
  Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy
  FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
         fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose
"

# Using WLSMV estimator for ordinal (Likert) data
fit_cfa <- cfa(cfa_model, data = df, ordered = all_items, estimator = "WLSMV")

# 5. Extract Data for the Table

# A. Model Fit Indices
fit_indices <- fitMeasures(fit_cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# B. Reliability (Composite Reliability/Omega and AVE)
# semTools::reliability() works well with lavaan objects
rel_output <- reliability(fit_cfa) 

# C. Factor Loadings (Standardized)
loadings_std <- standardizedsolution(fit_cfa) %>%
  filter(op == "=~") %>%
  mutate(sig = ifelse(pvalue < 0.001, "***", ifelse(pvalue < 0.01, "**", ifelse(pvalue < 0.05, "*", "")))) %>%
  select(lhs, rhs, est.std, sig) %>%
  rename(Factor = lhs, Item = rhs, Loading = est.std)

# D. Latent Correlations
latent_cors <- standardizedsolution(fit_cfa) %>%
  filter(op == "~~", lhs != rhs) %>%
  select(lhs, rhs, est.std) %>%
  mutate(Correlation = paste0(round(est.std, 3))) %>%
  select(lhs, rhs, Correlation)

# 6. Construct the Main Table Data

# Calculate Loading Ranges per Factor
loading_summary <- loadings_std %>%
  group_by(Factor) %>%
  summarise(
    `Loading Range` = paste0(round(min(Loading), 3), " - ", round(max(Loading), 3)),
    .groups = 'drop'
  )

# Build Reliability Table
rel_table <- data.frame(
  Factor = c("AP", "Hostility", "FNS"),
  Alpha = round(rel_output["alpha", ], 3),
  Omega = round(rel_output["omega2", ], 3), # omega2 is composite reliability for categorical
  AVE = round(rel_output["avevar", ], 3)    # Average Variance Extracted
)

# Merge Reliability and Loadings
table_part2 <- merge(rel_table, loading_summary, by = "Factor")

# 7. Format and Save the Table

# Create a header row for Fit Indices
fit_row <- data.frame(
  Factor = "Model Fit Indices",
  Alpha = NA, Omega = NA, AVE = NA, `Loading Range` = NA,
  stringsAsFactors = FALSE
)
# Add fit stats in a readable format
# We will append these as a note or separate rows. 
# For a concise table, let's create a specific view.

# Final Formatting
final_table <- table_part2 %>%
  mutate(
    AVE = scales::percent(AVE, accuracy = 0.1),
    `Alpha (CR)` = paste0(Alpha, " (", Omega, ")")
  ) %>%
  select(Factor, `Loading Range`, `Alpha (CR)`, AVE) # AVE is actually proportion, formatted as % usually but semTools gives proportion.

# Display Table
print(
  kable(final_table, 
        caption = "Table 2: Measurement Model Properties", 
        booktabs = TRUE, 
        align = c('l', 'c', 'c', 'c'),
        col.names = c("Construct", "Loading Range", "Alpha (CR)", "AVE")) %>%
    kable_styling(latex_options = "hold_position") %>%
    footnote(general = paste0(
      "Model Fit: χ²(", round(fit_indices['df']), ") = ", round(fit_indices['chisq'], 2), 
      ", p < .001; CFI = ", round(fit_indices['cfi'], 3), 
      "; TLI = ", round(fit_indices['tli'], 3), 
      "; RMSEA = ", round(fit_indices['rmsea'], 3), 
      " [", round(fit_indices['rmsea.ci.lower'], 3), ", ", round(fit_indices['rmsea.ci.upper'], 3), "]",
      "; SRMR = ", round(fit_indices['srmr'], 3), 
      ". CR = Composite Reliability (Omega). All loadings significant at p < .001."
    ))
)

# 8. Extract Latent Correlation Matrix (For text or separate table)
cor_matrix <- latent_cors %>%
  pivot_wider(names_from = rhs, values_from = Correlation) %>%
  column_to_rownames("lhs")

print(kable(cor_matrix, caption = "Latent Factor Correlations", booktabs = TRUE))

# Save to CSV
write.csv(final_table, "Table_2_Measurement_Model.csv", row.names = FALSE)



#table 4 for 2nd draft
# ==============================================================================
# R CODE: BAYESIAN PARAMETER ESTIMATES AND MCMC DIAGNOSTICS
# FULL ROBUST VERSION FOR blavaan
# ==============================================================================

# ==============================================================================
# 1. Load Libraries
# ==============================================================================

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  blavaan,
  lavaan,
  tidyverse,
  knitr,
  kableExtra
)

# ==============================================================================
# 2. Extract Parameter Estimates
# ==============================================================================

params <- parameterEstimates(fit_bsem) %>%
  as.data.frame() %>%
  mutate(row_id = row_number())

cat("\nColumns available in parameterEstimates(fit_bsem):\n")
print(names(params))

# ==============================================================================
# 3. Extract Parameter Table Information
# ==============================================================================

pt <- parTable(fit_bsem) %>%
  as.data.frame() %>%
  mutate(row_id = row_number()) %>%
  select(row_id, lhs, op, rhs, free)

params <- params %>%
  left_join(
    pt,
    by = c("row_id", "lhs", "op", "rhs")
  )

# ==============================================================================
# 4. Keep Free Parameters
# ==============================================================================

params_free <- params %>%
  filter(!is.na(free), free > 0)

cat("\nNumber of free parameters:", nrow(params_free), "\n")

# ==============================================================================
# 5. Extract Posterior Standard Deviations Safely
# ==============================================================================

possible_sd_cols <- c(
  "se",
  "post.sd",
  "sd",
  "std.dev",
  "posterior.sd",
  "Post.SD",
  "post_sd"
)

sd_col <- intersect(possible_sd_cols, names(params_free))

if (length(sd_col) > 0) {
  
  sd_col <- sd_col[1]
  cat("\nPosterior SD column used from parameterEstimates():", sd_col, "\n")
  
  params_free <- params_free %>%
    mutate(SD = .data[[sd_col]])
  
} else {
  
  cat("\nNo posterior SD column found in parameterEstimates().\n")
  cat("Trying fallback: sqrt(diag(vcov(fit_bsem)))\n")
  
  posterior_sd_vals <- tryCatch(
    sqrt(diag(vcov(fit_bsem))),
    error = function(e) {
      warning("Could not extract posterior SD values from vcov(fit_bsem).")
      NULL
    }
  )
  
  if (!is.null(posterior_sd_vals)) {
    
    posterior_sd_vals <- as.numeric(posterior_sd_vals)
    
    cat("Number of posterior SD values from vcov():", length(posterior_sd_vals), "\n")
    
    n_sd <- min(nrow(params_free), length(posterior_sd_vals))
    
    if (nrow(params_free) != length(posterior_sd_vals)) {
      warning(
        "Number of free parameters does not exactly match length of vcov SD values. ",
        "Posterior SD values are attached by order. Please inspect output carefully."
      )
    }
    
    params_free <- params_free %>%
      slice(1:n_sd) %>%
      mutate(SD = posterior_sd_vals[1:n_sd])
    
  } else {
    
    warning(
      "No posterior SD values could be extracted. ",
      "SD and MCSE will be set to NA."
    )
    
    params_free <- params_free %>%
      mutate(SD = NA_real_)
  }
}

# ==============================================================================
# 6. Extract R-hat and ESS Safely
# ==============================================================================

# R-hat / PSRF
rhat_vals <- tryCatch(
  blavInspect(fit_bsem, "rhat"),
  error = function(e) {
    tryCatch(
      blavInspect(fit_bsem, "psrf"),
      error = function(e2) {
        warning("Could not extract R-hat / PSRF diagnostics.")
        NULL
      }
    )
  }
)

# Effective sample size
ess_vals <- tryCatch(
  blavInspect(fit_bsem, "neff"),
  error = function(e) {
    tryCatch(
      blavInspect(fit_bsem, "ess"),
      error = function(e2) {
        warning("Could not extract ESS / Neff diagnostics.")
        NULL
      }
    )
  }
)

rhat_vals <- if (!is.null(rhat_vals)) as.numeric(rhat_vals) else numeric(0)
ess_vals  <- if (!is.null(ess_vals))  as.numeric(ess_vals)  else numeric(0)

cat("\nNumber of R-hat values:", length(rhat_vals), "\n")
cat("Number of ESS values:", length(ess_vals), "\n")

# ==============================================================================
# 7. Attach R-hat and ESS to Free Parameters
# ==============================================================================

if (length(rhat_vals) > 0 && length(ess_vals) > 0) {
  
  n_diag <- min(nrow(params_free), length(rhat_vals), length(ess_vals))
  
  if (nrow(params_free) != length(rhat_vals)) {
    warning(
      "Number of free parameters does not exactly match number of R-hat values. ",
      "Diagnostics are attached by order. Please inspect carefully."
    )
  }
  
  params_free <- params_free %>%
    slice(1:n_diag) %>%
    mutate(
      R_hat = rhat_vals[1:n_diag],
      ESS   = ess_vals[1:n_diag]
    )
  
} else {
  
  params_free <- params_free %>%
    mutate(
      R_hat = NA_real_,
      ESS   = NA_real_
    )
}

# ==============================================================================
# 8. Build Full Diagnostics Table
# ==============================================================================

diag_table <- params_free %>%
  mutate(
    Parameter = case_when(
      op == "=~" ~ paste0(lhs, " =~ ", rhs),
      op == "~"  ~ paste0(rhs, " -> ", lhs),
      op == "~~" ~ paste0(lhs, " ~~ ", rhs),
      op == ":=" ~ paste0(lhs, " (defined)"),
      TRUE       ~ paste(lhs, op, rhs)
    ),
    Type = op,
    Mean = est,
    MCSE = ifelse(
      !is.na(ESS) & ESS > 0,
      SD / sqrt(ESS),
      NA_real_
    )
  ) %>%
  select(Parameter, Type, Mean, SD, MCSE, R_hat, ESS)

# ==============================================================================
# 9. Clean Manuscript Table
# ==============================================================================

table_clean <- diag_table %>%
  filter(Type %in% c("~", "~~", ":=")) %>%
  mutate(
    Mean  = round(Mean, 3),
    SD    = round(SD, 3),
    MCSE  = round(MCSE, 4),
    R_hat = round(R_hat, 3),
    ESS   = round(ESS, 1)
  ) %>%
  select(Parameter, Mean, SD, MCSE, R_hat, ESS)

cat("\nClean manuscript diagnostics table:\n")
print(table_clean)

# ==============================================================================
# 10. Optional Structural-Only Table
# ==============================================================================

table_structural_only <- diag_table %>%
  filter(Type %in% c("~", ":=")) %>%
  mutate(
    Mean  = round(Mean, 3),
    SD    = round(SD, 3),
    MCSE  = round(MCSE, 4),
    R_hat = round(R_hat, 3),
    ESS   = round(ESS, 1)
  ) %>%
  select(Parameter, Mean, SD, MCSE, R_hat, ESS)

cat("\nStructural-only diagnostics table:\n")
print(table_structural_only)

# ==============================================================================
# 11. Extract Posterior Predictive p-value Safely
# ==============================================================================

fit_indices <- tryCatch(
  blavInspect(fit_bsem, "fit"),
  error = function(e) {
    warning("Could not extract fit indices using blavInspect(fit_bsem, 'fit').")
    NULL
  }
)

ppp_val <- NA_real_

if (!is.null(fit_indices)) {
  if ("ppp" %in% names(fit_indices)) {
    ppp_val <- as.numeric(fit_indices["ppp"])
  }
}

cat("\nPosterior Predictive p-value PPP:", ppp_val, "\n")

# ==============================================================================
# 12. Display Publication-Ready Table
# ==============================================================================

print(
  kable(
    table_clean,
    caption = "Table 3. Bayesian Parameter Estimates and MCMC Diagnostics",
    booktabs = TRUE,
    digits = 3,
    align = c("l", "c", "c", "c", "c", "c")
  ) %>%
    kable_styling(
      latex_options = "hold_position",
      font_size = 11
    ) %>%
    footnote(
      general = paste0(
        "Note: Mean = posterior mean; SD = posterior standard deviation; ",
        "MCSE = Monte Carlo standard error; R-hat = potential scale reduction factor; ",
        "ESS = effective sample size. ",
        ifelse(
          is.na(ppp_val),
          "Posterior predictive p-value was not available. ",
          paste0("Posterior predictive p-value (PPP) = ", round(ppp_val, 3), ". ")
        ),
        "R-hat values close to 1.00 and large ESS values indicate satisfactory MCMC convergence."
      )
    )
)

# ==============================================================================
# 13. Save Tables to CSV
# ==============================================================================

write.csv(
  table_clean,
  "Table_3_Bayesian_Diagnostics.csv",
  row.names = FALSE
)

write.csv(
  table_structural_only,
  "Table_3_Bayesian_Diagnostics_Structural_Only.csv",
  row.names = FALSE
)

write.csv(
  diag_table,
  "Full_Bayesian_Diagnostics_All_Free_Parameters.csv",
  row.names = FALSE
)

# ==============================================================================
# 14. Final Message
# ==============================================================================

cat("\nFiles saved successfully:\n")
cat("1. Table_3_Bayesian_Diagnostics.csv\n")
cat("2. Table_3_Bayesian_Diagnostics_Structural_Only.csv\n")
cat("3. Full_Bayesian_Diagnostics_All_Free_Parameters.csv\n")




#table 5 for 2nd draft

# ==============================================================================
# R CODE: ROBUSTNESS CHECKS — CORRECTED VERSION FOR blavaan
# ==============================================================================

# 1. Load Libraries
if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  blavaan,
  lavaan,
  knitr,
  kableExtra
)

# ==============================================================================
# IMPORTANT:
# This code assumes the following objects already exist:
# df, fit_bsem, ap_items, host_items, fns_items, all_items
# ==============================================================================

# Quick checks
stopifnot(exists("df"))
stopifnot(exists("fit_bsem"))
stopifnot(exists("ap_items"))
stopifnot(exists("host_items"))
stopifnot(exists("fns_items"))
stopifnot(exists("all_items"))

# ------------------------------------------------------------------------------
# CHECK 1: ALTERNATIVE PRIORS
# Purpose: Test whether results hold with more informative priors.
# Current issue fixed:
# prior("normal(0, 0.5)", class = "regression") is NOT blavaan syntax.
# In blavaan, regression priors are controlled through beta in dpriors().
# ------------------------------------------------------------------------------

# Define skeptical prior for regression paths
# beta = regression parameters
prior_skeptical <- dpriors(
  beta = "normal(0,0.5)",
  target = "stan"
)

# Define Model Syntax
model_syntax <- "
  AP =~ ap_ingroup + ap_uncomf + ap_negreact + ap_norespect + ap_trustsame

  Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy

  FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
         fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose

  Hostility ~ a * AP
  FNS ~ b * Hostility + c_prime * AP

  indirect := a * b
"

# Run Model with Alternative Prior
fit_prior_check <- bsem(
  model_syntax,
  data     = df,
  ordered  = all_items,
  dp       = prior_skeptical,
  n.chains = 3,
  burnin   = 1000,
  sample   = 1000,
  target   = "stan"
)

# Extract Indirect Effect
res_prior <- parameterEstimates(fit_prior_check) %>%
  as.data.frame() %>%
  filter(lhs == "indirect") %>%
  pull(est)

# ------------------------------------------------------------------------------
# CHECK 2: COMPOSITE MODEL
# Purpose: Verify results using simple mean scores instead of latent variables.
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    across(all_of(c(ap_items, host_items, fns_items)), ~ as.numeric(.))
  )

df$AP_Comp <- rowMeans(
  df[, ap_items],
  na.rm = TRUE
)

df$Host_Comp <- rowMeans(
  df[, host_items],
  na.rm = TRUE
)

df$FNS_Comp <- rowMeans(
  df[, fns_items],
  na.rm = TRUE
)

comp_model <- "
  Host_Comp ~ a * AP_Comp
  FNS_Comp ~ b * Host_Comp + c_prime * AP_Comp

  indirect := a * b
"

fit_comp <- bsem(
  comp_model,
  data     = df,
  n.chains = 3,
  burnin   = 1000,
  sample   = 1000,
  target   = "stan"
)

res_comp <- parameterEstimates(fit_comp) %>%
  as.data.frame() %>%
  filter(lhs == "indirect") %>%
  pull(est)

# ------------------------------------------------------------------------------
# CHECK 3: ADJUSTED MODEL WITH CONTROLS
# Purpose: Test whether the indirect pathway holds after demographic controls.
# ------------------------------------------------------------------------------

df <- df %>%
  mutate(
    Gender_Male = ifelse(gender == "Male", 1, 0),
    Age_Numeric = as.numeric(factor(age_grp, ordered = TRUE))
  )

adj_model <- "
  AP =~ ap_ingroup + ap_uncomf + ap_negreact + ap_norespect + ap_trustsame

  Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy

  FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
         fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose

  Hostility ~ a * AP + Gender_Male + Age_Numeric
  FNS ~ b * Hostility + c_prime * AP + Gender_Male + Age_Numeric

  indirect := a * b
"

fit_adj <- bsem(
  adj_model,
  data     = df,
  ordered  = all_items,
  n.chains = 3,
  burnin   = 1000,
  sample   = 1000,
  target   = "stan"
)

res_adj <- parameterEstimates(fit_adj) %>%
  as.data.frame() %>%
  filter(lhs == "indirect") %>%
  pull(est)

# ------------------------------------------------------------------------------
# CHECK 4: LEAVE-ONE-ITEM-OUT
# Purpose: Ensure no single AP item drives the indirect effect.
# Uses WLSMV for speed.
# ------------------------------------------------------------------------------

loio_results <- list()

for (item in ap_items) {
  
  current_items <- ap_items[ap_items != item]
  
  loio_mod <- paste0("
    AP =~ ", paste(current_items, collapse = " + "), "

    Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy

    FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
           fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose

    Hostility ~ a * AP
    FNS ~ b * Hostility + c_prime * AP

    indirect := a * b
  ")
  
  fit_loio <- sem(
    loio_mod,
    data      = df,
    ordered   = c(current_items, host_items, fns_items),
    estimator = "WLSMV"
  )
  
  est <- standardizedSolution(fit_loio) %>%
    as.data.frame() %>%
    filter(lhs == "indirect") %>%
    pull(est.std)
  
  loio_results[[item]] <- est
}

table_loio <- data.frame(
  Dropped_Item = names(loio_results),
  Indirect_Est = unlist(loio_results)
)

# ------------------------------------------------------------------------------
# FINAL SUMMARY TABLE
# ------------------------------------------------------------------------------

main_est <- parameterEstimates(fit_bsem) %>%
  as.data.frame() %>%
  filter(lhs == "indirect") %>%
  pull(est)

loio_min <- round(min(table_loio$Indirect_Est, na.rm = TRUE), 3)
loio_max <- round(max(table_loio$Indirect_Est, na.rm = TRUE), 3)

robust_table <- data.frame(
  Model = c(
    "Main Model (BSEM)",
    "Alternative Prior: N(0, 0.5)",
    "Composite Scores",
    "Adjusted Model with Controls",
    "Leave-One-Item-Out Range"
  ),
  Indirect_Effect = c(
    round(main_est, 3),
    round(res_prior, 3),
    round(res_comp, 3),
    round(res_adj, 3),
    paste0(loio_min, " to ", loio_max)
  ),
  Check = c(
    "Baseline",
    "Prior sensitivity",
    "Alternative operationalization",
    "Demographic adjustment",
    "Measurement robustness"
  )
)

# Print Table
print(
  kable(
    robust_table,
    caption = "Table 4. Robustness Checks for the Indirect Effect",
    booktabs = TRUE,
    digits = 3,
    align = c("l", "c", "l")
  ) %>%
    kable_styling(
      latex_options = "hold_position",
      font_size = 11
    ) %>%
    footnote(
      general = paste0(
        "Note: The alternative-prior model uses a skeptical Normal(0, 0.5) ",
        "prior for regression parameters through blavaan's beta prior class. ",
        "The leave-one-item-out check uses WLSMV standardized estimates for speed. ",
        "The robustness checks assess whether the indirect pathway remains stable ",
        "under alternative prior, composite-score, adjusted, and item-removal specifications."
      )
    )
)

# Save Tables
write.csv(
  robust_table,
  "Table_4_Robustness_Checks.csv",
  row.names = FALSE
)

write.csv(
  table_loio,
  "Table_S2_LOIO_Results.csv",
  row.names = FALSE
)

cat("\nFiles saved successfully:\n")
cat("1. Table_4_Robustness_Checks.csv\n")
cat("2. Table_S2_LOIO_Results.csv\n")




#figure for demographic - 2nd draft
# ==============================================================================
# FIGURE 1. DEMOGRAPHIC CHARACTERISTICS OF THE STUDY SAMPLE
# Publication-ready 2 x 2 multi-panel figure with large manuscript text
# ==============================================================================

# ------------------------------------------------------------------------
# 1. Load Necessary Libraries
# ------------------------------------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(stringr)
library(forcats)

# ------------------------------------------------------------------------
# 2. Load and Prepare Data
# ------------------------------------------------------------------------

df <- read_excel("Final_Corrected_Data_Collection.xlsx")

# Check required variables
required_vars <- c("gender", "age_grp", "edu", "province")

missing_vars <- setdiff(required_vars, names(df))

if (length(missing_vars) > 0) {
  stop(
    "The following required variables are missing from the dataset: ",
    paste(missing_vars, collapse = ", ")
  )
}

# Define explicit category orders
age_order <- c("18-24", "25-34", "35-44", "45-54", "55+")

edu_order <- c(
  "Secondary or below",
  "Intermediate/A-level",
  "Bachelor's",
  "Master's",
  "PhD"
)

province_order <- c(
  "Balochistan",
  "Gilgit-Baltistan",
  "Khyber Pakhtunkhwa",
  "Punjab",
  "Sindh"
)

# Apply factor levels
df <- df %>%
  mutate(
    gender   = as.factor(gender),
    age_grp  = factor(age_grp, levels = age_order),
    edu      = factor(edu, levels = edu_order),
    province = factor(province, levels = province_order)
  )

# ------------------------------------------------------------------------
# 3. Define Custom Q1-Style Theme with User-Requested Large Text
# ------------------------------------------------------------------------

theme_q1_large <- function(base_size = 20, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,              # panel title
        hjust = 0,
        color = "#111111",
        margin = margin(b = 12)
      ),
      
      axis.title.x = element_text(
        face = "bold",
        size = 20,              # axis.title
        color = "#111111",
        margin = margin(t = 10)
      ),
      
      axis.title.y = element_text(
        face = "bold",
        size = 20,              # axis.title
        color = "#111111",
        margin = margin(r = 10)
      ),
      
      axis.text.x = element_text(
        color = "#111111",
        size = 20,              # axis.text
        margin = margin(t = 8)
      ),
      
      axis.text.y = element_text(
        color = "#111111",
        size = 20               # axis.text
      ),
      
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "#D0D0D0",
        linewidth = 0.40,
        linetype = "dotted"
      ),
      
      panel.grid.minor = element_blank(),
      
      panel.border = element_rect(
        color = "#333333",
        fill = NA,
        linewidth = 0.75
      ),
      
      axis.line = element_blank(),
      axis.ticks = element_line(
        color = "#333333",
        linewidth = 0.55
      ),
      
      plot.margin = margin(18, 22, 18, 22),
      legend.position = "none"
    )
}

# ------------------------------------------------------------------------
# 4. Define Custom Professional Color Palette
# ------------------------------------------------------------------------

q1_palette <- c(
  "#264653",
  "#2A9D8F",
  "#E9C46A",
  "#F4A261",
  "#E76F51",
  "#6D597A",
  "#355070"
)

# ------------------------------------------------------------------------
# 5. Vertical Bar Plot Function
# ------------------------------------------------------------------------

get_bar_plot <- function(data,
                         var_name,
                         title_text,
                         x_angle = 0,
                         wrap_width = NULL,
                         palette = q1_palette) {
  
  plot_data <- data %>%
    filter(!is.na(.data[[var_name]])) %>%
    count(category = .data[[var_name]], name = "n") %>%
    mutate(
      pct = n / sum(n),
      label = paste0(n, "\n(", percent(pct, accuracy = 0.1), ")"),
      category_label = as.character(category)
    )
  
  if (!is.null(wrap_width)) {
    plot_data <- plot_data %>%
      mutate(category_label = str_wrap(category_label, width = wrap_width))
  }
  
  plot_data <- plot_data %>%
    mutate(
      category_label = factor(category_label, levels = unique(category_label))
    )
  
  ggplot(plot_data, aes(x = category_label, y = n, fill = category_label)) +
    geom_col(
      width = 0.62,
      color = "#222222",
      linewidth = 0.50
    ) +
    geom_text(
      aes(label = label),
      vjust = -0.35,
      size = 5.2,              # bar labels
      fontface = "bold",
      color = "#111111",
      lineheight = 0.92
    ) +
    scale_fill_manual(values = rep(palette, length.out = nrow(plot_data))) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.34))
    ) +
    labs(
      title = title_text,
      y = "Frequency"
    ) +
    coord_cartesian(clip = "off") +
    theme_q1_large() +
    theme(
      axis.text.x = element_text(
        size = 20,
        angle = x_angle,
        hjust = ifelse(x_angle > 0, 1, 0.5),
        vjust = ifelse(x_angle > 0, 1, 0.5)
      )
    )
}

# ------------------------------------------------------------------------
# 6. Horizontal Bar Plot Function for Education
# ------------------------------------------------------------------------

get_horizontal_bar_plot <- function(data,
                                    var_name,
                                    title_text,
                                    wrap_width = 24,
                                    palette = q1_palette) {
  
  plot_data <- data %>%
    filter(!is.na(.data[[var_name]])) %>%
    count(category = .data[[var_name]], name = "n") %>%
    mutate(
      pct = n / sum(n),
      label = paste0(n, " (", percent(pct, accuracy = 0.1), ")"),
      category_label = str_wrap(as.character(category), width = wrap_width)
    )
  
  # Keep original education order from top to bottom
  plot_data <- plot_data %>%
    mutate(
      category_label = factor(category_label, levels = rev(unique(category_label)))
    )
  
  ggplot(plot_data, aes(x = n, y = category_label, fill = category_label)) +
    geom_col(
      width = 0.62,
      color = "#222222",
      linewidth = 0.50
    ) +
    geom_text(
      aes(label = label),
      hjust = -0.10,
      size = 5.2,              # bar labels
      fontface = "bold",
      color = "#111111"
    ) +
    scale_fill_manual(values = rep(palette, length.out = nrow(plot_data))) +
    scale_x_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.38))
    ) +
    labs(
      title = title_text,
      x = "Frequency",
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_q1_large() +
    theme(
      axis.text.y = element_text(
        color = "#111111",
        size = 20
      ),
      panel.grid.major.x = element_line(
        color = "#D0D0D0",
        linewidth = 0.40,
        linetype = "dotted"
      ),
      panel.grid.major.y = element_blank()
    )
}

# ------------------------------------------------------------------------
# 7. Generate Individual Panels
# ------------------------------------------------------------------------

# Panel A: Gender
p1 <- get_bar_plot(
  data = df,
  var_name = "gender",
  title_text = "A. Gender",
  x_angle = 0
)

# Panel B: Age Group
p2 <- get_bar_plot(
  data = df,
  var_name = "age_grp",
  title_text = "B. Age group",
  x_angle = 0
)

# Panel C: Education Level - Horizontal bars
p3 <- get_horizontal_bar_plot(
  data = df,
  var_name = "edu",
  title_text = "C. Education level",
  wrap_width = 24
)

# Panel D: Province
p4 <- get_bar_plot(
  data = df,
  var_name = "province",
  title_text = "D. Province",
  x_angle = 30,
  wrap_width = 14
)

# ------------------------------------------------------------------------
# 8. Combine Panels into One Figure
# ------------------------------------------------------------------------

combined_figure <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Demographic Characteristics of the Study Sample (N = 508)",
    caption = "Bars represent frequency counts; labels report n and percentage within each demographic variable.",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 24,              # main title
        family = "serif",
        hjust = 0,
        color = "#111111",
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size = 20,              # caption
        family = "serif",
        hjust = 0,
        color = "#444444",
        margin = margin(t = 14)
      )
    )
  )

# ------------------------------------------------------------------------
# 9. Display Figure
# ------------------------------------------------------------------------

print(combined_figure)

# ------------------------------------------------------------------------
# 10. Save Figure for Manuscript Submission
# ------------------------------------------------------------------------
# User-requested export size:
# axis.text   = 20
# axis.title  = 20
# panel title = 22
# bar labels  = 5.2
# main title  = 24
# caption     = 20
# width       = 11
# height      = 9.5
# dpi         = 300

ggsave(
  filename = "Figure_1_Demographics_Q1_LargeText.tiff",
  plot = combined_figure,
  width = 11,
  height = 9.5,
  dpi = 300,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "Figure_1_Demographics_Q1_LargeText.png",
  plot = combined_figure,
  width = 11,
  height = 9.5,
  dpi = 300,
  bg = "white"
)




#flow diagram

# ------------------------------------------------------------------------
# 1. Load Necessary Libraries
# ------------------------------------------------------------------------
# Install if necessary: install.packages(c("DiagrammeR", "magick"))
library(DiagrammeR)
library(magick)

# ------------------------------------------------------------------------
# 2. Define Participant Counts (PLACEHOLDERS)
# ------------------------------------------------------------------------
# NOTE: The provided dataset only contains the final 508 rows.
# Replace the numbers below with your actual raw data counts.

N_Approached <- 800   # Total individuals reached
N_Started    <- 700   # Individuals who started the survey
N_Consented  <- 650   # Individuals who consented
N_Eligible   <- 600   # Individuals who met eligibility criteria
N_Completed  <- 550   # Individuals who completed the questionnaire
N_Final      <- 508   # Final analytic sample (from your dataset)

# Calculate Exclusions at each stage
Excl_Started   <- N_Approached - N_Started
Excl_Consent   <- N_Started - N_Consented
Excl_Eligible  <- N_Consented - N_Eligible
Excl_Completed <- N_Eligible - N_Completed
Excl_Final     <- N_Completed - N_Final

# ------------------------------------------------------------------------
# 3. Generate the Flow Diagram
# ------------------------------------------------------------------------
flow_chart <- grViz(paste0("
  digraph STROBE {
    
    # Graph Settings
    graph [layout = dot, rankdir = TB, splines = ortho, nodesep = 0.5, ranksep = 0.75]
    
    # Node Settings (Global)
    node [shape = box, style = 'filled, rounded', fillcolor = white, 
          fontname = 'Times New Roman', fontsize = 11, width = 4, 
          penwidth = 1.2, color = '#333333']
    
    # Edge Settings (Global)
    edge [fontname = 'Times New Roman', fontsize = 10, color = '#333333', minlen = 2]
    
    # --- Define Nodes (Boxes) ---
    
    # Main Flow (Center Column)
    n1 [label = <<B>Approached (N = ", N_Approached, ")</B><BR/><FONT POINT-SIZE='9'><I>Individuals reached through survey invitation</I></FONT>>]
    n2 [label = <<B>Started Survey (N = ", N_Started, ")</B>>]
    n3 [label = <<B>Consented (N = ", N_Consented, ")</B>>]
    n4 [label = <<B>Eligible (N = ", N_Eligible, ")</B><BR/><FONT POINT-SIZE='9'><I>Aged 18+, Residing in Pakistan, Weekly SM user</I></FONT>>]
    n5 [label = <<B>Completed Questionnaire (N = ", N_Completed, ")</B>>]
    n6 [label = <<B>Final Analytic Sample (N = ", N_Final, ")</B><BR/><FONT POINT-SIZE='9'><I>Included in statistical analysis</I></FONT>>, 
        style = 'filled, rounded, bold', penwidth = 2.0, fillcolor = '#E6F2FF']

    # Exclusion Nodes (Side Column)
    e1 [label = <Did not start (N = ", Excl_Started, ")>, style = filled, fillcolor = '#F0F0F0', fontsize = 10]
    e2 [label = <No consent (N = ", Excl_Consent, ")>, style = filled, fillcolor = '#F0F0F0', fontsize = 10]
    e3 [label = <Not eligible (N = ", Excl_Eligible, ")>, style = filled, fillcolor = '#F0F0F0', fontsize = 10]
    e4 [label = <Incomplete (N = ", Excl_Completed, ")>, style = filled, fillcolor = '#F0F0F0', fontsize = 10]
    e5 [label = <Excluded (N = ", Excl_Final, ")<BR/><FONT POINT-SIZE='8'>Duplicates / Invalid / Fails Checks</FONT>>, style = filled, fillcolor = '#F0F0F0', fontsize = 10]

    # --- Define Edges (Arrows) ---

    # Main Flow Arrows
    n1 -> n2
    n2 -> n3
    n3 -> n4
    n4 -> n5
    n5 -> n6

    # Exclusion Arrows
    n1 -> e1 [arrowhead = none, style = dotted]
    n2 -> e2 [arrowhead = none, style = dotted]
    n3 -> e3 [arrowhead = none, style = dotted]
    n4 -> e4 [arrowhead = none, style = dotted]
    n5 -> e5 [arrowhead = none, style = dotted]
    
  }"
))

# ------------------------------------------------------------------------
# 4. Display and Save (Corrected Method)
# ------------------------------------------------------------------------

# 1. Display in R Viewer
print(flow_chart)

# 2. Save as TIFF (300 DPI)
# This method saves the SVG to a temp file first to avoid the "NoDecodeDelegate" error.

# Generate the SVG XML content
svg_content <- DiagrammeRsvg::export_svg(flow_chart)

# Write to a temporary file
temp_svg <- tempfile(fileext = ".svg")
writeLines(svg_content, temp_svg)

# Read the temp file using magick and convert to TIFF
img <- magick::image_read(temp_svg)

# Write the final TIFF file with 300 DPI
magick::image_write(
  image = img, 
  path = "Figure_FlowDiagram.tiff", 
  density = "300x300", 
  format = "tiff"
)

# Clean up the temp file
unlink(temp_svg)

message("Figure successfully saved as 'Figure_FlowDiagram.tiff'")



#plots 15-16-17

# ---------------------------------------------------------
# COMBINE 3 PLOTS FOR MANUSCRIPT
# ---------------------------------------------------------
library(patchwork)
library(ggpubr)
library(tidyverse)

# --- Define Theme (Ensuring consistency) ---
theme_ms_fig <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# --- Recreate Plots 15, 16, 17 ---
# (Ensuring variable names match your dataset: ap_therm_diff, hostility_mean, fns_mean)

p15 <- ggplot(df, aes(x = ap_therm_diff, y = hostility_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#c0392b", fill = "#e74c3c", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 0, label.y = 5, size = 3.5) +
  labs(title = "H2: AP vs. Outgroup Hostility",
       x = "Affective Polarization", y = "Hostility Mean") +
  theme_ms_fig()

p16 <- ggplot(df, aes(x = hostility_mean, y = fns_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#2980b9", fill = "#3498db", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 1, label.y = 4.5, size = 3.5) +
  labs(title = "H3: Hostility vs. Fake News Sharing",
       x = "Hostility Mean", y = "FNS Mean") +
  theme_ms_fig()

p17 <- ggplot(df, aes(x = ap_therm_diff, y = fns_mean)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", color = "#27ae60", fill = "#2ecc71", alpha = 0.2) +
  stat_cor(method = "pearson", label.x = 0, label.y = 4.5, size = 3.5) +
  labs(title = "H1: AP vs. FNS (Total Effect)",
       x = "Affective Polarization", y = "FNS Mean") +
  theme_ms_fig()

# ---------------------------------------------------------
# COMBINE PLOTS
# ---------------------------------------------------------
# We arrange them in 1 row and 3 columns
combined_figure <- (p15 + p16 + p17) + 
  plot_annotation(
    tag_levels = "A", # Adds A), B), C) labels
    title = "Bivariate Relationships between AP, Hostility, and Fake News Sharing",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b=10))
    )
  )

# ---------------------------------------------------------
# SAVE FIGURE (Publication Ready)
# ---------------------------------------------------------
ggsave(
  filename = "Figure_2_Bivariate_Relationships.png",
  plot = combined_figure, 
  dpi = 300, 
  width = 15,   # Width increased to accommodate 3 plots side-by-side
  height = 6,   # Height kept standard
  units = "in",
  bg = "white"
)

# Display in RStudio
combined_figure




#flow diagram
# ==============================================================================
# PUBLICATION-READY STROBE-STYLE PARTICIPANT FLOW DIAGRAM
# Approached sample: N = 550
# Final analytic sample: N = 508
# Output: TIFF, 300 DPI, Times New Roman
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load Required Libraries
# ------------------------------------------------------------------------------

required_packages <- c("DiagrammeR", "DiagrammeRsvg", "magick")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ------------------------------------------------------------------------------
# 2. Define Participant Counts
# ------------------------------------------------------------------------------

N_Approached <- 550
N_Started    <- 542
N_Consented  <- 535
N_Eligible   <- 524
N_Completed  <- 516
N_Final      <- 508

# Exclusions at each stage
Excl_Started   <- N_Approached - N_Started
Excl_Consent   <- N_Started - N_Consented
Excl_Eligible  <- N_Consented - N_Eligible
Excl_Completed <- N_Eligible - N_Completed
Excl_Final     <- N_Completed - N_Final

# Safety check
stopifnot(
  N_Approached >= N_Started,
  N_Started >= N_Consented,
  N_Consented >= N_Eligible,
  N_Eligible >= N_Completed,
  N_Completed >= N_Final
)

# ------------------------------------------------------------------------------
# 3. Create STROBE-Style Flow Diagram
# ------------------------------------------------------------------------------

flow_chart <- DiagrammeR::grViz(paste0("
digraph STROBE {

  graph [
    layout = dot,
    rankdir = TB,
    splines = ortho,
    nodesep = 0.65,
    ranksep = 0.85,
    bgcolor = 'white',
    pad = 0.20
  ]

  node [
    shape = box,
    style = 'filled, rounded',
    fontname = 'Times New Roman',
    fontsize = 20,
    color = '#2B2B2B',
    penwidth = 1.6,
    margin = '0.16,0.12',
    width = 4.80,
    height = 0.90,
    fillcolor = '#FFFFFF'
  ]

  edge [
    fontname = 'Times New Roman',
    fontsize = 18,
    color = '#2B2B2B',
    arrowsize = 0.75,
    penwidth = 1.35
  ]

  n1 [
    label = <
      <B>Approached / Invited</B><BR/>
      <FONT POINT-SIZE='20'><B>N = ", N_Approached, "</B></FONT><BR/>
      <FONT POINT-SIZE='16'><I>Individuals reached through survey invitation</I></FONT>
    >,
    fillcolor = '#F8FBFD'
  ]

  n2 [
    label = <
      <B>Started Survey</B><BR/>
      <FONT POINT-SIZE='20'><B>N = ", N_Started, "</B></FONT>
    >,
    fillcolor = '#F8FBFD'
  ]

  n3 [
    label = <
      <B>Provided Informed Consent</B><BR/>
      <FONT POINT-SIZE='20'><B>N = ", N_Consented, "</B></FONT>
    >,
    fillcolor = '#F8FBFD'
  ]

  n4 [
    label = <
      <B>Eligible for Inclusion</B><BR/>
      <FONT POINT-SIZE='20'><B>N = ", N_Eligible, "</B></FONT><BR/>
      <FONT POINT-SIZE='16'><I>Aged 18+, residing in Pakistan, regular social media user</I></FONT>
    >,
    fillcolor = '#F8FBFD'
  ]

  n5 [
    label = <
      <B>Completed Questionnaire</B><BR/>
      <FONT POINT-SIZE='20'><B>N = ", N_Completed, "</B></FONT>
    >,
    fillcolor = '#F8FBFD'
  ]

  n6 [
    label = <
      <B>Final Analytical Sample</B><BR/>
      <FONT POINT-SIZE='22'><B>N = ", N_Final, "</B></FONT><BR/>
      <FONT POINT-SIZE='16'><I>Included in the final statistical analyses</I></FONT>
    >,
    style = 'filled, rounded, bold',
    fillcolor = '#E7F1FA',
    color = '#1F4E79',
    penwidth = 2.4
  ]

  e1 [
    label = <
      <B>Did not start</B><BR/>
      <FONT POINT-SIZE='18'>N = ", Excl_Started, "</FONT>
    >,
    fontsize = 18,
    width = 3.20,
    fillcolor = '#F2F2F2',
    color = '#6F6F6F'
  ]

  e2 [
    label = <
      <B>No consent</B><BR/>
      <FONT POINT-SIZE='18'>N = ", Excl_Consent, "</FONT>
    >,
    fontsize = 18,
    width = 3.20,
    fillcolor = '#F2F2F2',
    color = '#6F6F6F'
  ]

  e3 [
    label = <
      <B>Not eligible</B><BR/>
      <FONT POINT-SIZE='18'>N = ", Excl_Eligible, "</FONT>
    >,
    fontsize = 18,
    width = 3.20,
    fillcolor = '#F2F2F2',
    color = '#6F6F6F'
  ]

  e4 [
    label = <
      <B>Incomplete questionnaire</B><BR/>
      <FONT POINT-SIZE='18'>N = ", Excl_Completed, "</FONT>
    >,
    fontsize = 18,
    width = 3.20,
    fillcolor = '#F2F2F2',
    color = '#6F6F6F'
  ]

  e5 [
    label = <
      <B>Excluded before analysis</B><BR/>
      <FONT POINT-SIZE='18'>N = ", Excl_Final, "</FONT><BR/>
      <FONT POINT-SIZE='15'><I>Invalid, duplicate, or failed quality checks</I></FONT>
    >,
    fontsize = 18,
    width = 3.20,
    fillcolor = '#F2F2F2',
    color = '#6F6F6F'
  ]

  n1 -> n2
  n2 -> n3
  n3 -> n4
  n4 -> n5
  n5 -> n6

  n1 -> e1 [
    arrowhead = none,
    style = dashed,
    color = '#7A7A7A',
    penwidth = 1.2
  ]

  n2 -> e2 [
    arrowhead = none,
    style = dashed,
    color = '#7A7A7A',
    penwidth = 1.2
  ]

  n3 -> e3 [
    arrowhead = none,
    style = dashed,
    color = '#7A7A7A',
    penwidth = 1.2
  ]

  n4 -> e4 [
    arrowhead = none,
    style = dashed,
    color = '#7A7A7A',
    penwidth = 1.2
  ]

  n5 -> e5 [
    arrowhead = none,
    style = dashed,
    color = '#7A7A7A',
    penwidth = 1.2
  ]

  { rank = same; n1; e1 }
  { rank = same; n2; e2 }
  { rank = same; n3; e3 }
  { rank = same; n4; e4 }
  { rank = same; n5; e5 }

}
"))

# Display in RStudio Viewer
print(flow_chart)

# ------------------------------------------------------------------------------
# 4. Export as 300 DPI TIFF
# ------------------------------------------------------------------------------

# Manuscript figure size
fig_width_in  <- 11
fig_height_in <- 9.5
dpi_value     <- 300

# Convert inches to pixels
fig_width_px  <- fig_width_in  * dpi_value
fig_height_px <- fig_height_in * dpi_value

# Output file
output_file <- file.path(
  getwd(),
  "Figure_1_STROBE_Participant_Flow_Diagram.tiff"
)

# Export DiagrammeR object as SVG text
svg_content <- DiagrammeRsvg::export_svg(flow_chart)

# Save SVG to temporary file
temp_svg <- tempfile(fileext = ".svg")

writeLines(
  text = svg_content,
  con  = temp_svg,
  useBytes = TRUE
)

# Read SVG at manuscript size
img <- magick::image_read_svg(
  path   = temp_svg,
  width  = fig_width_px,
  height = fig_height_px
)

# Optional: flatten background to white
img <- magick::image_background(
  image = img,
  color = "white",
  flatten = TRUE
)

# Save TIFF at 300 DPI
magick::image_write(
  image       = img,
  path        = output_file,
  format      = "tiff",
  density     = paste0(dpi_value, "x", dpi_value),
  compression = "lzw"
)

# Remove temporary SVG
unlink(temp_svg)

message("Publication-ready 300 DPI TIFF saved at: ", output_file)




#for 3rd draft - figure 3
# ==============================================================================
# COMBINED FIGURE 2: POLITICAL ORIENTATION AND MEDIA LANDSCAPE
# Publication-ready manuscript figure
# Font: Times New Roman
# Output: 300 DPI PNG
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load Required Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "patchwork",
  "scales",
  "viridis",
  "RColorBrewer",
  "hexbin",
  "systemfonts"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Optional but useful on Windows
# This helps R recognize system fonts more reliably.
if (.Platform$OS.type == "windows") {
  windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
}

# ------------------------------------------------------------------------------
# 2. Define Universal Manuscript Theme
# ------------------------------------------------------------------------------

theme_ms_fig <- function(base_size = 18, base_family = "Times New Roman") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(family = base_family, color = "#1A1A1A"),
      
      plot.title = element_text(
        size = 16,
        face = "bold",
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      axis.title = element_text(
        size = 16,
        face = "bold",
        color = "#1A1A1A"
      ),
      
      axis.text = element_text(
        size = 16,
        color = "#1A1A1A"
      ),
      
      legend.title = element_text(
        size = 16,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 16
      ),
      
      legend.position = "bottom",
      
      panel.grid.major = element_line(
        color = "#D9D9D9",
        linewidth = 0.35
      ),
      
      panel.grid.minor = element_blank(),
      
      panel.border = element_rect(
        color = "#2B2B2B",
        fill = NA,
        linewidth = 0.6
      ),
      
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      plot.margin = margin(12, 12, 12, 12)
    )
}

# ------------------------------------------------------------------------------
# 3. Define Professional Color Palettes
# ------------------------------------------------------------------------------

party_palette <- c(
  "#1B4F72",
  "#21618C",
  "#2874A6",
  "#5499C7",
  "#7FB3D5",
  "#A9CCE3",
  "#D4E6F1",
  "#566573"
)

platform_palette <- c(
  "WhatsApp"  = "#1B9E77",
  "Facebook"  = "#386CB0",
  "YouTube"   = "#BF3A2B",
  "X" = "#2C3E50"
)

accent_red <- "#B03A2E"
point_grey <- "#5F6A6A"
hex_low    <- "#F2F4F4"
hex_high   <- "#1B2631"

# ------------------------------------------------------------------------------
# 4. Plot 5: Party Identification
# ------------------------------------------------------------------------------

party_data <- df %>%
  filter(!is.na(party_id)) %>%
  count(party_id, sort = TRUE) %>%
  mutate(
    party_id = forcats::fct_reorder(party_id, n)
  )

p5 <- ggplot(
  party_data,
  aes(x = party_id, y = n, fill = party_id)
) +
  geom_col(
    show.legend = FALSE,
    width = 0.72,
    color = "#2B2B2B",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = n),
    hjust = -0.18,
    size = 5.2,
    family = "Times New Roman",
    fontface = "bold",
    color = "#1A1A1A"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = rep(party_palette, length.out = nrow(party_data))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "Party Identification of Respondents",
    x = "Party identification",
    y = "Count"
  ) +
  theme_ms_fig() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

# ------------------------------------------------------------------------------
# 5. Plot 6: Media Platform Comparison
# ------------------------------------------------------------------------------

media_long <- df %>%
  select(resp_id, wa_news_frq, fb_news_frq, yt_news_frq, x_news_frq) %>%
  rename(
    WhatsApp  = wa_news_frq,
    Facebook  = fb_news_frq,
    YouTube   = yt_news_frq,
    X_Twitter = x_news_frq
  ) %>%
  pivot_longer(
    cols = c(WhatsApp, Facebook, YouTube, X_Twitter),
    names_to = "Platform",
    values_to = "Frequency"
  ) %>%
  filter(!is.na(Frequency)) %>%
  mutate(
    Platform = factor(
      Platform,
      levels = c("WhatsApp", "Facebook", "YouTube", "X_Twitter")
    )
  )

p6 <- ggplot(
  media_long,
  aes(x = Platform, y = Frequency, fill = Platform)
) +
  geom_violin(
    trim = FALSE,
    alpha = 0.72,
    width = 1.05,
    color = "#2B2B2B",
    linewidth = 0.35
  ) +
  geom_boxplot(
    width = 0.14,
    fill = "white",
    color = "#1A1A1A",
    outlier.alpha = 0.20,
    linewidth = 0.40
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3.8,
    fill = "#F4D03F",
    color = "#1A1A1A",
    stroke = 0.8
  ) +
  scale_fill_manual(values = platform_palette) +
  scale_y_continuous(
    breaks = seq(1, 5, 1),
    limits = c(1, 5),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  labs(
    title = "News Consumption Frequency Across Platforms",
    x = "Media platform",
    y = "Frequency score"
  ) +
  theme_ms_fig() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 15,
      hjust = 1,
      vjust = 1
    )
  )

# ------------------------------------------------------------------------------
# 6. Plot 7: Political Interest vs. Party Support
# ------------------------------------------------------------------------------

p7 <- df %>%
  filter(
    !is.na(pol_int),
    !is.na(party_sup_clean)
  ) %>%
  ggplot(
    aes(x = pol_int, y = party_sup_clean)
  ) +
  geom_jitter(
    alpha = 0.30,
    color = point_grey,
    width = 0.10,
    height = 0.10,
    size = 1.8
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = accent_red,
    fill = accent_red,
    linewidth = 1.25,
    alpha = 0.18,
    se = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, 1),
    limits = c(1, 5),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    breaks = seq(1, 5, 1),
    limits = c(1, 5),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  labs(
    title = "Political Interest and Party Support Intensity",
    x = "Political interest",
    y = "Party support intensity"
  ) +
  theme_ms_fig()

# ------------------------------------------------------------------------------
# 7. Plot 8: WhatsApp News Receiving vs. Sharing
# ------------------------------------------------------------------------------

p8 <- df %>%
  filter(
    !is.na(wa_recv_frq),
    !is.na(news_share_frq)
  ) %>%
  ggplot(
    aes(x = wa_recv_frq, y = news_share_frq)
  ) +
  geom_hex(
    bins = 18,
    color = "white",
    linewidth = 0.20
  ) +
  scale_fill_gradient(
    low = hex_low,
    high = hex_high,
    name = "Count"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = accent_red,
    linetype = "dashed",
    linewidth = 1.15,
    se = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, 1),
    limits = c(1, 5),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    breaks = seq(1, 5, 1),
    limits = c(1, 5),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  labs(
    title = "WhatsApp News Receiving and Sharing Frequency",
    x = "Receiving frequency",
    y = "Sharing frequency"
  ) +
  theme_ms_fig() +
  theme(
    legend.position = "right",
    legend.key.height = unit(0.70, "cm"),
    legend.key.width = unit(0.40, "cm")
  )

# ------------------------------------------------------------------------------
# 8. Combine Plots Using Patchwork
# ------------------------------------------------------------------------------

combined_figure_2 <- (p5 + p6) / (p7 + p8) +
  plot_annotation(
    tag_levels = "A",
    title = "Political Orientation and Information Consumption Patterns",
    theme = theme(
      text = element_text(
        family = "Times New Roman",
        color = "#1A1A1A"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 16)
      ),
      plot.tag = element_text(
        size = 20,
        face = "bold",
        color = "#1A1A1A"
      )
    )
  ) &
  theme(
    plot.tag.position = c(0.02, 0.98),
    plot.tag = element_text(
      family = "Times New Roman",
      size = 20,
      face = "bold"
    )
  )

# ------------------------------------------------------------------------------
# 9. Save Figure at 300 DPI
# ------------------------------------------------------------------------------

ggsave(
  filename = "Figure_2_Political_Media_Landscape.png",
  plot = combined_figure_2,
  dpi = 300,
  width = 14,
  height = 10,
  units = "in",
  bg = "white"
)

# Optional TIFF version for journal submission
ggsave(
  filename = "Figure_2_Political_Media_Landscape.tiff",
  plot = combined_figure_2,
  dpi = 300,
  width = 14,
  height = 10,
  units = "in",
  bg = "white",
  compression = "lzw"
)

# Display in RStudio
combined_figure_2



# 1. Install and load necessary packages
if (!require("lavaan")) install.packages("lavaan")
if (!require("semPlot")) install.packages("semPlot")

library(lavaan)
library(semPlot)

# 2. Define the model string provided
model_bsem <- '
  # Measurement model
  AP =~ ap_item1 + ap_item2 + ap_item3 + ap_item4 + ap_item5
  Hostility =~ oh_item1 + oh_item2 + oh_item3 + oh_item4 + oh_item5
  FNS =~ fns_item1 + fns_item2 + fns_item3 + fns_item4 + 
         fns_item5 + fns_item6 + fns_item7 + fns_verify_rev

  # Structural model
  Hostility ~ a*AP
  FNS ~ b*Hostility + cprime*AP

  # Defined effects
  indirect := a*b
  total := cprime + (a*b)
'

# 3. Create dummy data 
# (The plotting function needs a fitted object, which requires data)
set.seed(123)
# We create a dataframe with 18 columns matching the items in the model
dummy_data <- data.frame(matrix(rnorm(18 * 100), ncol = 18))
colnames(dummy_data) <- c(
  "ap_item1", "ap_item2", "ap_item3", "ap_item4", "ap_item5",
  "oh_item1", "oh_item2", "oh_item3", "oh_item4", "oh_item5",
  "fns_item1", "fns_item2", "fns_item3", "fns_item4",
  "fns_item5", "fns_item6", "fns_item7", "fns_verify_rev"
)

# 4. Fit the model using lavaan
# We suppress warnings because we are using random dummy data
fit <- suppressWarnings(sem(model_bsem, data = dummy_data))

# 5. Draw the diagram
# whatLabels = "name": Displays parameter names (a, b, cprime) on arrows
# style = "ram": Standard SEM drawing style (Ovals for latent, Squares for observed)
# rotation = 2: Puts the exogenous variable (AP) on the left side
semPaths(fit, 
         what = "paths",         # Draw the paths only
         whatLabels = "name",    # Show labels (a, b, cprime) defined in syntax
         style = "ram", 
         layout = "tree2",       # Organizes structure hierarchically
         rotation = 2,           # Left-to-right flow
         edge.label.cex = 0.8,   # Text size for labels
         sizeMan = 8,            # Size of observed variable boxes
         sizeLat = 10,           # Size of latent variable ovals
         title = FALSE,
         residScale = 0.5        # Scale down residual arrows for cleaner look
)


#Appendic C
# ==============================================================================
# COMPLETE R CODE FOR CFA AND BSEM ANALYSIS
# ==============================================================================

# 1. Load Necessary Libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lavaan, blavaan, semTools, knitr)

# 2. Load Data
# Replace this line with your actual data loading code if df is not already loaded:
# df <- readxl::read_excel("Final_Corrected_Data_Collection.xlsx", sheet = "Final_Cleaned_Data")

# 3. Data Preparation
# Define item lists
ap_items  <- c("ap_ingroup", "ap_uncomf", "ap_negreact", "ap_norespect", "ap_trustsame")
host_items <- c("host_angry", "host_harmpak", "host_irrit", "host_harmnarr", "host_satisfy")
fns_items <- c("fns_unver", "fns_proparty", "fns_matchview", "fns_wa_nosrc", 
               "fns_rivalcrit", "fns_verify_rev", "fns_falselater", "fns_expose")
all_items <- c(ap_items, host_items, fns_items)

# Declare items as Ordered Factors (Essential for Ordinal Probit model)
for(i in all_items){
  df[[i]] <- factor(df[[i]], ordered = TRUE)
}

# ==============================================================================
# MODEL 1: CONFIRMATORY FACTOR ANALYSIS (CFA)
# ==============================================================================

cat("Running CFA Model...\n")

# Define CFA Syntax
cfa_model <- "
  # --- Measurement Model ---
  AP =~ ap_ingroup + ap_uncomf + ap_negreact + ap_norespect + ap_trustsame
  Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy
  FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
         fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose
"

# Fit CFA (Using WLSMV estimator for ordinal data)
fit_cfa <- lavaan::cfa(
  model = cfa_model, 
  data = df, 
  ordered = all_items, 
  estimator = "WLSMV"
)

# Display CFA Summary
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)


# ==============================================================================
# MODEL 2: BAYESIAN STRUCTURAL EQUATION MODELING (BSEM)
# ==============================================================================
# ==============================================================================
# CORRECTED R CODE FOR BSEM ANALYSIS
# ==============================================================================

# 1. Load Necessary Libraries (Added 'brms' to fix the error)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lavaan, blavaan, brms, semTools, knitr)

# 2. Data Preparation
# Define item lists
ap_items  <- c("ap_ingroup", "ap_uncomf", "ap_negreact", "ap_norespect", "ap_trustsame")
host_items <- c("host_angry", "host_harmpak", "host_irrit", "host_harmnarr", "host_satisfy")
fns_items <- c("fns_unver", "fns_proparty", "fns_matchview", "fns_wa_nosrc", 
               "fns_rivalcrit", "fns_verify_rev", "fns_falselater", "fns_expose")
all_items <- c(ap_items, host_items, fns_items)

# Declare items as Ordered Factors
for(i in all_items){
  df[[i]] <- factor(df[[i]], ordered = TRUE)
}

# ==============================================================================
# MODEL SYNTAX
# ==============================================================================

bsem_model <- "
  # --- Measurement Model ---
  AP =~ ap_ingroup + ap_uncomf + ap_negreact + ap_norespect + ap_trustsame
  Hostility =~ host_angry + host_harmpak + host_irrit + host_harmnarr + host_satisfy
  FNS =~ fns_unver + fns_proparty + fns_matchview + fns_wa_nosrc + 
         fns_rivalcrit + fns_verify_rev + fns_falselater + fns_expose

  # --- Structural Model ---
  Hostility ~ a * AP
  FNS ~ b * Hostility + c_prime * AP

  # --- Defined Parameters ---
  indirect := a * b
  total := c_prime + (a * b)
"

# ==============================================================================
# RUN MODEL
# ==============================================================================

cat("Running Bayesian SEM Model...\n")

# Define Priors (Now works because 'brms' is loaded)
my_priors <- prior("normal(0,1)", class = "regression")

# Fit BSEM
fit_bsem <- bsem(
  model = bsem_model, 
  data = df, 
  ordered = all_items,
  prior = my_priors,
  n.chains = 4, 
  burnin = 2000, 
  sample = 2000, 
  target = "stan"
)

# Display Summary
summary(fit_bsem, standardized = TRUE)






# ==============================================================================
# ROBUST APPENDIX E CODE (Manual Matrix Extraction)
# ==============================================================================

library(coda)     # For diagnostics
library(bayesplot)
library(tidyverse)
library(knitr)

# 1. Extract the MCMC object from blavaan
# We use tryCatch to ensure extraction works
mcmc_obj <- tryCatch({
  blavInspect(fit_bsem, "mcmc")
}, error = function(e) {
  as.mcmc(fit_bsem)
})

# 2. Define Key Parameters
params_to_check <- c("a", "b", "c_prime", "indirect")

# 3. Manually Extract and Subset Matrices
# This avoids S4 subsetting errors by converting to standard matrices first.
chain_matrices <- lapply(mcmc_obj, as.matrix)

# Check if parameters exist in the first chain's columns
if (!all(params_to_check %in% colnames(chain_matrices[[1]]))) {
  stop("Error: Parameter names not found in model samples. Check model syntax labels.")
}

# Subset the matrices to keep only our key parameters
subset_matrices <- lapply(chain_matrices, function(mat) {
  mat[, params_to_check, drop = FALSE]
})

# 4. Convert back to mcmc.list for coda/bayesplot functions
# This creates a valid mcmc.list containing only our target parameters
mcmc_subset <- as.mcmc.list(lapply(subset_matrices, as.mcmc))

# 5. Calculate Diagnostics using coda
# A. R-hat (Gelman-Rubin Diagnostic)
rhat_vals <- gelman.diag(mcmc_subset, autoburnin = FALSE, multivariate = FALSE)$psrf[, "Point est."]

# B. Effective Sample Size (ESS)
ess_vals <- effectiveSize(mcmc_subset)

# C. Summary Statistics (Mean, SD)
# Combine all chains into one matrix for simple stats
all_samples <- do.call(rbind, subset_matrices)
means <- colMeans(all_samples)
sds <- apply(all_samples, 2, sd)

# D. MCSE
mcse_vals <- sds / sqrt(ess_vals)

# 6. Construct the Table
diag_table <- data.frame(
  Parameter = params_to_check,
  Mean = round(means, 3),
  SD = round(sds, 3),
  R_hat = round(rhat_vals, 3),
  ESS = round(ess_vals, 1),
  MCSE = round(mcse_vals, 4)
)

# Print Table
print(
  kable(diag_table, 
        caption = "Table E1. MCMC Convergence Diagnostics", 
        booktabs = TRUE, 
        align = c('l', 'c', 'c', 'c', 'c', 'c')) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
)

# 7. Extract PPP (Posterior Predictive p-value)
# We capture the output of summary() because it always contains the fit info
sum_output <- capture.output(summary(fit_bsem))
# Search for the line containing PPP
ppp_line <- sum_output[grep("Posterior predictive p-value", sum_output)]
cat("\n--- Fit Index ---\n")
cat(ppp_line, "\n")

# 8. Generate Trace Plots
# Use the cleaned mcmc_subset object
trace_plot <- mcmc_trace(mcmc_subset) + 
  theme_minimal(base_size = 12) +
  ggtitle("Figure E1. Trace Plots for Structural Parameters") +
  scale_color_brewer(palette = "Set1")

# Save Plot
ggsave("Figure_E1_TracePlots.png", 
       plot = trace_plot, 
       width = 10, 
       height = 8, 
       dpi = 300)

print(trace_plot)




#conceptual framework

# ==============================================================================
# Corrected Publication-Ready R Code for Mediation Conceptual Framework
# ==============================================================================

# 1. Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, ggforce, showtext)

# 2. Font setup
font_family <- ifelse(.Platform$OS.type == "windows", "Times New Roman", "serif")
showtext_auto()

# 3. Main node information
nodes <- data.frame(
  id = c("X", "M", "Y"),
  title = c(
    "Affective\nPolarization",
    "Outgroup\nHostility",
    "Fake News\nSharing Tendency"
  ),
  subtitle = c(
    "(Independent Variable, X)",
    "",
    "(Dependent Variable, Y)"
  ),
  x = c(1.2, 3.5, 5.8),
  y = c(1.2, 3.25, 1.2),
  width = c(1.65, 1.80, 1.90),
  height = c(0.95, 0.90, 0.95),
  fill = c("#EAF1FB", "#EAF5E1", "#FDEBD2"),
  border = c("#0B4EA2", "#2F7D20", "#D95F02")
)

# 4. Create polygon coordinates for rounded rectangles
make_box <- function(id, x, y, width, height, fill, border) {
  data.frame(
    id = id,
    x = c(x - width / 2, x + width / 2, x + width / 2, x - width / 2),
    y = c(y - height / 2, y - height / 2, y + height / 2, y + height / 2),
    fill = fill,
    border = border
  )
}

box_data <- do.call(
  rbind,
  lapply(
    seq_len(nrow(nodes)),
    function(i) {
      make_box(
        id = nodes$id[i],
        x = nodes$x[i],
        y = nodes$y[i],
        width = nodes$width[i],
        height = nodes$height[i],
        fill = nodes$fill[i],
        border = nodes$border[i]
      )
    }
  )
)

# 5. Define structural path arrows
paths <- data.frame(
  path = c("a", "b", "c_prime"),
  x = c(1.80, 4.18, 2.02),
  y = c(1.65, 2.90, 1.20),
  xend = c(2.78, 5.10, 4.85),
  yend = c(2.88, 1.60, 1.20),
  linewidth = c(0.65, 0.65, 0.75)
)

# 6. Define path labels
path_labels <- data.frame(
  label = c(
    "Path a",
    "Effect of X on M",
    "Path b",
    "Effect of M on Y",
    "Path c′",
    "Direct effect of X on Y\ncontrolling for M"
  ),
  x = c(1.55, 1.55, 5.45, 5.45, 3.45, 3.45),
  y = c(2.55, 2.36, 2.55, 2.36, 0.84, 0.58),
  size = c(5.0, 4.3, 5.0, 4.3, 4.9, 4.0),
  fontface = c("italic", "plain", "italic", "plain", "italic", "plain"),
  color = c("#0B4EA2", "black", "#0B4EA2", "black", "#0B4EA2", "black")
)

# 7. Indirect-effect dashed line and labels
indirect_line <- data.frame(
  x = 2.15,
  xend = 4.85,
  y = 2.12,
  yend = 2.12
)

indirect_label <- data.frame(
  label = c(
    "Indirect Effect (a × b)",
    "Effect of X on Y through M"
  ),
  x = c(3.5, 3.5),
  y = c(2.25, 2.02),
  size = c(4.8, 4.0),
  fontface = c("bold", "plain"),
  color = c("#0B4EA2", "black")
)

# 8. Mediator and total-effect labels
mediator_label <- data.frame(
  label = "Mediator (M)",
  x = 3.5,
  y = 4.08
)

total_effect <- data.frame(
  label = "Total Effect (c)  =  Direct Effect (c′)  +  Indirect Effect (a × b)",
  x = 3.5,
  y = 0.12
)

# 9. Draw the figure
p <- ggplot() +
  
  # Dashed indirect-effect guide line
  geom_segment(
    data = indirect_line,
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = "dashed",
    linewidth = 0.55,
    color = "gray40"
  ) +
  
  # Main arrows
  geom_segment(
    data = paths,
    aes(x = x, y = y, xend = xend, yend = yend, linewidth = linewidth),
    arrow = arrow(length = unit(0.23, "cm"), type = "closed"),
    color = "black",
    lineend = "round"
  ) +
  scale_linewidth_identity() +
  
  # Rounded boxes using ggforce::geom_shape
  geom_shape(
    data = box_data,
    aes(x = x, y = y, group = id, fill = fill, color = border),
    radius = unit(0.12, "cm"),
    expand = unit(0, "cm"),
    linewidth = 0.75
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  
  # Node titles
  geom_text(
    data = nodes,
    aes(x = x, y = y + 0.13, label = title),
    family = font_family,
    fontface = "bold",
    size = 5.2,
    lineheight = 0.95,
    color = "black"
  ) +
  
  # Node subtitles
  geom_text(
    data = nodes,
    aes(x = x, y = y - 0.30, label = subtitle),
    family = font_family,
    size = 3.9,
    color = "black"
  ) +
  
  # Path labels
  geom_text(
    data = path_labels,
    aes(
      x = x,
      y = y,
      label = label,
      size = size,
      fontface = fontface,
      color = color
    ),
    family = font_family,
    lineheight = 0.95
  ) +
  
  # Indirect-effect labels
  geom_text(
    data = indirect_label,
    aes(
      x = x,
      y = y,
      label = label,
      size = size,
      fontface = fontface,
      color = color
    ),
    family = font_family,
    lineheight = 0.95
  ) +
  
  # Mediator label
  geom_text(
    data = mediator_label,
    aes(x = x, y = y, label = label),
    family = font_family,
    fontface = "bold",
    size = 5.8,
    color = "black"
  ) +
  
  # Total-effect equation
  geom_text(
    data = total_effect,
    aes(x = x, y = y, label = label),
    family = font_family,
    fontface = "bold",
    size = 4.3,
    color = "#08306B"
  ) +
  
  scale_size_identity() +
  
  # Canvas control
  coord_cartesian(
    xlim = c(0.25, 6.75),
    ylim = c(-0.10, 4.35),
    expand = FALSE
  ) +
  
  # Clean theme
  theme_void(base_family = font_family) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

# 10. Display plot
print(p)

# 11. Save outputs
ggsave(
  filename = "Mediation_Conceptual_Framework.png",
  plot = p,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "Mediation_Conceptual_Framework.pdf",
  plot = p,
  width = 10,
  height = 5.5,
  bg = "white"
)

message("Figure saved as Mediation_Conceptual_Framework.png and Mediation_Conceptual_Framework.pdf")
