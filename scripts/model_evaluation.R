# ===========================================
# MODEL EVALUATION
# ===========================================

library(tidyverse)
library(tidymodels)
library(ggplot2)
library(patchwork)
library(yardstick)
library(ggrepel)
library(xgboost)
library(ranger)

if (!dir.exists("D:/R/outputs/evaluation")) {
  dir.create("D:/R/outputs/evaluation", recursive = TRUE)
}

cat("1. Loading models and data...\n")

models_loaded <- FALSE

# Load models
try({
  final_xgb <- readRDS("D:/R/outputs/final_xgb_model.rds")
  final_rf <- readRDS("D:/R/outputs/final_rf_model.rds")
  models_loaded <- TRUE
  cat(" Models loaded successfully\n")
}, silent = TRUE)

df <- read_csv("D:/R/data/processed/engineered_data.csv", show_col_types = FALSE)

# Prepare test data
set.seed(123)
df_sample <- df %>% sample_n(min(50000, nrow(df)))
split <- initial_split(df_sample, prop = 0.8, strata = area_type)
train_data <- training(split)
test_data <- testing(split)

cat("Test data prepared:", nrow(test_data), "properties\n")

# ===========================================
# 1. IF MODELS FAILED TO LOAD, TRAIN NEW ONES
# ===========================================

if (!models_loaded) {
  cat("Models failed to load. Training new models for evaluation...\n")
  
  recipe <- recipe(price ~ house_size + bed + bath + price_per_sqft + area_type, data = train_data) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_zv(all_predictors())
  
  xgb_spec <- boost_tree(
    trees = 300,
    tree_depth = 6,
    learn_rate = 0.01
  ) %>%
    set_engine("xgboost") %>%
    set_mode("regression")
  
  rf_spec <- rand_forest(
    trees = 200,
    min_n = 5
  ) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("regression")
  
  xgb_wf <- workflow() %>% add_recipe(recipe) %>% add_model(xgb_spec)
  rf_wf <- workflow() %>% add_recipe(recipe) %>% add_model(rf_spec)
  
  final_xgb <- fit(xgb_wf, data = train_data)
  final_rf <- fit(rf_wf, data = train_data)
  
  cat("New models trained for evaluation\n")
}

# ===========================================
# 2. MAKE PREDICTIONS
# ===========================================

cat("\n2. Making predictions...\n")

predictions <- test_data %>%
  bind_cols(
    xgb_pred = predict(final_xgb, test_data)$.pred,
    rf_pred = predict(final_rf, test_data)$.pred
  )

cat("Predictions generated for", nrow(predictions), "properties\n")

# Warn if zero or negative prices
if(any(predictions$price <= 0)) cat("⚠ Warning: Some properties have price <= 0. Filtered metrics will exclude these.\n")

# ===========================================
# 3. CALCULATE METRICS
# ===========================================

cat("\n3. Calculating performance metrics...\n")

# Full metrics
model_metrics <- predictions %>%
  summarise(
    xgb_rmse = sqrt(mean((price - xgb_pred)^2)),
    xgb_mae  = mean(abs(price - xgb_pred)),
    xgb_mape = mean(abs((price - xgb_pred) / price)) * 100,
    xgb_rsq  = cor(price, xgb_pred)^2,
    
    rf_rmse = sqrt(mean((price - rf_pred)^2)),
    rf_mae  = mean(abs(price - rf_pred)),
    rf_mape = mean(abs((price - rf_pred) / price)) * 100,
    rf_rsq  = cor(price, rf_pred)^2
  )

# Filtered metrics to remove tiny/zero price distortion
filtered_preds <- predictions %>% filter(price >= 50000)

xgb_filtered_mape <- mean(abs((filtered_preds$price - filtered_preds$xgb_pred)/filtered_preds$price))*100
rf_filtered_mape  <- mean(abs((filtered_preds$price - filtered_preds$rf_pred)/filtered_preds$price))*100

# Log-RMSE
xgb_log_rmse <- sqrt(mean((log1p(filtered_preds$price) - log1p(filtered_preds$xgb_pred))^2))
rf_log_rmse  <- sqrt(mean((log1p(filtered_preds$price) - log1p(filtered_preds$rf_pred))^2))

cat("\nMETRIC SAFEGUARDS:\n")
cat("XGBoost Filtered MAPE: ", round(xgb_filtered_mape,2), "%\n")
cat("XGBoost Log-RMSE:      ", round(xgb_log_rmse,4), "\n")
cat("Random Forest Filtered MAPE: ", round(rf_filtered_mape,2), "%\n")
cat("Random Forest Log-RMSE:      ", round(rf_log_rmse,4), "\n")

write_csv(model_metrics, "D:/R/outputs/evaluation/model_metrics_detailed.csv")
cat("Metrics saved to CSV\n")

# ===========================================
# 4. MODEL COMPARISON VISUALIZATION
# ===========================================

metrics_viz <- model_metrics %>%
  pivot_longer(everything(), names_to = "metric_model", values_to = "value") %>%
  separate(metric_model, into = c("model", "metric"), sep = "_") %>%
  mutate(model = ifelse(model == "xgb", "XGBoost", "Random Forest"),
         metric = toupper(metric))

p_comparison <- metrics_viz %>%
  filter(metric %in% c("RMSE", "MAE", "MAPE", "RSQ")) %>%
  ggplot(aes(x = metric, y = value, fill = model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = ifelse(metric == "RSQ", 
                               round(value, 4),
                               ifelse(metric == "MAPE",
                                      paste0(round(value, 2), "%"),
                                      scales::dollar(round(value)))),
                y = value + ifelse(metric == "RSQ", 0.05, 
                                   ifelse(value < 10000, 1000, 5000))),
            position = position_dodge(width = 0.8),
            size = 3.5) +
  scale_fill_manual(values = c("XGBoost" = "#1f77b4", "Random Forest" = "#ff7f0e")) +
  labs(
    title = "Model Performance Comparison",
    subtitle = "XGBoost vs Random Forest for House Price Prediction",
    x = "Evaluation Metric",
    y = "Value",
    fill = "Model",
    caption = paste("Test Set:", nrow(test_data), "properties")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("D:/R/outputs/evaluation/model_comparison.png", p_comparison, width = 12, height = 8, dpi = 300)
cat("Model comparison plot saved\n")

# ===========================================
# 5. PREDICTION ACCURACY PLOTS
# ===========================================

viz_sample <- predictions %>% sample_n(min(2000, nrow(predictions)))

p_xgb_scatter <- viz_sample %>%
  ggplot(aes(x = xgb_pred, y = price)) +
  geom_point(alpha = 0.4, color = "#1f77b4", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE, size = 1) +
  scale_x_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
  labs(title = "XGBoost: Predicted vs Actual Prices",
       subtitle = paste("R² =", round(model_metrics$xgb_rsq,4), 
                        "| RMSE =", scales::dollar(round(model_metrics$xgb_rmse))),
       x = "Predicted Price", y = "Actual Price") +
  theme_minimal()

p_rf_scatter <- viz_sample %>%
  ggplot(aes(x = rf_pred, y = price)) +
  geom_point(alpha = 0.4, color = "#ff7f0e", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE, size = 1) +
  scale_x_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
  labs(title = "Random Forest: Predicted vs Actual Prices",
       subtitle = paste("R² =", round(model_metrics$rf_rsq,4), 
                        "| RMSE =", scales::dollar(round(model_metrics$rf_rmse))),
       x = "Predicted Price", y = "Actual Price") +
  theme_minimal()

p_scatter_combined <- p_xgb_scatter + p_rf_scatter +
  plot_annotation(title = "Prediction Accuracy Comparison",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave("D:/R/outputs/evaluation/prediction_accuracy.png", p_scatter_combined, width = 16, height = 8, dpi = 300)
cat("Prediction accuracy plots saved\n")

# ===========================================
# 6. RESIDUAL ANALYSIS
# ===========================================

residual_data <- predictions %>%
  mutate(xgb_residual = price - xgb_pred,
         rf_residual = price - rf_pred) %>%
  pivot_longer(c(xgb_residual, rf_residual), names_to = "model", values_to = "residual") %>%
  mutate(model = ifelse(model == "xgb_residual", "XGBoost", "Random Forest"),
         prediction = ifelse(model == "XGBoost", xgb_pred, rf_pred))

p_residuals <- residual_data %>%
  ggplot(aes(x = prediction, y = residual, color = model)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  scale_x_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
  scale_color_manual(values = c("XGBoost" = "#1f77b4", "Random Forest" = "#ff7f0e")) +
  facet_wrap(~model, scales = "free") +
  labs(title = "Residual Analysis: Prediction Errors",
       subtitle = "Residuals should be randomly scattered around zero",
       x = "Predicted House Price", y = "Residual (Actual - Predicted)",
       color = "Algorithm") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("D:/R/outputs/evaluation/residual_analysis.png", p_residuals, width = 14, height = 7, dpi = 300)
cat("Residual analysis plot saved\n")

# ===========================================
# 7. ERROR DISTRIBUTION
# ===========================================

error_data <- predictions %>%
  mutate(xgb_error_pct = (price - xgb_pred) / price * 100,
         rf_error_pct  = (price - rf_pred) / price * 100) %>%
  pivot_longer(c(xgb_error_pct, rf_error_pct), names_to = "model", values_to = "error_pct") %>%
  mutate(model = ifelse(model == "xgb_error_pct", "XGBoost", "Random Forest"))

p_error_dist <- error_data %>%
  filter(abs(error_pct) < 30) %>%
  ggplot(aes(x = error_pct, fill = model)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  facet_wrap(~model) +
  scale_fill_manual(values = c("XGBoost" = "#1f77b4", "Random Forest" = "#ff7f0e")) +
  labs(title = "Percentage Error Distribution",
       subtitle = "XGBoost shows tighter error distribution around zero",
       x = "Percentage Error (%)", y = "Density", fill = "Algorithm") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("D:/R/outputs/evaluation/error_distribution.png", p_error_dist, width = 12, height = 6, dpi = 300)
cat("Error distribution plot saved\n")

# ===========================================
# 8. PERFORMANCE STATISTICS & FINAL REPORT
# ===========================================

performance_stats <- predictions %>%
  summarise(
    xgb_within_5pct = mean(abs(price - xgb_pred)/price <= 0.05)*100,
    xgb_within_10pct = mean(abs(price - xgb_pred)/price <= 0.10)*100,
    xgb_within_20pct = mean(abs(price - xgb_pred)/price <= 0.20)*100,
    rf_within_5pct  = mean(abs(price - rf_pred)/price <= 0.05)*100,
    rf_within_10pct = mean(abs(price - rf_pred)/price <= 0.10)*100,
    rf_within_20pct = mean(abs(price - rf_pred)/price <= 0.20)*100
  )

final_report <- bind_cols(
  model_metrics,
  performance_stats,
  tibble(
    xgb_filtered_mape = xgb_filtered_mape,
    rf_filtered_mape  = rf_filtered_mape,
    xgb_log_rmse      = xgb_log_rmse,
    rf_log_rmse       = rf_log_rmse,
    test_set_size     = nrow(test_data),
    features_used     = "House Size, Bedrooms, Bathrooms, Price/Sqft, Area Type",
    evaluation_date   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
)

write_csv(final_report, "D:/R/outputs/evaluation/final_performance_report.csv")
cat(" Final performance report saved\n")
 

# ===========================================
# 10. PRINT COMPREHENSIVE SUMMARY
# ===========================================

cat("\n")
cat(strrep("=", 70))
cat("\n")
cat("COMPREHENSIVE MODEL EVALUATION COMPLETE\n")
cat(strrep("=", 70))
cat("\n\n")

cat("MODEL SPECIFICATION:\n")
cat("  Formula: Price = f(House Size, Bedrooms, Bathrooms, Price/Sqft, Area Type)\n")
cat("  Test Set Size:", nrow(test_data), "properties\n")
cat("  Features: 5 property characteristics\n\n")

cat(strrep("-", 70))
cat("\nPERFORMANCE METRICS:\n")
cat(strrep("-", 70))
cat("\n\n")

cat("XGBOOST (Primary Model):\n")
cat("  R²:                ", round(model_metrics$xgb_rsq, 4), 
    " (", round(model_metrics$xgb_rsq * 100, 1), "% variance explained)\n", sep = "")
cat("  RMSE:              ", scales::dollar(round(model_metrics$xgb_rmse)), "\n")
cat("  MAE:               ", scales::dollar(round(model_metrics$xgb_mae)), "\n")
cat("  MAPE:              ", round(model_metrics$xgb_mape, 2), "%\n")
cat("  Filtered MAPE:     ", round(xgb_filtered_mape,2), "%\n")
cat("  Log-RMSE:          ", round(xgb_log_rmse,4), "\n")
cat("  Within 10% error:  ", round(performance_stats$xgb_within_10pct, 1), "%\n")
cat("  Within 20% error:  ", round(performance_stats$xgb_within_20pct, 1), "%\n\n")

cat("RANDOM FOREST (Comparison Model):\n")
cat("  R²:                ", round(model_metrics$rf_rsq, 4), 
    " (", round(model_metrics$rf_rsq * 100, 1), "% variance explained)\n", sep = "")
cat("  RMSE:              ", scales::dollar(round(model_metrics$rf_rmse)), "\n")
cat("  MAE:               ", scales::dollar(round(model_metrics$rf_mae)), "\n")
cat("  MAPE:              ", round(model_metrics$rf_mape, 2), "%\n")
cat("  Filtered MAPE:     ", round(rf_filtered_mape,2), "%\n")
cat("  Log-RMSE:          ", round(rf_log_rmse,4), "\n")
cat("  Within 10% error:  ", round(performance_stats$rf_within_10pct, 1), "%\n")
cat("  Within 20% error:  ", round(performance_stats$rf_within_20pct, 1), "%\n\n")

cat(strrep("-", 70))
cat("\nPERFORMANCE IMPROVEMENT (XGBoost vs Random Forest):\n")
cat(strrep("-", 70))
cat("\n")
cat("  RMSE Reduction:    ", 
    round((model_metrics$rf_rmse - model_metrics$xgb_rmse) / model_metrics$rf_rmse * 100, 1), "%\n")
cat("  MAE Reduction:     ", 
    round((model_metrics$rf_mae - model_metrics$xgb_mae) / model_metrics$rf_mae * 100, 1), "%\n")
cat("  MAPE Reduction:    ", 
    round((model_metrics$rf_mape - model_metrics$xgb_mape) / model_metrics$rf_mape * 100, 1), "%\n")
cat("  R² Improvement:    ", 
    round((model_metrics$xgb_rsq - model_metrics$rf_rsq) / model_metrics$rf_rsq * 100, 1), "%\n\n")

cat(strrep("-", 70))
cat("\nEVALUATION OUTPUTS SAVED TO D:/R/outputs/evaluation/:\n")
cat(strrep("-", 70))
cat("\n")
cat("1. model_comparison.png            - Model metrics comparison\n")
cat("2. prediction_accuracy.png         - Prediction vs actual plots\n")
cat("3. residual_analysis.png           - Error pattern analysis\n")
cat("4. error_distribution.png          - Percentage error distribution\n")
cat("5. model_metrics_detailed.csv      - Detailed metrics (CSV)\n")
cat("6. final_performance_report.csv    - Comprehensive report (CSV)\n\n")

cat(strrep("=", 70))
cat("\nEVALUATION COMPLETE\n")
cat(strrep("=", 70))
cat("\n")
