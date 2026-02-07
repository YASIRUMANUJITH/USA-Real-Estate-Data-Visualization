# ===========================================
# REAL ESTATE PRICE PREDICTION DEMONSTRATION
# ===========================================

cat("\n")
cat(strrep("=", 60))
cat("\nREAL ESTATE PRICE PREDICTION DEMONSTRATION\n")
cat(strrep("=", 60))
cat("\n")


cat("\n1. Loading libraries and data...\n")
library(tidyverse)
library(ggplot2)

df <- read_csv("D:/R/data/processed/engineered_data.csv", show_col_types = FALSE)
df_sample <- df %>% sample_n(5000)

cat("✓ Loaded", nrow(df_sample), "property records\n")

# ===========================================
# 2. QUICK DATA OVERVIEW
# ===========================================

cat("\n2. Quick data overview...\n")

cat("\nFirst 3 properties:\n")
print(
  df_sample %>%
    select(price, house_size, bed, bath, area_type) %>%
    head(3)
)

summary_stats <- df_sample %>%
  summarise(
    Avg_Price = mean(price),
    Avg_Size = mean(house_size),
    Avg_Bedrooms = mean(bed),
    Avg_Bathrooms = mean(bath),
    Urban_Count = sum(area_type == "urban"),
    Suburban_Count = sum(area_type == "suburban"),
    Rural_Count = sum(area_type == "rural")
  )

print(summary_stats)

# ===========================================
# 3. PRICE DISTRIBUTION
# ===========================================

cat("\n3. Creating Price Distribution...\n")

p1 <- ggplot(df_sample, aes(x = price)) +
  geom_histogram(
    bins = 30,
    fill = "#1F77B4",
    color = "black",
    alpha = 0.85
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(scale = 0.001, suffix = "K")
  ) +
  labs(
    title = "Property Price Distribution",
    x = "Price (Thousands)",
    y = "Number of Properties"
  ) +
  theme_minimal(base_size = 13)

print(p1)

# ===========================================
# 4. PRICE VS HOUSE SIZE
# ===========================================

cat("\n4. Creating Price vs House Size...\n")

p2 <- ggplot(df_sample, aes(house_size, price, color = area_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 0.001, suffix = "K")
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Price vs House Size by Area Type",
    x = "House Size (sq ft)",
    y = "Price (Thousands)",
    color = "Area Type"
  ) +
  theme_minimal(base_size = 13)

print(p2)

# ===========================================
# 5. MODEL PREDICTION DEMO
# ===========================================

cat("\n5. Model Prediction Demonstration...\n")

model_path <- "D:/R/outputs/final_xgb_model.rds"

if (file.exists(model_path)) {
  
  final_xgb <- readRDS(model_path)
  
  sample_property <- tibble(
    house_size = c(1500, 2000, 3000),
    bed = c(2, 3, 4),
    bath = c(1, 2, 3),
    price_per_sqft = c(100, 150, 200),
    area_type = c("urban", "suburban", "rural")
  )
  
  preds <- predict(final_xgb, sample_property)
  sample_property$predicted_price <- preds$.pred
  
  cat("\nPredicted Prices:\n")
  print(
    sample_property %>%
      mutate(predicted_price = scales::dollar(predicted_price))
  )
  
  sample_property_plot <- sample_property %>%
    mutate(property_id = factor(c("Urban Home", "Suburban Home", "Rural Home")))
  
  p3 <- ggplot(
    sample_property_plot,
    aes(property_id, predicted_price, fill = area_type)
  ) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = scales::dollar(predicted_price)),
      vjust = -0.4,
      size = 4
    ) +
    scale_fill_manual(
      values = c(
        urban = "#1F77B4",
        suburban = "#FF7F0E",
        rural = "#2CA02C"
      )
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(
      title = "Live Model Prediction Demo",
      x = "Property Type",
      y = "Predicted Price",
      fill = "Area Type"
    ) +
    theme_minimal(base_size = 13)
  
  print(p3)
  
  cat("Model prediction plot displayed\n")
  
} else {
  
  cat("Model file not found. Showing fallback example.\n")
  
  fallback <- df_sample %>%
    head(3) %>%
    select(price, house_size, bed, bath, area_type)
  
  print(fallback)
}



cat("\n")
cat(strrep("=", 60))
cat("\nDEMONSTRATION COMPLETE\n")
cat(strrep("=", 60))
cat("\n")
