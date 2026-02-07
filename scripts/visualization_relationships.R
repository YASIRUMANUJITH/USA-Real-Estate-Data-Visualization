# ===========================================
# GEOSPATIAL VISUALIZATIONS
# ===========================================

library(tidyverse)
library(ggplot2)
library(viridis)
library(maps)

# Load your feature-engineered data
df <- read_csv("D:/R/data/processed/engineered_data.csv")

# Sample for performance
df_sample <- df %>% sample_n(20000)

# Create output directory if it doesn't exist
if (!dir.exists("D:/R/outputs")) {
  dir.create("D:/R/outputs", recursive = TRUE)
}

# ===========================================
# STATE-LEVEL HEATMAPS AND BAR CHARTS
# ===========================================

if ("state" %in% colnames(df_sample)) {
  
  # Calculate state-level statistics
  state_summary <- df_sample %>%
    group_by(state) %>%
    summarise(
      avg_price = mean(price, na.rm = TRUE),
      avg_price_per_sqft = mean(price_per_sqft, na.rm = TRUE),
      property_count = n(),
      .groups = 'drop'
    ) %>%
    mutate(state_lower = tolower(state))
  
  # Load US states data
  states_map <- map_data("state")
  
  # Plot 1: Average Price by State
  p_state_price <- ggplot() +
    geom_polygon(data = states_map, 
                 aes(x = long, y = lat, group = group),
                 fill = "gray90", color = "white") +
    geom_map(data = state_summary, map = states_map,
             aes(fill = avg_price, map_id = state_lower),
             color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "plasma",
      labels = scales::dollar_format(scale = 0.001, suffix = "K"),
      name = "Avg Price ($)"
    ) +
    labs(
      title = "Average Property Price by State",
      subtitle = "Darker colors indicate higher average prices",
      caption = "Data source: USA Real Estate Dataset"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
      legend.position = "right",
      plot.caption = element_text(color = "gray50", hjust = 1)
    )
  
  # Plot 2: Price per Sqft by State
  p_state_pps <- ggplot() +
    geom_polygon(data = states_map, 
                 aes(x = long, y = lat, group = group),
                 fill = "gray90", color = "white") +
    geom_map(data = state_summary, map = states_map,
             aes(fill = avg_price_per_sqft, map_id = state_lower),
             color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "magma",
      name = "Price/sqft ($)"
    ) +
    labs(
      title = "Average Price per Square Foot by State",
      subtitle = "Darker colors indicate higher price per square foot",
      caption = "Data source: USA Real Estate Dataset"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12),
      legend.position = "right",
      plot.caption = element_text(color = "gray50", hjust = 1)
    )
  
  # Save maps
  ggsave("D:/R/outputs/state_avg_price_map.png", p_state_price, 
         width = 12, height = 8, dpi = 300)
  ggsave("D:/R/outputs/state_price_per_sqft_map.png", p_state_pps, 
         width = 12, height = 8, dpi = 300)
  
  # TOP STATES BAR CHART (Price)
  p_top_states <- state_summary %>%
    arrange(desc(avg_price)) %>%
    head(15) %>%
    ggplot(aes(x = reorder(state, avg_price), y = avg_price, fill = avg_price)) +
    geom_col() +
    scale_fill_viridis_c(option = "plasma") +
    scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
    coord_flip() +
    labs(
      title = "Top 15 States by Average Property Price",
      x = "State",
      y = "Average Price (Thousands)",
      caption = "Data source: USA Real Estate Dataset"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_text(color = "gray50", hjust = 1)
    )
  
  ggsave("D:/R/outputs/top_states_price.png", p_top_states, 
         width = 10, height = 8, dpi = 300)
  
  # STATE PROPERTY COUNT
  p_state_count <- state_summary %>%
    arrange(desc(property_count)) %>%
    head(15) %>%
    ggplot(aes(x = reorder(state, property_count), y = property_count, 
               fill = property_count)) +
    geom_col() +
    scale_fill_viridis_c(option = "cividis") +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    labs(
      title = "Top 15 States by Property Count",
      x = "State",
      y = "Number of Properties",
      caption = "Data source: USA Real Estate Dataset"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_text(color = "gray50", hjust = 1)
    )
  
  ggsave("D:/R/outputs/top_states_count.png", p_state_count, 
         width = 10, height = 8, dpi = 300)
  
  # ===========================================
  # SUMMARY REPORT
  # ===========================================
  
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""))
  cat("\n")
  cat("GEOSPATIAL VISUALIZATIONS CREATED SUCCESSFULLY!\n")
  cat(paste(rep("=", 60), collapse = ""))
  cat("\n")
  
  cat("\nSaved to D:/R/outputs/:\n")
  cat("1. state_avg_price_map.png\n")
  cat("2. state_price_per_sqft_map.png\n")
  cat("3. top_states_price.png\n")
  cat("4. top_states_count.png\n")
  
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""))
  cat("\n")
  
} else {
  cat("No 'state' column found in dataset. Skipping geospatial visualizations.\n")
}