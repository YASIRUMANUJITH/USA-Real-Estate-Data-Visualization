# ===========================================
# INTERACTIVE AND ADVANCED VISUALIZATIONS
# ===========================================

library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)
library(htmlwidgets)

# ===========================================
# LOAD DATA
# ===========================================

df <- read_csv("D:/R/data/processed/engineered_data.csv")

# Sample for performance
set.seed(42)
df_sample <- df %>% sample_n(20000)

# Create output directory
if (!dir.exists("D:/R/outputs")) {
  dir.create("D:/R/outputs", recursive = TRUE)
}

# ===========================================
# 2D INTERACTIVE PLOT (PLOTLY)
# ===========================================

p_interactive <- plot_ly(
  data = df_sample,
  x = ~house_size,
  y = ~price,
  color = ~area_type,
  colors = c(
    "urban" = "#BF40BF",
    "suburban" = "#377EB8",
    "rural" = "#4DAF4A"
  ),
  type = "scatter",
  mode = "markers",
  marker = list(size = 6, opacity = 0.7),
  text = ~paste(
    "<b>Property Details</b><br>",
    "Price: $", format(price, big.mark = ","), "<br>",
    "Size: ", house_size, " sqft<br>",
    "Bedrooms: ", bed, "<br>",
    "Bathrooms: ", bath, "<br>",
    "Price / sqft: $", round(price_per_sqft, 2), "<br>",
    "Area Type: ", area_type, "<br>",
    "State: ", state, "<br>",
    "City: ", city
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "<b>Interactive Property Analysis: Price vs House Size</b>",
      x = 0.05
    ),
    xaxis = list(title = "House Size (sq ft)", gridcolor = "lightgray"),
    yaxis = list(
      title = "Price ($)",
      tickformat = "$,.0f",
      gridcolor = "lightgray"
    ),
    hovermode = "closest",
    plot_bgcolor = "#f5f5f5",
    paper_bgcolor = "#f5f5f5"
  )

saveWidget(
  p_interactive,
  "D:/R/outputs/interactive_property_analysis_2D.html",
  selfcontained = TRUE
)

cat("2D interactive plot saved\n")

# ===========================================
# 3D INTERACTIVE PLOT 
# ===========================================

p_3d <- plot_ly(
  data = df_sample,
  x = ~house_size,
  y = ~price,
  z = ~price_per_sqft,
  color = ~area_type,
  colors = c(
    "urban" = "#BF40BF",
    "suburban" = "#377EB8",
    "rural" = "#4DAF4A"
  ),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4, opacity = 0.7),
  text = ~paste(
    "<b>Property Details</b><br>",
    "Price: $", format(price, big.mark = ","), "<br>",
    "Size: ", house_size, " sqft<br>",
    "Price / sqft: $", round(price_per_sqft, 2), "<br>",
    "Bedrooms: ", bed, "<br>",
    "Bathrooms: ", bath, "<br>",
    "Area Type: ", area_type
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = "<b>3D Property Analysis</b>",
    scene = list(
      xaxis = list(title = "House Size (sq ft)"),
      yaxis = list(title = "Price ($)"),
      zaxis = list(title = "Price per Sqft ($)")
    )
  )

saveWidget(
  p_3d,
  "D:/R/outputs/interactive_property_analysis_3D.html",
  selfcontained = TRUE
)

cat("3D interactive plot saved\n")

# ===========================================
# ADVANCED STATIC VISUALIZATION
# ===========================================

p_faceted <- df_sample %>%
  ggplot(aes(x = price, fill = area_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(
    ~bed,
    scales = "free_y",
    labeller = labeller(bed = function(x) paste(x, "Bedrooms"))
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(scale = 0.001, suffix = "K"),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  scale_fill_brewer(palette = "Set2", name = "Area Type") +
  labs(
    title = "Price Distribution by Bedroom Count and Area Type",
    subtitle = "Variation of property prices across area types",
    x = "Price (Thousands)",
    y = "Density",
    caption = "Data source: USA Real Estate Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(color = "gray50", hjust = 1)
  )

ggsave(
  "D:/R/outputs/faceted_price_distribution.png",
  p_faceted,
  width = 14,
  height = 10,
  dpi = 300
)

cat("Faceted static plot saved\n")


