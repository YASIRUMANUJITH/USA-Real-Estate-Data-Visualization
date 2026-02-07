# ==============================
# Data Understanding
# ==============================

source("scripts/load_packages.R")

cat("Loading raw data...\n")
df_raw <- read_csv("data/raw/Real.csv", show_col_types = FALSE)

# --------------------------------
# 1. STRUCTURE & SUMMARY
# --------------------------------
cat("\nDATA STRUCTURE:\n")
glimpse(df_raw)

cat("\nSUMMARY STATISTICS:\n")
print(summary(df_raw))

# --------------------------------
# 2. MISSING VALUES CHECK
# --------------------------------
cat("\nMISSING VALUE SUMMARY:\n")
missing_summary <- sapply(df_raw, function(x) sum(is.na(x)))
print(missing_summary)

missing_pct <- round(missing_summary / nrow(df_raw) * 100, 2)
cat("\nMISSING VALUE PERCENTAGE (%):\n")
print(missing_pct)

# --------------------------------
# 3. DUPLICATE CHECK
# --------------------------------
cat("\nDUPLICATE ROW CHECK:\n")
dup_count <- sum(duplicated(df_raw))
cat("Duplicate rows:", dup_count, "\n")

# --------------------------------
# 4. TARGET VARIABLE SANITY CHECK
# --------------------------------

cat("\nPRICE VARIABLE CHECK:\n")
cat("Min Price:", min(df_raw$price, na.rm = TRUE), "\n")
cat("Max Price:", max(df_raw$price, na.rm = TRUE), "\n")
cat("Mean Price:", mean(df_raw$price, na.rm = TRUE), "\n")

# Identify extreme outliers (99th percentile)
price_p99 <- quantile(df_raw$price, 0.99, na.rm = TRUE)
price_p01 <- quantile(df_raw$price, 0.01, na.rm = TRUE)

cat("1st percentile price:", price_p01, "\n")
cat("99th percentile price:", price_p99, "\n")


# --------------------------------
# 5. POTENTIAL LEAKAGE CHECK
# --------------------------------

cat("\nPOTENTIAL LEAKAGE COLUMNS CHECK:\n")

leakage_candidates <- c(
  "price_per_sqft",
  "prev_sold_date",
  "prev_sold_price",
  "listing_date"
)

present_leakage <- leakage_candidates[leakage_candidates %in% names(df_raw)]
print(present_leakage)

# --------------------------------
# 6. DATA TYPE CONVERSION
# --------------------------------
cat("\nConverting data types...\n")

df_raw <- df_raw %>%
  mutate(
    status = factor(status),
    city = factor(city),
    state = factor(state),
    zip_code = factor(zip_code),
    prev_sold_date = as.Date(prev_sold_date)
  )

# --------------------------------
# 7. SAVE DATA
# --------------------------------
saveRDS(df_raw, "data/processed/understood_data.rds")
cat("\nUnderstood data saved to data/processed/understood_data.rds\n")

cat("\nDATA UNDERSTANDING COMPLETE\n")
