source("scripts/load_packages.R")

cat(paste0(rep("=", 60), collapse = ""), "\n")
cat("01: DATA CLEANING SCRIPT\n")
cat(paste0(rep("=", 60), collapse = ""), "\n\n")

cat("SETTING UP DIRECTORIES...\n")
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

cat("LOADING RAW DATA...\n")
df <- readRDS("data/processed/understood_data.rds")
cat("Original data:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Columns:", paste(names(df), collapse = ", "), "\n\n")

cat("REMOVING UNWANTED COLUMNS...\n")
columns_to_remove <- c("status", "zip_code", "prev_sold_date")
df <- df %>% select(-any_of(columns_to_remove))
cat("Removed:", paste(columns_to_remove, collapse = ", "), "\n")
cat("Remaining columns:", ncol(df), "\n\n")

cat("BASIC DATA VALIDATION...\n")
cat("Initial rows:", nrow(df), "\n")

df_clean <- df %>%
  filter(
    price > 0,
    house_size > 0,
    bed >= 0,
    bath >= 0
  )
cat("After filtering invalid values:", nrow(df_clean), "rows\n")

before_na <- nrow(df_clean)
df_clean <- df_clean %>%
  drop_na(price, bed, bath, house_size, city, state)
cat("After removing NA in critical columns:", nrow(df_clean), "rows\n")
cat("Rows removed:", before_na - nrow(df_clean), "\n\n")

cat("REMOVING OUTLIERS (PRICE ONLY, IQR METHOD)...\n")

remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  x
}

original_count <- nrow(df_clean)
df_clean$price <- remove_outliers(df_clean$price)
df_clean <- df_clean %>% drop_na(price)
cat("After price outlier removal:", nrow(df_clean), "rows\n")
cat("Price outliers removed:", original_count - nrow(df_clean), "\n\n")

cat("CLEANING ACRE_LOT COLUMN (PRESERVING LARGE VALUES)...\n")

acre_na_count <- sum(is.na(df_clean$acre_lot) | df_clean$acre_lot == "N/A")
cat(
  "Missing acre_lot values:",
  acre_na_count,
  "(",
  round(acre_na_count / nrow(df_clean) * 100, 1),
  "%)\n"
)

df_clean$acre_lot <- ifelse(df_clean$acre_lot == "N/A", NA, df_clean$acre_lot)
df_clean$acre_lot <- as.numeric(df_clean$acre_lot)

cat("Imputing missing or invalid acre_lot with state median...\n")
df_clean <- df_clean %>%
  group_by(state) %>%
  mutate(
    acre_lot = ifelse(
      is.na(acre_lot) | acre_lot <= 0,
      median(acre_lot[acre_lot > 0], na.rm = TRUE),
      acre_lot
    )
  ) %>%
  ungroup()

cat("Large acre_lot values preserved (no outlier removal applied)\n\n")

cat("FINAL CLEANING CHECK...\n")
cat("Final row count:", nrow(df_clean), "\n")
cat("Final column count:", ncol(df_clean), "\n")
cat("Final columns:", paste(names(df_clean), collapse = ", "), "\n")

cat("\nDATA QUALITY SUMMARY:\n")
print(summary(df_clean[, c("price", "house_size", "bed", "bath", "acre_lot")]))

cat("\nSAVING CLEANED DATA...\n")
saveRDS(df_clean, "data/processed/cleaned_data.rds")
write.csv(df_clean, "data/processed/cleaned_data.csv", row.names = FALSE)
cat("Saved: data/processed/cleaned_data.rds\n")
cat("Saved: data/processed/cleaned_data.csv\n")

cat("\n", paste0(rep("=", 60), collapse = ""), "\n", sep = "")
cat("CLEANING COMPLETE!\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

cat("\nSUMMARY:\n")
cat("1. Removed 3 unwanted columns\n")
cat("2. Filtered invalid values\n")
cat("3. Handled missing data\n")
cat("4. Removed price outliers only (IQR)\n")
cat("5. Preserved original acre_lot values\n")
cat("6. Preserved", nrow(df_clean), "high-quality rows\n")

cat("\nNEXT: feature_engineering.R\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")
