source("scripts/load_packages.R")

cat("02: FEATURE ENGINEERING \n")
cat("======================================\n\n")

cat("LOADING CLEANED DATA...\n")
if (!file.exists("data/processed/cleaned_data.rds")) {
  stop("ERROR: Run 01_data_cleaning.R first!")
}

df_clean <- readRDS("data/processed/cleaned_data.rds")
cat("Loaded:", nrow(df_clean), "rows,", ncol(df_clean), "columns\n\n")

cat("CREATING FEATURES...\n")
df_final <- df_clean %>%
  mutate(
    price_per_sqft = round(price / house_size, 2),
    price_per_acre = round(price / acre_lot, 2),
    
    area_type = case_when(
      is.na(acre_lot)  ~ NA_character_,
      acre_lot <= 0.25 ~ "urban",
      acre_lot <= 1    ~ "suburban",
      acre_lot > 1     ~ "rural"
    )
  )

cat("Created: price_per_sqft, price_per_acre, area_type\n\n")

cat("CLEANING UP DATASET...\n")
if ("beds_per_bath" %in% names(df_final)) {
  df_final <- df_final %>% select(-beds_per_bath)
  cat("Removed: beds_per_bath column\n")
}

columns_to_consider_removing <- c("acre_lot_missing", "acre_lot_original")
existing_cols <- columns_to_consider_removing[
  columns_to_consider_removing %in% names(df_final)
]

if (length(existing_cols) > 0) {
  df_final <- df_final %>% select(-any_of(existing_cols))
  cat("Removed:", paste(existing_cols, collapse = ", "), "\n")
}

cat("Final columns:", paste(names(df_final), collapse = ", "), "\n")
cat("Total columns:", ncol(df_final), "\n\n")

cat("VALIDATING NEW FEATURES...\n")
price_per_sqft_na <- sum(is.na(df_final$price_per_sqft))
price_per_acre_na <- sum(is.na(df_final$price_per_acre))
area_type_na <- sum(is.na(df_final$area_type))

cat(
  "price_per_sqft - NA values:", price_per_sqft_na,
  "(", round(price_per_sqft_na / nrow(df_final) * 100, 2), "%)\n"
)
cat(
  "price_per_acre - NA values:", price_per_acre_na,
  "(", round(price_per_acre_na / nrow(df_final) * 100, 2), "%)\n"
)
cat(
  "area_type - NA values:", area_type_na,
  "(", round(area_type_na / nrow(df_final) * 100, 2), "%)\n\n"
)

cat("price_per_sqft: $",
    round(mean(df_final$price_per_sqft, na.rm = TRUE), 2),
    " per sqft\n", sep = "")
cat("price_per_acre: $",
    round(mean(df_final$price_per_acre, na.rm = TRUE), 2),
    " per acre\n\n", sep = "")

cat("area_type distribution:\n")
print(table(df_final$area_type, useNA = "ifany"))
cat("\n")

cat("SAVING FEATURE ENGINEERED DATA...\n")
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

engineered_rds_path <- "data/processed/engineered_data.rds"
engineered_csv_path <- "data/processed/engineered_data.csv"

saveRDS(df_final, engineered_rds_path)
cat("Saved RDS:", engineered_rds_path, "\n")

write.csv(df_final, engineered_csv_path, row.names = FALSE)
cat("Saved CSV:", engineered_csv_path, "\n\n")

cat("FINAL SUMMARY:\n")
cat("1. Loaded cleaned data:", nrow(df_clean), "rows\n")
cat("2. Added features: price_per_sqft, price_per_acre, area_type\n")
cat("3. Final dataset:", nrow(df_final), "rows,", ncol(df_final), "columns\n")
cat("4. Output files: engineered_data.rds and engineered_data.csv\n")

cat("\nREADY FOR ANALYSIS.\n")
cat("======================================\n")
