# ============================================
# Create USA Real Estate Project Structure
# ============================================

create_project_structure <- function() {
  
  project_root <- "D:/R"
  
  
  cat("Creating USA Real Estate project structure...\n")
  
  # Folder structure
  folders <- c(
    "data/raw",
    "data/processed",
    "scripts",
    "outputs/figures",
    "outputs/models",
    "outputs/metrics",
    "report",
    "Presentation"
   
  )
  
  for (folder in folders) {
    dir_path <- file.path(project_root, folder)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cat("Created:", folder, "\n")
    }
  }
  
  source_file <- file.path(project_root, "Real.csv")
  target_file <- file.path(project_root, "data/raw/Real.csv")
  
  if (file.exists(source_file) && !file.exists(target_file)) {
    file.rename(source_file, target_file)
    cat("📄 Moved: Real.csv → data/raw/\n")
  } else {
    cat("Dataset already in correct location or not found.\n")
  }
  
  cat("\n Project structure setup completed successfully!\n")
}

create_project_structure()
