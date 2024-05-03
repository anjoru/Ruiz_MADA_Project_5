library(here)

# Function to extract library names from a line of code
extract_libraries <- function(line) {
  matches <- gregexpr("library\\(([^)]+)\\)|require\\(([^)]+)\\)", line)
  unlist(lapply(regmatches(line, matches), function(x) gsub("library\\(|require\\(|\\)", "", x)))
}

# List of paths to the Quarto documents you are editing
qmd_paths <- list(
  here("R", "analysis-code", "analysis.qmd"),
  here("R", "eda-code", "EDA_unified.qmd"),
  here("R", "processing-code", "MADAproject_pt2_ruiz.qmd")
)

# Function to process each qmd file and extract libraries
process_qmd <- function(qmd_path) {
  qmd_content <- readLines(qmd_path)
  library_lines <- grep("library\\(|require\\(", qmd_content, value = TRUE)
  library_names <- unique(unlist(lapply(library_lines, extract_libraries)))
  library_names <- gsub("\"|'", "", library_names)  # Remove potential quotes
  library_names <- trimws(library_names)  # Trim whitespace
  unique(library_names)  # Ensure unique names
}

# Apply the function to each qmd file and combine the results
all_libraries <- unique(unlist(lapply(qmd_paths, process_qmd)))

# Sort libraries alphabetically
sorted_libraries <- sort(all_libraries)

# Get versions of these libraries
library_versions <- sapply(sorted_libraries, function(name) {
  if (name %in% rownames(installed.packages())) {
    as.character(packageVersion(name))
  } else {
    NA
  }
})

# Combine names and versions into a data frame
library_info <- data.frame(Library = sorted_libraries, Version = library_versions, stringsAsFactors = FALSE)

# Save to CSV file
write.csv(library_info, here("R", "library_versions.csv"), row.names = FALSE)

# Message to inform about CSV creation
cat("Library and version information saved to: ", here("results", "library_versions.csv"), "\n")
