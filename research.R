# Clear environment and set options
rm(list = ls())
options(scipen = 999, digits = 10)

# Create output directory
if (!dir.exists("output")) dir.create("output")

# Load data
helium_data <- read.csv("user_input_files/Helium.csv", stringsAsFactors = FALSE)
helium_data$Date <- as.Date(helium_data$Date, format = "%Y-%m-%d")

# Display dataset summary
cat("===============================================================================\n")
cat("         HELIUM CRYPTOCURRENCY CORRELATION ANALYSIS - R VERSION               \n")
cat("===============================================================================\n\n")

cat("Dataset Summary:\n")
cat(sprintf("  Observations: %d\n", nrow(helium_data)))
cat(sprintf("  Variables: %d\n", ncol(helium_data)))
cat(sprintf("  Date Range: %s to %s\n", min(helium_data$Date), max(helium_data$Date)))
cat(sprintf("  Duration: %.1f months\n\n", 
            as.numeric(difftime(max(helium_data$Date), min(helium_data$Date), units = "weeks"))/4.33))
#first five observation
cat("First 5 Observations:\n")
print(head(helium_data, 5))
cat("\n")

# Descriptive Statistics Function
compute_stats <- function(x, name) {
  cat(sprintf("%s Statistics:\n", name))
  cat(sprintf("  Count:    %d\n", length(x)))
  cat(sprintf("  Mean:     %.6f\n", mean(x, na.rm = TRUE)))
  cat(sprintf("  Median:   %.6f\n", median(x, na.rm = TRUE)))
  cat(sprintf("  Std Dev:  %.6f\n", sd(x, na.rm = TRUE)))
  cat(sprintf("  Variance: %.6f\n", var(x, na.rm = TRUE)))
  cat(sprintf("  Min:      %.6f\n", min(x, na.rm = TRUE)))
  cat(sprintf("  Max:      %.6f\n", max(x, na.rm = TRUE)))
  cat(sprintf("  Range:    %.6f\n", max(x) - min(x)))
  cat(sprintf("  Q1:       %.6f\n", quantile(x, 0.25)))
  cat(sprintf("  Q3:       %.6f\n", quantile(x, 0.75)))
  cat(sprintf("  IQR:      %.6f\n", IQR(x)))
  
  # Skewness and Kurtosis (manual calculation)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  skew <- (sum((x - m)^3) / n) / (s^3)
  kurt <- (sum((x - m)^4) / n) / (s^4) - 3
  cat(sprintf("  Skewness: %.6f\n", skew))
  cat(sprintf("  Kurtosis: %.6f\n\n", kurt))
}
compute_stats(helium_data$Close, "Closing Price (USD)")
compute_stats(helium_data$Volume, "Trading Volume")
# Correlation Analysis
cat("SECTION: CORRELATION ANALYSIS\n")
cat("=============================\n\n")

# Pearson Correlation Test
pearson_test <- cor.test(helium_data$Volume, helium_data$Close, method = "pearson")
cat("PEARSON PRODUCT-MOMENT CORRELATION TEST\n")
cat("Hypotheses: H0: rho = 0 vs H1: rho != 0\n\n")
cat(sprintf("  Pearson r:          %.6f\n", pearson_test$estimate))
cat(sprintf("  t-statistic:        %.6f\n", pearson_test$statistic))
cat(sprintf("  Degrees of freedom: %d\n", pearson_test$parameter))
cat(sprintf("  P-value:            %.4e\n", pearson_test$p.value))
cat(sprintf("  95%% CI:            [%.6f, %.6f]\n", 
            pearson_test$conf.int[1], pearson_test$conf.int[2]))

r_squared <- pearson_test$estimate^2
cat(sprintf("  R-squared:          %.6f (%.2f%% variance explained)\n\n", 
            r_squared, r_squared * 100))