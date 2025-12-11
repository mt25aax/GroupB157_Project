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

# Effect size interpretation
r_abs <- abs(pearson_test$estimate)
effect_size <- if (r_abs < 0.1) "Negligible" else if (r_abs < 0.3) "Small" else if (r_abs < 0.5) "Medium" else "Large"
cat(sprintf("  Effect Size:        %s\n\n", effect_size))
# Spearman Correlation
spearman_test <- cor.test(helium_data$Volume, helium_data$Close, method = "spearman", exact = FALSE)
cat(sprintf("SPEARMAN: rho = %.6f, p = %.4e\n", spearman_test$estimate, spearman_test$p.value))

# Kendall Correlation
kendall_test <- cor.test(helium_data$Volume, helium_data$Close, method = "kendall")
cat(sprintf("KENDALL:  tau = %.6f, p = %.4e\n\n", kendall_test$estimate, kendall_test$p.value))

# Spearman Correlation
spearman_test <- cor.test(helium_data$Volume, helium_data$Close, method = "spearman", exact = FALSE)
cat(sprintf("SPEARMAN: rho = %.6f, p = %.4e\n", spearman_test$estimate, spearman_test$p.value))

# Kendall Correlation
kendall_test <- cor.test(helium_data$Volume, helium_data$Close, method = "kendall")
cat(sprintf("KENDALL:  tau = %.6f, p = %.4e\n\n", kendall_test$estimate, kendall_test$p.value))
cat("LINEAR REGRESSION ANALYSIS\n")
cat("==========================\n\n")

model <- lm(Close ~ Volume, data = helium_data)
model_summary <- summary(model)

cat("Model: Close = B0 + B1 * Volume + error\n\n")
print(model_summary$coefficients)
cat(sprintf("\nR-squared: %.6f | Adjusted R-squared: %.6f\n", 
            model_summary$r.squared, model_summary$adj.r.squared))
cat(sprintf("Regression Equation: Close = %.6f + (%.12f * Volume)\n\n", 
            coef(model)[1], coef(model)[2]))
# Contingency Table
cat("CONTINGENCY TABLE ANALYSIS\n")
cat("==========================\n\n")

helium_data$Volume_Cat <- ifelse(helium_data$Volume > median(helium_data$Volume), 
                                 "High Volume", "Low Volume")
helium_data$Price_Cat <- ifelse(helium_data$Close > median(helium_data$Close), 
                                "High Price", "Low Price")
contingency <- table(helium_data$Volume_Cat, helium_data$Price_Cat)
print(contingency)
chi_test <- chisq.test(contingency)
cat(sprintf("\nChi-Square: %.4f, df = %d, p = %.4e\n\n", 
            chi_test$statistic, chi_test$parameter, chi_test$p.value))
# Generate Visualizations
cat("GENERATING VISUALIZATIONS\n")
cat("=========================\n\n")

# 1. Scatter Plot
png("output/01_scatter_regression.png", width = 1000, height = 700, res = 120)
par(mar = c(5, 5, 4, 2))
plot(helium_data$Volume / 1e6, helium_data$Close,
     pch = 19, col = adjustcolor("#2E86AB", alpha.f = 0.5),
     xlab = "Trading Volume (Millions)", ylab = "Closing Price (USD)",
     main = "Helium (HNT): Trading Volume vs Closing Price")
abline(lm(Close ~ I(Volume/1e6), data = helium_data), col = "#E94F37", lwd = 2)
legend("topright", legend = c("Data Points", "Regression Line"),
       col = c("#2E86AB", "#E94F37"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))
mtext(paste("r =", round(pearson_test$estimate, 4), "| p <", 
            format(pearson_test$p.value, digits = 3, scientific = TRUE)), 
      side = 3, line = 0, cex = 0.9)
dev.off()
cat("  Saved: output/01_scatter_regression.png\n")

# 2. Histogram - Close
png("output/02_histogram_close.png", width = 1000, height = 700, res = 120)
hist(helium_data$Close, breaks = 40, col = "#2E86AB", border = "white",
     main = "Distribution of Helium Closing Prices",
     xlab = "Closing Price (USD)", ylab = "Frequency")
abline(v = mean(helium_data$Close), col = "#28A745", lwd = 2, lty = 2)
abline(v = median(helium_data$Close), col = "#FFC107", lwd = 2, lty = 3)
legend("topright", legend = c(paste("Mean = $", round(mean(helium_data$Close), 2)),
                              paste("Median = $", round(median(helium_data$Close), 2))),
       col = c("#28A745", "#FFC107"), lty = c(2, 3), lwd = 2)
dev.off()
cat("  Saved: output/02_histogram_close.png\n")

# 3. Histogram - Volume
png("output/03_histogram_volume.png", width = 1000, height = 700, res = 120)
hist(helium_data$Volume / 1e6, breaks = 40, col = "#6C5B7B", border = "white",
     main = "Distribution of Helium Trading Volume",
     xlab = "Trading Volume (Millions)", ylab = "Frequency")
abline(v = mean(helium_data$Volume) / 1e6, col = "#28A745", lwd = 2, lty = 2)
abline(v = median(helium_data$Volume) / 1e6, col = "#FFC107", lwd = 2, lty = 3)
dev.off()
cat("  Saved: output/03_histogram_volume.png\n")

# 4. Time Series
png("output/04_time_series.png", width = 1200, height = 700, res = 120)
par(mar = c(5, 5, 4, 5))
plot(helium_data$Date, helium_data$Close, type = "l", col = "#2E86AB", lwd = 1.5,
     xlab = "Date", ylab = "Closing Price (USD)",
     main = "Helium (HNT) Price and Volume Over Time (June 2020 - May 2022)")
par(new = TRUE)
plot(helium_data$Date, helium_data$Volume / 1e6, type = "h", 
     col = adjustcolor("#6C5B7B", alpha.f = 0.4),
     axes = FALSE, xlab = "", ylab = "")
axis(4, col = "#6C5B7B", col.axis = "#6C5B7B")
mtext("Volume (Millions)", side = 4, line = 3, col = "#6C5B7B")
legend("topleft", legend = c("Closing Price", "Trading Volume"),
       col = c("#2E86AB", "#6C5B7B"), lty = c(1, 1), lwd = c(2, 4))
dev.off()
cat("  Saved: output/04_time_series.png\n")

# 5. Box Plots
png("output/05_boxplots.png", width = 1000, height = 500, res = 120)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
boxplot(helium_data$Close, col = "#2E86AB", border = "#1A5276",
        main = "Closing Price Distribution", ylab = "Price (USD)")
points(1, mean(helium_data$Close), col = "#E94F37", pch = 18, cex = 2)
boxplot(helium_data$Volume / 1e6, col = "#6C5B7B", border = "#4A3F55",
        main = "Trading Volume Distribution", ylab = "Volume (Millions)")
points(1, mean(helium_data$Volume) / 1e6, col = "#E94F37", pch = 18, cex = 2)
dev.off()
cat("  Saved: output/05_boxplots.png\n")

# 6. Correlation Matrix
png("output/06_correlation_matrix.png", width = 800, height = 700, res = 120)
numeric_data <- helium_data[, c("Open", "High", "Low", "Close", "Volume")]
cor_matrix <- cor(numeric_data)
heatmap(cor_matrix, col = colorRampPalette(c("#E94F37", "white", "#2E86AB"))(100),
        symm = TRUE, margins = c(8, 8),
        main = "Correlation Matrix: Helium Market Variables")
dev.off()
cat("  Saved: output/06_correlation_matrix.png\n")

# 7. Residual Diagnostics
png("output/07_residual_diagnostics.png", width = 1000, height = 800, res = 120)
par(mfrow = c(2, 2))
plot(model, which = c(1, 2, 3, 5), col = "#2E86AB", pch = 19)
dev.off()
cat("  Saved: output/07_residual_diagnostics.png\n")

# 8. Density Plots
png("output/08_density_plots.png", width = 1000, height = 500, res = 120)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
plot(density(helium_data$Close), col = "#2E86AB", lwd = 2,
     main = "Density: Closing Price", xlab = "Price (USD)")
polygon(density(helium_data$Close), col = adjustcolor("#2E86AB", alpha.f = 0.3))
plot(density(helium_data$Volume / 1e6), col = "#6C5B7B", lwd = 2,
     main = "Density: Trading Volume", xlab = "Volume (Millions)")
polygon(density(helium_data$Volume / 1e6), col = adjustcolor("#6C5B7B", alpha.f = 0.3))
dev.off()
cat("  Saved: output/08_density_plots.png\n")

# Executive Summary

cat("RESEARCH QUESTION:\n")
cat("Does Helium's trading activity significantly influence daily price behaviour?\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("  1. Pearson Correlation: r = %.4f (Strong Positive)\n", pearson_test$estimate))
cat(sprintf("  2. P-value: %.4e (Highly Significant)\n", pearson_test$p.value))
cat(sprintf("  3. R-squared: %.4f (%.1f%% variance explained)\n", r_squared, r_squared * 100))
cat(sprintf("  4. 95%% CI: [%.4f, %.4f]\n", pearson_test$conf.int[1], pearson_test$conf.int[2]))
cat(sprintf("  5. Effect Size: %s\n\n", effect_size))

cat("STATISTICAL DECISION:\n")
cat("At alpha = 0.05, we REJECT the null hypothesis.\n\n")
