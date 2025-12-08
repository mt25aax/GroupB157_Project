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
