# Clear environment and set options
rm(list = ls())
options(scipen = 999, digits = 10)

# Create output directory
if (!dir.exists("output")) dir.create("output")

# Load data
helium_data <- read.csv("user_input_files/Helium.csv", stringsAsFactors = FALSE)
helium_data$Date <- as.Date(helium_data$Date, format = "%Y-%m-%d")
