library(tidyverse)
library(zoo)
library(ggplot2)
options(scipen=999)

source("~/Documents/fia_sensitivity/zoom_sensitivity_analysis_helper.R")

# Load in data
financials <- read_csv("~/Documents/fia_sensitivity/financial_statements_transposed.csv")
forecast.data <- read_csv("~/Documents/fia_sensitivity/forecast_data.csv")
forecast.assumptions <- read_csv("~/Documents/fia_sensitivity/forecast_assumptions.csv")

# Add terminal value boolean column to financial data
financials <- financials %>% 
  mutate(is.terminal = ifelse(years.from.today == max(years.from.today), TRUE, FALSE))

# Change variables for tests COMMENT OUT WHEN DONE
#forecasts$forecast.jan.2033[forecasts$readable.name == "Sales Growth"] <- 0.045
#forecasts$forecast.jan.2033[forecasts$readable.name == "Cost of Goods Sold/Sales"] <- 0.35

# Set initial parameters for zoom (ZM)
shares.outstanding <- 291889.41
most.recent.fiscal.year.end <- as.Date("2021-01-31")
date.of.valuation <- as.Date("2021-04-09")
cost.of.capital <- 0.08

# Get company value for starting data
value.per.share <-
  CalculateCompanyValue(financials, forecast.data, forecast.assumptions,
                        cost.of.capital, shares.outstanding)

################# Sensitivity analysis per variable 
# Create empty data frame for results
results1 <- data.frame(variable=character(nrow(forecast.assumptions)),
                      stringsAsFactors = FALSE)

# Create sensitivity levels to test (-20%, -10%, 0%, 10%, 20%)
levels <- c(0.8, 0.9, 1, 1.1, 1.2)

# Create columns for all level values
for (j in 1:length(levels)) {
  column.name <- paste0('value.', levels[j] * 100, '.percent')
  results1$placeholder <- double(nrow(results1))
  names(results1)[names(results1) == "placeholder"] <- column.name
}

# Loop through all sensitivity variables
for (i in 1:nrow(forecast.assumptions)) {
  results1$variable[i] <- toString(forecast.assumptions[i, 'variable'])
  
  # Loop through all sensitivity levels that we want to analyze
  for (j in 1:length(levels)) {
    # Create an adjusted forecast with the single variable changed
    adjusted.assumptions <- forecast.assumptions
    adjusted.assumptions[i, 'forecast.value'] =
      adjusted.assumptions[i, 'forecast.value'] * levels[j]
    
    # Calculate value per share based on the adjusted value
    value.per.share.i.j <- 
      CalculateCompanyValue(financials, forecast.data, adjusted.assumptions, 
                            cost.of.capital, shares.outstanding)
    
    # Save results to the correct columns
    value.col <- paste0('value.', levels[j] * 100, '.percent')
    results1[i, value.col] <- adjusted.assumptions[i, 'forecast.value']
    
    forecast.col <- paste0('forecast.', levels[j] * 100, '.percent')
    results1[i, forecast.col] <- value.per.share.i.j
  }
  
  # Print progress status
  print(paste0('Sensitivity analysis complete for: ', results1$variable[i]))
}

# Add fields for generating table for top fields
results1 <- results1 %>%
  mutate(change.per.percent = (forecast.110.percent - forecast.90.percent) / 20) %>%
  arrange(-abs(change.per.percent))

# Output results
write.csv(results1, "~/Documents/fia_sensitivity/results/individual_variable_results_v4.csv")


################# Sensitivity analysis for the full forecast
# Create empty data frame for results
results2 <- c()

# Set configuration value
num.iterations = 5000

# Run this analysis as many times as iterations provided
for (i in 1:num.iterations) {
  # Make a copy of the forecasts to adjust
  adjusted.assumptions.i <- forecast.assumptions
  
  # Add random noise to the forecast by looping through all variables
  for (j in 1:nrow(adjusted.assumptions.i)) {
    # Get sd for variable
    min.deviation.j <- adjusted.assumptions.i$min.terminal[j]
    max.deviation.j <- adjusted.assumptions.i$max.terminal[j]
    sd.j <- abs(max.deviation.j - min.deviation.j) / (1.645 * 2)
    
    # Get new variable value
    value.j <- rnorm(1, adjusted.assumptions.i$forecast.value[j], sd.j)
    
    # Clip to 0 and 1
    #value.j <- ifelse(value.j < 0,  0, ifelse(value.j > 1, 1, value.j))
    
    # Adjust value in adjsuted forecast table
    adjusted.assumptions.i$forecast.value[j] <- value.j
  }
  
  # Calculate value per share based on adjusted financials
  value.per.share.i <- 
    CalculateCompanyValue(financials, forecast.data, adjusted.assumptions.i, 
                          cost.of.capital, shares.outstanding)
  
  # Append results into results vector
  results2 <- append(results2, value.per.share.i)
  
  # Print progress status
  if (i %% 5 == 0) {
    print(paste0('Sensitivity analysis iterations completed so far: ', i))
  }
}

# Output results for later use
write.csv(results2, "~/Documents/fia_sensitivity/results/full_forecast_results_v4.csv")

# Chart results (exported to clipboard with x=2000, y=1000)
tibble(val = results2) %>%
  ggplot(., aes(val)) + 
  geom_histogram(binwidth = 20, boundary = 0, color = "#2D8CFF", fill = "#D7E6F9") +
  scale_x_continuous(limits = c(0, 1000)) +
  theme_minimal() +
  theme(text = element_text(size=20)) +
  labs(y = "Number of Simulations", x = "ZM Value Per Share ($)")

# Look at key statistics
median(results2)
mean(results2)
quantile(results2, probs = seq(0, 1, by= 0.05))
