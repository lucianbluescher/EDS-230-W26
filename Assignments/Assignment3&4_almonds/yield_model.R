# Crop Yield Anomaly Model
# Based on Lobell et al. (2006) statistical yield models

library(dplyr)

# Crop Model Definitions

# Each crop has a polynomial equation using monthly climate variables
# Negative month indices indicate months from the year prior to harvest

crop_models <- list(
  almonds = list(
    equation = function(Tn2, P1) {
      -0.015 * Tn2 - 0.0046 * Tn2^2 - 0.07 * P1 + 0.0043 * P1^2 + 0.28
    },
    months = list(Tn = 2, P = 1)  # Tn from month 2 (Feb), P from month 1 (Jan)
  ),
  
  wine_grapes = list(
    equation = function(Tn4, P6, P9) {
      2.65 * Tn4 - 0.17 * Tn4^2 + 4.78 * P6 - 4.93 * P6^2 - 2.24 * P9 + 1.54 * P9^2 - 10.50
    },
    months = list(Tn = 4, P = c(6, 9))
  ),
  
  table_grapes = list(
    equation = function(Tn7, Tn4, P1, P10) {
      6.93 * Tn7 - 0.19 * Tn7^2 + 2.61 * Tn4 - 0.15 * Tn4^2 + 
        0.035 * P1 + 0.024 * P1^2 + 1.71 * P10 - 0.673 * P10^2 - 73.89
    },
    months = list(Tn = c(7, 4), P = c(1, 10))
  ),
  
  oranges = list(
    equation = function(Tn_12, P5) {
      1.08 * Tn_12 - 0.20 * Tn_12^2 + 4.99 * P5 - 1.97 * P5^2 - 2.47
    },
    months = list(Tn = -12, P = 5)  # Tn from 12 months before harvest
  ),
  
  walnuts = list(
    equation = function(Tx_11, P2) {
      0.68 * Tx_11 - 0.020 * Tx_11^2 + 0.038 * P2 - 0.0051 * P2^2 - 5.83
    },
    months = list(Tx = -11, P = 2)
  ),
  
  avocados = list(
    equation = function(Tx_8, Tn5, P10) {
      17.71 * Tx_8 - 0.29 * Tx_8^2 + 3.25 * Tn5 - 0.14 * Tn5^2 + 
        1.00 * P10 - 0.31 * P10^2 - 288.09
    },
    months = list(Tx = -8, Tn = 5, P = 10)
  )
)


# process_climate_data
# This function aggregates daily climate data to two monthly summaries
# - Temperatures: monthly mean
# - Precipitation: monthly sum


process_climate_data <- function(clim_data) {
  monthly_clim <- clim_data %>%
    group_by(year, month) %>%
    summarise(
      Tn_monthly = mean(tmin_c, na.rm = TRUE),  # Monthly mean minimum temp
      Tx_monthly = mean(tmax_c, na.rm = TRUE),  # Monthly mean maximum temp
      P_monthly = sum(precip, na.rm = TRUE),     # Monthly total precipitation
      .groups = 'drop'
    )
  
  return(monthly_clim)
}


# extract_climate_vars

# This function extracts climate variables for specific months relative to harvest year and handles negative months (from previous year) and positive months (current year)


extract_climate_vars <- function(monthly_clim, harvest_year, months_needed) {
  vars <- list()
  
  for (var_type in names(months_needed)) {
    month_nums <- months_needed[[var_type]]
    
    # Map variable type to column name
    var_map <- list("Tn" = "Tn_monthly", "Tx" = "Tx_monthly", "P" = "P_monthly")
    temp_var <- var_map[[var_type]]
    
    # Handle multiple months for same variable type
    if (length(month_nums) > 1) {
      var_values <- numeric(length(month_nums))
      for (i in seq_along(month_nums)) {
        m <- month_nums[i]
        # Negative months are from previous year
        if (m < 0) {
          target_year <- harvest_year - 1
          target_month <- abs(m)
        } else {
          target_year <- harvest_year
          target_month <- m
        }
        
        value <- monthly_clim %>%
          filter(year == target_year, month == target_month) %>%
          pull(!!sym(temp_var))
        
        var_values[i] <- ifelse(length(value) > 0 && !is.na(value[1]), value[1], NA)
      }
      vars[[var_type]] <- var_values
    } else {
      # Single month
      m <- month_nums
      if (m < 0) {
        target_year <- harvest_year - 1
        target_month <- abs(m)
      } else {
        target_year <- harvest_year
        target_month <- m
      }
      
      value <- monthly_clim %>%
        filter(year == target_year, month == target_month) %>%
        pull(!!sym(temp_var))
      
      vars[[var_type]] <- ifelse(length(value) > 0 && !is.na(value[1]), value[1], NA)
    }
  }
  
  return(vars)
}


# CropYieldModel

# This is the primary function that calculates crop yield anomalies from daily climate data

# Input:
#   clim - data frame with columns: day, month, year, tmax_c, tmin_c, precip
#   crop_name - character string: "almonds", "wine_grapes", "table_grapes", 
#               "oranges", "walnuts", or "avocados"
#
# Output:
#   list with yield_anomalies, harvest_years, max/min/mean yield anomaly, crop name
# ----------------------------------------------------------------------------
CropYieldModel <- function(clim, crop_name = "almonds") {
  
  # Validate crop name
  if (!crop_name %in% names(crop_models)) {
    stop(paste("Crop '", crop_name, "' not supported. Available: ", 
               paste(names(crop_models), collapse = ", "), sep = ""))
  }
  
  # Process daily data to monthly aggregates
  monthly_clim <- process_climate_data(clim)
  
  # Determine harvest years (need at least one year before for negative month indices)
  harvest_years <- sort(unique(monthly_clim$year))
  harvest_years <- harvest_years[harvest_years > min(harvest_years)]
  
  # Get crop model specification
  model <- crop_models[[crop_name]]
  months_needed <- model$months
  equation <- model$equation
  
  # Calculate yield anomalies for each harvest year
  yield_anomalies <- numeric(length(harvest_years))
  valid_years <- logical(length(harvest_years))
  
  for (i in seq_along(harvest_years)) {
    hy <- harvest_years[i]
    
    # Extract climate variables for this harvest year
    clim_vars <- extract_climate_vars(monthly_clim, hy, months_needed)
    
    # Check for missing data
    if (any(is.na(unlist(clim_vars)))) {
      yield_anomalies[i] <- NA
      valid_years[i] <- FALSE
      next
    }
    
    valid_years[i] <- TRUE
    
    # Call equation with appropriate arguments based on crop
    if (crop_name == "almonds") {
      yield_anomalies[i] <- equation(clim_vars$Tn, clim_vars$P)
    } else if (crop_name == "wine_grapes") {
      yield_anomalies[i] <- equation(clim_vars$Tn, clim_vars$P[1], clim_vars$P[2])
    } else if (crop_name == "table_grapes") {
      yield_anomalies[i] <- equation(clim_vars$Tn[1], clim_vars$Tn[2], 
                                     clim_vars$P[1], clim_vars$P[2])
    } else if (crop_name == "oranges") {
      yield_anomalies[i] <- equation(clim_vars$Tn, clim_vars$P)
    } else if (crop_name == "walnuts") {
      yield_anomalies[i] <- equation(clim_vars$Tx, clim_vars$P)
    } else if (crop_name == "avocados") {
      yield_anomalies[i] <- equation(clim_vars$Tx, clim_vars$Tn, clim_vars$P)
    }
  }
  
  # Calculate summary statistics (excluding NA values)
  valid_anomalies <- yield_anomalies[valid_years]
  
  # Return results
  return(list(
    yield_anomalies = yield_anomalies,
    harvest_years = harvest_years,
    max_yield_anomaly = ifelse(length(valid_anomalies) > 0, 
                               max(valid_anomalies, na.rm = TRUE), NA),
    min_yield_anomaly = ifelse(length(valid_anomalies) > 0, 
                               min(valid_anomalies, na.rm = TRUE), NA),
    mean_yield_anomaly = ifelse(length(valid_anomalies) > 0, 
                                mean(valid_anomalies, na.rm = TRUE), NA),
    crop = crop_name,
    n_years = sum(valid_years)
  ))
}