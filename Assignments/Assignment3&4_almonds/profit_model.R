# Crop Profit Model
# This script contains a function to calculate profit from crop yield anomalies for any supported crop.

# Requires sourcing yield profit model
#source(here::here("Assignments", "Assignment3&4_almonds", "yield_model.R"))


# Input:
#   clim - data frame with columns: day, month, year, tmax_c, tmin_c, precip
#   crop_name - character string: "almonds", "wine_grapes", "table_grapes",
#               "oranges", "walnuts", or "avocados" (default: "almonds")
#   acres - number of acres (default: 100)
#   baseline_yield - baseline yield in tons per acre (default: 1.0)
#   baseline_price - baseline price per ton per acre (default: 5000)
#   correlation_slope - correlation between profit and yield anomaly (default: -1.04)
#   irrigation_cost - cost of irrigation per acre (default: 500)
#   planting_cost - cost of planting per acre (default: 300)
#   harvest_cost - cost of harvest per acre (default: 400)
#
# Output:
#   list with results:
#         * harvest_year: year of harvest
#         * yield_anomaly: yield anomaly from climate model
#         * yield_change_pct: percentage change in yield
#         * revenue: total revenue for that year
#         * cost: total cost for that year
#         * profit: total profit for that year
#     - mean_profit: mean profit across all years
#     - max_profit: maximum profit across all years
#     - min_profit: minimum profit across all years
#     - total_cost_per_acre: sum of all costs per acre
#     - acres: number of acres
#     - baseline_yield: baseline yield used
#     - baseline_price: baseline price used
#     - crop: crop name
# ----------------------------------------------------------------------------

CropProfitModel <- function(clim,
                            crop_name = "almonds",
                            acres = 100,
                            baseline_yield = 1.0,
                            baseline_price = 5000,
                            correlation_slope = -1.04,
                            irrigation_cost = 500,
                            planting_cost = 300,
                            harvest_cost = 400) {

  # Step 1: Calculate yield anomalies using the crop yield model

  yield_results <- CropYieldModel(clim, crop_name = crop_name)

  # Extract yield anomalies and harvest years
  yield_anomalies <- yield_results$yield_anomalies
  harvest_years <- yield_results$harvest_years

  # Step 2: Calculate % yield change for each year

  # This represents how much the yield deviates from baseline
  yield_change_pct <- (baseline_yield + yield_anomalies) / baseline_yield

  # Step 3: Calculate revenue for each year

  # The correlation slope represents how profit responds to yield changes

  # For almonds, the correlation slope is typically -1.04 (Table 5. Lobell 2006)
  # This formula adjusts revenue based on the correlation between yield and profit
  revenue <- acres * (baseline_price * (yield_change_pct * correlation_slope))

  # Step 4: Calculate cost for each year

  total_cost_per_acre <- irrigation_cost + planting_cost + harvest_cost
  cost <- acres * total_cost_per_acre

  # Step 5: Calculate profit for each year
  profit <- revenue - cost

  # Create results data frame
  results_df <- data.frame(
    harvest_year = harvest_years,
    yield_anomaly = yield_anomalies,
    yield_change_pct = yield_change_pct,
    revenue = revenue,
    cost = cost,
    profit = profit
  )

  # Calculate summary statistics
  valid_profits <- profit[!is.na(profit)]

  # Return results
  return(list(
    results = results_df,
    mean_profit = ifelse(length(valid_profits) > 0,
                         mean(valid_profits, na.rm = TRUE), NA),
    max_profit = ifelse(length(valid_profits) > 0,
                        max(valid_profits, na.rm = TRUE), NA),
    min_profit = ifelse(length(valid_profits) > 0,
                        min(valid_profits, na.rm = TRUE), NA),
    total_cost_per_acre = total_cost_per_acre,
    acres = acres,
    baseline_yield = baseline_yield,
    baseline_price = baseline_price,
    crop = crop_name
  ))
}

