library(tidyverse)
library(tidyquant)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

# Process the unemployment data
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )


# Replace the unemployment rate for October 2025 with the September 2025 value
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Calculate month-over-month change in unemployment rate
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

# Filter for recent data
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

range(unemp_recent$date)   # should start at 2015-01-01
count(unemp_recent, region) # each region should have the same number of rows

# Summary Statistics for each region
unemp_summary <- unemp |>
  group_by(region) |>
  summarise(
    meanrate = mean(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE)
  )

unemp_top_increase <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE, na_rm = TRUE)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE, na_rm = TRUE)



   
    
    
    
    
    
    
    
    
    