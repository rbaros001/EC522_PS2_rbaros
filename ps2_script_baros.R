library(tidyverse)
library(tidyquant)

## Part 3
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

## Part 4
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

# Summary Statistics for each region (wu)
unemp_summary <- unemp |>
  group_by(region) |>
  summarise(
    meanrate = mean(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE)
  )

# (wu)
unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarise(
    meanrateR = mean(unemployment_rate, na.rm = TRUE),
    max_rateR = max(unemployment_rate, na.rm = TRUE),
    min_rateR = min(unemployment_rate, na.rm = TRUE)
  )

# Find the minimum and maximum extremes
unemp_top_increase <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE, na_rm = TRUE)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE, na_rm = TRUE)

# Find the minimum and maximum extremes excluding 2020 (wu)
unemp_top_increase_no_covid <- unemp |>
  filter(date < as.Date("2020-01-01") | date > as.Date("2020-12-31")) |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE, na_rm = TRUE)

unemp_top_decreases_no_covid <- unemp |>
  filter(date < as.Date("2020-01-01") | date > as.Date("2020-12-31")) |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE, na_rm = TRUE)
   
## Part 5 
# Create a Line Plot 
library(ggplot2)

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Unemployment Rate Over Time by Region",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

# Create Individual line plots
ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Unemployment Rate Over Time by Region",
    x = "Date",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()

# Produce a bar chart
ggplot(unemp_summary, aes(x = fct_reorder(region, meanrate), y = meanrate, fill = region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Unemployment Rate by Region",
    x = "Region",
    y = "Average Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Create your own
top_by_region <- unemp |>
  filter(date < as.Date("2020-01-01") | date > as.Date("2020-12-31")) |>
  group_by(region) |>
  slice_min(unemployment_rate_change, n = 1, with_ties = FALSE, na_rm = TRUE) |>
  ungroup() |>
  mutate(label = paste0(format(date, "%b %Y"), "\nRate: ", unemployment_rate, "%"))

ggplot(top_by_region,
       aes(x = reorder(region, unemployment_rate_change),
           y = unemployment_rate_change, fill = region)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = label), vjust = 1.1, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.05))) +
  labs(
    title = "Largest Month-Over-Month Decrease in Unemployment by Region",
    subtitle = "Excluding 2020",
    x = "Region",
    y = "Change in unemployment rate (percentage points)"
  ) +
  theme_minimal()







    
    