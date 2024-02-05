library(tidyverse)
library(fable)
library(tsibble)
library(lubridate)

# Load datasets
wind <- readxl::read_excel(
  "data/SunCable-30 year Time Series-Wind energy.xlsx",
  sheet = 1,
  skip = 9,
  col_names = c("datetime", "year", "windspeed", "wind_energy")
) |>
  mutate( # Impose assumed date and time - ~^*^~"cleaning"~^*^~
    datetime = ymd_hms("1993-01-01 00:00:00") + hours(seq(1, n()))
  ) |>
  select(-c("datetime", "windspeed", "year"))


solar <- read_csv(
  "data/SunCable-15 year Time Series-Solar energy-570_bi_6.csv",
  col_types = cols_only(
    col_skip(),
    col_datetime(format = "%d/%m/%Y %H:%M"),
    col_guess()
  )
) |>
  rename(datetime = "Date and time", solar_energy = "Energy produced [kWh]")


# Create forecasts (these are essentially the exact examples from the fable README...)

solar_forecast <- as_tsibble(solar, index = datetime) |>
  model(
    ets = ETS(box_cox(solar_energy, 0.3)),
#    arima = ARIMA(log(solar_energy)),
    snaive = SNAIVE(solar_energy)
  ) |>
  forecast(h = "1 years")

forecast <- as_tsibble(wind, index = date) |>
  model(
    ets = ETS(box_cox(wind_energy, 0.3)),
#    arima = ARIMA(log(wind_energy)),
    snaive = SNAIVE(wind_energy)
  ) |>
  forecast(h = "1 years")