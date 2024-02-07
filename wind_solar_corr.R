box::use(./read_data[ws])

library(tidyverse)
library(forecast)

# Check daily cross-correlation between solar and wind
stats::ccf(ws$solar_energy, ws$wind_energy, lag.max = 24)
# Check monthly cross-correlation between solar and wind
ws_monthly <- ws |>
  group_by(month = floor_date(date, "month")) |>
  summarize(
    mean_solar_energy = mean(solar_energy),
    mean_wind_energy = mean(wind_energy)
  )
stats::ccf(
  ws_monthly$mean_solar_energy,
  ws_monthly$mean_wind_energy,
  lag.max = 24
)

# Check daily autocorrelations
stats::acf(ws$solar_energy, lag.max = 365*24)
stats::acf(ws$wind_energy, lag.max = 48)

## fit arima to wind energy
wind.arima <- forecast::auto.arima(
  ws$wind_energy,
  order = c(0, 1, 1),
  seasonal = c(0, 1, 1)
)

forecast::auto.arima(ws$wind_energy, d = 24, D = 365 * 24)

diff(ws$wind_energy, 24) |> hist()