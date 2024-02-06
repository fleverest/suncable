box::use(./read_data[wind, solar])

library(tidyverse)
library(sarima)

ws <- merge(wind, solar) |> as_tibble()

# Check cross-correlation between solar and wind
stats::ccf(ws$solar_energy, ws$wind_energy, lag.max = 48)

# Check autocorrelations
stats::acf(ws$solar_energy, lag.max = 48)
stats::acf(ws$wind_energy, lag.max = 48)

## AirPassengers example
## fit the classic airline model using arima()
ap.arima <- arima(ws$wind_energy, order = c(0,1,1), seasonal = c(0,1,1))

forecast::auto.arima(diff(ws$wind_energy, 24))

diff(ws$wind_energy, 24) |> hist()