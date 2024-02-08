box::use(./read_data[ws])

library(tidyverse)
library(fable)
library(ggplot2)

ws_p10 <- ws |>
  # got lambda2~10 from geoR::boxcoxfit
  mutate(solar_energy = solar_energy + 10) |>
  # Get 4 years
  filter(year(date) %in% 2007:2011) |>
  as_tsibble()


# A dynamic harmonic regression model with multiple seasonality with periods 1 day and 1 year?
fit <- ws_p10 |>
  model(
    ARIMA(
      log(solar_energy) ~ PDQ(0, 0, 0) +
                          pdq(d = 0) +
                          fourier(period = 24, K = 5) +
                          fourier(period = 24 * 365, K = 5)
    )
  )