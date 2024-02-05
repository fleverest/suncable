library(tidyverse)

wind <- readxl::read_excel(
  "data/SunCable-30 year Time Series-Wind energy.xlsx",
  sheet = 1,
  skip = 9,
  col_names = c("datetime", "year", "windspeed", "net_out_after_export")
) |>
  mutate(
    date = ymd_hms("1993-01-01 00:00:00") + hours(seq(1, n())))
  )


solar <- read.csv2("data/SunCable-15 year Time Series-Solar energy-570_bi_6.csv", sep = ',', skip = 1)[, c(2, 3)]
names(solar) <- c("datetime", "solar_energy")
