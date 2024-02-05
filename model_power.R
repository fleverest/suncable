library(tidyverse)

wind <- readxl::read_excel(
  "data/SunCable-30 year Time Series-Wind energy.xlsx",
  sheet = 1,
  skip = 9,
  col_names = c("datetime", "year", "windspeed", "net_out_after_export")
) |>
  mutate(
    date = ymd_hms("1993-01-01 00:00:00") + hours(seq(1, n()))
  )


solar <- read_csv("data/SunCable-15 year Time Series-Solar energy-570_bi_6.csv",
                  col_types = cols_only(col_skip(),
                                        col_datetime(format = "%d/%m/%Y %H:%M"),
                                        col_guess()))
names(solar) <- c("datetime", "solar_energy")
