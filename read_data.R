box::use(
  dplyr[select, mutate, n, rename, as_tibble],
  lubridate[ymd_hms, hours, force_tz],
  readr[read_csv],
  vroom[...]
)

# Load datasets
wind <- readxl::read_excel(
  "data/SunCable-30 year Time Series-Wind energy.xlsx",
  sheet = 1,
  skip = 9,
  col_names = c("datetime", "year", "windspeed", "wind_energy")
) |>
  mutate( # Impose assumed date and time - ~^*^~"cleaning"~^*^~
    date = ymd_hms("1993-01-01 10:00:00", tz = "Australia/Darwin") + hours(seq(0, n() - 1))
  ) |>
  select(-c(datetime, windspeed, year))


solar <- read_csv(
  "data/SunCable-15 year Time Series-Solar energy-570_bi_6.csv",
  col_types = cols_only(
    col_skip(),
    col_datetime(format = "%d/%m/%Y %H:%M"),
    col_guess()
  )
) |>
  rename(date = "Date and time", solar_energy = "Energy produced [kWh]") |>
  mutate(date = force_tz(date, "Australia/Darwin"))

ws <- merge(wind, solar) |> as_tibble()