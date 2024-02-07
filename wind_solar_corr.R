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

ws_month_day <- ws |>
  mutate(
    solar_bin = cut(
      solar_energy / 1e3,
      breaks = seq(0, by = 35, length.out = 30),
      include.lowest = TRUE
    ),
    wind_bin = cut(
      wind_energy,
      breaks = seq(0, 3000, by = 100),
      include.lowest = TRUE
    )
  ) |>
  group_by(
    hr = as.factor(hour(date)),
    mnth = as.factor(month(date))
  )

# solar energy heat map
ws_month_day |>
  group_by(hr, solar_bin, .drop = FALSE) |>
  count(name = "solar_density") |>
  ggplot(aes(x = hr, y = solar_bin)) +
  geom_raster(aes(fill = solar_density)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    y = "Hourly Solar Energy (MWh)",
    x = "Hour of day",
    fill = "Density",
    title = "working title (solar heat map)"
  )
ggsave("plots/solar_heat_map.pdf")

# solar energy heat map monthly
ws_month_day |>
  group_by(hr, mnth, solar_bin, .drop = FALSE) |>
  count(name = "solar_density") |>
  ggplot(aes(x = hr, y = solar_bin)) +
  geom_raster(aes(fill = solar_density)) +
  facet_wrap(~ mnth) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    y = "Hourly Solar Energy (MWh)",
    x = "Hour of day",
    fill = "Density",
    title = "working title (solar heat map)"
  )
ggsave("plots/solar_heat_map_monthly.pdf")

# wind energy heat map
ws_month_day |>
  group_by(hr, wind_bin, .drop = FALSE) |>
  count(name = "wind_density") |>
  ggplot(aes(x = hr, y = wind_bin)) +
  geom_raster(aes(fill = wind_density)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    y = "Hourly Wind Energy (MWh?????)",
    x = "Hour of day",
    fill = "Density",
    title = "working title (wind heat map)"
  )
ggsave("plots/wind_heat_map.pdf")

# Wind heatmap by month
ws_month_day |>
  group_by(hr, mnth, wind_bin, .drop = FALSE) |>
  count(name = "wind_density") |>
  ggplot(aes(x = hr, y = wind_bin)) +
  geom_raster(aes(fill = wind_density)) +
  facet_wrap(~mnth) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    y = "Hourly Wind Energy (MWh?????)",
    x = "Hour of day",
    fill = "Density",
    title = "working title (wind heat map)"
  )
ggsave("plots/wind_heat_map_monthly.pdf")

# wind histogram 2am
ws_month_day |>
  filter(hour(date) == 2) |>
  ggplot(aes(x = wind_energy)) +
  geom_histogram()


# wind solar heatmap
ws |>
  mutate(
    wind_bin = cut(wind_energy, 20),
    solar_bin = cut(solar_energy, 20)
  ) |>
  count()

# wind solar scatter
ws |>
  filter(year(date) < 2008) |>
  ggplot(aes(x=solar_energy, y=wind_energy)) + geom_point()

# Visualize daily distribution
coeff <- mean(ws$solar_energy) / mean(ws$wind_energy)
ws |>
  group_by(hr = hour(date)) |>
  summarize(
    wind25 = quantile(wind_energy, 0.25),
    wind50 = median(wind_energy),
    wind75 = quantile(wind_energy, 0.75),
    solar25 = quantile(solar_energy, 0.25),
    solar50 = median(solar_energy),
    solar75 = quantile(solar_energy, 0.75)
  ) |>
  ggplot(aes(x = hr)) +
  geom_point(aes(y = solar50, colour = "Solar")) +
  geom_point(aes(y = wind50 * coeff, colour = "Wind")) +
  scale_y_continuous(
    name = "Solar Energy",
    sec.axis = sec_axis(
      trans = ~ . * coeff,
      name = "Wind Energy"
    )
  )
