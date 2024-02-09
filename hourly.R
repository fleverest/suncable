# Exploring the hourly data.


library(tidyverse)

# Load the data.
ws <- readr::read_csv("data/wind_and_solar.csv") |>
  mutate(date = with_tz(date, "Australia/Darwin")) |>
  mutate(hr   = hour(date),
         dy   = yday(date),
         dm   = mday(date),
         mnth = month(date),
         yr   = year(date))


## Make some plots.

pSolarLines <- ws |>
  ggplot(aes(x = hr, y = solar_energy / 1e3)) +
  geom_line(aes(group = dm), color = "orangered", alpha = 0.2) +
  facet_grid(yr ~ mnth) +
  theme_bw()

pSolarBoxplots <- ws |>
  ggplot(aes(x = factor(hr), y = solar_energy / 1e3)) +
  geom_boxplot(fill = "orangered", outlier.shape = "cross", outlier.size = 1) +
  facet_grid(yr ~ mnth) +
  theme_bw()

pWindLines <- ws |>
  ggplot(aes(x = hr, y = wind_energy / 1e3)) +
  geom_line(aes(group = dm), color = "blue", alpha = 0.2) +
  facet_grid(yr ~ mnth) +
  theme_bw()

pWindBoxplots <- ws |>
  ggplot(aes(x = factor(hr), y = wind_energy / 1e3)) +
  geom_boxplot(fill = "blue", outlier.shape = "cross", outlier.size = 1) +
  facet_grid(yr ~ mnth) +
  theme_bw()

pdf("plots/hourly-raw.pdf", width = 20, height = 15)
print(pSolarLines)
print(pSolarBoxplots)
print(pWindLines)
print(pWindBoxplots)
dev.off()
