# Exploring Box-Cox transformations of daily energy production.
# (Aggregating across the 24 hours in each day.)


library(tidyverse)
library(forecast)

# Load the data.
ws <- readr::read_csv("data/wind_and_solar.csv") |>
  mutate(date = with_tz(date, "Australia/Darwin"))

# Calculate daily energy production.
ws_daily <- ws |>
  group_by(dayofyear = date(date)) |>
  summarise(wind = sum(wind_energy),
            solar = sum(solar_energy)) |>
  mutate(dy   = yday(dayofyear),
         mnth = month(dayofyear),
         yr   = year(dayofyear))


## Box-Cox transformation.

# Estimated coefficients.
wind_boxcox_coefficients <- ws_daily |>
  group_by(mnth) |>
  summarise(box_lambda = BoxCox.lambda(wind))

# Apply the transformation, plus some further ones.
ws_daily_box <- ws_daily |>
  group_by(mnth) |>
  mutate(windBox         = BoxCox(wind, lambda = "auto"),
         windBoxCentered = windBox - median(windBox),
         windBoxScl      = windBoxCentered / sd(windBoxCentered))


## Plots.

p1 <- wind_boxcox_coefficients |>
  ggplot(aes(x = mnth, y = box_lambda)) +
  geom_point()

p2 <- ws_daily |>
  ggplot(aes(x = factor(mnth), y = wind / 1e3)) +
  geom_boxplot()

p3 <- ws_daily_box |>
  ggplot(aes(x = factor(mnth), y = windBoxScl)) +
  geom_boxplot()

p4 <- ws_daily |>
  ggplot(aes(x = wind / 1e3)) +
  geom_histogram() +
  facet_wrap(~ factor(mnth))

p5 <- ws_daily_box |>
  ggplot(aes(x = windBoxScl)) +
  geom_histogram() +
  facet_wrap(~ factor(mnth))

pdf("plots/daily-boxcox-exploration.pdf")
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
dev.off()
