# Some exploration and modelling using daily energy production.
# (Aggregating across the 24 hours in each day.)


library(tidyverse)
library(forecast)
library(tsibble)

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


## Some plots.

ws_daily |>
  ggplot(aes(x = dy, y = solar / 1e3)) +
  geom_line() +
  facet_wrap(~ yr)

ws_daily |>
  ggplot(aes(x = dy, y = wind / 1e3)) +
  geom_line() +
  facet_wrap(~ yr)
ggsave("plots/daily-wind-raw.pdf")

# Scatter plot of wind vs solar.
ws_daily |>
  ggplot(aes(x = solar / 1e3, y = wind / 1e3)) +
  geom_point() +
  facet_wrap(~ yr)
ggsave("plots/daily-windVsSolar-raw.pdf")


## Use STL to estimate yearly seasonal component.

slr <- msts(ws_daily$solar / 1e3, seasonal.periods = 365)
wnd <- msts(ws_daily$wind  / 1e3, seasonal.periods = 365)

slr.mstl <- mstl(slr, s.window = "periodic", robust = TRUE)
wnd.mstl <- mstl(wnd, s.window = "periodic", robust = TRUE)

plot(slr.mstl)
plot(wnd.mstl)

# Note: the seasonal components look a bit noisy.
plot(slr.mstl[1:365, "Seasonal365"])
plot(wnd.mstl[1:365, "Seasonal365"])

# Plan for modelling seasonality:
#
# Use loess to smooth the seasonal component,
# then subtract it out to get a seasonally adjusted series.
#
# To make the loess 'sync up' at end of the year, take 3 years' worth of data
# and fit the loess, retaining only the middle portion.

# Check that the year starts at index 185.
stopifnot(ws_daily[185,"dy"] == 1)


# Do loess with solar.

slrrepl <- slr.mstl[seq(from = 185, length.out = 3 * 365), "Seasonal365"]
plot(slrrepl)

slrrdf <- data.frame(x = seq(3 * 365) - 365, y = slrrepl)
slrloess <- loess(y ~ x, slrrdf, span = 0.1)

plot(slrloess)
lines(slrrdf$x, predict(slrloess), col = 2, lwd = 2)


# Do loess with wind.

wndrepl <- wnd.mstl[seq(from = 185, length.out = 3 * 365), "Seasonal365"]
plot(wndrepl)

wnddf <- data.frame(x = seq(3 * 365) - 365, y = wndrepl)
wndloess <- loess(y ~ x, wnddf, span = 0.1)

plot(wndloess)
lines(wnddf$x, predict(wndloess), col = 2, lwd = 2)


# Some plots of the STL modelling.

pdf("plots/daily-stl.pdf")

plot(slr.mstl)
plot(wnd.mstl)

plot(slr.mstl[185 + 0:364, "Seasonal365"])
lines(1:365, predict(slrloess, data.frame(x = 1:365)), col = 2, lwd = 2)

plot(wnd.mstl[185 + 0:364, "Seasonal365"])
lines(1:365, predict(wndloess, data.frame(x = 1:365)), col = 2, lwd = 2)

dev.off()



# Seasonally adjust the solar and wind series.
ws_daily |>
  mutate(solar365 = 1e3 * predict(slrloess, data.frame(x = ws_daily$dy)),
         wind365  = 1e3 * predict(wndloess, data.frame(x = ws_daily$dy))) |>
  mutate(solarAdj = solar - solar365,
         windAdj  = wind  - wind365) -> ws_daily


# Quick visual checks.
with(subset(ws_daily, yr == 2007), plot(dy, solar / 1e3, type = "l"))
with(subset(ws_daily, yr == 2007), plot(dy, solarAdj / 1e3, type = "l"))
with(subset(ws_daily, yr == 2007), plot(dy, wind / 1e3, type = "l"))
with(subset(ws_daily, yr == 2007), plot(dy, windAdj / 1e3, type = "l"))



## Various plots.

# Original series.

p11 <- ws_daily |>
  ggplot(aes(x = dy, y = solar / 1e3)) +
  geom_line() +
  facet_wrap(~ yr)
p12 <-ws_daily |>
  ggplot(aes(x = dy, y = solar / 1e3)) +
  geom_point() +
  facet_wrap(~ yr)
p13 <- ws_daily |>
  ggplot(aes(x = dy, y = solar / 1e3)) +
  geom_point()

p21 <- ws_daily |>
  ggplot(aes(x = dy, y = wind / 1e3)) +
  geom_line() +
  facet_wrap(~ yr)
p22 <-ws_daily |>
  ggplot(aes(x = dy, y = wind / 1e3)) +
  geom_point() +
  facet_wrap(~ yr)
p23 <- ws_daily |>
  ggplot(aes(x = dy, y = wind / 1e3)) +
  geom_point()

pdf("plots/daily-solar-raw.pdf")
print(p11)
print(p12)
print(p13)
dev.off()

pdf("plots/daily-wind-raw.pdf")
print(p21)
print(p22)
print(p23)
dev.off()


# Seasonally adjusted series (wind).
p31 <- ws_daily |>
  ggplot(aes(x = dy, y = windAdj / 1e3)) +
  geom_line() +
  facet_wrap(~ yr)
p32 <- ws_daily |>
  ggplot(aes(x = dy, y = windAdj / 1e3)) +
  geom_point() +
  facet_wrap(~ yr)
p33 <- ws_daily |>
  ggplot(aes(x = dy, y = windAdj / 1e3)) +
  geom_point()
p34 <- ws_daily |>
  ggplot(aes(x = factor(mnth), y = windAdj / 1e3)) +
  geom_boxplot()
#p35 <- ws_daily |>
#  ggplot(aes(x = factor(mnth), y = log(log(windAdj + 1e5)))) +
#  geom_boxplot()

# Seasonally adjusted series (solar).
p41 <- ws_daily |>
  ggplot(aes(x = dy, y = solarAdj / 1e3)) +
  geom_line() +
  facet_wrap(~ yr)
p42 <- ws_daily |>
  ggplot(aes(x = dy, y = solarAdj / 1e3)) +
  geom_point() +
  facet_wrap(~ yr)
p43 <- ws_daily |>
  ggplot(aes(x = dy, y = solarAdj / 1e3)) +
  geom_point()
p44 <- ws_daily |>
  ggplot(aes(x = factor(mnth), y = solarAdj / 1e3)) +
  geom_boxplot()
#p45 <- ws_daily |>
#  ggplot(aes(x = factor(mnth), y = log(log(solarAdj + 1e5)))) +
#  geom_boxplot()

pdf("plots/daily-solar-adjusted.pdf")
print(p41)
print(p42)
print(p43)
print(p44)
dev.off()

pdf("plots/daily-wind-adjusted.pdf")
print(p31)
print(p32)
print(p33)
print(p34)
dev.off()


# Center and scale.
ws_daily |>
  mutate(solarAdjCent = solarAdj - mean(solarAdj),
         windAdjCent  = windAdj  - mean(windAdj)) |>
  group_by(mnth) |>
  mutate(solarAdjCent = solarAdjCent / sd(solarAdjCent),
         windAdjCent  = windAdjCent / sd(windAdjCent)) ->
  ws_daily_adj


# Some plots (solar).
p51 <- ws_daily_adj |>
  ggplot(aes(x = dy, y = solarAdjCent)) +
  geom_line() +
  facet_wrap(~ yr)
p52 <- ws_daily_adj |>
  ggplot(aes(x = dy, y = solarAdjCent)) +
  geom_point() +
  facet_wrap(~ yr)
p53 <- ws_daily_adj |>
  ggplot(aes(x = dy, y = solarAdjCent)) +
  geom_point()
p54 <- ws_daily_adj |>
  ggplot(aes(x = factor(mnth), y = solarAdjCent)) +
  geom_boxplot()

# Some plots (wind).
p61 <- ws_daily_adj |>
  ggplot(aes(x = dy, y = windAdjCent)) +
  geom_line() +
  facet_wrap(~ yr)
p62 <- ws_daily_adj |>
  ggplot(aes(x = dy, y = solarAdjCent)) +
  geom_point() +
  facet_wrap(~ yr)
p63 <- ws_daily_adj |>
  ggplot(aes(x = dy, y = windAdjCent)) +
  geom_point()
p64 <- ws_daily_adj |>
  ggplot(aes(x = factor(mnth), y = windAdjCent)) +
  geom_boxplot()

pdf("plots/daily-solar-adjusted-centered.pdf")
print(p51)
print(p52)
print(p53)
print(p54)
dev.off()

pdf("plots/daily-wind-adjusted-centered.pdf")
print(p61)
print(p62)
print(p63)
print(p64)
dev.off()

## Check autocorrelation and cross-correlation.

p71 <- ws_daily_adj |>
  ggplot(aes(x = solarAdjCent, y = windAdjCent)) +
  geom_point() +
  facet_wrap(~ yr)

p72 <- ws_daily_adj |>
  ungroup() |>
  as_tsibble(index = dayofyear) |>
  select(solarAdjCent,
         windAdjCent) |>
  ggAcf()

pdf("plots/daily-windVsSolar-adjusted-centered.pdf")
print(p71)
print(p72)
dev.off()
