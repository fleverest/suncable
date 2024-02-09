library(tidyverse)
library(forecast)
library(tsibble)


df <- read_csv("data/wind_and_solar.csv") |>
    pivot_longer(!date) |>
    as_tsibble(key = name, index = date)


head(df)

df_solar <- df |>
    filter(name == "solar_energy") |>
    mutate(
        day_of_year = lubridate::yday(date),
        hour_of_day = lubridate::hour(date) > 8 & lubridate::hour(date) < 16,
        symmetric_hour = factor(lubridate::hour(date)),
        previous_value = lag(value, 1)
    )

model <- glm(sqrt(value) ~ previous_value + cos(2 * pi * day_of_year / 365) + sin(2 * pi * day_of_year / 365) + symmetric_hour, data = df_solar)

summary(model)
plot(model)

df_solar$predictions <- predict(model, newdata = df_solar, type="response")^2

ggplot(df_solar |> dplyr::filter(year(date) == 2009, month(date) %in% 1:3), aes(x = date)) +
    geom_line(aes(y = (value - predictions)/319376))

ggplot(df_solar |> dplyr::filter(year(date) == 2009, month(date) %in% 1:3), aes(x = date)) +
    geom_line(aes(y = value)) +
    geom_line(aes(y = predictions), colour = "red", alpha = 0.5)

df_solar |>
    group_by(month(date)) |>
    summarise(data_average = mean(value), predictions_average = mean(predictions)) |>
    ggplot(aes(x = `month(date)`)) +
        geom_line(aes(y = data_average - predictions_average))


df_wind <- df |>
    filter(name == "wind_energy") |>
    mutate(
        day_of_year = lubridate::yday(date),
        hour_of_day = lubridate::hour(date) > 8 & lubridate::hour(date) < 16,
        symmetric_hour = factor(lubridate::hour(date)),
        previous_value = lag(value, 1)
    )

model_wind <- glm(sqrt(value) ~ previous_value + cos(2 * pi * day_of_year / 365) + sin(2 * pi * day_of_year / 365) + symmetric_hour, data = df_wind)

df_wind$predictions <- predict(model_wind, newdata = df_wind, type = "response")^2

ggplot(df_wind |> dplyr::filter(year(date) == 2009, month(date) %in% 1:3), aes(x = date)) +
    geom_line(aes(y = (value - predictions) / 319376))

ggplot(df_wind |> dplyr::filter(year(date) == 2009, month(date) %in% 1:3), aes(x = date)) +
    geom_line(aes(y = value)) + geom_line(aes(y = predictions), colour = "red")

df_wind |>
    group_by(month(date)) |>
    summarise(data_average = mean(value), predictions_average = mean(predictions)) |>
    ggplot(aes(x = `month(date)`)) +
    geom_line(aes(y = data_average - predictions_average))

wind_sims <- simulate(model_wind, nsim = 100) |>
    mutate(across(everything(), function(x) {
        x^2
    })) |>
    mutate(date = df_wind$date[-1])

solar_sims <- simulate(model, nsim = 100) |> 
mutate(across(everything(), function(x) {
    x^2
})) |> mutate(date = df_solar$date[-1])

ggplot(solar_sims |> filter(year(date) == 2006), aes(x = date, y = sim_25)) +
    geom_line()

write_csv(
    x = solar_sims,
    file = "data/solar_sims.csv"
)

write_csv(
    x = wind_sims,
    file = "data/wind_sims.csv"
)
