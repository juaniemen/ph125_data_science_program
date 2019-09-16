library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
head(temp_carbon)
temp_carbon %>% filter(year == 2018)
                       

dat <- temp_carbon %>% filter(!is.na(temp_anomaly))
p <- dat %>% ggplot(aes(year, temp_anomaly)) + geom_point() + geom_hline(aes(yintercept = 0), col= "blue")
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") + 
  geom_line(aes(year, ocean_anomaly), col="lightblue", alpha= .8) +
  geom_line(aes(year, land_anomaly), col="brown", alpha= .5) +
  geom_vline(xintercept = 2018) + 
  geom_vline(xintercept = 1880) 


# ----------------------- Greenhouse gases
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# ------------------
dat <- temp_carbon %>% filter(!is.na(carbon_emissions))
ggplot(dat, aes(year, carbon_emissions)) + geom_line() + geom_vline(xintercept = 1850) +
  geom_vline(xintercept = 1970) + xlim(1950,2014)


# ---------------- Historic Co2

historic_co2 %>%  ggplot(aes(year,co2)) + geom_line(aes(col=source)) + xlim(-800000,-775000)
historic_co2 %>%  ggplot(aes(year,co2)) + geom_line(aes(col=source)) + xlim(-375000,-330000)
historic_co2 %>%  ggplot(aes(year,co2)) + geom_line(aes(col=source)) + xlim(-140000,-120000)
historic_co2 %>%  ggplot(aes(year,co2)) + geom_line(aes(col=source)) + xlim(-3000,2018)

