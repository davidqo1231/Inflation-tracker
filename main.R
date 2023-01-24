library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# WRANGLING
Zoznam <- read.csv("https://raw.githubusercontent.com/davidqo1231/Inflation-tracker/main/cpiweights.csv", sep = ";") %>%
  mutate(Vaha = Vaha/10)

#Posledný mesiac v aktuálnom roku
last_month = as.Date("2022-12-01")

df_yoy <- read.csv("https://data.statistics.sk/api/v2/dataset/sp0029ms/all/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12./all/mj_romr?lang=sk&type=csv", sep = ";", header = FALSE) %>%
  slice(7:nrow(.)) %>%
  rename(Rok = 1, Mesiac = 2, Kod = 3, Jednotka = 4, YoY = 5) %>%
  mutate(Mesiac = as.numeric(Mesiac),
         YoY = as.numeric(YoY)) %>%
  arrange(Rok, Mesiac) %>%
  group_by(Kod) %>%
  mutate(Datum = as.Date(seq(ymd("2018-01-01"), ymd(last_month), by = "months"))) %>%
  ungroup() %>%
  mutate(YoY = YoY-100) %>%
  select(Datum, Kod, YoY) %>%
  drop_na()

df_mom <- read.csv("https://data.statistics.sk/api/v2/dataset/sp0029ms/all/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12./all/mj_predch_obdobie?lang=sk&type=csv", sep = ";", header = FALSE) %>%
  slice(7:nrow(.)) %>%
  rename(Rok = 1, Mesiac = 2, Kod = 3, Jednotka = 4, MoM = 5) %>%
  mutate(Mesiac = as.numeric(Mesiac),
         MoM = as.numeric(MoM)) %>%
  arrange(Rok, Mesiac) %>%
  group_by(Kod) %>%
  mutate(Datum = as.Date(seq(ymd("2018-01-01"), ymd(last_month), by = "months"))) %>%
  ungroup() %>%
  mutate(MoM = MoM-100) %>%
  select(Datum, Kod, MoM) %>%
  drop_na()

last_obs <- df_yoy$Datum[nrow(df_yoy)] 

df_last <- df_yoy %>%
  left_join(df_mom, by = c("Kod", "Datum")) %>%
  filter(Datum == last_obs) %>%
  mutate(YoY = round(YoY, digits = 1),
         MoM = round(MoM, digits = 1))


df_table <- df_yoy %>%
  left_join(df_mom, by = c("Kod", "Datum")) %>%
  # Zoberie len posledných 12 mesiacov od posledného pozorovania
  filter(Datum > last_obs %m-% months(12)) %>%
  mutate(YoY = round(YoY, digits = 1),
         MoM = round(MoM, digits = 1)) %>%
  group_by(Kod) %>%
  summarize(YoY_data = list(YoY),
            MoM_data = list(MoM), .groups = "drop")


df_final <- Zoznam %>%
  left_join(df_last, by = "Kod") %>%
  left_join(df_table, by = "Kod")

rm("df_last", "df_mom", "df_table", "df_yoy", "Zoznam", "last_month", "last_obs")


#
df <- df_final %>%
  filter(YoY > -20) %>%
  arrange(YoY)

df$right <- cumsum(df$Vaha) + 0*c(0:(nrow(df)-1))
df$left <- df$right - df$Vaha + 0.1

x.axis.labels <- seq(10, 100, 10) # positions of the subtle ticks
y.axis.labels <- seq(0,50,10)

save(df_final, file = "dataframe.Rda")

