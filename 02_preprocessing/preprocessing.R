library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(rvest)
library(sf)
library(forecast)

save_data <- FALSE

# Namen der Bezirke und Katastralgemeiden -------------------------------------------------------------------------
district_area_mapping <-
  read_html("https://de.wikipedia.org/wiki/Wiener_Katastralgemeinden") %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table') %>%
  html_table() %>%
  clean_names()


# Einlesen und cleanen der Transaktionsdaten -------------------------------------------------------------------------
data <- read_csv2(
  here("01_data", "kaufpreissammlung_liegenschaften.csv"),
  locale = readr::locale(encoding = "latin1")
) %>%  clean_names() %>% 
  left_join(select(district_area_mapping, katastral_gemeinde, bezirk),
            by = c("katastralgemeinde" = "katastral_gemeinde")) %>% 
  mutate(erwerbsdatum = dmy(erwerbsdatum),
         jahr = year(erwerbsdatum),
         jahr = if_else(jahr == 2106, 2016, jahr),
         plz = as.factor(plz),
         ez = as.factor(ez) ,
         bezirk = str_extract(bezirk, "\\d{1,2}\\.\\s.+?(?=\\d)"),
         verauserer = case_when(verausserer_code %in% c(1, 2, 4, 5, 6, 7, 10, 11, 12, 14) ~ "Gebietskörperschaften, jur. Personen mit öffentlichem Charakter",
                                verausserer_code == 3 ~ "Gemeinnützige Bauvereinigungen",
                                verausserer_code == 8 ~ "Jur. Personen des Privatrechts",
                                verausserer_code == 9 ~ "Privatpersonen",
                                TRUE ~ "Sonstige"
         ),
         erwerber  = case_when(erwerbercode %in% c(1, 2, 4, 5, 6, 7, 10, 11, 12, 14) ~ "Gebietskörperschaften, jur. Personen mit öffentlichem Charakter",
                               erwerbercode == 3 ~ "Gemeinnützige Bauvereinigungen",
                               erwerbercode == 8 ~ "Jur. Personen des Privatrechts",
                               erwerbercode == 9 ~ "Privatpersonen",
                               TRUE ~ "Sonstige"
         ),
         rel_kaufpreis = kaufpreis / gst_fl
  ) %>% 
  filter(str_detect(plz, "\\d{4}"), between(jahr, 1987, 2019), !is.na(bezirk)) 


# Shape-Daten der Wiener Gemeindebezirke mit Namen ------------------------
map_data <-
  st_read(here("01_data", "ZAEHLBEZIRKOGDPolygon.shp"), options = "ENCODING=WINDOWS-1252") %>%
  clean_names() %>%
  group_by(bez) %>%
  summarize(geometry = st_union(geometry),
            flaeche = sum(flaeche)) %>%
  left_join(
    data %>% distinct(bezirk) %>%
      mutate(bez_num = str_extract(bezirk, "\\d{1,2}") %>% str_pad(2, side = "left", 0)),
    by = c("bez" = "bez_num")
  ) %>% 
  mutate(
    bez_num = as.numeric(bez)
  ) %>% 
  select(-flaeche) %>% 
  rename(Bezirk = bezirk)


# Aggregation der Transaktions-Daten nach Erwerber, Bezirk und Jahr -------
ts_data <- data %>% 
  filter(erwerber %in% c("Jur. Personen des Privatrechts", "Privatpersonen")) %>% 
  group_by(erwerber, jahr, bezirk) %>% 
  summarize(
    `Median-Preis (gesamt, €)` = round(median(kaufpreis, na.rm = TRUE), 2),
    `Median-Preis (relativ, €)` = round(median(rel_kaufpreis, na.rm = TRUE), 2),
    `Anzahl Transaktionen`= n(),
    `Gesamt-Volumen (€)` = sum(kaufpreis, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  rename(
    Jahr = jahr,
    Erwerber = erwerber,
    Bezirk = bezirk
  ) %>% 
  mutate(bez_num = str_extract(Bezirk, "\\d{1,2}"),
         Erwerber = if_else(Erwerber == "Privatpersonen", Erwerber, "Unternehmen")) %>% 
  ungroup()


# Forecast-Daten ----------------------------------------------------------
generate_forecasts <- function(data, district) {
  
  district_data <-
    data %>%
    filter(bez_num == district, Erwerber == "Unternehmen")  
  
  district <-unique(district_data$Bezirk)
  district_ts <- ts(district_data$`Anzahl Transaktionen`, start = 1987)
  
  fc_1 <- forecast(auto.arima(district_ts), h = 10)
  fc_2 <- forecast(ets(district_ts), h = 10)
  fc_3 <- forecast(auto.arima(district_ts, d = 1), h = 10)
  fc_4 <- forecast(auto.arima(district_ts, d = 2), h = 10)
  
  map(list(fc_1, fc_2, fc_3, fc_4),
      ~ as_tibble(fortify(.x))) %>%
    bind_rows(.id = "model") %>%
    rename(
      "Low95" = "Lo 95",
      "Jahr" = "Index",
      "Daten" = "Data",
      "Low80" = "Lo 80",
      "High95" = "Hi 95",
      "High80" = "Hi 80",
      "Vorhersage" = "Point Forecast"
    ) %>%
    mutate(
      Modell = case_when(
        model == 1 ~ "ARIMA (0,1,0)",
        model == 2 ~ "ETS",
        model == 3 ~ "ARIMA (0,1,0) mit Drift",
        model == 4 ~ "ARIMA (0, 1, 2) mit Drift"
      ),
      Bezirk = district
    )
}

fc_data <- map(c(10, 11, 21, 22, 23), ~ generate_forecasts(ts_data, .x)) %>% bind_rows() %>% select(-model)


# Daten speichern ---------------------------------------------------------
if(save_data) {

  data_list <- list(fc_data,
                    ts_data,
                    data,
                    district_area_mapping)
  
  path_list <- list(here("01_data", "processed", "fc_data.csv"),
                    here("01_data", "processed", "metrics_by_buyer_year_district.csv"),
                    here("01_data", "processed", "data_preprocessed.csv"),
                    here("01_data", "processed", "district_area_mapping.csv"))
  
  walk2(data_list, path_list, ~ write_csv(.x, .y))
  st_write(map_data, here("01_data", "processed", "districts.shp"))

}
