## Prepare data for shiny app

library(tidyverse)
## pfade fuer daten
data_pth = 'I:/Data_Science/BSV2025/Data/'

# Indexierungsfile laden
gvz_index <- read_csv2(paste0(data_pth, 'versicherungsindex_gvz.csv')) %>% 
  # take 'illegal' characters and replaces them with periods
  rename_all(make.names) %>% 
  mutate(jahr = as.integer(format(as.Date(Jahr,"%d.%m.%Y"), "%Y")),
         index = as.integer(Versicherungsindex.GVZ)) %>% 
  dplyr::select(c(5, 6))

# Schadendaten mit Brandopfern laden und filtern: Nur Feuerschäden zwischen 1981 und 2018
brand <- read_csv2(paste0(data_pth, 'gvz_schaeden_mit_brandopfern_201812.csv'),
                   locale(encoding = "iso-8859-1"), col_names = TRUE,
                   col_types = cols(
                     SchadenSumme = col_number(),
                     SchadenDatum = col_date(format = "%d.%m.%Y"))) %>% 
  filter(str_detect(SchadenArtBezeichnung, 'Feuer'),
         Ausdr3 > 1981 & Ausdr3 < 2018) %>% 
  rename(verletzte = SchadenVerletzte, todesopfer = SchadenTodesopfer) %>% 
  dplyr::select(c(2, 22:23))

# Schadendaten laden und filtern: Nur Feuerschäden zwischen 1982 und 2017
schad <- read_csv2(paste0(data_pth, 'Schaeden_20181015.csv'),
                   locale(encoding = "iso-8859-1"), col_names = TRUE,
                   col_types = cols(
                     SchadenSumme = col_number(),
                     FkoZaTot = col_number(),
                     SchadenDatum = col_date(format = "%d.%m.%Y"),
                     FkoVersDa = col_date(format = "%d.%m.%Y"))) %>% 
  filter(str_detect(SchadenArtBezeichnung, 'Feuer'),
         Ausdr3 > 1981 & Ausdr3 < 2018) %>% 
  ## schaeden indexieren
  left_join(gvz_index, by = c("Ausdr3" = "jahr")) %>% 
  mutate(schad_index = round(max(index) / index * SchadenSumme),
         schadensatz = ifelse(SbwVerWert > 0, SchadenSumme / SbwVerWert, 0),
         zweck = as.character(str_trim(gsub("\\,", "", gsub("\\d", "", GebaeudeZweckText)),
                                       "left")),
         zweckcode = as.integer(str_extract(GebaeudeZweckText, "[[:digit:]]+")),
         zweckcode_kat = as.integer(str_sub(zweckcode, 1, 2)),
         schadenursache = as.character(str_trim(gsub("\\d", "", CodTextDt), "left")),
         schadencode = as.integer(str_extract(CodTextDt, "[[:digit:]]+"))) %>% 
  left_join(brand, by = c('SchadenId' = 'SchadenId')) %>% 
  # Annahme: NA's in Todesopfer = 0
  mutate(todesopfer = ifelse(is.na(todesopfer), 0, todesopfer),
         verletzte = ifelse(is.na(verletzte), 0, verletzte)) %>% 
  dplyr::select(-c(3, 5, 6, 11, 14:17, 20:22, 24, 25))

## doppelten Schaden ID's nur einmal beruecksichtigen!

## welche schaden ID sind doppelt vorhanden?
schad_duplicates = schad %>%
  group_by(SchadenId) %>%
  filter(n() > 1) %>%
  ## duplikate nur einmal anzeigen
  filter(row_number() == 1)

## doppelte schaden ID Zeilen nur einmal
schad = schad %>%
  ## duplikate nur einmal
  distinct(SchadenId, .keep_all = TRUE)

## Aktuelles Portfolio laden und Versicherungssumme nach Zweck zusammenfassen
portfolio <- read_csv2(paste0(data_pth, 'gebaeudebestand_201801_georef.csv'),
                       locale(encoding = "iso-8859-1"), col_names = TRUE,
                       col_types = cols(
                         katasterNr = col_number(),
                         versSum = col_number())) %>% 
  dplyr::select(c('objektId', 'zweckcode', 'zweckBeschr', 'zweckKat', 'versSum', 'volumen')) %>% 
  mutate(zweckcode_kat2 = as.integer(zweckcode/100))

## Schadenfile mit Volumen ergaenzen
schad <- schad %>% left_join(dplyr::select(portfolio, c(objektId, volumen)),
                             by = c('GebaeudeId' = 'objektId')) %>% 
  # selektiere die darzustellenden Variablen
  dplyr::select(c(9, 10, 12:14, 16, 17, 19:22))

# export data in a csv
write_csv(schad, "data/schad_shiny.csv")
