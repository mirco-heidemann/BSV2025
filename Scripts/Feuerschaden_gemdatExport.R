## Data Manipulation mit dplyr and stringr
## Brandschaden von 2018 aus Gemdat für Vasco
## GemDat Schaeden seit 1981

## Mirco Heidemann, Januar 2019

# ## relative pfade spezifizieren
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

## relative pfade
data_pth = './Data/'

## Datenimport
dat <- read_csv2(paste0(data_pth, 'Schaden_20190110.csv'),
                 locale(encoding = "iso-8859-1"), col_names = TRUE,
                 col_types = cols_only(
                   'GebaeudeId' = col_integer(),
                   'SchadenId' = col_integer(),
                   'SchadenDatum' = "?",
                   'SchadenSumme' = col_number(),
                   'SbwVerWert' = col_number(),
                   'GebaeudeBaujahr' = col_integer(),
                   'SchadenArtBezeichnung' = col_character(),
                   'CodTextDt' = col_character(),
                   'GebaeudeZweckText' = col_character(),
                   'Ausdr3' = col_integer(),
                   'GebaeudeStrasse' = col_character(),
                   'GebaeudeHausNr' = "?",
                   'GebaeudePlz' = col_integer(),
                   'GebaeudeOrt'= col_character()
                   )) %>% 
  rename(geb_Id = GebaeudeId,
         schad_id = SchadenId,
         schad_datum = SchadenDatum,
         schad_sum = SchadenSumme,
         versicherungs_sum = SbwVerWert,
         baujahr = GebaeudeBaujahr,
         schad_art = SchadenArtBezeichnung,
         schad_CodText = CodTextDt,
         zweck = GebaeudeZweckText,
         schad_jahr = Ausdr3,
         strasse = GebaeudeStrasse,
         haus_nr = GebaeudeHausNr,
         plz = GebaeudePlz,
         ort = GebaeudeOrt)

df <- dat %>% 
  mutate(zweck_code = as.integer(str_extract(zweck, "[[:digit:]]+")),
         zwk_code_text = as.character(str_trim(gsub("\\,", "",
                                                    gsub("\\d", "", zweck)), "left")),
         schad_code = as.integer(str_extract(schad_CodText, "[[:digit:]]+")),
         schad_ursache = as.character(str_trim(gsub("\\d", "", schad_CodText), "left"))) %>% 
  # doppelte schaden ID Zeilen nur einmal
  distinct(schad_id, .keep_all = TRUE) %>% 
  # nur Feuerschaden über Null von 2018
  filter(str_detect(schad_art, "Feuer"),
         schad_sum > 0,
         schad_jahr > 2017 & schad_jahr < 2019) %>% 
  select(-c(zweck, schad_CodText, schad_art, schad_jahr))

## export data in a csv
# write.csv(df, "data/brandschaden_gemdat_2018.csv", row.names = F)

## Import Portfolio for georef
portfolio <- read_csv2(paste0(data_pth, 'gebaeudebestand_201801_georef.csv'),
                       locale(encoding = "iso-8859-1"), col_names = TRUE,
                       col_types = cols_only(
                         'objektId' = col_integer(),
                         # 'zweckcode' = col_integer(),
                         # 'zweckBeschr' = col_character(),
                         # 'zweckKat' = col_integer(),
                         # 'versSum' = col_number(),
                         # 'volumen' = col_number(),
                         # 'gebBaujahr' = col_integer(),
                         'geoxLV95' = col_character(),
                         'geoyLV95' = col_character()
                         ))

df_georef <- df %>% 
  left_join(portfolio, c('geb_Id' = 'objektId')) %>% 
  filter(!is.na(geoxLV95)) %>% 
  mutate(x = as.numeric(geoxLV95),
         y = as.numeric(geoyLV95))

## export data in a csv
write.csv(df_georef, "data/brandschaden_gemdat_2018_georef.csv", row.names = F)
#write_csv(df_georef, "data/brandschaden_gemdat_2018_georef.csv")
