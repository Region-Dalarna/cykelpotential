## Globala inställningar för Shinyappen: cykelpotential

# Ladda nödvändiga paket
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggiraph)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(leaflet)
library(geojsonsf)
library(jsonlite)

# ladda in nödvändiga funktioner
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen, FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)
# shinyOptions(cache = cachem::cache_disk(
#   dir      = "C:/sti/till/en/cache-mapp",  # anpassa till  servermiljö
#   max_size = 500 * 1024^2                   # 500 MB, justera efter behov
# ))


#---- Hjälpfunktion----
# Bygger ett stabilt, url-säkert id för en underflik-navset baserat på menynamnet
inre_flik_id <- function(meny) {
  paste0("underflik_", gsub("[^a-zA-Z0-9]+", "_", tolower(meny)))
}


#----Uppkoppling till databas----
con_rutt <- shiny_uppkoppling_las("ruttanalyser", db_user = "shiny_las_sekretess")

#----Hämta rutter------
alla_kolumner_i_vyn <- dbGetQuery(con_rutt, "
  SELECT a.attname AS column_name
  FROM pg_attribute a
  JOIN pg_class c ON a.attrelid = c.oid
  JOIN pg_namespace n ON c.relnamespace = n.oid
  WHERE n.nspname = 'ruttanalys_cykel'
    AND c.relname = 'cykelpotential_vy'
    AND a.attnum > 0
    AND NOT a.attisdropped;
")$column_name

pass_dold_kolumner <- alla_kolumner_i_vyn[grepl("^(pass_|dold_)", alla_kolumner_i_vyn)]

alla_rutter <- sf::st_read(
  con_rutt,
  query = paste0(
    "SELECT lank_id, gatunamn_namn, streetview_url, geom, ",
    paste(pass_dold_kolumner, collapse = ", "),
    " FROM ruttanalys_cykel.cykelpotential_vy;"
  ),
  quiet = TRUE
  ) %>%
  st_simplify(dTolerance = 5, preserveTopology = TRUE) %>%
  sf::st_transform(4326) %>%
  st_zm(drop = TRUE, what = "ZM")


# Skola – cykelklassat / NVDB (statistik)
skola_cykel_stat <- tbl(
  con_rutt,
  dbplyr::in_schema("ruttanalys_cykel", "rutter_skola_cykelklass")
) %>%
  dplyr::select(
    kommun,
    kommun_namn,
    bef,
    desokod,
    skolkommun_namn,
    cost_cykel_min,
    cost_elcykel_min,
    avstand_m,
    skolform
  ) %>%
  dplyr::collect()

skola_nvdb_stat <- tbl(
  con_rutt,
  dbplyr::in_schema("ruttanalys_cykel", "rutter_skola_nvdb")
) %>%
  dplyr::select(
    kommun,
    kommun_namn,
    bef,
    desokod,
    skolkommun_namn,
    cost_cykel_min,
    cost_elcykel_min,
    avstand_m,
    skolform
  ) %>%
  dplyr::collect()


# Arbete – cykelklassat / NVDB
arbete_cykel_stat <- tbl(
  con_rutt,
  dbplyr::in_schema("ruttanalys_cykel", "rutter_arbete_cykelklass")
) %>%
  dplyr::select(
    kommun,
    kommun_namn,
    bef,
    desokod,
    astkommun_namn,
    cost_cykel_min,
    cost_elcykel_min,
    avstand_m
  ) %>%
  dplyr::collect()

arbete_nvdb_stat <- tbl(
  con_rutt,
  dbplyr::in_schema("ruttanalys_cykel", "rutter_arbete_nvdb")
) %>%
  dplyr::select(
    kommun,
    kommun_namn,
    bef,
    desokod,
    astkommun_namn,
    cost_cykel_min,
    cost_elcykel_min,
    avstand_m
  ) %>%
  dplyr::collect()

#----Hämta geografiska gränser----
con <- shiny_uppkoppling_las("geodata")

# Kommuner
kommuner <- tbl(
  con,
  dbplyr::in_schema("karta", "kommun_scb")
  ) %>%
  filter(lanskod_tx == "20") %>%
  dplyr::pull(knnamn)

# Kommungränser
kommungranser <- tbl(
  con,
  dbplyr::in_schema("karta", "kommun_scb")
) %>%
  filter(lanskod_tx == "20") %>%
  collect() %>%
  df_till_sf() %>%
  st_transform(4326) %>%
  st_zm(drop = TRUE, what = "ZM")

# Lantmäteriets kommungränser
kommungranser_lm <- tbl(
  con,
  dbplyr::in_schema("karta", "kommun_lm")
) %>%
  filter(lankod == "20") %>%
  collect() %>%
  df_till_sf(geom_col = "geom") %>%
  st_transform(4326) %>%
  st_zm(drop = TRUE, what = "ZM")

# DeSO
deso_niva <- tbl(
  con,
  dbplyr::in_schema("karta", "deso")
) %>%
  filter(lanskod == "20") %>%
  collect() %>%
  df_till_sf() %>%
  st_transform(4326) %>%
  st_zm(drop = TRUE, what = "ZM")

# Länsgräns
lansgrans <- tbl(
  con,
  dbplyr::in_schema("karta", "lan_lm")
) %>%
  filter(lankod == "20") %>%
  collect() %>%
  df_till_sf(geom_col = "geom") %>%
  st_transform(4326) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_exterior_ring()

DBI::dbDisconnect(con)

