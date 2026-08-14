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

# ladda in nödvändiga funktioner
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# Allmänna options - TRUE = visa inte R-felmeddelanden i appen, FALSE = visa felmeddelanden från R på webben
options(shiny.sanitize.errors = FALSE)
# shinyOptions(cache = cachem::cache_disk(
#   dir      = "C:/sti/till/en/cache-mapp",  # anpassa till  servermiljö
#   max_size = 500 * 1024^2                   # 500 MB, justera efter behov
# ))

#----Uppkoppling till databas----
con_rutt <- shiny_uppkoppling_las("ruttanalyser", db_user = "shiny_las_sekretess")

#----Hämta rutter------
alla_rutter <- sf::st_read(
  con_rutt,
  layer = DBI::Id(schema = "ruttanalys_cykel", table = "cykelpotential_vy"),
  quiet = TRUE
) %>%
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
  pull(knnamn)

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

