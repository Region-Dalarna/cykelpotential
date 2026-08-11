# =============================================================
# server.R  –  Cykelpotential Dalarna
# =============================================================
# Alla andelar beräknas nu konsekvent som:
#   sum(bef[nåbara inom gräns]) / sum(bef)
# inom respektive grupp (kommun eller DeSO) i cykeldatat.
# pend_data/totals används inte längre som nämnare.
# =============================================================

# ------------------------------------------------------------------
# Konfigurationstabell (matchar panel_konfig i ui.R)
# ------------------------------------------------------------------
tabb_konfig <- tibble::tribble(
  ~tabb_id,               ~typ,                ~avstand_eller_tid,      ~enhet,
  "skola_cykel_grund",     "grundskolan",       c("2000","3000","5000"), "km",
  "skola_cykel_grund_all", "grundskolan_all",   c("2000","3000","5000"), "km",
  "skola_cykel_gym",       "gymnasiet",         c("15","30","45"),       "min",
  "skola_cykel_gym_all",   "gymnasiet_all",     c("15","30","45"),       "min",
  "arbete_cykel",          "arbete",            c("15","30","45"),       "min",
  "arbete_cykel_all",      "arbete_all",        c("15","30","45"),       "min",
  "arbete_elcykel",        "arbete_elcykel",    c("15","30","45"),       "min",
  "arbete_elcykel_all",    "arbete_elcykel_all",c("15","30","45"),       "min"
)

# ------------------------------------------------------------------
# Gränsvärden och etiketter (single source of truth)
# ------------------------------------------------------------------
granser_km    <- c(2000, 3000, 5000)
etiketter_km  <- c("≤2 km", "≤3 km", "≤5 km")

granser_min   <- c(15, 30, 45)
etiketter_min <- c("≤15 min", "≤30 min", "≤45 min")

bar_farger    <- c("#178571", "#93cec1", "#c5e9e2")
karta_palett  <- c("#b6f0fd", "#8edded", "#158daf", "#00577b")


# ------------------------------------------------------------------
# Girafe-stilar
# ------------------------------------------------------------------
girafe_hover <- "stroke-width: 1.5px; cursor: pointer !important;"

girafe_tooltip <- "
  background-color: white !important;
  color: black !important;
  border-radius: 8px;
  font-family: 'Poppins', sans-serif;
  box-shadow: none !important;
  font-size: 14px;
  padding: 6px 10px;"

girafe_opts <- function(zoom_max = 5) {
  list(
    opts_hover(css   = girafe_hover),
    opts_tooltip(css = girafe_tooltip, opacity = 1),
    opts_zoom(min = 1, max = zoom_max),
    opts_toolbar(saveaspng = FALSE)
  )
}


# ------------------------------------------------------------------
# Hjälpfunktion: filtrera cykeldata på skolform och kommuner
# ------------------------------------------------------------------
filtrera_cykeldata <- function(cykel_data, skolform = NULL, kommuner) {
  df <- cykel_data
  if (!is.null(skolform) && "skolform" %in% names(df)) {
    df <- df %>% dplyr::filter(skolform == !!skolform)
  }
  df %>% dplyr::filter(kommun_namn %in% kommuner)
}


# ------------------------------------------------------------------
# Hjälpfunktion: beräkna andelar mot sum(bef)
#
# Nämnaren är sum(bef) för HELA gruppen, inklusive rader där
# varde_kol är NA (dvs. ej nåbara). Täljaren är sum(bef) för de
# rader som är nåbara inom respektive gräns. Andelen blir därmed
# "andel av alla i gruppen som når målet inom gränsen".
#
# Returnerar en tibble med grupp_kol, totalt samt en kolumn
# andel_<gräns> (0–1) per gränsvärde.
# ------------------------------------------------------------------
berakna_andelar <- function(data, varde_kol, granser, grupp_kol = "kommun_namn") {
  data %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grupp_kol))) %>%
    dplyr::group_modify(function(d, ...) {
      varde  <- d[[varde_kol]]
      totalt <- sum(d$bef, na.rm = TRUE)

      andelar <- vapply(granser, function(g) {
        if (totalt == 0) return(NA_real_)   # skydd mot division med noll
        sum(d$bef[!is.na(varde) & varde <= g], na.rm = TRUE) / totalt
      }, numeric(1))

      tibble::as_tibble_row(
        c(setNames(as.list(andelar), paste0("andel_", granser)),
          list(totalt = totalt))
      )
    }) %>%
    dplyr::ungroup()
}

# ------------------------------------------------------------------
# Länssnitt: samma beräkningslogik som berakna_andelar(), men
# grupperat över HELA datasetet (alla kommuner) istället för per kommun.
# ------------------------------------------------------------------
berakna_lanssnitt <- function(cykel_data, varde_kol, granser, skolform = NULL) {
  df <- cykel_data
  if (!is.null(skolform) && "skolform" %in% names(df)) {
    df <- df %>% dplyr::filter(skolform == !!skolform)
  }
  df %>%
    dplyr::mutate(alla = "Länet") %>%
    berakna_andelar(varde_kol, granser, grupp_kol = "alla")
}

# ------------------------------------------------------------------
# Plockar ut länssnittet för sista (största) tröskeln, som procent
# avrundat till heltal. Används på startsidan för headline-siffror.
# ------------------------------------------------------------------
startsida_stat <- function(cykel_data, varde_kol, granser, grans, skolform = NULL) {
  lans      <- berakna_lanssnitt(cykel_data, varde_kol, granser, skolform)
  huvud_kol <- paste0("andel_", grans)
  round(lans[[huvud_kol]] * 100, 0)
}
# ------------------------------------------------------------------
# Generisk barplot
# ------------------------------------------------------------------
make_cykel_barplot <- function(cykel_data, varde_kol, granser, etiketter,
                               legend_titel, skolform = NULL, kommuner,
                               zoom_max = 5) {
  req(cykel_data)

  df <- filtrera_cykeldata(cykel_data, skolform, kommuner)

  df_summary <- berakna_andelar(df, varde_kol, granser) %>%
    tidyr::pivot_longer(dplyr::starts_with("andel_"),
                        names_to = "grans", values_to = "andel") %>%
    dplyr::mutate(
      grans = factor(grans,
                     levels = paste0("andel_", granser),
                     labels = etiketter)
    )

  farger <- setNames(bar_farger[seq_along(etiketter)], etiketter)

  p <- ggplot(df_summary, aes(x = kommun_namn, y = andel, fill = grans)) +
    geom_col_interactive(
      aes(tooltip = paste0(
        "<b>Kommun:</b> ", kommun_namn, "<br>",
        "<b>", legend_titel, ":</b> ", grans, "<br>",
        "<b>Andel:</b> ", scales::percent(andel, accuracy = 0.1)
      )),
      position = position_dodge()
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                       limits = c(0, 1)) +
    scale_fill_manual(values = farger) +
    labs(x = "Kommun", y = "Andel", fill = legend_titel) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  girafe(ggobj = p, options = girafe_opts(zoom_max = zoom_max))
}


# ------------------------------------------------------------------
# Generisk kommunkarta (ersätter make_kommunkarta_avstand + _tid)
# Färgsätter på den sista (största) gränsen, popup visar alla.
# ------------------------------------------------------------------
make_kommunkarta <- function(cykel_data, varde_kol, granser, etiketter,
                             legend_titel, skolform = NULL, kommuner) {
  req(cykel_data)

  df <- filtrera_cykeldata(cykel_data, skolform, kommuner)

  df_summary <- berakna_andelar(df, varde_kol, granser) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("andel_"),
                                ~ round(.x * 100, 1)))

  karta_join <- dplyr::left_join(kommungranser, df_summary,
                                 by = c("knnamn" = "kommun_namn"))

  huvud_kol <- paste0("andel_", granser[length(granser)])
  pal <- leaflet::colorNumeric(palette  = karta_palett,
                               domain   = karta_join[[huvud_kol]],
                               na.color = "grey80")

  # Popup: en rad per gräns
  andel_matris <- sf::st_drop_geometry(karta_join)[paste0("andel_", granser)]
  popup_text <- paste0(
    "<b>Kommun:</b> ", karta_join$knnamn, "<br>",
    apply(andel_matris, 1, function(r) {
      paste0("<b>Andel ", etiketter, ":</b> ", r, "%", collapse = "<br>")
    })
  )

  leaflet::leaflet(karta_join) %>%
    leaflet::addTiles(urlTemplate = "", options = leaflet::tileOptions(noWrap = TRUE)) %>%
    leaflet::addPolygons(
      group       = "kommuner",
      layerId     = ~knnamn,
      fillColor   = ~pal(get(huvud_kol)),
      fillOpacity = 0.7,
      weight      = 0.1,
      # popup       = popup_text,
      label       = lapply(popup_text, htmltools::HTML),   # <-- NY: hover istället för/utöver popup
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        textsize  = "13px",
        opacity   = 0.95,
        style     = list(
          "font-family" = "inherit",
          "padding"     = "6px 10px"
        )
      ),
      options     = leaflet::pathOptions(bubblingMouseEvents = FALSE)
    ) %>%
    leaflet::addLegend(
      pal = pal, values = karta_join[[huvud_kol]],
      title = legend_titel, position = "bottomright"
    )
}


# ------------------------------------------------------------------
# DeSO drill-down – återanvändbar bind-funktion
# Använder samma berakna_andelar som övriga vyer.
# ------------------------------------------------------------------
bind_kommunkarta_deso <- function(map_id, cykel_data, deso_niva, input, session,
                                  varde_kol  = "avstand_m",
                                  granser    = granser_km,
                                  etiketter  = etiketter_km,
                                  skolform   = NULL,
                                  reset_bbox = c(12.2, 60.0, 16.5, 62.3)) {

  observeEvent(input[[paste0(map_id, "_shape_click")]], {
    klick <- input[[paste0(map_id, "_shape_click")]]
    req(klick$id)

    deso_vald <- deso_niva %>%
      dplyr::filter(kommunnamn == klick$id, !sf::st_is_empty(sf::st_geometry(.)))
    req(nrow(deso_vald) > 0)

    bbox <- sf::st_bbox(deso_vald)

    data_filtrerad <- cykel_data %>% dplyr::filter(kommun_namn == klick$id)
    if (!is.null(skolform) && "skolform" %in% names(data_filtrerad)) {
      data_filtrerad <- data_filtrerad %>% dplyr::filter(skolform == !!skolform)
    }

    deso_stat <- data_filtrerad %>%
      berakna_andelar(varde_kol, granser, grupp_kol = "desokod") %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("andel_"),
                                  ~ round(.x * 100, 1)))

    huvud_kol <- paste0("andel_", granser[length(granser)])

    deso_join <- dplyr::left_join(deso_vald, deso_stat, by = "desokod")
    pal_deso  <- leaflet::colorNumeric(
      palette  = karta_palett,
      domain   = deso_join[[huvud_kol]],
      na.color = "grey80"
    )

    andel_matris <- sf::st_drop_geometry(deso_join)[paste0("andel_", granser)]
    popup_text <- paste0(
      "<b>Kommun:</b> ", deso_join$kommunnamn, "<br>",
      "<b>DeSO:</b> ", deso_join$desokod, "<br>",
      apply(andel_matris, 1, function(r) {
        paste0("<b>Andel ", etiketter, ":</b> ", r, "%", collapse = "<br>")
      })
    )

    leaflet::leafletProxy(map_id) %>%
      leaflet::clearGroup("deso") %>%
      leaflet::hideGroup("kommuner") %>%
      leaflet::clearControls() %>%
      leaflet::fitBounds(unname(bbox["xmin"]), unname(bbox["ymin"]), unname(bbox["xmax"]), unname(bbox["ymax"])) %>%
      leaflet::addPolygons(
        data        = deso_join,
        group       = "deso",
        layerId     = ~desokod,
        fillColor   = ~pal_deso(get(huvud_kol)),
        fillOpacity = 0.7,
        weight      = 1,
        color       = "white",
        # popup       = popup_text,
        label       = lapply(popup_text, htmltools::HTML),   # <-- NY: hover istället för/utöver popup
        labelOptions = leaflet::labelOptions(
          direction = "auto",
          textsize  = "13px",
          opacity   = 0.95,
          style     = list(
            "font-family" = "inherit",
            "padding"     = "6px 10px"
          )
        ),
        options     = leaflet::pathOptions(bubblingMouseEvents = FALSE)
      ) %>%
      leaflet::addLegend(
        pal = pal_deso, values = deso_join[[huvud_kol]],
        title = klick$id, position = "bottomright"
      )
  }, ignoreInit = TRUE)

  observeEvent(input[[paste0(map_id, "_click")]], {
    leaflet::leafletProxy(map_id) %>%
      leaflet::clearGroup("deso") %>%
      leaflet::showGroup("kommuner") %>%
      leaflet::clearControls() %>%
      leaflet::fitBounds(reset_bbox[1], reset_bbox[2], reset_bbox[3], reset_bbox[4])
  }, ignoreInit = TRUE)
}


# =============================================================
# SERVER
# =============================================================
shinyServer(function(input, output, session) {

  thematic::thematic_shiny()

  # (Visa/dölj av delade kartan sköts av conditionalPanel i ui.R –
  #  shinyjs-observern är borttagen.)

  # ---- Konfiguration för aktiv flik ----
  aktuell_konfig <- reactive({
    tabb_konfig %>% filter(tabb_id == input$nav)
  })

  aktuellt_val <- reactive({
    req(input$rutter_delad)
    input$rutter_delad
  })

  # ---- Kartdata för delad karta ----
  karta_data <- reactive({
    req(input$nav, aktuell_konfig(), aktuellt_val())

    konfig <- aktuell_konfig()
    req(nrow(konfig) > 0)

    typ <- konfig$typ[1]
    val <- aktuellt_val()

    prefix <- if (grepl("elcykel", typ)) {
      "pass_elcykel"
    } else {
      "pass_cykel"
    }

    suffix <- if (grepl("_all$", input$nav)) {
      "nvdb"
    } else {
      "cykelklass"
    }

    typ_bas <- sub("_all$", "", typ)
    typ_bas <- sub("^arbete_elcykel$", "arbete", typ_bas)

    pass_kol <- paste0(
      prefix,
      "_",
      val,
      "_",
      typ_bas,
      "_",
      suffix
    )
    message(
      "=== karta_data === nav:", input$nav,
      " typ:", typ,
      " val:", val,
      " kol:", pass_kol,
      " finns:", pass_kol %in% names(alla_rutter)
    )

    if (!(pass_kol %in% names(alla_rutter))) {
      print(sort(names(alla_rutter)))
    }


    req(pass_kol %in% names(alla_rutter))

    df   <- alla_rutter %>% filter(!is.na(.data[[pass_kol]]), .data[[pass_kol]] > 0)
    vals <- as.numeric(df[[pass_kol]])

    pal <- colorNumeric(
      palette = viridis::viridis(10, option = "plasma", direction = -1),
      domain  = vals
    )

    df$color_val <- pal(vals)
    df$pass_val  <- vals
    df$pal       <- list(pal)
    # df$popup     <- paste0(
    #   "<b>Gatunamn:</b> ", df$gatunamn_namn, "<br>",
    #   "<b>Potentiella passager:</b> ",
    #   ifelse(vals < 10, "<10", as.character(vals)), "<br>",
    #   "<b>Streetview:</b> <a href='", df$streetview_url, "' target='_blank'>Google Streetview</a>"
    # )
    df
  }) %>% bindCache(input$nav, aktuellt_val())

  # ---- Maximera karta ----
  observeEvent(input$expand_karta, {
    expanded <- input$expand_karta %% 2 == 1
    session$sendCustomMessage("toggle-expand", list(map = "delad_karta", expand = expanded))
    updateActionButton(session, "expand_karta",
                       label = if (expanded) "🗗 Minimera karta" else "⤢ Maximera karta")
  })

  # ---- Delad karta: basrender ----
  output$delad_karta <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 14.0, lat = 61.5, zoom = 7) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        var settleTimer = null;

        function scheduleHide() {
          clearTimeout(settleTimer);
          settleTimer = setTimeout(function() {
            requestAnimationFrame(function() {
              requestAnimationFrame(function() {
                doljSpinnerMedGolvtid();
              });
            });
          }, 50);
        }

        map.on('layeradd', scheduleHide);
        map.on('moveend', scheduleHide);
      }
    ") %>%
      addPolygons(
        data = lansgrans,
        fillColor = "transparent",
        fillOpacity = 0,
        weight = 1,
        color = "black"
      ) %>%
      addPolygons(
        data        = kommungranser_lm,
        group       = "kommuner",
        layerId     = ~kommunnamn,
        fillColor   = "transparent",
        fillOpacity = 0,
        weight      = 1,
        color       = "black",
        options     = pathOptions(interactive = FALSE)
      ) %>%
      hideGroup("kommuner")
  })

  # ---- Delad karta: uppdatera vid flikbyte / val ----
  observe({
    req(karta_data())
    df   <- karta_data()
    session$sendCustomMessage("show-spinner", list(n = nrow(df)))
    pal  <- df$pal[[1]]
    bbox <- try(sf::st_bbox(df), silent = TRUE)

    tiles <- if (isTRUE(input$basemap_delad == "dark")) {
      providers$CartoDB.DarkMatter
    } else {
      providers$CartoDB.Positron
    }

    proxy <- leafletProxy("delad_karta") %>%
      clearTiles() %>%
      addProviderTiles(tiles) %>%
      clearGroup("vagnat") %>%
      clearControls() %>%
      clearPopups() %>%
      addPolylines(
        data    = df,
        group   = "vagnat",
        layerId = ~lank_id,
        color   = ~color_val,
        weight  = 3,
        opacity = 1,
        # popup   = ~popup
      ) %>%
      addLegend(pal = pal, values = df$pass_val,
                position = "bottomright", title = "Antal passager")

    if (!inherits(bbox, "try-error") && !is.null(bbox) && all(is.finite(bbox))) {
      proxy %>% fitBounds(unname(bbox["xmin"]), unname(bbox["ymin"]), unname(bbox["xmax"]), unname(bbox["ymax"]))
    }
  })

  # Bygg popup endast för de vägsegment man klickar på.
  observeEvent(input$delad_karta_shape_click, {
    klick <- input$delad_karta_shape_click
    req(klick$id)

    df  <- karta_data()
    rad <- df[df$lank_id == klick$id, ]
    req(nrow(rad) > 0)

    popup_text <- paste0(
      "<b>Gatunamn:</b> ", rad$gatunamn_namn, "<br>",
      "<b>Potentiella passager:</b> ",
      ifelse(rad$pass_val < 10, "<10", as.character(rad$pass_val)), "<br>",
      "<b>Streetview:</b> <a href='", rad$streetview_url, "' target='_blank'>Google Streetview</a>"
    )

    leaflet::leafletProxy("delad_karta") %>%
      leaflet::addPopups(lng = klick$lng, lat = klick$lat, popup = popup_text)
  })

  # ---- Delad karta: visa/dölj kommungränser ----
  observeEvent(input$visa_kommungranser, {
    proxy <- leafletProxy("delad_karta")
    if (isTRUE(input$visa_kommungranser)) {
      proxy %>% showGroup("kommuner")
    } else {
      proxy %>% hideGroup("kommuner")
    }
  }, ignoreInit = TRUE)

  # ---- Kontrollpanel: radiobuttons per flik ----
  output$map_controls_ui <- renderUI({
    req(input$nav)

    konfig <- aktuell_konfig()
    req(nrow(konfig) > 0)   # stoppar på start-/om-fliken

    enhet <- konfig$enhet[1]
    req(!is.na(enhet))

    val_choices <- if (enhet == "km") {
      c("2 km" = "2000", "3 km" = "3000", "5 km" = "5000")
    } else {
      c("15 minuter" = "15", "30 minuter" = "30", "45 minuter" = "45")
    }

    tagList(
      radioButtons("basemap_delad", "Kartbakgrund",
                   choices = c("Ljus" = "light", "Mörk" = "dark"),
                   selected = "light"),
      shiny::tags$hr(style = "margin: 8px 0;"),
      radioButtons("rutter_delad", "Visa rutter",
                   choices = val_choices, selected = val_choices[1]),
      shiny::tags$hr(style = "margin: 8px 0;"),
      checkboxInput("visa_kommungranser", "Kommungränser", value = FALSE)
    )
  })

  # ---- Statiska plots & kommunkartor ----
  # Alla anrop går genom make_cykel_barplot / make_kommunkarta,
  # och alla andelar beräknas mot sum(bef) i respektive dataset.
  # bindCache() gör att beräkningen bara sker EN gång totalt
  # (delat mellan alla användare), inte per ny session.

  # Grundskola – cykelbara vägar (avstånd)
  output$barplot_grund <- renderGirafe({
    make_cykel_barplot(skola_cykel_stat, "avstand_m", granser_km, etiketter_km,
                       legend_titel = "Avståndsgräns",
                       skolform = "grundskolan", kommuner = kommuner, zoom_max = 1)
  }) %>% bindCache("barplot_grund")

  output$kommunkarta_grund <- renderLeaflet({
    make_kommunkarta(skola_cykel_stat, "avstand_m", granser_km, etiketter_km,
                     legend_titel = "Andel inom 5 km",
                     skolform = "grundskolan", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_grund")

  # Grundskola – alla vägar (avstånd)
  output$barplot_grund_all <- renderGirafe({
    make_cykel_barplot(skola_nvdb_stat, "avstand_m", granser_km, etiketter_km,
                       legend_titel = "Avståndsgräns",
                       skolform = "grundskolan", kommuner = kommuner, zoom_max = 1)
  }) %>% bindCache("barplot_grund_all")

  output$kommunkarta_grund_all <- renderLeaflet({
    make_kommunkarta(skola_nvdb_stat, "avstand_m", granser_km, etiketter_km,
                     legend_titel = "Andel inom 5 km",
                     skolform = "grundskolan", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_grund_all")

  # Gymnasium – cykelbara vägar (tid, cykel)
  output$barplot_gym <- renderGirafe({
    make_cykel_barplot(skola_cykel_stat, "cost_cykel_min", granser_min, etiketter_min,
                       legend_titel = "Tidsgräns",
                       skolform = "gymnasiet", kommuner = kommuner)
  }) %>% bindCache("barplot_gym")

  output$kommunkarta_gym <- renderLeaflet({
    make_kommunkarta(skola_cykel_stat, "cost_cykel_min", granser_min, etiketter_min,
                     legend_titel = "Andel inom 45 min",
                     skolform = "gymnasiet", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_gym")

  # Gymnasium – alla vägar (tid, cykel)
  output$barplot_gym_all <- renderGirafe({
    make_cykel_barplot(skola_nvdb_stat, "cost_cykel_min", granser_min, etiketter_min,
                       legend_titel = "Tidsgräns",
                       skolform = "gymnasiet", kommuner = kommuner)
  }) %>% bindCache("barplot_gym_all")

  output$kommunkarta_gym_all <- renderLeaflet({
    make_kommunkarta(skola_nvdb_stat, "cost_cykel_min", granser_min, etiketter_min,
                     legend_titel = "Andel inom 45 min",
                     skolform = "gymnasiet", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_gym_all")

  # Arbete cykel – cykelbara vägar (tid, cykel)
  output$barplot_arb <- renderGirafe({
    make_cykel_barplot(arbete_cykel_stat, "cost_cykel_min", granser_min, etiketter_min,
                       legend_titel = "Tidsgräns", kommuner = kommuner)
  }) %>% bindCache("barplot_arb")

  output$kommunkarta_arb <- renderLeaflet({
    make_kommunkarta(arbete_cykel_stat, "cost_cykel_min", granser_min, etiketter_min,
                     legend_titel = "Andel inom 45 min", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_arb")

  # Arbete cykel – alla vägar (tid, cykel)
  output$barplot_arb_all <- renderGirafe({
    make_cykel_barplot(arbete_nvdb_stat, "cost_cykel_min", granser_min, etiketter_min,
                       legend_titel = "Tidsgräns", kommuner = kommuner)
  }) %>% bindCache("barplot_arb_all")

  output$kommunkarta_arb_all <- renderLeaflet({
    make_kommunkarta(arbete_nvdb_stat, "cost_cykel_min", granser_min, etiketter_min,
                     legend_titel = "Andel inom 45 min", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_arb_all")

  # Arbete elcykel – cykelbara vägar (tid, elcykel)
  output$barplot_arb_elcykel <- renderGirafe({
    make_cykel_barplot(arbete_cykel_stat, "cost_elcykel_min", granser_min, etiketter_min,
                       legend_titel = "Tidsgräns", kommuner = kommuner)
  }) %>% bindCache("barplot_arb_elcykel")

  output$kommunkarta_arb_elcykel <- renderLeaflet({
    make_kommunkarta(arbete_cykel_stat, "cost_elcykel_min", granser_min, etiketter_min,
                     legend_titel = "Andel inom 45 min", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_arb_elcykel")

  # Arbete elcykel – alla vägar (tid, elcykel)
  output$barplot_arb_elcykel_all <- renderGirafe({
    make_cykel_barplot(arbete_nvdb_stat, "cost_elcykel_min", granser_min, etiketter_min,
                       legend_titel = "Tidsgräns", kommuner = kommuner)
  }) %>% bindCache("barplot_arb_elcykel_all")

  output$kommunkarta_arb_elcykel_all <- renderLeaflet({
    make_kommunkarta(arbete_nvdb_stat, "cost_elcykel_min", granser_min, etiketter_min,
                     legend_titel = "Andel inom 45 min", kommuner = kommuner)
  }) %>% bindCache("kommunkarta_arb_elcykel_all")

  # ---- DeSO drill-down (samtliga kommunkartor) ----
  bind_kommunkarta_deso("kommunkarta_grund", skola_cykel_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "avstand_m", granser = granser_km, etiketter = etiketter_km,
                        skolform = "grundskolan")

  bind_kommunkarta_deso("kommunkarta_grund_all", skola_nvdb_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "avstand_m", granser = granser_km, etiketter = etiketter_km,
                        skolform = "grundskolan")

  bind_kommunkarta_deso("kommunkarta_gym", skola_cykel_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "cost_cykel_min", granser = granser_min, etiketter = etiketter_min,
                        skolform = "gymnasiet")

  bind_kommunkarta_deso("kommunkarta_gym_all", skola_nvdb_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "cost_cykel_min", granser = granser_min, etiketter = etiketter_min,
                        skolform = "gymnasiet")

  bind_kommunkarta_deso("kommunkarta_arb", arbete_cykel_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "cost_cykel_min", granser = granser_min, etiketter = etiketter_min)

  bind_kommunkarta_deso("kommunkarta_arb_all", arbete_nvdb_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "cost_cykel_min", granser = granser_min, etiketter = etiketter_min)

  bind_kommunkarta_deso("kommunkarta_arb_elcykel", arbete_cykel_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "cost_elcykel_min", granser = granser_min, etiketter = etiketter_min)

  bind_kommunkarta_deso("kommunkarta_arb_elcykel_all", arbete_nvdb_stat, deso_niva,
                        input = input, session = session,
                        varde_kol = "cost_elcykel_min", granser = granser_min, etiketter = etiketter_min)

  # ---- Sammanfattande siffror för länet ----
  output$start_stats_ui <- renderUI({
    arbete_pct     <- startsida_stat(arbete_cykel_stat, "cost_cykel_min", granser_min,
                                     grans = 15)
    grundskola_pct <- startsida_stat(skola_cykel_stat, "avstand_m", granser_km,
                                     grans = 2000, skolform = "grundskolan")
    gymnasium_pct  <- startsida_stat(skola_cykel_stat, "cost_cykel_min", granser_min,
                                     grans = 15, skolform = "gymnasiet")

    vardevbox_tema = bslib::value_box_theme(bg = "var(--rd-accent-soft)", fg = "var(--rd-primary-dark)")

    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        value = paste0(arbete_pct, "%"),
        title = NULL,
        "av sysselsatta når sin arbetsplats inom 15 min med cykel",
        theme = vardevbox_tema
      ),
      bslib::value_box(
        value = paste0(grundskola_pct, "%"),
        title = NULL,
        "av grundskoleelever når sin skola inom 2 km med cykel",
        theme = vardevbox_tema
      ),
      bslib::value_box(
        value = paste0(gymnasium_pct, "%"),
        title = NULL,
        "av gymnasieelever når sin skola inom 15 min med cykel",
        theme = vardevbox_tema
      )
    )
  }) %>% bindCache("start_stats_ui")
})
