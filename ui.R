# =============================================================
# ui.R  –  Cykelpotential Dalarna
# =============================================================

source('global.R')

# ------------------------------------------------------------------
# Konfiguration för alla analysflikar
# (single source of truth – lägg till/ta bort flikar här)
# ------------------------------------------------------------------
panel_konfig <- list(

  list(
    menu    = "Grundskola",
    title   = "Cykelbara vägar",
    value   = "skola_cykel_grund",
    info    = paste(
      "Analys över cykelpotentialen för grundskoleelever på utvalt vägnät.",
      "Kartan och statistiken utgår från det för grundskola samt gymnasium",
      "utvalda cykelbara vägnätet vilket består av klasserna",
      "'B1', 'C1', 'C2', 'C3', 'G1' och 'G2'."
    ),
    bar_header  = "Andel som når grundskolan på cykel per kommun (cykelbara vägar)",
    karta_header = "Andel som når grundskolan på cykel inom 5 km",
    bar_id      = "barplot_grund",
    karta_id    = "kommunkarta_grund"
  ),

  list(
    menu    = "Grundskola",
    title   = "Alla vägar",
    value   = "skola_cykel_grund_all",
    info    = paste(
      "Analys över cykelpotentialen för grundskoleelever.",
      "Kartan och statistiken utgår från den potentiella cykelpendlingen",
      "när alla vägar får användas."
    ),
    bar_header  = "Andel som når grundskolan på cykel per kommun (alla vägar)",
    karta_header = "Andel som når grundskolan på cykel inom 5 km",
    bar_id      = "barplot_grund_all",
    karta_id    = "kommunkarta_grund_all"
  ),

  list(
    menu    = "Gymnasium",
    title   = "Cykelbara vägar",
    value   = "skola_cykel_gym",
    info    = paste(
      "Analys över cykelpotentialen för gymnasieelever.",
      "Kartan och statistiken utgår från det för grundskola samt gymnasium",
      "utvalda cykelbara vägnätet vilket består av klasserna",
      "'B1', 'C1', 'C2', 'C3', 'G1' och 'G2'."
    ),
    bar_header  = "Andel som når gymnasiet på cykel per kommun (cykelbara vägar)",
    karta_header = "Andel som når gymnasiet på cykel",
    bar_id      = "barplot_gym",
    karta_id    = "kommunkarta_gym"
  ),

  list(
    menu    = "Gymnasium",
    title   = "Alla vägar",
    value   = "skola_cykel_gym_all",
    info    = paste(
      "Analys över cykelpotentialen för gymnasieelever.",
      "Kartan och statistiken utgår från den potentiella cykelpendlingen",
      "när alla vägar får användas."
    ),
    bar_header  = "Andel som når gymnasiet på cykel per kommun (alla vägar)",
    karta_header = "Andel som når gymnasiet på cykel",
    bar_id      = "barplot_gym_all",
    karta_id    = "kommunkarta_gym_all"
  ),

  list(
    menu    = "Arbete - cykel",
    title   = "Cykelbara vägar",
    value   = "arbete_cykel",
    info    = paste(
      "Analys över cykelpotentialen för sysselsatta till sin arbetsplats med cykel.",
      "Kartan och statistiken utgår från den potentiella cykelpendlingen",
      "när endast utvalda cykelklasser för arbetpendling får användas.",
      "Cykelklasserna 'B1'–'B4', 'C1'–'C3', 'G1' och 'G2' ingår."
    ),
    bar_header  = "Andel som når arbetet på cykel per kommun (cykelbara vägar)",
    karta_header = "Andel som når arbetet på cykel",
    bar_id      = "barplot_arb",
    karta_id    = "kommunkarta_arb"
  ),

  list(
    menu    = "Arbete - cykel",
    title   = "Alla vägar",
    value   = "arbete_cykel_all",
    info    = paste(
      "Analys över cykelpotentialen för sysselsatta till sin arbetsplats med cykel.",
      "Kartan och statistiken utgår från den potentiella cykelpendlingen",
      "när hela vägnätet får användas."
    ),
    bar_header  = "Andel som når arbetet på cykel per kommun (alla vägar)",
    karta_header = "Andel som når arbetet på cykel",
    bar_id      = "barplot_arb_all",
    karta_id    = "kommunkarta_arb_all"
  ),

  list(
    menu    = "Arbete - elcykel",
    title   = "Cykelbara vägar",
    value   = "arbete_elcykel",
    info    = paste(
      "Analys över cykelpotentialen för sysselsatta till sin arbetsplats med elcykel.",
      "Kartan och statistiken utgår från den potentiella elcykelpendlingen",
      "när endast utvalda cykelklasser får användas.",
      "Cykelklasserna 'B1'–'B4', 'C1'–'C3', 'G1' och 'G2' ingår."
    ),
    bar_header  = "Andel som når arbetet på elcykel per kommun (cykelbara vägar)",
    karta_header = "Andel som når arbetet på elcykel",
    bar_id      = "barplot_arb_elcykel",
    karta_id    = "kommunkarta_arb_elcykel"
  ),

  list(
    menu    = "Arbete - elcykel",
    title   = "Alla vägar",
    value   = "arbete_elcykel_all",
    info    = paste(
      "Analys över cykelpotentialen för sysselsatta till sin arbetplats med elcykel.",
      "Kartan och statistiken utgår från att hela vägnätet får användas."
    ),
    bar_header  = "Andel som når arbetet på elcykel per kommun (alla vägar)",
    karta_header = "Andel som når arbetet på elcykel",
    bar_id      = "barplot_arb_elcykel_all",
    karta_id    = "kommunkarta_arb_elcykel_all"
  )
)


# ------------------------------------------------------------------
# Hjälpfunktion: bygger ett analyspanel (bar + kommunkarta)
# ------------------------------------------------------------------
cykel_panel_ui <- function(cfg) {
  bslib::nav_panel(
    title = cfg$title,
    value = cfg$value,

    div(
      id    = paste0("plot_container_", cfg$value),
      class = "plot-container-cykelklass",

      bslib::card(cfg$info),

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(cfg$bar_header),
          girafeOutput(cfg$bar_id, height = "500px")
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(cfg$karta_header),
          leafletOutput(cfg$karta_id, height = "500px"),
          bslib::card_footer(
            shiny::tags$div(
              class = "alert alert-info d-flex align-items-center gap-2 mb-0",
              role  = "alert",
              style = "font-size: 0.8rem; padding: 6px 10px;",
              shiny::tags$i(class = "bi bi-info-circle-fill"),
              shiny::tags$span(
                style = "font-style: italic;",
                "Klicka på en kommun för att visa tillgänglighet på DeSO-nivå."
              )
            )
          )
        )
      )
    )
  )
}


# ------------------------------------------------------------------
# Hjälpfunktion: bygger nav_menu med tillhörande nav_panels
# ------------------------------------------------------------------
bygg_nav_menyer <- function(konfig_lista) {
  # Gruppera efter menu-namn och bevara ordning
  meny_namn <- unique(sapply(konfig_lista, `[[`, "menu"))

  lapply(meny_namn, function(meny) {
    paneler <- Filter(function(k) k$menu == meny, konfig_lista)
    do.call(
      bslib::nav_menu,
      c(list(meny), lapply(paneler, cykel_panel_ui))
    )
  })
}


# ------------------------------------------------------------------
# Delad karta (visas på alla flikar utom Start och Om)
# ------------------------------------------------------------------
delad_karta_ui <- function() {
  div(
    id = "global_map_wrapper",
    conditionalPanel(
      condition = "!['start', 'om'].includes(input.nav)",
      div(
        id    = "global_map_container",
        class = "map-container-cykelklass",
        leafletOutput("delad_karta", width = "100%", height = "100%"),
        actionButton("expand_karta", "⤢ Maximera karta", class = "map-expand-btn"),
        absolutePanel(
          id        = "map_controls",
          draggable = TRUE,
          class     = "map-controls-panel",
          uiOutput("map_controls_ui")
        )
      )
    )
  )
}


# ------------------------------------------------------------------
# Startsida
# ------------------------------------------------------------------
start_panel_ui <- function() {
  bslib::nav_panel(
    title = "Start",
    value = "start",

    div(
      id = "start_hero",
      h1("Cykelpotentialstudie för Dalarnas län"),
      uiOutput("start_stats_ui"),

      p(
        class = "lead-in",
        paste(
          "Cykelpotentialen utgör ett mått på hur många som potentiellt sett skulle kunna",
          "ta sig till sin skola eller arbetsplats med cykel eller elcykel inom valda",
          "tids-/avståndsgränser givet att de väljer den absolut kortaste vägen.",
          "Således visar inte cykelpotentialen den faktiska pendlingen."
        )
      ),

      div(
        class = "text-box",
        p(paste(
          "Ruttanalyserna som ligger till grund för cykelpotentialen har gjorts på antingen",
          "hela vägnätet eller på ett urval av vägnätet som bedömts vara mer eller mindre",
          "cykelbart. Vägnätet har klassificerats i cykelklasserna B1, B2, B3, B4, B5,",
          "C1, C2, C3, G1 och G2."
        )),
        p(paste(
          "Cykelklassningen baseras på ett antal variabler i NVDB som väglänkens slitlager,",
          "hastighet, vägtyp, årsmedelsdygnstrafik, bredd samt väghållare."
        )),
        p(paste(
          "Ruttanalyserna utgår från SCB:s data om var befolkningen bor och går i skola eller arbetar."
        )),
      ),

      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = "B (B1–B5) · Blandtrafik",
          paste(
            "Vägar i blandtrafik där B1 är de vägar med lägst hastighet och/eller låg",
            "trafikering. B5 är sådana vägar som inte anses cykelbara, som motorvägar",
            "eller vägar med hastigheter som överstiger 90 km/h."
          )
        ),
        bslib::accordion_panel(
          title = "C (C1–C3) · Separat cykelyta",
          paste(
            "Vägar där cyklister har en egen separat yta att färdas på. C1 utgör de",
            "flesta GC-vägar, C2 utgör GC-vägar med grusunderlag och C3 utgör",
            "gatupassager utan utmärkning."
          )
        ),
        bslib::accordion_panel(
          title = "G (G1–G2) · Grusvägar",
          "Grusvägar där G1 är grusvägar med driftsbidrag och G2 de utan driftsbidrag."
        )
      ),

      p(
        class = "source-note",
        HTML("Cykelklassningen har utgått från Tyréns modell och Trafikverkets rapport <i>Cykelkleder för rekreation och turism</i>.")
      )
    )
  )
}


# ------------------------------------------------------------------
# Om-sida
# ------------------------------------------------------------------
# om_panel_ui <- function() {
#   bslib::nav_panel(
#     title = "Om",
#     value = "om",
#
#     shiny::div(
#       id = "om_hero",
#
#       h2("Vad är cykelpotential?"),
#       p(
#         class = "lead-in",
#         paste(
#           "Cykelpotentialen visar hur många invånare i Dalarnas län som skulle kunna nå",
#           "sin skola eller arbetsplats med cykel eller elcykel inom en given tid eller ett",
#           "givet avstånd, om de cyklade den kortaste möjliga vägen. Måttet bygger på",
#           "faktiska avstånd och restider i vägnätet — inte på hur människor faktiskt",
#           "reser idag."
#         )
#       ),
#       p(
#         class = "text-box",
#         HTML(paste0(
#           "Cykelpotentialen är alltså en <b>möjlighetsanalys</b>, inte en pendlingsstatistik: ",
#           "den visar var förutsättningarna för cykling redan är goda, och var infrastrukturen ",
#           "sätter gränser för vad som är realistiskt."
#         ))
#       ),
#
#       h2("Varför har rapporten tagits fram?"),
#       shiny::div(
#         class = "text-box",
#         p("Den här analysen ger ett gemensamt underlag för att:"),
#         shiny::tags$ul(
#           class = "om-lista",
#           shiny::tags$li("identifiera var satsningar på cykelinfrastruktur skulle göra störst skillnad,"),
#           shiny::tags$li("följa upp hur förändringar i vägnätet (nya cykelvägar, uppgraderad standard) påverkar tillgängligheten över tid,"),
#           shiny::tags$li("skilja på potentialen längs hela vägnätet och den mer försiktiga bilden man får om man bara räknar utpekat cykelbara vägar.")
#         ),
#         p(paste(
#           "Genom att beräkna potentialen både för hela vägnätet och för ett urval av",
#           "cykelklassade vägar blir det tydligt var bristande infrastruktur — snarare",
#           "än avstånd i sig — är det som begränsar cyklingen."
#         ))
#       )
#     )
#   )
# }


# ------------------------------------------------------------------
# Huvud-UI
# ------------------------------------------------------------------
shinyUI(
  fluidPage(
    theme = bslib::bs_theme(version = 5),

    shiny::tags$head(
      shiny::tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      shiny::tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      shiny::tags$link(rel = 'stylesheet', type = 'text/css', href = 'app.css'),
      shiny::tags$script(src = 'expand.js'),
      shiny::tags$link(
        rel  = 'stylesheet',
        href = 'https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css'
      )
    ),

    # ---- Global spinner-overlay ----------------------------------------
    shiny::tags$div(
      id    = "app_spinner_overlay",
      class = "app-spinner-overlay",
      shiny::tags$div(class = "app-spinner")
    ),

    # ---- Header (matchar .rd-header i regiondalarna_ruf.css) --------------
    shiny::tags$div(
      class = 'rd-header',
      shiny::tags$div(class = 'rd-header__title', 'Cykelpotential'),
      shiny::tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        shiny::tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna'),
        shiny::tags$span('Samhällsanalys')
      )
    ),

    shiny::tags$div(
      id = "rd-content-row",
      shiny::tags$div(
        id = "rd-tabs-wrapper",
        do.call(
          bslib::navset_tab,
          c(
            list(id = "nav"),
            list(start_panel_ui()),
            bygg_nav_menyer(panel_konfig)
            # list(om_panel_ui())
          )
        )
      ),
      delad_karta_ui()
    ),

    # ---- Footer (matchar .rd-footer i regiondalarna_ruf.css) --------------
    shiny::tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      shiny::tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
