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
  meny_namn <- unique(sapply(konfig_lista, `[[`, "menu"))

  lapply(meny_namn, function(meny) {
    paneler <- Filter(function(k) k$menu == meny, konfig_lista)

    bslib::nav_panel(
      title = meny,
      value = meny,
      bslib::navset_pill(
        id = inre_flik_id(meny),
        !!!lapply(paneler, cykel_panel_ui)
      )
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
          "C1, C2, C3, G1 och G2. Begreppet cykelbarhet avser här endast",
          "analysens definition av vilka vägar som bedömts som cykelbara.",
          "Faktisk cykelbarhet kan variera beroende på individuella förutsättningar och lokala förhållanden."
        )),
        p(paste(
          "Cykelklassningen baseras på ett antal variabler i NVDB (Nationella vägdatabasen) som väglänkens slitlager,",
          "hastighet, vägtyp, årsmedelsdygnstrafik, bredd samt väghållare."
        )),
        p(paste(
          "Ruttanalyserna utgår från SCB:s data om var befolkningen bor och går i skola eller arbetar."
        )),
        p(paste(
          "Av sekretesskäl visas inte enstaka vägavsnitt med väldigt få resande ",
          "som leder fram till en enskild start- eller målpunkt."
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
        "Cykelklassningen är framarbetad med stöd i denna ",
        tags$i(
          tags$a(
            href = "https://www.diva-portal.org/smash/record.jsf?pid=diva2%3A2003569&dswid=-6569",
            "rapport kring cykelbarhetsklassificering",
            target = "_blank"
          )
        ),
        "och Trafikverkets rapport ",
        tags$i(
          tags$a(
            href = "https://bransch.trafikverket.se/for-dig-i-branschen/Planera-och-utreda/samhallsplanering/planera-for-transporter-i-samhallsplaneringen/Personresor/cykel-i-samhallsplaneringen/Cykelleder-for-rekreation-och-turism/",
            "Cykelleder för rekreation och turism",
            target = "_blank"
          )
        ),
        "."
      )
    )
  )
}


# ------------------------------------------------------------------
# Om metoden (sista fliken). value = "om" måste behållas: conditionalPanel,
# expand.js och server.R använder "om" för att dölja den delade kartan.
# ------------------------------------------------------------------
om_panel_ui <- function() {
  bslib::nav_panel(
    title = "Om metoden",
    value = "om",

    div(
      id = "om_hero",

      h2("Om metoden"),
      p(
        class = "lead-in",
        paste(
          "Här beskrivs kort hur cykelpotentialen har beräknats och var du kan vända dig med frågor."
        )
      ),

      h2("Så är analysen gjord"),
      div(
        class = "text-box",
        shiny::tags$ol(
          class = "om-lista",
          shiny::tags$li(paste(
            "Utgångspunkten är SCB:s data om var befolkningen bor och var de går i skola",
            "eller arbetar."
          )),
          shiny::tags$li(paste(
            "Vägnätet kommer från NVDB (Nationella vägdatabasen). Varje väglänk har klassats efter hur lämplig den är",
            "att cykla på, utifrån bland annat hastighet, trafikmängd, vägtyp, underlag",
            "och väghållare."
          )),
          shiny::tags$li(paste(
            "För varje relation mellan bostad och skola eller arbetsplats beräknas den",
            "kortaste vägen i vägnätet."
          )),
          shiny::tags$li(paste(
            "Rutterna som ryms inom vald tids- eller avståndsgräns räknas samman till antal",
            "potentiella passager per väglänk, och till andelar per kommun och DeSO."
          )),
          shiny::tags$li(paste(
            "Väglänkar där enstaka resenärer skulle kunna identifieras döljs",
            "(se frågan om sekretess nedan)."
          ))
        )
      ),

      h2("Frågor och svar"),
      bslib::accordion(
        open = FALSE,

        bslib::accordion_panel(
          title = "Visar resultatet hur många som faktiskt cyklar?",
          paste(
            "Nej. Cykelpotentialen är en möjlighetsanalys: den visar hur många som skulle",
            "kunna nå sitt mål med cykel eller elcykel inom en viss tid eller ett visst",
            "avstånd, givet att de väljer den kortaste vägen. Den visar var förutsättningarna",
            "är goda och var vägnätet sätter gränser, inte hur människor reser idag."
          )
        ),

        bslib::accordion_panel(
          title = "Vad är skillnaden mellan Cykelbara vägar och Alla vägar?",
          paste(
            "Under Alla vägar får rutterna använda hela vägnätet. Under Cykelbara vägar får",
            "de bara använda ett urval av vägklasser som bedömts som lämpliga att cykla på,",
            "och vägar med hög hastighet eller mycket trafik är bortsorterade. Jämförelsen",
            "visar hur mycket av potentialen som beror på vägarnas utformning snarare än",
            "på avståndet."
          )
        ),

        bslib::accordion_panel(
          title = "Vilka hastigheter har antagits?",
          paste(
            "Schablonhastigheter: 10 km/h för skolelever, och för arbetspendling 16 km/h med",
            "cykel och 22 km/h med elcykel. Cykel och elcykel får samma rutt, bara restiden",
            "skiljer. Grundskolans gränser anges i avstånd (2, 3 och 5 km), övriga i tid",
            "(15, 30 och 45 minuter)."
          )
        ),

        bslib::accordion_panel(
          title = "Vad är en passage?",
          paste(
            "Antalet personer vars kortaste rutt, inom vald gräns, går över väglänken.",
            "Varje person räknas högst en gång per länk. Siffran visar alltså hur många som",
            "potentiellt skulle kunna använda länken, inte hur många som gör det."
          )
        ),

        bslib::accordion_panel(
          title = "Hur räknas andelarna i diagram och kommunkartor?",
          paste(
            "Andelen är antalet personer som når målet inom gränsen, delat med alla personer",
            "i området. De som inte får någon rutt ingår i nämnaren. Diagrammen och kommunkartorna påverkas inte av att enskilda",
            "väglänkar döljs på den delade kartan."
          )
        ),

        bslib::accordion_panel(
          title = "Varför döljs vissa vägavsnitt på kartan? (sekretess)",
          tagList(
            p(paste(
              "Visas en väglänk med mycket få resenärer som leder fram till en enskild start-",
              "eller målpunkt, till exempel en återvändsgata, kan man ibland räkna ut vilka",
              "som bor eller arbetar där. Därför döljs sådana länkar."
            )),
            p(paste(
              "Det görs stegvis: en länk i en återvändsgränd med färre än 10 passager döljs,",
              "och om nästa länk i kedjan då också har färre än 10 döljs även den. Så fortsätter",
              "det tills en länk med minst 10 passager nås, som får synas. Därtill kan en",
              "angränsande länk med färre än 19 passager döljas, eftersom skillnaden mot den",
              "dolda länken annars kan vara mycket liten."
            )),
            p(paste(
              "Länkar mitt i ett sammanhängande nät döljs inte bara för att få resenärer",
              "passerar där. Metoden är egenutvecklad och tillämpas för varje karta för sig,",
              "så en länk kan vara synlig på en karta och dold på en annan."
            ))
          )
        ),

        bslib::accordion_panel(
          title = "Varför saknas rutter fram till en skola eller arbetsplats som ligger nära bostäder?",
          paste(
            "Under Cykelbara vägar används bara vägar som klassats som lämpliga. Om den enda",
            "förbindelsen till en skola eller arbetsplats går via en väg som inte klassats så,",
            "hittas ingen rutt och antalet passager blir noll, även om målet ligger nära",
            "bostäderna. Jämför med fliken Alla vägar. Skillnaden kan peka på en brist i",
            "vägnätet, men också på att en väg klassats för strängt."
          )
        ),

        bslib::accordion_panel(
          title = "Hur tillförlitlig är klassningen av vägarna?",
          paste(
            "Klassningen bygger på uppgifter i NVDB och är känslig både för var gränsvärdena",
            "dras och för hur uppgifterna har rapporterats in. Den ger en bra bild på regional",
            "nivå, men enskilda väglänkar kan vara felklassade. Lokalkännedom är värdefull",
            "för att hitta sådana fel."
          )
        ),

        bslib::accordion_panel(
          title = "Vad tas inte hänsyn till?",
          paste(
            "Bland annat lutning, korsningar och passager, väder och årstid, belysning och underhåll,",
            "samt hur trygg eller trevlig en väg upplevs."
          )
        )
      ),

      h2("Frågor och synpunkter"),
      div(
        class = "text-box",
        p(
          "Kontakta Region Dalarna, Samhällsanalys: ",
          shiny::tags$a(
            href = "mailto:samhallsanalys@regiondalarna.se",
            "samhallsanalys@regiondalarna.se"
          )
        )
      )
    )
  )
}


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
            bygg_nav_menyer(panel_konfig),
            list(om_panel_ui())
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
