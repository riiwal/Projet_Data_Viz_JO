tagList(
  tags$head(tags$style(HTML("
    #socdem .section-title { text-align:center; margin: 28px 0 16px 0; font-weight:700; }
    #socdem .block-space   { margin-bottom: 28px; }
    #socdem .mycard        { border:0; border-radius:18px; box-shadow:0 6px 24px rgba(16,24,40,.06); }
    #socdem .mycard .card-body { padding: 18px; }
  "))),
  
  navset_card_tab(
    nav_panel(
      title = "Résultats des élections",
      div(id = "socdem",
          column(width = 10, offset = 1,
                 div(class = "block-space",
                     fluidRow(
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("carteparticipation", height = "320px")))
                       ),
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("cartegagnant", height = "320px")))
                       )
                     )
                 ),
                 fluidRow(
                   column(
                     12,
                     card(class = "mycard", card_body(plotOutput("histogrammeresultat", height = "340px")))
                   )
                 )
          )
      )
    ),
    nav_panel(
      title = "Analyse Socio-Démographique",
      div(id = "socdem",
          column(width = 10, offset = 1,
                 h2(class = "section-title", "Habitat & ancrage territorial"),
                 div(class = "block-space",
                     fluidRow(
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("histogrammehabitat", height = "300px")))
                       ),
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("histoAAV", height = "300px")))
                       )
                     )
                 ),
                 
                 h2(class = "section-title", "Structure sociale & diplômes"),
                 div(class = "block-space",
                     fluidRow(
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("bar_diplomes", height = "300px")))
                       ),
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("bar_csp", height = "300px")))
                       )
                     )
                 ),
                 
                 h2(class = "section-title", "Modes de vie & mobilités"),
                 div(class = "block-space",
                     fluidRow(
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("bar_menages", height = "300px")))
                       ),
                       column(
                         6,
                         card(class = "mycard", card_body(plotOutput("bar_mobilites", height = "300px")))
                       )
                     )
                 ),
                 
                 h2(class = "section-title", "Conditions de vie & accès aux services"),
                 div(class = "block-space",
                     fluidRow(
                       column(
                         4,
                         card(class = "mycard", card_body(plotOutput("bar_logement", height = "280px")))
                       ),
                       column(
                         4,
                         card(class = "mycard", card_body(plotOutput("bar_acc_education", height = "280px")))
                       ),
                       column(
                         4,
                         card(class = "mycard", card_body(plotOutput("bar_acc_soins", height = "280px")))
                       )
                     )
                 )
          )
      )
    )
  )
)
