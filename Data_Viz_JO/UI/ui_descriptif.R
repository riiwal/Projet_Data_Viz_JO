tagList(
  navset_card_tab(
    
    # --- Résultats des élections ---
    nav_panel(
      title = "Résultats des élections",
      column(
        width = 10, offset = 1,
        fluidRow(
          column(
            6,
            card(card_body(plotOutput("carteparticipation", height = "320px")))
          ),
          column(
            6,
            card(card_body(plotOutput("cartegagnant", height = "320px")))
          )
        ),
        br(), br(),
        fluidRow(
          column(
            12,
            card(card_body(plotOutput("histogrammeresultat", height = "340px")))
          )
        )
      )
    ),
    
    # --- Analyse Socio-Démographique ---
    nav_panel(
      title = "Analyse Socio-Démographique",
      column(
        width = 10, offset = 1,
        
        h2("Habitat & ancrage territorial", align = "center"),
        fluidRow(
          column(
            6,
            card(card_body(plotOutput("histogrammehabitat", height = "300px")))
          ),
          column(
            6,
            card(card_body(plotOutput("histoAAV", height = "300px")))
          )
        ),
        
        br(), br(),
        h2("Structure sociale & diplômes", align = "center"),
        fluidRow(
          column(
            6,
            card(card_body(plotOutput("bar_diplomes", height = "300px")))
          ),
          column(
            6,
            card(card_body(plotOutput("bar_csp", height = "300px")))
          )
        ),
        
        br(), br(),
        h2("Modes de vie & mobilités", align = "center"),
        fluidRow(
          column(
            6,
            card(card_body(plotOutput("bar_menages", height = "300px")))
          ),
          column(
            6,
            card(card_body(plotOutput("bar_mobilites", height = "300px")))
          )
        ),
        
        br(), br(),
        h2("Conditions de vie & accès aux services", align = "center"),
        fluidRow(
          column(
            4,
            card(card_body(plotOutput("bar_logement", height = "280px")))
          ),
          column(
            4,
            card(card_body(plotOutput("bar_acc_education", height = "280px")))
          ),
          column(
            4,
            card(card_body(plotOutput("bar_acc_soins", height = "280px")))
          )
        )
      )
    )
  )
)
