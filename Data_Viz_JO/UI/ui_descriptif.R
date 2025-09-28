tagList(
  navset_card_tab(
    nav_panel(
      title = "Résultats des élections",
      fluidRow(
        column(
          6,
          card(
            class = "plot-card",
            card_header("Participation"),
            card_body(plotOutput("carteparticipation"))
          )
        ),
        column(
          6,
          card(
            card_header("Gagnant par circonscription"),
            plotOutput("cartegagnant")
          )
        )
      ),
      fluidRow(
        column(
          12,
          card(
            card_header("Résultats nationaux (pondérés)"),
            plotOutput("histogrammeresultat")
          )
        )
      )
    ),
    nav_panel(title = "Analyse Socio-Démographique")
  )
)
