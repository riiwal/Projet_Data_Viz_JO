tagList(
  navset_card_tab(
    nav_panel(
      title = "Graphe des individus",
      fluidRow(column(
        12,
        card(
            class = "plot-card",
            card_header("Individus"),
            card_body(plotOutput("PCA_ind"))
      ))
        )
    ),
    nav_panel(
      title = "graphe des variables",
      fluidRow((column(
        12,
        card(
          class = "plot-card",
          card_header("Varibles"),
          card_body(plotOutput("PCA_var"))
      ))))
  )
))