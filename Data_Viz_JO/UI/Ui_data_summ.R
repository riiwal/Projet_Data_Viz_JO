library(shiny)
library(DT)

tagList(
  navset_card_tab(
    nav_panel(
      title = "Description des données",
      fluidRow(
        column(
          4,
          selectInput("theme", "Choisissez un thème pour le résumé :",
                      choices = c("Démographie", "Activité","Education","CSP","Menages", "Logement", "Transport", "Accès", "Economie", "Votes"),
                      selected = "Démographie")
        ),
        column(
          8,
          card(
            card_header("description des variables"),
            tableOutput("summary")
          )
        )
      )
    ),
    nav_panel(title = "Données",
              fluidRow(
                column(
                  12,
                  card(DTOutput("table")
                  )
                )
              ))
  )
)