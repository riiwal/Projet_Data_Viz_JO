tagList(
  navset_card_tab(
    nav_panel(title = "Arbre gagnant",
              fluidRow(
                column(
                  12,
                  card(
                    visNetworkOutput("tree")
                  )
                )
              )),
    nav_panel(
      title = "Arbre par candidat",
      fluidRow(
        column(
          4,
          selectInput(
            inputId = "cand",
            label = "Choisissez le candidat :",
            choices = c(
              "Nathalie Arthaud" = "Arthaud_exp",
              "Fabrice Roussel" = "Roussel_exp",
              "Emmanuel Macron" = "Macron_exp",
              "Jean Lassalle" = "Lassalle_exp",
              "Marine Le Pen" = "LePen_exp",
              "Eric Zemmour" = "Zemmour_exp",
              "Jean-Luc Mélenchon" = "Melenchon_exp",
              "Anne Hidalgo" = "Hidalgo_exp",
              "Yannick Jadot" = "Jadot_exp",
              "Valérie Pécresse" = "Pecresse_exp",
              "Philippe Poutou" = "Poutou_exp",
              "Nicolas Dupont-Aignan" = "DupontAignan_exp"
            ),
            selected = "Macron_exp"
          )
        ),
        column(
          8,
          card(
            visNetworkOutput("tree_cand")
          )
        )
      )
      
    )
  )
)