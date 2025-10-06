tagList(
  fluidRow(
    column(
      6,
      card(class = "mycard",
           card_header("Individus"), card_body(plotOutput("PCA_ind")))
    ),
    column(
      6,
      card(class = "mycard",
           card_header("Varibles"),
           card_body(plotOutput("PCA_var")))
    )))
#   
#   navset_card_tab(
#     nav_panel(
#       title = "Graphe analyse en composantes principales",
#       fluidRow(column(
#         6,
#         card(
#             class = "plot-card",
#             card_body()
#       )),
#         column(
#         6,
#         card(
#           class = "plot-card",
#           card_header("Varibles"),
#           card_body(plotOutput("PCA_var"))
#       ))))
#   )
# )